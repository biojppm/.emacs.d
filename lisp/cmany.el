;; cmany.el --- cmany integration for Emacs. -*-coding: utf-8 -*-

;; Copyright (C) 2017 Joao Paulo Magalhaes <dev@jpmag.me>

;; Author:      Joao Paulo Magalhaes <dev@jpmag.me>
;; Created:     2017-03-20
;; Version:     0.1
;; Keywords:    cmany, cmake, rtags
;; URL:         http://github.com/biojppm/cmany.el.git

;; This file is not part of GNU Emacs.

;; This file adds facilities to Emacs for interacting
;; with cmany (http://github.com/biojppm/cmany.git ).

;; This extension is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; This extension is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;; Visit <http://www.gnu.org/copyleft/gpl.html> for more information.

;;; Depends:

;; cmany.el uses facilities from projectile and rtags, if they are
;; available (via featurep). These are NOT hard dependencies.
;; cmany.el has the following hard dependencies:
(require 'term-run)

;;; Install:

;; Put this file along your Emacs-Lisp `load-path' and add the following
;; into your ~/.emacs startup file.
;;
;;      <at standards TAB position explain what lisp code is needed>
;;      (autoload 'example-install "example" "" t)
;;      (autoload 'example-mode    "example" "" t)

;;; Commentary:

;; cmany.el provides emacs integration for cmany, which is a
;; batch-build tool for cmake-based projects.

;;; Code:

(defgroup cmany nil
  "cmany customizations")

(defcustom cmany-build-dir-prefix "build/"
  "the path (relative to the project dir) under which the build
directories should be placed"
  :group 'cmany
  :type 'string
  :safe #'stringp
  )

(defcustom cmany-build-before-debug 1
  "Whether cmany should do a build before starting a debug."
  :group 'cmany
  :type 'boolean
  :safe #'booleanp
  )

(defcustom cmany-rtags-enabled 1
  "Whether cmany should announce the current build directory to rtags."
  :group 'cmany
  :type 'boolean
  :safe #'booleanp
  )

;;-----------------------------------------------------------------------------
(defvar cmany-proj-dir nil
  "The directory where the current CMakeLists.txt project is located."
  )

(defvar cmany-build-dir nil
  "The directory of the current cmany build."
  )

(defvar cmany-target ""
  "The current active target"
  )

(defvar cmany-cmd "cmany {cmd} -E -c clang++ -t Debug {projdir} {target}"
  "The command form to use when calling cmany."
  )

;;-----------------------------------------------------------------------------
;; utility functions

(defun cmany--log (fmt &rest args)
  (apply 'message fmt args)
  )

(defun cmany--visit-buffer (name)
  "Create or visit a buffer with the given name."
  (when (not (get-buffer name))
    (split-window-sensibly (selected-window))
    ;(other-window 1)
    (get-buffer-create name)
    ;;(term (getenv "SHELL"))
    )
  (switch-to-buffer-other-window name)
  )

(defun cmany--write-to-file (file data)
  "http://stackoverflow.com/a/36196312/5875572"
  (with-temp-file file
    (prin1 data (current-buffer))
    )
  )

(defun cmany--read-from-file (file symbol)
  "http://stackoverflow.com/a/36196312/5875572"
  (when (boundp symbol)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (set symbol (read (current-buffer)))
      )
    )
  )

(defun cmany--get-cmd-output (workdir cmd)
  "Run PROGRAM with ARGS and return the output in a string if it returns 0;
  otherwise return an empty string."
  (let ((d default-directory))
     (cmany--log "cmany cmd exec: %s" cmd)
     (cd workdir)
     (with-temp-buffer
       (cmany--log "cmany cmd exec at dir: %s (was at %s)" (pwd) d)
       (let ((p (call-process-shell-command cmd nil (current-buffer))))
         (cmany--log "cmany cmd return: %d" p)
         (cmany--log "cmany cmd output: %s" (buffer-string))
         (if (eq p 0)
             (progn (setq -cmcs (buffer-string)))
             (progn (setq -cmcs ""))
           )
         )
       )
     (cd d)
     -cmcs
     )
  )

(defun cmany--has-proj-dir ()
  "is cmany-proj-dir already set"
  (and (boundp 'cmany-proj-dir)
       (not (equal cmany-proj-dir nil))
       (not (string-equal 'cmany-proj-dir "")))
  )

(defun cmany--has-build-dir ()
  "is cmany-build-dir already set"
  (and (boundp 'cmany-build-dir)
       (not (equal cmany-build-dir nil))
       (not (string-equal 'cmany-build-dir "")))
  )

;;-----------------------------------------------------------------------------
(defun cmany--format-cmd (which-cmd &optional target)
  "return a formatted cmany command based on the current proj/build"
  ;; http://stackoverflow.com/a/17325791/5875572
  (let* ((cmd (replace-regexp-in-string (regexp-quote "{cmd}") which-cmd cmany-cmd nil 'literal)))
    (setq cmd (replace-regexp-in-string (regexp-quote "{projdir}") cmany-proj-dir cmd nil 'literal))
    (when (not target)
      (setq target "")
      )
    (setq cmd (replace-regexp-in-string (regexp-quote "{target}") target cmd nil 'literal))
    cmd
    )
  )

(defun cmany--default-build-command ()
  "get the cmany build command for the current proj/build/target "
  (cmany--format-cmd "build" cmany-target)
  )


;;-----------------------------------------------------------------------------
(defun cmany--get-cmany-output (cmd &rest more-args)
  (let* ((base-cmd (cmany--format-cmd cmd))
         (full-cmd (concat base-cmd more-args))
         )
    (cmany--log "cmany cmd base: %s" base-cmd)
    (cmany--log "cmany cmd full: %s" full-cmd)
    (cmany--get-cmd-output cmany-proj-dir full-cmd)
    )
  )

(defun cmany--get-cmany-lines (cmd &rest more-args)
  (let ((out (apply 'cmany--get-cmany-output cmd more-args)))
    (split-string out "\n")
    )
  )

;;-----------------------------------------------------------------------------
(defun cmany--guess-proj-dir ()
  (let ((r ""))
    ;; if projectile is available, get the current project root
    (when (featurep 'projectile)
      (setq r (projectile-expand-root ".")))
    (if (not (string-equal 'r ""))
        (progn
          ;; yep, got project root through projectile
          (cmany--log "proj dir from projectile: %s" r)
          r)
      (progn
        ;; no deal; go up in the fs tree to find CMakeLists.txt
        (setq r (locate-dominating-file (buffer-file-name) "CMakeLists.txt"))
        (if (not (string-equal 'r ""))
            (progn
              ;; yep, got project root through locate-dominating-file
              (cmany--log "proj dir from locate-dominating-file: %s" r)
              r)
          ;; otherwise, just use the current directory
          (cmany--log "proj dir from current directory: %s"
                      (file-name-directory (buffer-file-name)))
          (file-name-directory (buffer-file-name)))
        )
      )
    )
  )

(defun cmany--get-default-proj-dir ()
  (if (cmany--has-proj-dir)
      ;; if there's a current cmany-proj-dir, use it
      (progn
        (cmany--log "cmany-proj-dir already defined: %s" cmany-proj-dir)
        cmany-proj-dir)
      ;; otherwise, make a guess
    (cmany--guess-proj-dir)
    )
  )

(defun cmany--exec-prompt-build-dir ()
  (interactive)
  (let* ((pfx (concat cmany-proj-dir cmany-build-dir-prefix)) ;; the full path to the builds dir
         (bds (cmany--get-cmany-lines "show_builds"))  ;; extract the list of current builds
         (bd (car bds)) ;; pick the first
         (bdr (ido-read-directory-name "cmany build dir: " pfx bd nil bd)))
    (cmany--log "read directory: %s" bdr)
    bdr
    )
  )

;;-----------------------------------------------------------------------------
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html

(defun cmany-rtags-start ()
  "start the rtags daemon"
  (interactive)
  (when (and cmany-rtags-enabled (featurep 'rtags))
    ;;(if (not (boundp 'cmany--rtags-rdm))
    ;;    (progn
    ;;      (cmany--log "starting rdm")
    ;;      (setq cmany--rtags-rdm (start-process "cmany-rtags-rdm" "*rdm*" "rdm")))
    ;;  (progn (cmany--log "rdm is already running")))
    ;;)
    (rtags-start-process-unless-running)
    )
  )

(defun cmany-rtags-announce-build-dir (&optional dir)
  (interactive
   (list (if cmany-rtags-enabled
             (ido-read-directory-name "build dir: " cmany-build-dir)
             ""
           )
         )
   )
  (when (and cmany-rtags-enabled (featurep 'rtags))
    (cmany-rtags-start)
    (start-process (concat "cmany-rtags-rc" " " dir) "*rdm*" "rc" "-J" dir)
    )
  )

;;-----------------------------------------------------------------------------

(defun cmany--load-configs ()
  (when (or (not (boundp 'cmany--configs))
            (equal cmany--configs nil))
    (let ((fn (concat user-emacs-directory "cmany.save")))
      (if (file-exists-p fn)
          (cmany--read-from-file fn 'cmany--configs)
        (setq cmany--configs
              `((,(cmany--guess-proj-dir)
                 '(("cmany-build-dir" . "")
                   ("cmany-target" . "")
                   ("cmany-cmd" . ""))
                 ))
              )
        )
      )
    )
  )

(defun cmany--save-configs ()
  (if (or (not (boundp 'cmany--configs))
          (equal cmany--configs nil))
      (setq cmany--configs ())
    )
  (cmany--log "configs before: %s" cmany--configs)
  (let ((pc
         `(("cmany-build-dir" ,cmany-build-dir)
           ("cmany-target" ,cmany-target)
           ("cmany-cmd" ,cmany-cmd))
         )
        )
    (format "%s" pc)
    (setq cmany--configs `((,cmany-proj-dir ,pc)))
    )
  (cmany--log "configs after: %s" cmany--configs)
  (cmany--write-to-file
   (concat user-emacs-directory "cmany.save")
   cmany--configs)
  )

(defun cmany--clear-last-commands ()
  "this clears the last stored commands"
  (setq cmany--last-build "")
  (setq cmany--last-configure "")
  (setq cmany--last-debug "")
  )

;;-----------------------------------------------------------------------------
;;;###autoload
(defun cmany-set-proj-dir (&optional dir)
  "set the project dir used by cmany"
  (interactive
   (list (ido-read-directory-name
          "cmany proj dir: " (cmany--get-default-proj-dir))))
  (cmany--load-configs)
  (cmany--log "cmany set proj dir: %s" dir)
  (setq cmany-proj-dir dir)
  (cmany--clear-last)
  (cmany--save-configs)
  )

;;;###autoload
(defun cmany-set-build-dir (&optional dir)
  "set the build dir used by cmany"
  (interactive
   (list (file-name-as-directory
           (call-interactively 'cmany--exec-prompt-build-dir))))
  (cmany--load-configs)
  (cmany--log "cmany set build dir: %s" dir)
  (setq cmany-build-dir dir)
  (cmany--clear-last)
  (cmany--save-configs)
  (cmany-rtags-announce-build-dir cmany-build-dir)
  )

;;;###autoload
(defun cmany-set-cmd (&optional cmd)
  "set the cmany command form"
  (interactive
   (list (read-string "cmany command: " cmany-cmd)))
  (cmany--load-configs)
  (cmany--log "cmany set command: %s" cmd)
  (setq cmany-cmd cmd)
  (cmany--clear-last)
  (cmany--save-configs)
  )

;;;###autoload
(defun cmany-set-target (&optional tgt)
  "set the current active target for building and debugging"
  (interactive
   (list (ido-completing-read
          "cmany current target: "
          (cmany--get-cmany-lines "show_targets") nil nil cmany-target)))
  (cmany--load-configs)
  (cmany--log "cmany set target: %s" tgt)
  (setq cmany-target tgt)
  (cmany--clear-last)
  (cmany--save-configs)
  )

;;-----------------------------------------------------------------------------
;;;###autoload
(defun cmany-configure (cmd)
  (interactive
   (list
    (read-string
     "enter configure cmd: "
     (if (and (boundp 'cmany--last-configure) (not (string-equal 'cmany--last-configure "")))
         (progn cmany--last-configure)
         (progn (cmany--format-cmd "configure"))
         )
     )))
  (setq cmany--last-configure cmd)
  (let ((d default-directory))
    (cd cmany-proj-dir)
    (compile cmd)
    (cd d)
    )
  )

;;;###autoload
(defun cmany-configure-again()
  (cmany-configure cmany--last-configure)
  )

;;-----------------------------------------------------------------------------
;;;###autoload
(defun cmany-build (cmd)
  "build the current target"
  (interactive
   (list
    (read-string
     "enter build cmd: "
     (if (and (boundp 'cmany--last-build) (not (string-equal 'cmany--last-build "")))
         (progn cmany--last-build)
         (progn (cmany--default-build-command))
         )
     )))
  (setq cmany--last-build cmd)
  (let ((d default-directory))
    (cd cmany-proj-dir)
    (compile cmd)
    (cd d)
    )
  )

;;;###autoload
(defun cmany-build-again()
  (cmany-build cmany--last-build)
  )

;;-----------------------------------------------------------------------------
;;;###autoload
(defun cmany-debug (cmd)
  "debug the current target"
  (interactive
   (list
    (read-string
     "enter gdb cmd: "
     (if (and (boundp 'cmany--last-debug) (not (string-equal 'cmany--last-debug "")))
         (progn cmany--last-debug)
         (progn (format "gdb -i=mi %s" (concat cmany-build-dir cmany-target)))
         )
     )))
  (setq cmany--last-debug cmd)
  (when cmany-build-before-debug
    (cmany-build (cmany--default-build-command))
    )
  (call-interactively (gdb cmd))
  )

;;;###autoload
(defun cmany-debug-again()
  (cmany-debug cmany--last-debug)
  )

;;-----------------------------------------------------------------------------
;;;###autoload
(defun cmany-edit-cache ()
  "interactively edit the cmake cache for the current build
  directory using either ccmake or cmake-gui"
  (interactive)
  (let ((d default-directory))
    (cd cmany-build-dir)
    (if (executable-find "ccmake")
        (progn
          (cmany--log "cmany: editing cache via ccmake at %s" cmany-build-dir)
          (term-run "ccmake" (cmany--visit-buffer "*ccmake*") ".")
          )
      (progn
        (if (executable-find "cmake-gui")
            (progn (cmany--log "cmany: editing cache via cmake-gui at %s" cmany-build-dir)
                   (start-process "cmake-gui" nil "cmake-gui" "."))
            (progn (cmany--log "ERROR: cmany: could not find ccmake or cmake-gui in the exec-path: %s" exec-path))
          )
        )
      )
    (cd d)
    )
  )

;;-----------------------------------------------------------------------------
(defun cmany-install-keybindings ()
  (interactive)
  )


(defun cmany-hook ()
  (cmany--load-configs)
  )

;;-----------------------------------------------------------------------------
;;;###autoload
(defun cmany-wizard ()
  "interactively configure the cmany params: project dir, cmd
form, build dir and active target"
  (interactive)
  (call-interactively 'cmany-set-proj-dir)
  (call-interactively 'cmany-set-cmd)
  (call-interactively 'cmany-set-build-dir)
  (call-interactively 'cmany-set-target)
  )

;;-----------------------------------------------------------------------------
;;;###autoload
(define-minor-mode cmany-mode
  "cmany.el: simple and batched cmake integration"
  :group cmany
  :lighter " cmany"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c m W") 'cmany-wizard)
            (define-key map (kbd "C-c m P") 'cmany-set-proj-dir)
            (define-key map (kbd "C-c m D") 'cmany-set-build-dir)
            (define-key map (kbd "C-c m T") 'cmany-set-target)
            (define-key map (kbd "C-c m K") 'cmany-set-cmd)

            (define-key map (kbd "C-c m A") 'cmany-rtags-announce-build-dir)

            (define-key map (kbd "C-c m C") 'cmany-configure)
            (define-key map (kbd "C-c m c") 'cmany-configure-again)
            (define-key map (kbd "C-c m B") 'cmany-build)
            (define-key map (kbd "C-c m b") 'cmany-build-again)
            (define-key map (kbd "C-c m G") 'cmany-debug)
            (define-key map (kbd "C-c m g") 'cmany-debug-again)

            (define-key map (kbd "C-c m e") 'cmany-edit-cache)
            map)
  )

(provide 'cmany-mode)
