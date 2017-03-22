;; cmany.el --- cmany integration for Emacs. -*-coding: utf-8 -*-

;; Copyright (C) 2017 Joao Paulo Magalhaes <dev@jpmag.me>

;; Author:      Joao Paulo Magalhaes <dev@jpmag.me>
;; Created:     2017-03-20
;; Version:     0.1
;; Keywords:    cmany, cmake, rtags
;; URL:         http://github.com/biojppm/cmany.git

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

(require 'term-run)

;;; Install:

;; Put this file along your Emacs-Lisp `load-path' and add the following
;; into your ~/.emacs startup file.
;;
;;      <at standards TAB position explain what lisp code is needed>
;;      (autoload 'example-install "example" "" t)
;;      (autoload 'example-mode    "example" "" t)

;;; Commentary:

;; <Write how the package came to be. What problem it solves.
;; why would it be useful. What are the benfits. Compaed to other
;; similar packages? Give exmaples how to use and what keys
;; user might be interested in binding.
;;
;; In short, this is the manual section of the package>


;;; Code:

;; Write code here. defcustom first, then defconst, defvar,
;; defsubst/defmacro and defuns last
;;
;; Remember to add ###autoload stanzas to important variables and functions
;;
;; *ALWAYS* use a PACKAGE-* prefix for variables, functions. This
;; keeps the name space safe.
;;
;; *NEVER* modify user's environment just by loading this file.
;;
;; *NEVER* add any global key bindings unconditionally. For those
;; purposes add PACKAGE-install-keybindings, PACKAGE-install-hooks
;; setup functions and instruct in "Install:" user call to those.
;;
;; See:
;; http://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Library-Headers
;; http://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Autoload
;; http://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Customization


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

;;;;-----------------------------------------------------------------------------
;;;; http://tess.oconnor.cx/2006/03/json.el
;;
;;(defun cmany--encode-props()
;;  (json-encode '(:proj cmany-proj-dir :build cmany-build-dir :args cmany-args :target cmany-target))
;;  )
;;(defun cmany--decode-props(jstring)
;;  (json-read-from-string jstring)
;;  (setq cmany-proj-dir proj)
;;  (setq cmany-build-dir build)
;;  (setq cmany-args args)
;;  (setq cmany-target target)
;;  )
;;
;;;;-----------------------------------------------------------------------------
;;(defun cmany--save-config ()
;;  (let ((jsonstring (cmany--encode-props))
;;        (filename (concat user-emacs-directory "cmany-last.json")))
;;    (write-region jsonstring nil filename)
;;    )
;;  )
;;(defun cmany--load-config ()
;;  (let ((filename (concat user-emacs-directory "cmany-last.json")))
;;    (if (file-exists-p fn)
;;        (progn
;;          ;; load the file into a string
;;          (with-temp-buffer (insert-file-contents fn)(jstring))
;;          (message "found a previous session at %s: %s" filename jstring)
;;          (cmany-decode-props jstring)
;;          )
;;      )
;;    )
;;  )

;;-----------------------------------------------------------------------------

(defun cmany--log (fmt &rest args)
  (apply 'message fmt args)
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
  ;; http://stackoverflow.com/a/17325791/5875572
  (let* ((cmd (replace-regexp-in-string (regexp-quote "{cmd}") which-cmd cmany-cmd nil 'literal)))
    (cmany--log "cmd1=%s" cmd)
    (setq cmd (replace-regexp-in-string (regexp-quote "{projdir}") cmany-proj-dir cmd nil 'literal))
    (cmany--log "cmd2=%s" cmd)
    (when (not target)
      (setq target "")
      )
    (setq cmd (replace-regexp-in-string (regexp-quote "{target}") target cmd nil 'literal))
    (cmany--log "cmd3=%s" cmd)
    cmd
    )
  )

(defun cmany--default-build-command ()
  (cmany--format-cmd "build" cmany-target)
  )


;;-----------------------------------------------------------------------------
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
(defun cmany--get-default-proj-dir ()
  (if (cmany--has-proj-dir)
      ;; if there's a current cmany-proj-dir, use it
      (progn
        (cmany--log "cmany-proj-dir already defined: %s" cmany-proj-dir)
        cmany-proj-dir)
      ;; otherwise...
      (progn
        ;; if projectile is available, get the current project root
        (let ((r ""))
          (when (featurep 'projectile)
            (setq r (projectile-expand-root ".")))
          (if (not (string-equal 'r ""))
              (progn
                ;; yep, got project root through projectile
                (cmany--log "proj dir from projectile: %s" r)
                r)
              (progn
                ;; otherwise, just use the current directory
                (cmany--log "proj dir from current directory: %s"
                            (file-name-directory (buffer-file-name)))
                (file-name-directory (buffer-file-name)))
            )
          )
        )
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
(defun cmany-set-proj-dir (&optional dir)
  "set the project dir used by cmany"
  (interactive
   (list (ido-read-directory-name
          "cmany proj dir: " (cmany--get-default-proj-dir))))
  (cmany--log "cmany set proj dir: %s" dir)
  (setq cmany-proj-dir dir)
  )

(defun cmany-set-build-dir (&optional dir)
  "prompt for the build dir used by cmany"
  (interactive
   (list (file-name-as-directory
           (call-interactively 'cmany--exec-prompt-build-dir))))
  (cmany--log "cmany set build dir: %s" dir)
  (setq cmany-build-dir dir)
  (cmany-rtags-announce-build-dir cmany-build-dir)
  )

(defun cmany-set-cmd (&optional cmd)
  "prompt for the build dir used by cmany"
  (interactive
   (list (read-string "cmany command: " cmany-cmd)))
  (cmany--log "cmany set command: %s" cmd)
  (setq cmany-cmd cmd)
  )

(defun cmany-set-target (&optional tgt)
  "prompt for the build dir used by cmany"
  (interactive
   (list (ido-completing-read
          "cmany current target: "
          (cmany--get-cmany-lines "show_targets") nil nil cmany-target)))
    (cmany--log "cmany set target: %s" tgt)
    (setq cmany-target tgt)
  )

;;-----------------------------------------------------------------------------
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html

(defun cmany-rtags-start ()
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
  (interactive (list cmany-build-dir))
  (when (and cmany-rtags-enabled (featurep 'rtags))
    (cmany-rtags-start)
    (start-process (concat "cmany-rtags-rc" " " dir) "*rdm*" "rc" "-J" dir)
    )
  )

;;-----------------------------------------------------------------------------
(defun cmany-setup ()
  (interactive)
  (call-interactively 'cmany-set-proj-dir)
  (call-interactively 'cmany-set-cmd)
  (call-interactively 'cmany-set-build-dir)
  (call-interactively 'cmany-set-target)
  )

;;-----------------------------------------------------------------------------
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

;;-----------------------------------------------------------------------------
(defun cmany--visit-buffer (name)
  "Create or visit a terminal buffer."
  (interactive)
  (when (not (get-buffer name))
    (split-window-sensibly (selected-window))
    ;(other-window 1)
    (get-buffer-create name)
    ;;(term (getenv "SHELL"))
    )
  (switch-to-buffer-other-window name)
  )

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

;;-----------------------------------------------------------------------------
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

;;-----------------------------------------------------------------------------
(defun cmany-install-keybindings ()
  (interactive)

  (define-key "C-c m S" 'cmany-setup)
  (define-key "C-c m P" 'cmany-set-proj-dir)
  (define-key "C-c m B" 'cmany-set-build-dir
  (define-key "C-c m T" 'cmany-set-target)
  (define-key "C-c m C" 'cmany-set-cmd)
  (define-key "C-c m R" 'cmany-rtags-start)

  (define-key "C-c m c" 'cmany-configure)
  (define-key "C-c m e" 'cmany-edit-cache)
  (define-key "C-c m b" 'cmany-build)
  (define-key "C-c m d" 'cmany-debug)
  )
