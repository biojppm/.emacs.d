(defcustom cmany-rtags-enabled 1
  "Whether cmany should announce the current build directory to rtags."
  :group 'cmany
  :type 'boolean
  :safe #'booleanp
  )

(defcustom cmany-build-dir-prefix "build/"
  ""
  :group 'cmany
  :type 'string
  :safe #'stringp
  )

(defvar cmany-proj-dir nil
  "The directory where the project CMakeLists.txt is located."
  )

(defvar cmany-build-dir nil
  "The directory of the current cmany build."
  )

(defvar cmany-cmd "cmany %s -E -c clang++ -t Debug %s"
  "The arguments to use when running cmany"
  )

(defvar cmany-target ""
  "The current active target"
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
(defun cmany--has-proj-dir ()
  "is cmany-proj-dir already set"
  (and (boundp 'cmany-proj-dir) (not (string-equal 'cmany-proj-dir "")))
  )

(defun cmany--has-build-dir ()
  "is cmany-build-dir already set"
  (and (boundp 'cmany-build-dir) (not (string-equal 'cmany-build-dir "")))
  )

;;-----------------------------------------------------------------------------
(defun cmany--get-cmd-output (workdir cmd)
  "Run PROGRAM with ARGS and return the output in a string if it returns 0;
  otherwise return an empty string."
  (let ((d default-directory))
     (message "cmany cmd exec: %s" cmd)
     (cd workdir)
     (with-temp-buffer
       (message "cmany cmd exec at dir: %s (was at %s)" (pwd) d)
       (let ((p (call-process-shell-command cmd nil (current-buffer))))
         (message "cmany cmd return: %d" p)
         (message "cmany cmd output: %s" (buffer-string))
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
  (let* ((base-cmd (format cmany-cmd cmd cmany-proj-dir))
         (full-cmd (concat base-cmd more-args))
         )
    (message "cmany cmd base: %s" base-cmd)
    (message "cmany cmd full: %s" full-cmd)
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
  ;; if there's a current cmany-proj-dir, use it
  (if (cmany--has-proj-dir)
      (progn
        (message "cmany-proj-dir already defined: %s" cmany-proj-dir)
        cmany-proj-dir)
      ;; otherwise...
      (progn
        ;; if projectile is available, get the current project root
        (setq r "")
        (when (featurep 'projectile)
          (setq r (projectile-project-root)))
        (if (and (boundp 'r) (not (string-equal 'r "")))
            (progn
              ;; yep, got project root through projectile
              (message "cmany-proj-dir from projectile: %s" r)
              r)
            (progn
              ;; otherwise, just use the current directory
              (message "cmany-proj-dir from current directory: %s"
                       (file-name-directory (buffer-file-name)))
              (file-name-directory (buffer-file-name)))
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
    (message "read directory: %s" bdr)
    bdr
    )
  )

;;-----------------------------------------------------------------------------
(defun cmany-set-proj-dir (&optional dir)
  "set the project dir used by cmany"
  (interactive
   (list (ido-read-directory-name
          "cmany proj dir: " (cmany--get-default-proj-dir))))
  (message "cmany set proj dir: %s" dir)
  (setq cmany-proj-dir dir)
  )

(defun cmany-set-build-dir (&optional dir)
  "prompt for the build dir used by cmany"
  (interactive
   (list (file-name-as-directory
           (call-interactively 'cmany--exec-prompt-build-dir))))
  (message "cmany set build dir: %s" dir)
  (setq cmany-build-dir dir)
  )

(defun cmany-set-cmd (&optional cmd)
  "prompt for the build dir used by cmany"
  (interactive
   (list (read-string "cmany command: " cmany-cmd)))
  (message "cmany set command: %s" cmd)
  (setq cmany-cmd cmd)
  )

(defun cmany-set-target (&optional tgt)
  "prompt for the build dir used by cmany"
  (interactive
   (list (ido-completing-read
          "cmany current target: "
          (cmany--get-cmany-lines "show_targets") nil nil cmany-target)))
    (message "cmany set target: %s" tgt)
    (setq cmany-target tgt)
  )

;;-----------------------------------------------------------------------------
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html

(defun cmany-rtags-start ()
  (interactive)
  (when (and cmany-rtags-enabled (featurep 'rtags))
    ;;(if (not (boundp 'cmany--rtags-rdm))
    ;;    (progn
    ;;      (message "starting rdm")
    ;;      (setq cmany--rtags-rdm (start-process "cmany-rtags-rdm" "*rdm*" "rdm")))
    ;;  (progn (message "rdm is already running")))
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
  (cmany-rtags-announce-build-dir cmany-build-dir)
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
         (progn (format cmany-cmd "configure"))
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
(defun cmany-build (cmd)
  (interactive
   (list
    (read-string
     "enter build cmd: "
     (if (and (boundp 'cmany--last-build) (not (string-equal 'cmany--last-build "")))
         (progn cmany--last-build)
         (progn (concat (format cmany-cmd "build") " " cmany-target))
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
  (call-interactively (gdb cmd))
  )
