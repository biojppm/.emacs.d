
(defcustom cmany-proj-dir nil
  "The directory where the project CMakeLists.txt is located."
  :group 'cmany
  :type 'directory
  :safe #'stringp
  )

(defcustom cmany-build-dir nil
  "The directory of the current cmany build."
  :group 'cmany
  :type 'string
  :safe #'stringp
  )

(defcustom cmany-args "-E -c clang++ -t Debug"
  "The arguments to use when running cmany"
  :group 'cmany
  :type 'string
  :safe #'stringp
  )

(defcustom cmany-target ""
  "The current active target"
  :group 'cmany
  :type 'string
  :safe #'stringp
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

(defun cmany--has-proj-dir ()
  (and (boundp 'cmany-proj-dir) (not (string-equal 'cmany-proj-dir "")))
  )

(defun cmany--has-build-dir ()
  (and (boundp 'cmany-build-dir) (not (string-equal 'cmany-build-dir "")))
  )

;;-----------------------------------------------------------------------------
(defun cmany--get-default-proj-dir ()
  "get a default value for cmany-proj-dir"
  ;; if there's a current cmany-proj-dir, use it
  (if (cmany--has-proj-dir)
      (progn
        (message "cmany-proj-dir already defined: %s" cmany-proj-dir)
        cmany-proj-dir)
      ;; otherwise...
      (progn
        ;; if projectile is available, get its project root
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

;;-----------------------------------------------------------------------------
(defun cmany--get-default-build-dir ()
  (let ((d default-directory))
    (if (not (cmany--has-proj-dir))
        (progn (setq cmany-proj-dir (cmany--get-default-proj-dir))))
    (cd cmany-proj-dir)
    (setq cmd (format "cmany show_builds %s %s" cmany-args cmany-proj-dir))
    (message "cmany cmd: %s" cmd)
    (setq bd (shell-command-to-string cmd))
    (setq bd (car (split-string bd "\n")))
    (setq bd (concat cmany-proj-dir "build/" bd ))
    (cd d)
    bd
    )
  )

;;-----------------------------------------------------------------------------
(defun cmany-prompt-proj-dir (dir)
  "interactively prompt for the project dir"
  (interactive
   (list (read-directory-name "cmany proj dir: "
                              (cmany--get-default-proj-dir))))
  (setq cmany-proj-dir dir)
  )

(defun cmany-prompt-build-dir (dir)
  "interactively prompt for the build dir"
  (interactive
   (list (read-string "cmany build dir: " (cmany--get-default-build-dir))))
  (setq cmany-build-dir dir)
  )

;;-----------------------------------------------------------------------------
(defun cmany-prompt-args (args)
  (interactive
   (list (read-string "cmany arguments: " cmany-args)))
  (setq cmany-args args)
  )

(defun cmany-select-target ()
  (interactive)
  (let ((d default-directory))
    (cd cmany-proj-dir)
    (pwd)
    (setq cmd (format "cmany show_targets %s %s" cmany-args cmany-proj-dir))
    (message "cmany cmd: %s" cmd)
    (setq tgts (shell-command-to-string cmd))
    (cd d)
    (message "cmany output: %s" tgts)
    (setq tgts (split-string tgts "\n"))
    (when (featurep 'ido)
      (let ((arg (ido-completing-read "Select default target: " tgts)))
        (message "cmany selected target: %s" arg)
        (setq cmany-target arg)
        arg
        )
      )
    )
  )

;;-----------------------------------------------------------------------------
(defun cmany-setup ()
  (interactive)
  (call-interactively 'cmany-prompt-proj-dir)
  (call-interactively 'cmany-prompt-args)
  (call-interactively 'cmany-prompt-build-dir)
  (call-interactively 'cmany-select-target)
  )

;;-----------------------------------------------------------------------------
(defun cmany-configure (cmd)
  (interactive
   (list
    (read-string
     "cmany configure: "
     (if (and (boundp 'cmany--last-configure) (not (string-equal 'cmany--last-configure "")))
         (progn cmany--last-configure)
         (progn (format "cmany configure %s %s" cmany-args cmany-proj-dir))
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
     "cmany build: "
     (if (and (boundp 'cmany--last-build) (not (string-equal 'cmany--last-build "")))
         (progn cmany--last-build)
         (progn (format "cmany build %s %s" cmany-args cmany-proj-dir))
         )
     )))
  (setq cmany--last-build cmd)
  (let ((d default-directory))
    (cd cmany-proj-dir)
    (compile cmd)
    (cd d)
    )
  )
