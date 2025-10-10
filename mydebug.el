;; https://github.com/positron-solutions/transient-showcase

(require 'transient)
(require 'projectile)
(require 'json)


(defun my-debug--filter-dir (dir)
  ;; https://stackoverflow.com/questions/17325713/looking-for-a-replace-in-string-function-in-elisp
  (string-replace (getenv "HOME") "~" dir))

(defun my-debug--read-dir (prompt default-value)
  (let ((base (file-name-base default-value)))
    (read-directory-name (format "%s: " prompt) default-value base)))

(defun my-debug--read-file (prompt default-value fallback-dir)
  (let* (
         (fallback-dir (if (null fallback-dir) (file-name-directory (buffer-file-name (current-buffer))) fallback-dir))
         (dir (if (null default-value) fallback-dir (file-name-directory default-value)))
         (base (if (null default-value) "" (file-name-base default-value)))
        )
    (read-file-name (format "%s: " prompt) dir base)))


;;----------------------------------------------------
;; dir-proj
(defvar my-debug--dir-proj nil "Project directory")

(defun my-debug--dir-proj--ensure ()
  (when (null my-debug--dir-proj)
    (my-debug--dir-proj--set
     (my-debug--filter-dir
      (or projectile-project-root
          (file-name-directory (buffer-file-name (current-buffer))))
      )
     )
    )
  (my-debug--filter-dir my-debug--dir-proj)
  )

(defun my-debug--dir-proj--set (dir)
  (interactive (list (my-debug--read-dir "proj-dir" (my-debug--dir-proj--ensure))))
  (let ((dir (my-debug--filter-dir dir)))
    (setq my-debug--dir-proj (my-debug--filter-dir dir))
    (my-debug--dir-build--set nil) ;; reset the build dir
    )
  )

(defun my-debug--dir-proj--describe ()
  (format "proj-dir: %s"
          (propertize (my-debug--dir-proj--ensure)
                      'face 'transient-value)))


;;----------------------------------------------------
;; dir-build
(defvar my-debug--dir-build nil "Build directory")

(defun my-debug--dir-build--ensure ()
  (when (null my-debug--dir-build)
    (setq my-debug--dir-build
          (concat (my-debug--ensure-dir-proj) "build")
          )
    )
  (my-debug--filter-dir my-debug--dir-build)
  )

(defun my-debug--dir-build--set (dir)
  (interactive (list (my-debug--read-dir "build-dir" (my-debug--dir-build--ensure))))
  (let ((dir (if dir (my-debug--filter-dir dir) dir)))
    (message "set-dir-build! %s -> %s" my-debug--dir-build dir)
    (setq my-debug--dir-build dir)
    )
  )

(defun my-debug--dir-build--describe ()
  (let* ((dir (string-replace my-debug--dir-proj "<proj>/" (my-debug--dir-build--ensure))))
    (format "build-dir: %s"
            (propertize dir
                        'face 'transient-value)))
  )

;;----------------------------------------------------
;; target

(defvar my-debug--config--target nil "")

(defun my-debug--config--target--set (target)
  (interactive (list (my-debug--read-file
                      "target"
                      my-debug--config--target
                      my-debug--dir-build)))
  (message "set-target! %s -> %s" my-debug--config--target target)
  (setq my-debug--config--target (my-debug--filter-dir target))
  )

(defun my-debug--config--target--describe ()
  (let* ((target my-debug--config--target)
         (target (or target " "))
         (target (string-replace my-debug--dir-build "<build>/" target))
         )
    (format "target: %s" (propertize target 'face 'transient-value)))
  )


;;----------------------------------------------------
;; target

(defvar my-debug--config--cwd nil "")

(defun my-debug--config--cwd--set (cwd)
  (interactive (list (my-debug--read-dir
                      "cwd"
                      my-debug--dir-build)))
  (message "set-cwd! %s -> %s" my-debug--config--cwd cwd)
  (setq my-debug--config--cwd (my-debug--filter-dir cwd))
  )

(defun my-debug--config--cwd--describe ()
  (let* ((cwd my-debug--config--cwd)
         (cwd (or cwd " "))
         (cwd (string-replace my-debug--dir-proj "<proj>/" cwd))
         )
    (format "cwd: %s" (propertize cwd 'face 'transient-value)))
  )


(defvar my-debug--config--args nil "")

(defun my-debug--cwd--set ()
  (interactive)
  (message "set-cwd!")
  )
(defun my-debug--args--set ()
  (interactive)
  (message "set-args!")
  )


(defun my-debug--launch ()
  (interactive)
  (message "launch!")
  )


(defvar my-debug--config--name nil "")

(defun my-debug--config--save ()
  (interactive)
  (message "config-save!")
  )
(defun my-debug--config--load ()
  (interactive)
  (message "config-load!")
  )
(defun my-debug--config--make ()
  (interactive)
  (message "config-make!")
  )
(defun my-debug--config--kill ()
  (interactive)
  (message "config-kill!")
  )


(transient-define-prefix my-debug ()
  "My Debug"
  ["My Debug"
   ["Actions"
    ("d" "Start debug" my-debug--launch)
    ]
   ["Edit config"
    ("-t" "Set target" my-debug--config--target--set :transient t
     :description my-debug--config--target--describe
     )
    ("-d" "Set cwd" my-debug--config--cwd--set :transient t
     :description my-debug--config--cwd--describe)
    ("-a" "Set args" my-debug--set-args)
    ]
   ["Edit proj"
    ("-P" "Set proj dir" my-debug--dir-proj--set :transient t
     :description my-debug--dir-proj--describe
     )
    ("-B" "Set build dir" my-debug--dir-build--set :transient t
     :description my-debug--dir-build--describe)
    ]
   ["Config"
    ("S" "Save current config to launch.json" my-debug--config--save)
    ("L" "Load config from launch.json" my-debug--config--load)
    ("M" "Make config" my-debug--config--make)
    ("K" "Kill config" my-debug--config--kill)
    ]
   ]
  )
;;(global-set-key (kbd "s-,") 'mytransient-prefix)
