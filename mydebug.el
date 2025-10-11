;; https://github.com/positron-solutions/transient-showcase
;; https://jd.codes/posts/transient-emacs/


(require 'transient)
(require 'projectile)
(require 'json)


;;----------------------------------------------------
;; helpers

(defun --my-debug--read-dir (prompt default-value)
  (let ((base (file-name-base default-value)))
    (read-directory-name (format "%s: " prompt) default-value base)))

(defun --my-debug--read-file (prompt default-value fallback-dir)
  (let* (
         (fallback-dir (if (null fallback-dir) (file-name-directory (buffer-file-name (current-buffer))) fallback-dir))
         (dir (if (null default-value) fallback-dir (file-name-directory default-value)))
         (base (if (null default-value) "" (file-name-base default-value)))
        )
    (read-file-name (format "%s: " prompt) dir base)))

(defun --my-debug--filter-home (path)
  ;; https://stackoverflow.com/questions/17325713/looking-for-a-replace-in-string-function-in-elisp
  (string-replace (getenv "HOME") "~" path))

(defun --my-debug--describe (name var)
  (if (and var (not (string-blank-p var)))
      (format "%s: %s" name (propertize var 'face 'transient-value))
    name))


;;----------------------------------------------------
;; proj-dir

(defvar my-debug--proj-dir nil "Project directory")

(defun my-debug--proj-dir--ensure ()
  (when (null my-debug--proj-dir)
    (my-debug--proj-dir--set
     (--my-debug--filter-home
      (or projectile-project-root
          (file-name-directory (buffer-file-name (current-buffer))))
      )
     )
    )
  (--my-debug--filter-home my-debug--proj-dir)
  )

(defun my-debug--proj-dir--set (dir)
  (interactive (list (--my-debug--read-dir "proj-dir" (my-debug--proj-dir--ensure))))
  (let ((dir (--my-debug--filter-home dir)))
    (setq my-debug--proj-dir (--my-debug--filter-home dir))
    ;; reset other proj vars
    (my-debug--build-dir--set nil)
    (my-debug--launch-json--set nil)
    )
  )

(defun my-debug--proj-dir--describe ()
  (--my-debug--describe "proj-dir" (my-debug--proj-dir--ensure)))


;;----------------------------------------------------
;; build-dir

(defvar my-debug--build-dir nil "Build directory")

(defun my-debug--build-dir--ensure ()
  (when (null my-debug--build-dir)
    (setq my-debug--build-dir
          (concat (my-debug--proj-dir--ensure) "build")
          )
    )
  (--my-debug--filter-home my-debug--build-dir)
  )

(defun my-debug--build-dir--set (dir)
  (interactive (list (--my-debug--read-dir "build-dir" (my-debug--build-dir--ensure))))
  (let ((dir (if dir (--my-debug--filter-home dir) dir)))
    (message "set-build-dir! %s -> %s" my-debug--build-dir dir)
    (setq my-debug--build-dir dir)
    )
  )

(defun my-debug--build-dir--describe ()
  (let* (
         (dir (my-debug--build-dir--ensure))
         (dir (string-replace my-debug--proj-dir "<proj>/" dir))
         )
    (--my-debug--describe "build-dir" dir)
    )
  )


;;----------------------------------------------------
;; launch-json

(defvar my-debug--launch-json nil "Path to launch.json")

(defun my-debug--launch-json--ensure ()
  (when (null my-debug--launch-json)
    (setq my-debug--launch-json
          (concat (my-debug--proj-dir--ensure) "launch.json")
          )
    )
  (--my-debug--filter-home my-debug--launch-json)
  )

(defun my-debug--launch-json--set (dir)
  (interactive (list (--my-debug--read-dir "launch-json" (my-debug--launch-json--ensure))))
  (let ((dir (if dir (--my-debug--filter-home dir) dir)))
    (message "set-launch-json! %s -> %s" my-debug--launch-json dir)
    (setq my-debug--launch-json dir)
    )
  )

(defun my-debug--launch-json--describe ()
  (let* (
         (path (my-debug--launch-json--ensure))
         (path (string-replace my-debug--proj-dir "<proj>/" path))
         )
    (--my-debug--describe "launch-json" path)
    )
  )


;;----------------------------------------------------
;; exe

(defvar my-debug--exe nil "")

(defun my-debug--exe--ensure ()
  (when (null my-debug--exe)
    (setq my-debug--exe (concat (my-debug--build-dir--ensure) "exe"))
    )
  (--my-debug--filter-home my-debug--exe)
  )

(defun my-debug--exe--set (exe)
  (interactive (list (--my-debug--read-file
                      "exe"
                      (my-debug--exe--ensure)
                      my-debug--build-dir)))
  (message "set-exe! %s -> %s" my-debug--exe exe)
  (setq my-debug--exe (--my-debug--filter-home exe))
  )

(defun my-debug--exe--describe ()
  (let* ((exe (my-debug--exe--ensure))
         (exe (string-replace my-debug--build-dir "<build>/" exe)))
    (--my-debug--describe "exe" exe)
    )
  )


;;----------------------------------------------------
;; cwd

(defvar my-debug--cwd nil "")

(defun my-debug--cwd--ensure ()
  (when (null my-debug--cwd)
    (setq my-debug--cwd (my-debug--proj-dir--ensure))
    )
  (--my-debug--filter-home my-debug--cwd)
  )

(defun my-debug--cwd--set (cwd)
  (interactive (list (--my-debug--read-dir
                      "cwd"
                      my-debug--build-dir)))
  (message "set-cwd! %s -> %s" my-debug--cwd cwd)
  (setq my-debug--cwd (--my-debug--filter-home cwd))
  )

(defun my-debug--cwd--describe ()
  (let* ((cwd (my-debug--cwd--ensure))
         (cwd (string-replace my-debug--proj-dir "<proj>/" cwd))
         )
    (--my-debug--describe "cwd" cwd)
    )
  )


;;----------------------------------------------------
;; args

(defvar my-debug--args "" "")

(defun my-debug--args--set (args)
  (interactive (list (read-string "args: " my-debug--args)))
  (message "set-args! %s -> %s" my-debug--args args)
  (setq my-debug--args args)
  )

(defun my-debug--args--describe ()
  (let* ((args my-debug--args)
         (args (or args ""))
         )
    (--my-debug--describe "args" args)
    )
  )


;;----------------------------------------------------
;; args

(defvar my-debug--name "mycfg" "")

(defun my-debug--name--set (name)
  (interactive (list (read-string "name: " my-debug--name)))
  (message "set-name! %s -> %s" my-debug--name name)
  (setq my-debug--name name)
  )

(defun my-debug--name--describe ()
  (let* ((name my-debug--name)
         (name (or name ""))
         )
    (--my-debug--describe "name" name)
    )
  )


;;----------------------------------------------------


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


(defun my-debug--launch ()
  (interactive)
  (message "launch!")
  )

(transient-define-prefix my-debug ()
  "My Debug"

  ["Edit config"
   ("-n" "Set name" my-debug--name--set :transient t
    :description my-debug--name--describe
    )
   ("-e" "Set exe" my-debug--exe--set :transient t
    :description my-debug--exe--describe
    )
   ("-d" "Set cwd" my-debug--cwd--set :transient t
    :description my-debug--cwd--describe
    )
   ("-a" "Set args" my-debug--args--set :transient t
    :description my-debug--args--describe
    )
   ]

  ["Edit proj"
   ("-P" "Set proj dir" my-debug--proj-dir--set :transient t
    :description my-debug--proj-dir--describe)
   ("-B" "Set build dir" my-debug--build-dir--set :transient t
    :description my-debug--build-dir--describe)
   ("-L" "Set launch.json" my-debug--launch-json--set :transient t
    :description my-debug--launch-json--describe)
   ]

  ["Actions"

   ["Debug"
    ("d" "Start debug" my-debug--launch)
    ]

   ["launch.json"
    ("S" "Save current config to launch.json" my-debug--config--save)
    ("L" "Load config from launch.json" my-debug--config--load)
    ("M" "Make config" my-debug--config--make)
    ("K" "Kill config" my-debug--config--kill)
    ]

   ]
  )
;;(global-set-key (kbd "s-,") 'mytransient-prefix)
