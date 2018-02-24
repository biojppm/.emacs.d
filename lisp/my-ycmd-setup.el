;; https://nilsdeppe.com/posts/emacs-c++-ide2

;; ycmd (YouCompleteMeDaemon)

;; Set up YouCompleteMe for emacs:
;; https://github.com/Valloric/ycmd
;; https://github.com/abingham/emacs-ycmd


(defvar my:ycmd-server-command '("python3" "/home/nils/Research/ycmd/ycmd"))
(defvar my:ycmd-global-config (concat emacs-dir "ycm_extra_conf.py"))
(defvar my:ycmd-extra-conf-whitelist 'my:ycmd-global-config)
(defvar my:python-location (executable-find (nth 0 my:ycmd-server-command)))

(if (not my:python-location)
    (warn
     "Could not start YouCompleteMeDaemon because the python executable could
not be found.\nSpecified executable is: '%s'\nPlease set my:ycmd-server-command
appropriately in ~/.emacs.el.\n" (nth 0 my:ycmd-server-command)))

(if (not (file-directory-p (nth 1 my:ycmd-server-command)))
    (warn "Could not YouCompleteMeDaemon because the specified directory does
not exist.\nSpecified directory is: '%s'\nPlease set my:ycmd-server-command
appropriately in ~/.emacs.el.\n" (nth 1 my:ycmd-server-command)))

(if (and my:python-location
         (file-directory-p (nth 1 my:ycmd-server-command)))

    (use-package ycmd
      :ensure t
      :init
      (add-hook 'after-init-hook #'global-ycmd-mode)
      :config
      (progn
        (set-variable 'ycmd-server-command my:ycmd-server-command)
        (set-variable 'ycmd-extra-conf-whitelist my:ycmd-extra-conf-whitelist)
        (set-variable 'ycmd-global-config my:ycmd-global-config)
        (setq ycmd-force-semantic-completion t)

        (use-package company-ycmd
          :ensure t
          :config
          (company-ycmd-setup)
          )

        (use-package flycheck-ycmd
          :ensure t
          :init
          (add-hook 'c-mode-common-hook 'flycheck-ycmd-setup)
          )

        ;; Add displaying the function arguments in mini buffer using El Doc
        (use-package ycmd-eldoc
          :init
          ;; For some reason ycmd-eldoc doesn't work properly in Python mode.
          ;; I get a "connection refused" error when it connects to JediHTTP
          (add-hook 'c-mode-common-hook
                    (lambda ()
                      (ycmd-eldoc-mode t)))
          )
        )
      )
  )
