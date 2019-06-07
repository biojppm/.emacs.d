;; https://www.reddit.com/r/emacs/comments/audffp/tip_how_to_use_a_stable_and_fast_environment_to/

(use-package lsp-mode
  :defer t
  :commands lsp
  :init
  (message "lsp-mode INIT")
  (use-package lsp-ui :commands lsp-ui-mode)
  (message "lsp-mode 1")
  (use-package company-lsp :commands company-lsp)
  )

(use-package ccls
  :after projectile
  :ensure-system-package ccls
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  (message "ccls-executable: %s" ccls-executable)
  ;; https://github.com/MaskRay/ccls/wiki/Project-Setup
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
           projectile-project-root-files-top-down-recurring))
  :config
  (push ".ccls-cache" projectile-globally-ignored-directories)
  )
