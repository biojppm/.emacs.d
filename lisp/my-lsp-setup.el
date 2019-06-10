;; https://www.reddit.com/r/emacs/comments/audffp/tip_how_to_use_a_stable_and_fast_environment_to/

;; https://github.com/MaskRay/ccls/wiki/lsp-mode
(use-package lsp-mode
  :defer t
  :commands lsp
  :init
  (message "lsp-mode INIT")
  (use-package lsp-ui
    :defer t
    :commands lsp-ui-mode
    :init
    (message "lsp-ui INIT")
    ;; https://github.com/emacs-lsp/lsp-ui/blob/master/lsp-ui-sideline.el
    (setq lsp-ui-sideline-delay 5.0)
    :bind
    ("C-c C-?" . lsp-ui-sideline-toggle-symbols-info)
    (message "lsp-ui INIT")
    )
  (message "lsp-mode 1")
  (use-package company-lsp
    :defer t
    :commands company-lsp)
  (message "lsp-mode 2")
  (setq lsp-enable-on-type-formatting nil)
  ;; fix M-? fail: https://github.com/emacs-lsp/lsp-java/issues/122
  (setq xref-prompt-for-identifier
        '(not xref-find-definitions
              xref-find-definitions-other-window
              xref-find-definitions-other-frame
              xref-find-references
              )
        )
  (message "lsp-mode DONE")
  )

(use-package ccls
  :defer t
  :after projectile
  ;;:ensure-system-package ccls ;; https://github.com/jwiegley/use-package/issues/698
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
