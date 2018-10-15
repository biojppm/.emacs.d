;; CQUERY
;; https://github.com/cquery-project/cquery/wiki/Emacs

;; Diagnostics
;;
;;  M-x lsp-capabilities LSP workspace is initialized correctly
;;  M-: xref-backend-functions is (lsp--xref-backend) for cross references
;;  M-: completion-at-point-functions is (lsp-completion-at-point) for completion

;; for completing headers: Type #i" (or #include ") for quote-style includes and #i< (or #include <) for system headers.



(use-package cquery
  ;;:defer t
  :init
  (use-package lsp-mode
    :config
    (lsp-cquery-enable)
    )
  (lsp-mode)
  (message "CQUERY CRL")
  (setq cquery-extra-init-params
        '(
          :index (:comments 2)
          :cacheFormat "msgpack"
          :completion (:detailedLabel t)
          )
        )
  :config
  (message "CQUERY CFG")
  (require 'lsp-ui-doc)
  (require 'lsp-imenu)
  ;; Completion:
  (require 'company-lsp)
  (message "CQUERY COMPANY")
  (add-to-list 'company-backends 'company-lsp)
  ;; disable client-side cache and sorting because the server does a better job
  (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
  ;; dont prompt for identifier on calls to xref-find-references
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references)
  :bind
  (:map c-mode-base-map
        ("M-<left>"  . xref-pop-marker-stack)
        ("M-<right>" . xref-push-marker-stack)
        ("M-."       . xref-find-definitions)
        ("M-?"       . xref-find-references)
        )
  )
