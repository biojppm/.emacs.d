;; CQUERY
;; https://github.com/cquery-project/cquery/wiki/Emacs

;; Diagnostics
;;
;;  M-x lsp-capabilities LSP workspace is initialized correctly
;;  M-: xref-backend-functions is (lsp--xref-backend) for cross references
;;  M-: completion-at-point-functions is (lsp-completion-at-point) for completion

;; for completing headers: Type #i" (or #include ") for quote-style includes and #i< (or #include <) for system headers.

(use-package cquery
  :defer t
  :commands lsp
  :init
  (message "CQUERY INIT")
  ;; Arch Linux aur/cquery
  (setq cquery-executable (executable-find "cquery"))
  (message "cquery executable: %s" cquery-executable)
  ;; ;; Initialization options
  ;; ;; Log file
  ;; (setq cquery-extra-args '("--log-file=/tmp/cq.log"))
  ;; ;; Cache directory, both relative and absolute paths are supported
  ;; (setq cquery-cache-dir ".cquery_cached_index")
  (setq cquery-extra-init-params
        '(
          :index (:comments 2)
          :cacheFormat "msgpack"
          :completion (:detailedLabel t)
          )
        )
  (message "lsp-cquery enable 0")
  (use-package lsp-mode)
  (message "cquery 0")
  (use-package lsp-ui)
  (message "cfg 0")
  (require 'lsp-ui-doc)
  (message "cfg 1")
  (use-package lsp-imenu)
  (message "cfg 2")
  ;; Completion:
  (use-package company-lsp)
  (message "cfg 3")
  (message "CQUERY INIT --- DONE")
  :config
  (message "CQUERY CFG")
  (message "CQUERY CFG --- DONE")
  :bind
  (:map c-mode-base-map
        ("M-<left>"  . xref-pop-marker-stack)
        ("M-<right>" . xref-push-marker-stack)
        ("M-."       . xref-find-definitions)
        ("M-?"       . xref-find-references)
        )
  )
