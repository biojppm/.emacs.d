;; https://www.reddit.com/r/emacs/comments/audffp/tip_how_to_use_a_stable_and_fast_environment_to/

;; https://github.com/MaskRay/ccls/wiki/lsp-mode
(use-package lsp-mode
  :defer t
  :commands lsp
  :init
  (message "lsp-mode INIT start")
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-file-watchers nil)
  (message "lsp-mode INIT end")
  :config
  (message "lsp-mode CONFIG")
  (message "lsp-mode 1")
  ;; fix M-? fail: https://github.com/emacs-lsp/lsp-java/issues/122
  (setq xref-prompt-for-identifier
        '(not xref-find-definitions
              xref-find-definitions-other-window
              xref-find-definitions-other-frame
              xref-find-references
              )
        )
  (message "lsp-mode 2")
  (use-package company-lsp
    :defer t
    :commands company-lsp
    :init
    (message "company-lsp INIT")
    (message "company-lsp DONE")
    )
  (message "lsp-mode 3")
  (use-package lsp-ui
    :defer t
    :commands lsp-ui-mode
    :init
    (message "lsp-ui INIT")
    ;; https://github.com/emacs-lsp/lsp-ui/blob/master/lsp-ui-sideline.el
    (setq lsp-ui-sideline-delay 5.0)
    (setq lsp-ui-doc-delay 5.0)
    (message "lsp-ui DONE")
    :bind
    ("C-c C-?" . lsp-ui-sideline-toggle-symbols-info)
    )
  (message "lsp-mode DONE")
  )


;;--------------------------------
;; lsp-booster
;; https://github.com/blahgeek/emacs-lsp-booster

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(advice-add 'lsp-resolve-final-command
            :around #'lsp-booster--advice-final-command)

;;
;;(use-package ccls
;;  :defer t
;;  :after projectile
;;  ;;:ensure-system-package ccls ;; https://github.com/jwiegley/use-package/issues/698
;;  :init
;;  (message "ccls INIT")
;;  (message "ccls INIT DONE")
;;  :config
;;  (message "ccls CONFIG")
;;  ;;(ccls-args nil)
;;  (setq ccls-executable (executable-find "ccls"))
;;  (message "ccls-executable: %s" ccls-executable)
;;  ;; https://github.com/MaskRay/ccls/wiki/Project-Setup
;;  ;;(projectile-project-root-files-top-down-recurring
;;  ;; (append '("compile_commands.json" ".ccls")
;;  ;;         projectile-project-root-files-top-down-recurring))
;;  (push ".ccls-cache" projectile-globally-ignored-directories)
;;  (message "ccls CONFIG DONE")
;;  )
