;; RTAGS
;; http://diobla.info/doc/rtags
;; https://vxlabs.com/2016/04/11/step-by-step-guide-to-c-navigation-and-completion-with-emacs-and-the-clang-based-rtags/

(use-package rtags
  :defer t
  :init
  (setq rtags-completions-enabled t)
  (setq rtags-autostart-diagnostics t)
  (setq rtags-rc-log-enabled t)
  :config
  (require 'company-rtags)
  (add-to-list 'company-backends 'company-rtags)
  (rtags-diagnostics)
  :bind
  (:map c-mode-base-map
        ("M-<left>"  . rtags-location-stack-back)
        ("M-<right>" . rtags-location-stack-forward)
        ("M-."       . rtags-find-symbol-at-point)
        ("M-,"       . rtags-find-references-at-point)
        ("C-c r ."   . rtags-find-symbol-at-point)
        ("C-c r ,"   . rtags-find-references-at-point)
        ("C-c r v"   . rtags-find-virtuals-at-point)
        ("C-c r V"   . rtags-print-enum-value-at-point)
        ("C-c r /"   . rtags-find-all-references-at-point)
        ("C-c r Y"   . rtags-cycle-overlays-on-screen)
        ("C-c r >"   . rtags-find-symbol)
        ("C-c r <"   . rtags-find-references)
        ("C-c r -"   . rtags-location-stack-back)
        ("C-c r +"   . rtags-location-stack-forward)
        ("C-c r D"   . rtags-diagnostics)
        ("C-c r G"   . rtags-guess-function-at-point)
        ("C-c r p"   . rtags-set-current-project)
        ("C-c r P"   . rtags-print-dependencies)
        ("C-c r e"   . rtags-reparse-file)
        ("C-c r E"   . rtags-preprocess-file)
        ("C-c r R"   . rtags-rename-symbol)
        ("C-c r M"   . rtags-symbol-info)
        ("C-c r S"   . rtags-display-summary)
        ("C-c r O"   . rtags-goto-offset)
        ("C-c r ;"   . rtags-find-file)
        ("C-c r F"   . rtags-fixit)
        ("C-c r X"   . rtags-fix-fixit-at-point)
        ("C-c r B"   . rtags-show-rtags-buffer)
        ("C-c r I"   . rtags-imenu)
        ("C-c r T"   . rtags-taglist)
        )
  )
