;;; ws-butler-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ws-butler-global-mode ws-butler-mode) "ws-butler"
;;;;;;  "ws-butler.el" (22635 11690 0 0))
;;; Generated autoloads from ws-butler.el

(autoload 'ws-butler-mode "ws-butler" "\
White space cleanup, without obtrusive white space removal.

Whitespaces at EOL and EOF are trimmed upon file save, and only
for lines modified by you.

\(fn &optional ARG)" t nil)

(defvar ws-butler-global-mode nil "\
Non-nil if Ws-Butler-Global mode is enabled.
See the command `ws-butler-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ws-butler-global-mode'.")

(custom-autoload 'ws-butler-global-mode "ws-butler" nil)

(autoload 'ws-butler-global-mode "ws-butler" "\
Toggle Ws-Butler mode in all buffers.
With prefix ARG, enable Ws-Butler-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Ws-Butler mode is enabled in all buffers where
`(lambda nil (unless (apply (function derived-mode-p) ws-butler-global-exempt-modes) (ws-butler-mode)))' would do it.
See `ws-butler-mode' for more information on Ws-Butler mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("ws-butler-pkg.el") (22635 11690 804000
;;;;;;  0))

;;;***

(provide 'ws-butler-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ws-butler-autoloads.el ends here
