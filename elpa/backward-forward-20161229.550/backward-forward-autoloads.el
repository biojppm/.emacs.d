;;; backward-forward-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "backward-forward" "backward-forward.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from backward-forward.el

(defvar backward-forward-mode nil "\
Non-nil if Backward-Forward mode is enabled.
See the `backward-forward-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `backward-forward-mode'.")

(custom-autoload 'backward-forward-mode "backward-forward" nil)

(autoload 'backward-forward-mode "backward-forward" "\
enables or disable backward-forward minor mode.

This is a minor mode.  If called interactively, toggle the
`Backward-Forward mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='backward-forward-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

when backward-forward mode is enabled, it keeps track of mark pushes across
all buffers in a variable backward-forward-mark-ring, and allows you to navigate backwards
and forwards across these marks using <C-left> and <C-right>.  to customize
the navigation behavior one must customize the mark pushing behavior --
add 'advice' to a command to make it push a mark before invocation if you
want it to be tracked.  see backward-forward.el for examples and more
information.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "backward-forward" '("backward-forward-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; backward-forward-autoloads.el ends here
