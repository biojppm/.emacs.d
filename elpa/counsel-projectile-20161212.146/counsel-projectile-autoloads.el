;;; counsel-projectile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (counsel-projectile-off counsel-projectile-on counsel-projectile
;;;;;;  counsel-projectile-switch-project counsel-projectile-ag counsel-projectile-switch-to-buffer
;;;;;;  counsel-projectile-find-dir counsel-projectile-find-file)
;;;;;;  "counsel-projectile" "counsel-projectile.el" (22636 11768
;;;;;;  0 0))
;;; Generated autoloads from counsel-projectile.el

(autoload 'counsel-projectile-find-file "counsel-projectile" "\
Jump to a project's file using completion.

Replacement for `projectile-find-file'.  With a prefix ARG
invalidates the cache first.

\(fn &optional ARG)" t nil)

(autoload 'counsel-projectile-find-dir "counsel-projectile" "\
Jump to a project's directory using completion.

With a prefix ARG invalidates the cache first.

\(fn &optional ARG)" t nil)

(autoload 'counsel-projectile-switch-to-buffer "counsel-projectile" "\
Switch to a project buffer.

\(fn)" t nil)

(autoload 'counsel-projectile-ag "counsel-projectile" "\
Ivy version of `projectile-ag'.

\(fn &optional OPTIONS)" t nil)

(autoload 'counsel-projectile-switch-project "counsel-projectile" "\
Switch to a project we have visited before.

Invokes the command referenced by
`projectile-switch-project-action' on switch.  With a prefix ARG
invokes `projectile-commander' instead of
`projectile-switch-project-action.'

\(fn &optional ARG)" t nil)

(autoload 'counsel-projectile "counsel-projectile" "\
Use projectile with Ivy instead of ido.

With a prefix ARG invalidates the cache first.

\(fn &optional ARG)" t nil)

(eval-after-load 'projectile '(progn (define-key projectile-command-map (kbd "SPC") #'counsel-projectile)))

(autoload 'counsel-projectile-on "counsel-projectile" "\
Turn on counsel-projectile key bindings.

\(fn)" t nil)

(autoload 'counsel-projectile-off "counsel-projectile" "\
Turn off counsel-projectile key bindings.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("counsel-projectile-pkg.el") (22636 11768
;;;;;;  837000 0))

;;;***

(provide 'counsel-projectile-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; counsel-projectile-autoloads.el ends here
