;;; emacsql-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "emacsql" "emacsql.el" (0 0 0 0))
;;; Generated autoloads from emacsql.el

(autoload 'emacsql-show-last-sql "emacsql" "\
Display the compiled SQL of the s-expression SQL expression before point.
A prefix argument causes the SQL to be printed into the current buffer.

\(fn &optional PREFIX)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql" '("emacsql-")))

;;;***

;;;### (autoloads nil "emacsql-compiler" "emacsql-compiler.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from emacsql-compiler.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql-compiler" '("emacsql-")))

;;;***

;;;### (autoloads nil "emacsql-mysql" "emacsql-mysql.el" (0 0 0 0))
;;; Generated autoloads from emacsql-mysql.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql-mysql" '("emacsql-mysql-")))

;;;***

;;;### (autoloads nil "emacsql-pg" "emacsql-pg.el" (0 0 0 0))
;;; Generated autoloads from emacsql-pg.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql-pg" '("emacsql-pg-connection")))

;;;***

;;;### (autoloads nil "emacsql-psql" "emacsql-psql.el" (0 0 0 0))
;;; Generated autoloads from emacsql-psql.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql-psql" '("emacsql-psql-")))

;;;***

;;;### (autoloads nil "emacsql-sqlite" "emacsql-sqlite.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from emacsql-sqlite.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql-sqlite" '("emacsql-sqlite-")))

;;;***

;;;### (autoloads nil "emacsql-sqlite-builtin" "emacsql-sqlite-builtin.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emacsql-sqlite-builtin.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql-sqlite-builtin" '("emacsql-sqlite-builtin-connection")))

;;;***

;;;### (autoloads nil "emacsql-sqlite-common" "emacsql-sqlite-common.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emacsql-sqlite-common.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql-sqlite-common" '("emacsql-")))

;;;***

;;;### (autoloads nil "emacsql-sqlite-module" "emacsql-sqlite-module.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emacsql-sqlite-module.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql-sqlite-module" '("emacsql-sqlite-module-connection")))

;;;***

;;;### (autoloads nil nil ("emacsql-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; emacsql-autoloads.el ends here
