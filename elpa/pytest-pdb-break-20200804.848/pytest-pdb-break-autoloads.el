;;; pytest-pdb-break-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pytest-pdb-break" "pytest-pdb-break.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from pytest-pdb-break.el

(autoload 'pytest-pdb-break-here "pytest-pdb-break" "\
Run pytest on the test at point and break at BREAKPOINT.
BREAKPOINT may be a line number or cons of the form (FILENAME . LNUM).
NODE-ID-PARTS should be a list of pytest node-id components and
SESSION-OPTS a list of additional options.  `prefix-arg' behavior is
determined by `pytest-pdb-break-options-function'.

\(fn SESSION-OPTS BREAKPOINT NODE-ID-PARTS)" t nil)

(register-definition-prefixes "pytest-pdb-break" '("pytest-pdb-break-"))

;;;***

;;;### (autoloads nil "pytest-pdb-break-extra" "pytest-pdb-break-extra.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from pytest-pdb-break-extra.el

(autoload 'pytest-pdb-break-run-fail "pytest-pdb-break-extra" "\
Run pytest on test at point, dropping into PDB on first failure.
Otherwise, finish normally." t nil)

(autoload 'pytest-pdb-break-advise-elpy-shell-get-proc "pytest-pdb-break-extra" "\
A minor-mode hookee to help Elpy's send-related commands.
This is experimental, but `elpy-shell-send-statement' and
`elpy-shell-send-defun' seem to work.  Unlike the stock send commands,
the Elpy versions echo the input to the REPL (with proper indentation
and continuation ellipses)." nil nil)

(register-definition-prefixes "pytest-pdb-break-extra" '("pytest-pdb-break-"))

;;;***

;;;### (autoloads nil nil ("pytest-pdb-break-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pytest-pdb-break-autoloads.el ends here
