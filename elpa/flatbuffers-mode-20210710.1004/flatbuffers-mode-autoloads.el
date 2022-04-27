;;; flatbuffers-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flatbuffers-mode" "flatbuffers-mode.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from flatbuffers-mode.el

(autoload 'flatbuffers-mode "flatbuffers-mode" "\
Major mode for Flatbuffers code.

\\{flatbuffers-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.fbs\\'" . flatbuffers-mode))

(register-definition-prefixes "flatbuffers-mode" '("flatbuffers-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flatbuffers-mode-autoloads.el ends here
