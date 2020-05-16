;;; emms-mark-ext-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "emms-mark-ext" "emms-mark-ext.el" (0 0 0 0))
;;; Generated autoloads from emms-mark-ext.el

(autoload 'emms-mark-mark-tagged "emms-mark-ext" "\
Mark all tracks whose TAG field matches REGEXP.
A prefix argument means to unmark them instead.
NOTE: if emms-mark-mode is not turned on, this function will
turn it on.

\(fn TAG REGEXP ARG)" t nil)

(autoload 'emms-tag-editor-alter-notes-tag "emms-mark-ext" "\
Alter arbitrary word tags to the info-note tag of tracks.
The info-tag will have a list of words separated by \":\".
If a prefix arg is supplied then the words should be removed from the
info-note tag for each track.
If region is selected then only alter fields within region.
WORDS should be a list of words (as strings) to add/remove. 
If nil then the words will be prompted for from the user with completion, until a blank is entered.
At each prompt the user can either enter one of the default words in emms-tag-editor-word-list or a new word.
If a new word is entered the user is prompted to add it to emms-tag-editor-word-list, where it will be saved.

\(fn WORDS ARG)" t nil)

(autoload 'emms-tag-editor-clear-field "emms-mark-ext" "\
Clear contents of a field for all tracks in tags editor.
If region is selected then only alter fields within region.

\(fn FIELD)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-mark-ext" '("emms-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; emms-mark-ext-autoloads.el ends here
