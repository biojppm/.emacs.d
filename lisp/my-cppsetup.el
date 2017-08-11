; LOTS OF INFO: https://tuhdo.github.io/c-ide.html
; https://martinralbrecht.wordpress.com/2014/11/03/c-development-with-emacs/
; see https://github.com/Sarcasm/irony-mode
; https://vxlabs.com/2016/04/11/step-by-step-guide-to-c-navigation-and-completion-with-emacs-and-the-clang-based-rtags/
; TODO: use libclang to provide c++/c
; http://tuhdo.github.io/c-ide.html
; https://github.com/Golevka/emacs-clang-complete-async

;; some useful C-mode shortcuts (see https://www.gnu.org/software/emacs/manual/html_node/emacs/C-Modes.html):
;;
;; C-c C-<DEL> or C-c <DEL>
;;    Delete the entire block of whitespace preceding point
;;    (c-hungry-delete-backwards).
;; C-c C-d or C-c C-<DELETE> or C-c <DELETE>
;;    Delete the entire block of whitespace after point
;;    (c-hungry-delete-forward).
;;
;; C-M-a
;;    move point to beginning of the current function or top-level
;;    definition
;; C-M-e
;;    move point to end of the current function or top-level
;;    definition
;;
;; C-c C-p
;;    Move point back over a preprocessor conditional, leaving the
;;    mark behind. A prefix argument acts as a repeat count. With a
;;
;; C-c C-n
;;    Move point forward across a preprocessor conditional, leaving
;;    the mark behind. A prefix argument acts as a repeat count. With
;;
;; M-a
;;    Move point to the beginning of the innermost C statement
;;    (c-beginning-of-statement). If point is already at the beginning
;;    statement. With prefix argument n, move back n − 1
;;    statements. In comments or in strings which span more than one
;;    line, this command moves by sentences instead of statements.
;;
;; M-e
;;    Move point to the end of the innermost C statement or sentence;
;;    like M-a except that it moves in the other direction
;;
;;
;; C-c C-l
;;    Toggle electric action (c-toggle-electric-state). With a
;;    positive prefix argument, this command enables electric action,
;;
;; C-c C-a
;;    Toggle the auto-newline feature (c-toggle-auto-newline). With a
;;    prefix argument, this command turns the auto-newline feature on
;;
;;
;; C-c C-w or M-x
;;    subword-mode Enable (or disable) subword mode.
;;
;; C-M-h
;;    Put mark at the end of a function definition, and put point at
;;    the beginning (c-mark-function).
;;
;; M-x c-context-line-break
;;    This command inserts a line break and indents the new line in a
;;    manner appropriate to the context.
;;
;; M-q
;;    Fill a paragraph, handling C and C++ comments (c-fill-paragraph)
;;
;; C-c C-e
;;    Run the C preprocessor on the text in the region
;;
;; C-c C-\
;;    Insert or align ‘\’ characters at the ends of the lines of the
;;    region (c-backslash-region)
;;
;; M-x
;;    cpp-highlight-buffer Highlight parts of the text according to
;;    its preprocessor conditionals.
;;
;; C-c C-s
;;    Display the syntactic information about the current source line
;;    (c-show-syntactic-information). This information directs how the
;;
;; M-x ff-find-related-file
;;    Find a file “related” in a special way to the file visited by
;;    the current buffer. Typically this will be the header file
;;    variable ff-related-file-alist specifies how to compute related
;;    file names.
;;

;;
;; non-CEDET setup (fast boot, doesn't require CEDET)
;; this setup is available for all
(defun my-c-hook()
  (interactive)

  (message "my-c-hook: entered")

  (hook-snips)

  (local-set-key (kbd "RET") 'c-context-line-break)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (local-set-key (kbd "M-q") 'c-fill-paragraph)

  ; Turn off tab character
  (setq-default indent-tabs-mode nil)

  ;; fuck electric mode (maybe? https://www.gnu.org/software/emacs/manual/html_node/emacs/Electric-C.html)
  (c-toggle-electric-state -1)
  ;(electric-indent-mode -1)

  ;; enable subword-mode
  (subword-mode 1)

  ;; shadow ifdef'ed-out code
  (setq hide-ifdef-shadow t)
  (hide-ifdef-mode 1)

  ;; Tabs, alignment, etc
  ;; taken from http://stackoverflow.com/questions/663588/emacs-c-mode-incorrect-indentation
  ;; Indent size
  (setq c-default-style "linux")
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace [0])
  (setq c++-tab-always-indent t)
  (setq standard-indent 4)
  (setq c-basic-offset 4)                  ;; Default is 2
  (setq c-indent-level 4)                  ;; Default is 2
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
  (setq tab-width 4)
  (setq indent-tabs-mode nil)  ; use spaces only if nil

  ; open related files in other window (ff-find-other-file)
  ; http://www.emacswiki.org/emacs/FindOtherFile
  (defvar my-cpp-other-file-alist
    '(("\\.cpp\\'" (".hpp" ".h" ".def.hpp" ".test.cpp" ".ipp" ".inl"))
      ("\\.hpp\\'" (".cpp" ".ipp" ".def.hpp" ".test.cpp" ".inl"))
      ("\\.ipp\\'" (".hpp" ".cpp" ".h"))
      ("\\.cxx\\'" (".hxx" ".h" ".ixx" ".inl"))
      ("\\.ixx\\'" (".cxx" ".h" ".hxx"))
      ("\\.hxx\\'" (".ixx" ".cxx" ".inl"))
      ("\\.c\\'" (".h"))
      ("\\.h\\'" (".c" ".cpp" ".inl" ".cxx"))
      ))
  (setq-default ff-other-file-alist 'my-cpp-other-file-alist)
; (setq ff-always-in-other-window 1) ; this doesnt work as it makes emacs open a
                                     ; new window every time instead of reusing an existing one
;  (setq ff-always-in-other-frame 1) ; will this work?

  ;; smart-parens
  ;; when pressing RET, the curly braces automatically
  ;; add another newline
  (sp-with-modes '(c-mode c++-mode)
                 (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
                 (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                                           ("* ||\n[i]" "RET"))))

  (message "my-c-hook: finished")
)


(defun load-ide-demo()
  (interactive)
  (add-to-list 'load-path (concat user-emacs-directory "emacs-c-ide-demo/custom"))

  (message "load-c-ide-demo: starting")

  (use-package counsel
    :defer t)
  (use-package counsel-projectile
    :defer t)
  (use-package volatile-highlights
    :defer t)
  (use-package dtrt-indent
    :defer t)
  (use-package anzu
    :defer t)
  (use-package clean-aindent-mode
    :defer t)

  (message "load-c-ide-demo: packages done.")

  (require 'setup-general)

  (message "load-c-ide-demo: stage 1 done.")

  (if (version< emacs-version "24.4")
      (require 'setup-ivy-counsel)
    (require 'setup-helm)
    (require 'setup-helm-gtags))

  (message "load-c-ide-demo: stage 2 done.")

  ;; (require 'setup-ggtags)
  (require 'setup-cedet)
  (require 'setup-editing)

  (message "load-c-ide-demo: stage 3 done.")

  (message "load-c-ide-demo: finished")
  )


;; CEDET
;; see:
;; http://www.logilab.org/173886
;; http://cedet.sourceforge.net/setup.shtml
;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
;; http://cxwangyi.wordpress.com/2010/08/21/using-cedet-with-emacs/
(defun load-cedet()
  (interactive)
  (add-to-list 'load-path "~/.emacs.d/cedet-1.1")
  (add-to-list 'load-path "~/.emacs.d/cedet-1.1/cogre")
  (add-to-list 'load-path "~/.emacs.d/cedet-1.1/ede")
  (add-to-list 'load-path "~/.emacs.d/cedet-1.1/eieio")
  (add-to-list 'load-path "~/.emacs.d/cedet-1.1/semantic")
  (add-to-list 'load-path "~/.emacs.d/cedet-1.1/speedbar")
  (add-to-list 'load-path "~/.emacs.d/cedet-1.1/srecode")

  (message "load-cedet: stage 1")
  (load-file "~/.emacs.d/cedet-1.1/common/cedet.el")
  (message "load-cedet: stage 2")
  (load-file "~/.emacs.d/cedet-1.1/contrib/semantic-tag-folding.el")
  (message "load-cedet: stage 3")

  (require 'semantic-ia)          ; names completion and display of tags
  (message "load-cedet: stage 4")

  (require 'semantic-gcc)         ; auto locate system include files
  (require 'semanticdb)
  (require 'eassist)
  (require 'semantic-tag-folding)

  (require 'auto-complete-exuberant-ctags)
  (ac-exuberant-ctags-setup)
  ;In your project root directory, do the following command to make tags file:
  ;ctags --verbose -R --fields="+afikKlmnsSzt"

  (message "load-cedet: stage 5")

  (global-ede-mode 1)                  ; enable the project management system
  ;(semantic-decoration-mode t) ; ERROR in emacs24
  (semantic-load-enable-minimum-features)
  (message "load-cedet: semantic-enable-minimum-features ok")
  (semantic-load-enable-code-helpers)  ; Enable prototype help and smart completion
  (message "load-cedet: semantic-load-enable-code-helpers ok")
  (semantic-load-enable-excessive-code-helpers)
  (message "load-cedet: semantic-load-enable-excessive-code-helpers")
  (global-srecode-minor-mode 1)        ; Enable template insertion menu
  (message "load-cedet: global-srecode-minor-mode")

  ;(semantic-add-system-include "~/3rd-party/boost-1.43.0/include/" 'c++-mode)
  ;(semantic-add-system-include "~/3rd-party/protobuf-2.3.0/include" 'c++-mode)

  (global-semanticdb-minor-mode 1)

  (add-hook 'speedbar-load-hook (lambda () (require 'semantic/sb)))

  ;(concat eassist-header-switches "hh" "cc" "hpp" "cpp")

  (message "load-cedet: stage 6")

  (defun my-cedet-hook ()
    (interactive)

    (global-semanticdb-minor-mode 1)

    (message "my-cedet-hook: entered")

    (semantic-default-c-setup)

    (local-set-key (kbd "C-.") 'semantic-ia-complete-symbol)
    (local-set-key (kbd "C-:") 'semantic-ia-complete-symbol-menu)
    (local-set-key (kbd "C-?") 'semantic-ia-complete-tip)
    (local-set-key (kbd "C-c >") 'semantic-complete-analyze-inline)

    (message "my-cedet-hook: stage 2")
    (local-set-key (kbd "C-c i") 'semantic-decoration-include-visit)
                                 ; go to the header file #included under cursor

    (local-set-key (kbd "C-c j") 'semantic-ia-fast-jump)
                                 ; go to definition
    (local-set-key (kbd "C-c J") 'semantic-complete-jump)
                                 ; go to definition

    (message "my-cedet-hook: stage 3")

    (local-set-key (kbd "C-c q") 'semantic-ia-show-doc)
    (local-set-key (kbd "C-c s") 'semantic-ia-show-summary)
    (local-set-key (kbd "C-c p") 'semantic-analyze-proto-impl-toggle)
    (local-set-key (kbd "C-c +") 'semantic-tag-folding-show-block)
    (local-set-key (kbd "C-c -") 'semantic-tag-folding-fold-block)
    (local-set-key (kbd "C-c C-+") 'semantic-tag-folding-show-all)
    (local-set-key (kbd "C-c C--") 'semantic-tag-folding-fold-all)
    (local-set-key (kbd "C-c t") 'eassist-switch-h-cpp)
    (local-set-key (kbd "C-c e") 'eassist-list-methods)


    (local-set-key (kbd "C-c C-r") 'semantic-symref)
                                   ; show references to the symbol
                                   ; under the cursor
    (local-set-key (kbd "C-c C-R") 'semantic-symref-symbol)
                                   ; show references to a symbol
                                   ; which will be prompted

    (message "my-cedet-hook: stage 4")
  )
  (add-hook 'c-mode-common-hook 'my-cedet-hook)
  ;(add-hook 'c++-mode-common-hook 'my-c-hook) ; c++-hook invokes c-hook

  (global-semantic-tag-folding-mode 1)


  (message "load-cedet: stage 7")

  ;; gnu global support
  (require 'semanticdb-global)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode)

  (message "load-cedet: stage 8")

  ;; ctags
  (require 'semanticdb-ectag)
  (semantic-load-enable-primary-exuberent-ctags-support)

  ;(global-semantic-idle-tag-highlight-mode 1) ; ERROR in emacs24

  (message "load-cedet: stage 9")

  (add-to-list 'load-path "~/.emacs.d/ecb-master")
  (require 'ecb)

  (message "load-cedet: finished!")
)

;-------------------------------------------------------------------------------
; ======== FORCE SEMANTIC PARSING OF FILES A DIRECTORY ========
; see http://stackoverflow.com/questions/18230838/semantic-cedet-how-to-force-parsing-of-source-files

(defvar c-files-regex ".*\\.\\(c\\|cpp\\|cxx\\|cc\\|h\\|hpp\\|hxx\\|hh\\|cu\\|cl\\)"
  "A regular expression to match any c/c++ related files under a directory")

(defun my-semantic-parse-tree (root regex)
  "
   This function is an attempt of mine to force semantic to
   parse all source files under a root directory. Arguments:
   -- root: The full path to the root directory
   -- regex: A regular expression against which to match all files in the directory
  "
  ;(message "PARSING: ENTERING DIR: %s" root)
  (let (
        ;;make sure that root has a trailing slash and is a dir
        (root (file-name-as-directory root))
        (files (directory-files root t ))
       )
    ;; remove current dir and parent dir from list
    (setq files (delete (format "%s." root) files))
    (setq files (delete (format "%s.." root) files))
    (while files
      (setq file (pop files))
      (if (not(file-accessible-directory-p file))
          ;;if it's a file that matches the regex we seek
          (progn (when (string-match-p regex file)
               (message "PARSING: %s" file)
               (save-excursion
                 (semanticdb-file-table-object file))
           ))
          ;;else if it's a directory
          (my-semantic-parse-tree file regex)
      )
    )
  )
)

(defun my-semantic-parse-current-dir (regex)
  "
   Parses all files under the current directory matching regex
  "
  (my-semantic-parse-tree (file-name-directory(buffer-file-name)) regex)
)

(defun my-parse-curdir-c ()
  "
   Parses all the c/c++ related files under the current directory
   and inputs their data into semantic
  "
  (interactive)
  (my-semantic-parse-current-dir c-files-regex)
)

(defun my-parse-dir-c (dir)
  "Prompts the user for a directory and parses all c/c++ related files
   under the directory
  "
  (interactive (list (read-directory-name "Provide the directory to search in:")))
  (my-semantic-parse-tree (expand-file-name dir) c-files-regex)
)

(provide 'lk-file-search)


