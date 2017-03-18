;; see http://ergoemacs.org/emacs/elisp_basics.html
;; see http://ergoemacs.org/emacs/emacs_make_modern.html for lots of goodies
;; see http://stackoverflow.com/questions/5795451/how-to-detect-that-emacs-is-in-terminal-mode
;; Many of the use-package calls were taken from https://github.com/ljos/.emacs.d/blob/master/init.el

(setq emacs-dir (file-name-directory load-file-name))
(message (format "emacs-dir %s" emacs-dir))

;;; Emacs Load Path
(add-to-list 'load-path (concat emacs-dir "lisp"))

;;http://ikaruga2.wordpress.com/2011/04/11/testing-for-windows-in-emacs/
(defvar this-is-windows (string-match "windows" (symbol-name system-type)))

;-------------------------------------------------------------------------------
;;setup backup stuff
;(require 'backup-dir)
;(setq bkup-backup-directory-info '((t "~/.backups" ok-create full-path)))
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-by-copying-when-linked t
   backup-directory-alist '((".*" . "~/.backups")) ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;;reload files automatically
;;http://www.emacswiki.org/emacs/RevertBuffer
(global-auto-revert-mode 1)

(setq visible-bell t)       ; Disable bell
(setq inhibit-startup-screen t) ; Disable startup screen
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ; remove toolbar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ; remove menu
;;Avoid the symlink Version-Control warning
;;http://www.gnu.org/software/emacs/manual/html_node/emacs/General-VC-Options.html
;;http://stackoverflow.com/questions/15390178/emacs-and-symbolic-links
(setq vc-follow-symlinks t)

;; http://emacs.stackexchange.com/questions/7126/run-command-in-new-frame
(defun run-command-in-new-frame-simple (command)
  (select-frame (make-frame))
  (funcall #'command))

(defun run-command-in-new-frame (prefixarg command-name)
  "open a new frame and interactively run a command in it"
  (interactive (list current-prefix-arg (read-extended-command)))
  (let ((command (intern-soft command-name)))
    (unless command
      (error "%s is not a valid command name" command-name))
    (select-frame (make-frame))
    (let ((prefix-arg prefixarg))
      (command-execute command))))

;-------------------------------------------------------------------------------
;;MELPA - package installer
;;http://melpa.milkbox.net/#/getting-started

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
;(when (not package-archive-contents)
;; (package-refresh-contents))
(package-initialize)
;(setq package-enable-at-startup nil)

;;causes the package(s) to be installed automatically if not already present
;(setq use-package-always-ensure t)

;;use-package: https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;-------------------------------------------------------------------------------

;; Using use-package: see https://github.com/jwiegley/use-package
;; an example with inline explanations
;(use-package the-package
;
;; ; code to execute before the package is loaded
;; :init (setq the-package-variable t)
;
;; ; code to execute after the package is loaded. When the load is done lazily,
;; ; this execution is deferred until after the autoload occurs.
;; :config (the-package-mode 1)
;
;; ; create autoloads for these commands and defer loading of the
;; ; package until any them are used
;; :commands (the-package-cmd3 the-package-cmd4)
;
;; ; bind a key to primary commands within the package. Note the following:
;; ;   * Special keys like tab or F1-Fn can be written in square brackets,
;; ;     ie [tab] instead of "tab"
;; ;   * To bind a key within a local keymap that only exists after the
;; ;     package is loaded, use a :map modifier, taking the local keymap
;; ;     to bind to. See https://github.com/jwiegley/use-package#binding-within-local-keymaps
;; :bind (("C-." . the-package-cmd1)  ; commands here are implicitly added to the commands list
;;        ("C-:" . the-package-cmd2))
;
;; ; establish a deferred binding within the auto-mode-alist variable
;; :mode ("\\.py\\'" . python-mode)
;
;; ; establish a deferred binding within the interpreter-mode-alist variable
;; :interpreter ("\\.py\\'" . python-mode)
;; )


;;-------------------------------------------------------------------------------
;; Important shortcuts:

;; C-h k
;; F1 k
;;    describe the command which is bound to a shortcut
;; C-h c <command-name>
;; F1 c
;;    describe command
;; C-h w <command-name>
;; F1 w
;;    describe only the shortcuts where the command is bound (not the full documentation)
;; C-h f <function-name>
;; F1 f
;;    describe command name OR non-interactive function
;; C-h ?
;;    get help on getting help

;; M-x
;;    Run an interactive command ('execute-extended-command)
;; M-:
;;    Run elisp code
;; M-x eval-region
;;    execute selected elisp code
;; M-x eval-buffer
;;    execute selected elisp buffer
;; C-x e
;;    execute elisp code region or buffer

;; F3
;; C-x (
;;    start recording macro
;; F4
;; C-x )
;;    stop recording macro

;; C-x z
;;    repeat previous command. After this, pressing z will repeat again

(global-set-key [f2] 'repeat)  ; does the same as C-x z

;;------------------------------------------------------------------------------
;; Rectangular editing http://ergoemacs.org/emacs/emacs_string-rectangle_ascii-art.html

;; C-x space
;; C-space <movement> C-space
;;    start rectangular selection
;; C-x r t
;;    (replace-rectangle) replace text in selected rectangle
;; C-x r k
;;    (kill-rectangle) kill rectangle
;; C-x r d
;;    (delete-rectangle) delete rectangle
;; C-x r y
;;    (yank-rectangle) paste rectangular selection
;; C-x r o
;;    (open-rectangle) insert a whitespace rectangle into the region
;; C-x r N
;;    (rectangle-number-lines) insert numbers in a vertical column


;;------------------------------------------------------------------------------

(if window-system
  (progn
    ; turn on highlighting current line
    (global-hl-line-mode 1)

    (scroll-bar-mode 0)

;;   ;(set-frame-width (selected-frame) 90) ; set the editor window width in columns
    (require 'maximize)
;;   (require 'frame-cmds)
;
;;   (global-set-key (kbd "S-C-<f10>") 'maximize-toggle-frame-vmax)
;;   (global-set-key (kbd "S-M-<f10>") 'enlarge-frame)
;;   (global-set-key (kbd   "C-<f10>") 'shrink-frame)
;
;;   (global-set-key (kbd "S-C-<f11>") 'maximize-toggle-frame-hmax)
;;   (global-set-key (kbd "S-M-<f11>") 'enlarge-frame-horizontally)
;;   (global-set-key (kbd   "C-<f11>") 'shrink-frame-horizontally)
;
;;   (global-set-key (kbd "S-C-<f12>") 'toggle-max-frame)
  )
)

;;-------------------------------------------

(require 'column-marker)
;;http://askubuntu.com/questions/4820/keeping-emacs-from-splitting-the-window-when-openning-multiple-files
(add-hook 'emacs-startup-hook (lambda ()
  (if window-system
    (progn (maximize-toggle-frame-vmax))
    )
  (delete-other-windows)
  (column-marker-1 80)
  (column-marker-2 100)
  ;(set-frame-width (selected-frame) 130)
  )
)

;;-------------------------------------------
;; DIRTREE: https://github.com/zk/emacs-dirtree
;;
;; shortcuts:
;;
;; RET open node or open file in other window
;; U  go up to parent directory
;; n  next node
;; p  previous node
;; g  refresh node
;; D  delete tree
;;
;; http://stackoverflow.com/questions/9546562/emacs-dirtree-directory-tree-view-setup
(use-package dirtree
  :commands (dirtree dirtree-show)
  :bind  ("C-o" . dirtree-show)
)

;------------------------------------------------------------------
(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

;;http://lists.gnu.org/archive/html/help-gnu-emacs/2007-05/msg00975.html
(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer, ie, lock a buffer to the current window"
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))

;;(Try to) get a function to show the file tree on the left
(defun myide()
  (interactive)
;(maximize-frame-horizontally)
  (if (and window-system (< (frame-width) 130))
    (progn
      (set-frame-width (selected-frame) 130)))
  (setq dtw 30)
  ;------------------------------ set up a file tree window
  (split-window-horizontally)
  (dirtree "." ".")
  (sticky-buffer-mode 1)
  (set-window-width dtw)
  ; set a smaller font size on the file tree window
  '(buffer-face-mode t)
  (setq buffer-face-mode-face '(:height 80))
  (buffer-face-mode)
  (visual-line-mode 0)
  (linum-mode 0)         ; WTF? these two are necessary
  (setq linum-mode 0)    ; WTF? these two are necessary
  (other-window 1) ;switch the cursor
)
(global-set-key (kbd "C-<f6>") 'myide)


;;-------------------------------------------
;;COLOR-THEME

;;THEME GALLERY: https://pawelbx.github.io/emacs-theme-gallery/
;;zenburn-theme: https://github.com/bbatsov/zenburn-emacs
;;solarized-theme: https://github.com/bbatsov/solarized-emacs
;;monokai-theme: https://github.com/oneKelvinSmith/monokai-emacs
;;firebelly-theme: https://github.com/startling/firebelly
;;hemisu-dark-theme: https://github.com/andrzejsliwa/hemisu-theme

;;If Emacs looks considerably uglier in a terminal try setting this
;;environment variable:
;;export TERM=xterm-256color

(setq color-theme-is-global t)

(defun my-set-theme-zenburn()
  ; zenburn-theme: https://github.com/bbatsov/zenburn-emacs
  (interactive)
  (load-theme 'zenburn t)
)

(defun my-set-theme-solarized()
  ; solarized-theme: https://github.com/bbatsov/solarized-emacs
  (interactive)
  ;;; make the fringe stand out from the background
  ;(setq solarized-distinct-fringe-background t)
  ;;; Don't change the font for some headings and titles
  ;(setq solarized-use-variable-pitch nil)
  ;;; make the modeline high contrast
  ;(setq solarized-high-contrast-mode-line t)
  ;;; Use less bolding
  ;(setq solarized-use-less-bold t)
  ;; Use more italics
  (setq solarized-use-more-italic t)
  ;;; Use less colors for indicators such as git:gutter, flycheck and similar
  ;(setq solarized-emphasize-indicators nil)
  ;;; Don't change size of org-mode headlines (but keep other size-changes)
  ;(setq solarized-scale-org-headlines nil)
  ;;; Avoid all font-size changes
  ;(setq solarized-height-minus-1 1)
  ;(setq solarized-height-plus-1 1)
  ;(setq solarized-height-plus-2 1)
  ;(setq solarized-height-plus-3 1)
  ;(setq solarized-height-plus-4 1)
  (load-theme 'solarized-dark t)
)

(defun my-set-theme-monokai()
  ; monokai-theme: https://github.com/oneKelvinSmith/monokai-emacs
  (interactive)
  (load-theme 'monokai t)
  (if window-system
      (progn
        (set-face-background 'hl-line "#373737")
        (set-face-background 'region "#666666")
        )
    )
)

(defun my-set-theme-firebelly()
  ; firebelly-theme: https://github.com/startling/firebelly
  (interactive)
  (load-theme 'firebelly t)
)

(defun my-set-theme-hemisu()
  ; hemisu-dark-theme: https://github.com/andrzejsliwa/hemisu-theme
  (interactive)
  (load-theme 'hemisu-dark t)
)

(defun my-set-theme-tango-2()
  (interactive)
  (load-theme 'tango-2 t)
)

(my-set-theme-monokai)
;(my-set-theme-tango-2)

;;-------------------------------------------
;; Show line numbers
(global-linum-mode 1)
;;Use C-<F5> to toggle line numbers
(global-set-key (kbd "C-<f5>") 'linum-mode)

;; Show column number
(column-number-mode 1)

;; Soft-wrap long lines
(global-visual-line-mode 1) ; for all files

(defun disable-line-wrapping()
  (interactive)
  (visual-line-mode 0)
  (toggle-truncate-lines 1)
)

(use-package window-number
  :commands window-number-mode
  :load-path "site-lisp/"
  :init (window-number-mode))

;;-------------------------------------------------------------------------
;; Ansi term

;; Terminal buffer configuration. https://www.emacswiki.org/emacs/AnsiTermHints
(add-hook 'term-mode-hook 'my-term-mode-hook)
(defun my-term-mode-hook ()
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20611
  (setq bidi-paragraph-direction 'left-to-right))

;; Set Windows-specific preferences if running in a Windows environment.
(defun udf-windows-setup () (interactive)
  ;(setq git-shell-exe "C:\\Git\\bin\\sh")
  (setq git-shell-exe "C:\\Git\\bin\\bash")
  (setq git-shell-dir "C:\\Git\\bin\\")
  ;(setq git-shell-dir (file-name-directory git-shell-exe)) ; this turns \ into /
  (add-to-list 'exec-path git-shell-dir)
  (setenv "PATH"
          (concat git-shell-dir ";"
                  (getenv "PATH")))
  ; http://emacs.stackexchange.com/questions/22049/git-bash-in-emacs-on-windows
  (prefer-coding-system 'utf-8)
  (setq explicit-shell-file-name git-shell-exe)
  (setq explicit-bash.exe-args '("--login" "-i"))
  (setq term-buffer-maximum-size 8192)
  )

(if (eq system-type 'windows-nt)
    (udf-windows-setup))

;;-------------------------------------------------------------------------
;; IDO mode: (Interactively DO things)
;; https://www.masteringemacs.org/article/introduction-to-ido-mode

(require 'flx-ido) ; https://github.com/lewang/flx
(setq ido-everywhere t) ; enable basic IDO support for files and buffers
(setq ido-enable-flex-matching t)
(ido-mode 1)
(flx-ido-mode)

;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; set garbage-collection threshold to 10MB to speed up flx-ido:
;; see https://github.com/lewang/flx
(setq gc-cons-threshold 10000000)

;;FIND FILE AT POINT:
;;either this:
(setq ido-use-filename-at-point 'guess)
;;... or this
;(setq ido-use-url-at-point t) ; disables find file at point
;;don't ask
(setq ido-create-new-buffer 'always)
;;give priority to certain extensions when presenting candidates in the minibuffer
(setq ido-file-extensions-order '(
   ".hpp" ".h" ".cpp" ".c"
   ".py"
   ".cmake"
   ".txt" ".el" ".ini" ".cfg" ".cnf"))
;;Takes a list of buffers to ignore in C-x b
;(setq ido-ignore-buffers '())
;;Takes a list of directories to ignore in C-x d and C-x C-f
;(setq ido-ignore-directories '())
;;Takes a list of files to ignore in C-x C-f
;(setq ido-ignore-files '())

;;General-purpose IDO Commands
;
;;To skip IDO's current suggestion and accept what's already typed-in,
;;hit C-j
;
;;Tricks for windows:
;; * for opening a file/dir in a different drive (eg D:), do: C-x C-f D:/
;;   and IDO will intelligently switch to D:
;; * for opening a file/dir in the home directory do: C-X C-f ~/
;
;;C-b Reverts to the old switch-buffer completion engine. Available in Buffers.
;;C-f Reverts to the old find-file completion engine. Available in Files
;;C-d Opens a dired buffer in the current directory. Available in Dirs / Files
;;C-a Toggles showing ignored files (see ido-ignore-files). Available in Files / Buffers
;;C-c Toggles if searching of buffer and file names should ignore case. (see
;;    ido-case-fold). Available in Dirs / Files / Buffers
;;TAB Attempt to complete the input like the normal completing read
;;    functionality. Available in Dirs / Files / Buffers
;;C-p Toggles prefix matching; when it's on the input will only match the
;;    beginning of a filename instead of any part of it.
;
;;Files
;
;;C-s / C-r
;;            Moves to the next and previous match, respectively. Available
;;            everywhere
;;C-t
;;            Toggles matching by Emacs regular expression.. Available everywhere
;;Backspace
;;            Deletes characters as usual or goes up one directory if it makes
;;            sense to do so.. Available everywhere
;;C-SPC / C-@
;;            Restricts the completion list to anything that matches your
;;            current input. Available everywhere
;;//
;;            Like most Linux shells two forward slashes in a path means
;;            "ignore the preceding path, and go back to the top-most
;;            directory". Works the same in Ido but it's more interactive: it
;;            will go to the root / (or the root of the current drive in
;;            Windows) Available in Files
;;~/
;;            Jumps to the home directory. On Windows this would be typically
;;            be %USERPROFILE% or %HOME%, if it is defined. Available in
;;            Files / Dirs
;;M-d
;;            Searches for the input in all sub-directories to the directory
;;            you're in.. Available in Files
;;C-k
;;            Kills the currently focused buffer or deletes the file
;;            depending on the mode.. Available in Files / Buffers
;;M-m
;;            Creates a new sub-directory to the directory you're
;;            in. Available in Files
;
;;OK, so you probably won't get in the habit of using all the commands;
;;that's fine, but some are more important to remember than others, like:
;;Backspace; C-s and C-r; // and ~/; and C-d.
;
;;If Ido is getting in your way, remember the fallback commands:
;; C-f for files; C-b for buffers.


;;-------------------------------------------------------------------------
;; Auto complete

;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;(setq ac-comphist-f)
;(ac-config-default)
;;(require 'auto-complete-etags)

;;General Usage: Completion will start automatically after you type a
;;few letters. Use M-n and M-p to select, <return> to complete or <tab>
;;to complete the common part. Search through the completions with C-s,
;;C-r and C-o. Press M-(digit) to quickly complete with one of the
;;first 10 candidates. When the completion candidates are shown, press
;;<f1> to display the documentation for the selected candidate, or C-w
;;to see its source. Not all back-ends support this.
;
;;The variable company-backends specifies a list of backends that
;;company-mode uses to retrieves completion candidates for you.

(use-package company
  :config
  (global-company-mode)
  ;; https://tuhdo.github.io/c-ide.html#orgheadline15
  (add-to-list 'company-backends 'company-c-headers)
  (setq company-backends (delete 'company-semantic company-backends))
  :bind
  (:map c-mode-base-map ("(tab)" . company-complete)
   :map c++-mode-base-map ("(tab)" . company-complete)
  )
)

;; stop safety warnings when this var is given in .dir-locals.el
;; see http://emacs.stackexchange.com/questions/18774/trust-dir-locals-el
(put 'company-clang-arguments 'safe-local-variable #'listp)
(put 'c4stl-dir 'safe-local-variable #'stringp)

;;-------------------------------------------------------------------------

(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh"
	tramp-auto-save-directory
        (expand-file-name "~/.emacs.d/auto-save-list"))
  )

;;=========================================================================
;; CURSOR MOVEMENT

;;-------------------------------------------------------------------------
;; Smooth scrolling
;; http://www.emacswiki.org/emacs/SmoothScrolling

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-conservatively 10000) ;; scroll just one line when hitting bottom of window
(setq auto-window-vscroll nil)

;; scroll without moving the cursor
(defun gcm-scroll-down ()
  (interactive)
  (scroll-up 1))
(defun gcm-scroll-up ()
  (interactive)
  (scroll-down 1))
(global-set-key (kbd "S-<down>") 'gcm-scroll-down)
(global-set-key (kbd "S-<up>") 'gcm-scroll-up)

;; Recentering:
;; C-l
;;     Scroll the selected window so the current line is the
;;     center-most text line; on subsequent consecutive invocations,
;;     make the current line the top line, the bottom line, and so on
;;     in cyclic order. Possibly redisplay the screen too
;;     (recenter-top-bottom).
;; M-x recenter
;;     Scroll the selected window so the current line is the
;;     center-most text line. Possibly redisplay the screen too.
;; C-M-l
;;     Scroll heuristically to bring useful information onto the
;;     screen (reposition-window).

;;-------------------------------------------------------------------------
;;Move to first non-whitespace or beginning of line
;;http://superuser.com/questions/331221/jump-to-first-non-whitespace-character-in-line-in-emacs
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key [home] 'smart-beginning-of-line)
(global-set-key "\C-a" 'smart-beginning-of-line)

;;-------------------------------------------------------------------------
;;Windows navigation + splitting

;;we need to call this again in some hooks
(defun win-nav-rsz()
  "set shortcuts to resize the window"
  (interactive)
  ; http://www.emacswiki.org/emacs/WindowResize
  (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
  (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "S-C-<down>") 'shrink-window)
  (global-set-key (kbd "S-C-<up>") 'enlarge-window)

  ; move window focus (eg, frame in other progs) FOCUS
  (global-set-key (kbd "M-s M-<up>") 'windmove-up)
  (global-set-key (kbd "M-s M-<down>") 'windmove-down)
  (global-set-key (kbd "M-s M-<right>") 'windmove-right)
  (global-set-key (kbd "M-s M-<left>") 'windmove-left)
  ; More window/frame shortcuts:
  ; C-x 0: delete-window
  ; C-x 1: delete-other-windows
  ; C-x 2: split-window-vertically
  ; C-x 3: split-window-horizontally
  ; C-x 4 0: kill-buffer-and-window
  ; C-x 5 0: kill-buffer-and-window
  ; C-x +: balance-windows
  ; C-x o: other-window

  ; C-x C-b: list-buffers
  ; C-x b: go to buffer
)

(win-nav-rsz)

;;=========================================================================
;; EDITING

;;------------------------------------------------------------------
;; drag line/selection up or down
;; http://emacs.stackexchange.com/questions/13941/move-selected-lines-up-and-down

(use-package drag-stuff
  :commands (drag-stuff-up drag-stuff-down)
  :bind
  (("M-<up>" . drag-stuff-up)
   ("M-<down>" . drag-stuff-down))
)

;;------------------------------------------------------------------
;;easily show the kill-ring

;;http://pragmaticemacs.com/emacs/counsel-yank-pop-with-a-tweak/
(use-package counsel
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("S-M-y" . ivy-previous-line)
   ("M-y" . ivy-next-line)))

;;-------------------------------------------------------------------------
;; Undo tree

;;treats undo history as a branching tree of changes, similar to the
;;way Vim handles it. This makes it substantially easier to undo and
;;redo any change, while preserving the entire history of past
;;states. The undo-tree visualizer is particularly helpful in complex
;;cases.
;; C-_  C-/  (`undo-tree-undo')  Undo changes.
;; M-_  C-?  (`undo-tree-redo')  Redo changes.
;; `undo-tree-switch-branch'  Switch undo-tree branch.
;; C-x r u  (`undo-tree-save-state-to-register')  Save current buffer state to register.
;; C-x r U  (`undo-tree-restore-state-from-register') Restore buffer state from register.
;;
;; C-x u  (`undo-tree-visualize')  Visualize the undo tree.
;; In the undo-tree visualizer:
;;
;; <up>  p  C-p  (`undo-tree-visualize-undo')  Undo changes.
;; <down>  n  C-n  (`undo-tree-visualize-redo')  Redo changes.
;; <left>  b  C-b  (`undo-tree-visualize-switch-branch-left')  Switch to previous undo-tree branch.
;; <right>  f  C-f  (`undo-tree-visualize-switch-branch-right')  Switch to next undo-tree branch.
;; C-<up>  M-{  (`undo-tree-visualize-undo-to-x')  Undo changes up to last branch point.
;; C-<down>  M-}  (`undo-tree-visualize-redo-to-x')  Redo changes down to next branch point.
;; <down>  n  C-n  (`undo-tree-visualize-redo')  Redo changes.
;; <mouse-1>  (`undo-tree-visualizer-mouse-set')  Set state to node at mouse click.
;; t  (`undo-tree-visualizer-toggle-timestamps')  Toggle display of time-stamps.
;; d  (`undo-tree-visualizer-toggle-diff')   Toggle diff display.
;; s  (`undo-tree-visualizer-selection-mode')   Toggle keyboard selection mode.
;; q  (`undo-tree-visualizer-quit')    Quit undo-tree-visualizer.
;; C-q  (`undo-tree-visualizer-abort')    Abort undo-tree-visualizer.
;; ,  <   Scroll left.
;; .  >   Scroll right.
;; <pgup>  M-v   Scroll up.
;; <pgdown>  C-v   Scroll down.

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  )

;;-------------------------------------------------------------------------
;;Insert a new line and jump to it, indenting
;;http://superuser.com/questions/331660/how-to-insert-a-new-line-and-jump-to-it-in-emacs
(global-set-key (kbd "<S-return>") "\C-e\C-m")   ; create line after current and indent
(global-set-key (kbd "<S-C-return>") "\C-p\C-e\C-m") ; create line before current and indent

;;------------------------------------------------------------------
;; ===== Delete the previous word without adding it to the killring =====
;;see http://stackoverflow.com/questions/6133799/delete-a-word-without-adding-it-to-the-kill-ring-in-emacs?rq=1

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

;;when in the minibuffer, don't yank deleted words to the killring
(define-key minibuffer-local-map [M-backspace] 'backward-delete-word)

;;------------------------------------------------------------------
;; ===== Delete adjacent whitespace forward/backward =====

(use-package hungry-delete
  :commands (hungry-delete-forward
	     hungry-delete-backward)
  :bind (("C-c C-<delete>" . hungry-delete-forward)
         ("C-c C-<backspace>" . hungry-delete-backward)))

;;------------------------------------------------------------------
;; ===== Function to delete a line =====

;; There's also the command kill-whole-line (C-S-backspace), which
;; does the same without saving the column position.

;; First define a variable which will store the previous column position
(defvar nuke-line-previous-column nil "Save the column position")

;; Define the nuke-line function. The line is killed, then the newline
;; character is deleted. The column which the cursor was positioned at is then
;; restored. Because the kill-line function is used, the contents deleted can
;; be later restored by usibackward-delete-char-untabifyng the yank commands.
(defun nuke-line()
  "Kill an entire line, including the trailing newline character"
  (interactive)

  ;; Store the current column position, so it can later be restored for a more
  ;; natural feel to the deletion
  (setq nuke-line-previous-column (current-column))

  ;; Now move to the end of the current line
  (end-of-line)

  ;; Test the length of the line. If it is 0, there is no need for a
  ;; kill-line. All that happens in this case is that the new-line character
  ;; is deleted.
  (if (= (current-column) 0)
    (delete-char 1)

    ;; This is the 'else' clause. The current line being deleted is not zero
    ;; in length. First remove the line by moving to its start and then
    ;; killing, followed by deletion of the newline character, and then
    ;; finally restoration of the column position.
    (progn
      (beginning-of-line)
      (kill-line)
      (delete-char 1)
      (move-to-column nuke-line-previous-column))))

;; Now bind the delete line function to Shift- delete
(global-set-key [S-delete] 'nuke-line)

;;------------------------------------------------------------------
;; http://stackoverflow.com/a/4717026/5875572

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.  With negative N, comment out
original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(global-set-key (kbd "C-d") 'duplicate-line-or-region)
(global-set-key (kbd "C-d") 'duplicate-line-or-region)
(global-set-key (kbd "C-d") 'duplicate-line-or-region)
(global-set-key (kbd "C-d") 'duplicate-line-or-region)
(global-set-key (kbd "C-d") 'duplicate-line-or-region)
(global-set-key (kbd "C-d") 'duplicate-line-or-region)
(global-set-key (kbd "C-d") 'duplicate-line-or-region)
(global-set-key (kbd "C-d") 'duplicate-line-or-region)

;;-------------------------------------------------------------------------

;;a minor mode that provides many features for manipulating
;;pairs. Pair can be simple as parentheses or brackets, or can be
;;programming tokens such as if … fi or if … end in many
;;languages. The most basic and essential feature is automatic closing
;;of a pair when user inserts an opening one.
(use-package smartparens
  :commands (smartparens-mode
             smartparens-strict-mode
             sp-with-modes)
  :bind (:map smartparens-strict-mode-map
              ("C-}" . sp-forward-slurp-sexp)
              ("M-s" . sp-backward-unwrap-sexp)
              ("C-c [" . sp-select-next-thing)
              ("C-c ]" . sp-select-next-thing-exchange))
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode +1)
  (smartparens-global-mode 1)
  )
;; something else interacted with this. Manually enabling seems to fix it.
(smartparens-global-mode 1)

(use-package highlight-symbol
  :commands highlight-symbol-mode
  :config
  (setq highlight-symbol-idle-delay 0.2)
  (add-hook 'highlight-symbol-mode-hook
            (function
             (lambda () (highlight-symbol-nav-mode +1)(message "highlight porra")))))

;(use-package smart-mode-line
;; :init
;; (setq-default sml/vc-mode-show-backend t
;		sml/theme 'respectful)
;; (sml/setup))

;;-------------------------------------------------------------------------
;;SNIPPETS

(defun use-snips()
  (use-package yasnippet
    :config
    )
)

(defun hook-snips()
  (message "enabling YASnippet...")
  ; insert our dir at the front of the default snippets
  ;(setq sdir (concat emacs-dir "snippets"))
  ;(message (format "sdir=%s" sdir))
  ;(message (format "snippet_dirs=%s" yas-snippet-dirs))
  ;(setq yas-snippet-dirs (add-to-list 'yas-snippet-dirs sdir))
  (message (format "yas-snippet-dirs=%s" yas-snippet-dirs))
  (yas-reload-all)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map [C-tab] 'yas-expand)
  (yas-minor-mode-on)
  (message "enabling YASnippet: done")
)

(defun yasl()
  (interactive)
  (yas-describe-tables)
)

;;-------------------------------------------------------------------------
;;MODES
;;to enable a mode at runtime, type M-x the-mode-name

;;Turn off tab character
(setq-default indent-tabs-mode nil)

;;Indent size
(setq standard-indent 4)

;;Auto-close bracket pairs
;(electric-pair-mode 1)

;; show unnecessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (disable-line-wrapping)
            (setq show-trailing-whitespace 1)))

;; The ws-butler package takes care of whitespaces discretely by
;; fixing up whitespaces only for those lines you touched.
(use-package ws-butler
  :defer t
  :commands ws-butler-mode
  :init (add-hook 'prog-mode-hook #'ws-butler-mode)
  )


;;; C/C++
(load "my-cppsetup")
(use-package clang-format
  :defer t
  :bind ("C-c C-M-f" . clang-format-region))
(use-package c-mode
  :defer t
  :config
  (use-snips)(add-hook 'c-mode-common-hook #'my-c-hook))
(use-package cc-mode
  :defer t
  :config
  (use-snips)(add-hook 'c-mode-common-hook #'my-c-hook))



;;; PHP
(use-package php-mode
  :defer t
  :config (define-abbrev php-mode-abbrev-table "ex" "extends")
  (use-snips)(add-hook 'php-mode-hook #'hook-snips)
)


;;; C#
(use-package csharp-mode
  :config (c-set-offset 'substatement-open 0)
  (use-snips)(add-hook 'csharp-mode-hook #'hook-snips)
  :mode (("\\.cs\\'" . csharp-mode)
         ("\\.CS\\'" . csharp-mode)))


;;; Python
;;https://elpy.readthedocs.io/en/
;;https://github.com/jorgenschaefer/elpy
(defun my-python-hook ()
  (win-nav-rsz)
  (global-set-key [C-<up>] 'backward-paragraph)
  (global-set-key [C-<down>] 'forward-paragraph)
  )
(use-package python
  :defer t
  :mode ("\\.py" . python-mode)
  :config
  (use-package elpy
    :commands elpy-enable
    :config
;;   (setq elpy-rpc-python-command "python3"
;;         elpy-modules (dolist (elem '(elpy-module-highlight-indentation
;;                                     elpy-module-yasnippet))
;;                        (remove elem elpy-modules))
;;         )
    (elpy-use-ipython))
  (elpy-enable)
  (use-snips)(add-hook 'python-mode-hook #'hook-snips)
  ;(add-hook 'python-mode-hook #'smartparens-strict-mode)
  (add-hook 'python-mode-hook #'my-python-hook)
  )
(use-package cython-mode
  :mode (("\\.py[xdi]" . cython-mode)))


;;; GNU R
;;  instructions on ESS: http://ess.r-project.org/Manual/ess.html
;;  http://cran.r-project.org/doc/FAQ/R-FAQ.html#R-and-Emacs
(use-package ess-site
  :ensure ess
  :mode (("\\.R\\'" . R-mode)
         ("\\.r\\'" . R-mode))
  :commands R
  :config
  (use-snips)(add-hook 'R-mode-hook #'hook-snips)
  (add-hook 'R-mode-hook #'subword-mode)
  (add-hook 'R-mode-hook #'smartparens-strict-mode))


;;; XML
;;http://superuser.com/questions/383520/how-to-efficiently-type-in-a-pair-of-xml-tags-in-emacs
(use-package nxml
  :init (setq nxml-slash-auto-complete-flag t)
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.xsd\\'" . nxml-mode)
         ("\\.xslt\\'" . nxml-mode)
         ("\\.xsl\\'" . nxml-mode)
         ("\\.rng\\'" . nxml-mode)
         ("\\.dtllp\\'" . nxml-mode)))
(defun unhtml (start end)
  "escape html characters: &, <, >.
  taken from: http://shallowsky.com/blog/linux/editors/emacs-escape-html.html"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (replace-string "&" "&amp;")
      (goto-char (point-min))
      (replace-string "<" "&lt;")
      (goto-char (point-min))
      (replace-string ">" "&gt;")
      )))


;;; YAML
(use-package yaml-mode
  :config (use-snips)(add-hook 'yaml-mode-hook #'hook-snips)
  :mode ("\\.yml\\'" . yaml-mode)
)


;;; CMake
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :config (use-snips)(add-hook 'cmake-mode-hook #'hook-snips)
  (setq cmake-tab-width 4)
  )


;;; GLSL
(use-package glsl-mode
  :config (use-snips)(add-hook 'glsl-mode-hook #'hook-snips)
  :mode ("\\.glsl\\'" . glsl-mode)
  )


;;; HLSL
(use-package hlsl-mode
  :config (use-snips)(add-hook 'hlsl-mode-hook #'hook-snips)
  :mode (("\\.hlsl\\'" . hlsl-mode)
         ("\\.hlsli\\'" . hlsl-mode)
         ("\\.fx\\'" . hlsl-mode)
         ("\\.usf\\'" . hlsl-mode))
  )


;;; Octave/Matlab
;;see http://www.gnu.org/software/octave/doc/v4.0.1/Using-Octave-Mode.html#Using-Octave-Mode
(use-package octave-mode
  :init
  (abbrev-mode 1)
  (auto-fill-mode 1)
  (if (eq window-system 'x)
      (font-lock-mode 1))
  :config
  (use-snips)(add-hook 'octave-mode-hook #'hook-snips)
  :mode ("\\.m$" . octave-mode)
  )


;;; Lisp
;(use-package lisp-mode
;; :config
;; (use-package elisp-slime-nav
;;   :commands elisp-slime-nav-mode)
;; (use-package macrostep
;;   :bind ("C-c e" . macrostep-expand))
;; (use-package slime
;;   :commands (slime slime-lisp-mode-hook)
;;   :config
;;   (add-to-list 'slime-contribs 'slime-fancy)
;;   (slime-setup)
;;   (add-hook 'slime-repl-mode-hook #'smartparens-strict-mode))
;
;; (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
;; (add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
;; (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
;; (add-hook 'ielm-mode-hook #'elisp-slime-nav-mode)
;; (add-hook 'ielm-mode-hook #'turn-on-eldoc-mode)
;; (add-hook 'lisp-interaction-mode-hook #'turn-on-eldoc-mode)
;
;; (add-hook 'lisp-mode-hook #'smartparens-strict-mode)
;; (add-hook 'lisp-mode-hook #'slime-lisp-mode-hook)
;
;; (setq inferior-lisp-program "sbcl --dynamic-space-size 1024"))

;;-------------------------------------------------------------------------
;;Running Compilations under Emacs: https://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation.html
;
;;M-x compile
;;    Run a compiler asynchronously under Emacs, with error messages going to the *compilation* buffer.
;;M-x recompile
;;    Invoke a compiler with the same command as in the last invocation of M-x compile.
;;M-x kill-compilation
;;    Kill the running compilation subprocess.

;;Compilation mode commands: https://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation-Mode.html#Compilation-Mode
;
;;M-g M-n
;;M-g n
;;C-x `
;;    Visit the locus of the next error message or match (next-error).
;;M-g M-p
;;M-g p
;;    Visit the locus of the previous error message or match (previous-error).
;;M-n
;;    Move point to the next error message or match, without visiting its locus (compilation-next-error).
;;M-p
;;    Move point to the previous error message or match, without visiting its locus (compilation-previous-error).
;;M-}
;;    Move point to the next error message or match occurring in a different file (compilation-next-file).
;;M-{
;;    Move point to the previous error message or match occurring in a different file (compilation-previous-file).
;;C-c C-f
;;    Toggle Next Error Follow minor mode, which makes cursor motion in the compilation buffer produce automatic source display.
;

;;see https://www.emacswiki.org/emacs/CompilationMode#toc4
(defun my-compile()
  "run compile"
  (interactive)
  (progn
    (call-interactively 'compile)
    (setq cur (selected-window))
    (setq w (get-buffer-window "*compilation*"))
    (select-window w)
    (disable-line-wrapping)
    (linum-mode 0)
    ;(setq h (window-height w))
    ;(shrink-window (- h 10))
    (select-window cur)
    )
  )
(defun my-compilation-hook()
  )
(add-hook 'compilation-mode-hook 'my-compilation-hook)
(global-set-key [C-pause] 'kill-compilation)
(global-set-key [S-f6] 'my-compile)
(global-set-key [f6] 'recompile)
(global-set-key [f8] 'next-error)
(global-set-key [S-f8] 'previous-error)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;-------------------------------------------------------------------------
;; Debugging https://www.gnu.org/software/emacs/manual/html_node/emacs/GDB-Graphical-Interface.html#GDB-Graphical-Interface
;;
;; M-x gdb
;;    start an interactive gdb GUI session
;; M-x gdb-restore-windows
;;    restore the many windows layout
;;
;; If gdb-many-windows is non-nil, then M-x gdb displays the following frame layout:
;;
;; +----------------------------------------------------------------------+
;; |                               GDB Toolbar                            |
;; +-----------------------------------+----------------------------------+
;; | GUD buffer (I/O of GDB)           | Locals buffer                    |
;; +-----------------------------------+----------------------------------+
;; | Source buffer                     | I/O buffer (of debugged program) |
;; |                                   | (comint-mode)                    |
;; |                                   |                                  |
;; +-----------------------------------+----------------------------------+
;; | Stack buffer                      | Breakpoints buffer               |
;; | RET      gdb-select-frame         | SPC    gdb-toggle-breakpoint     |
;; |                                   | RET    gdb-goto-breakpoint       |
;; |                                   | D      gdb-delete-breakpoint     |
;; +-----------------------------------+----------------------------------+
;;
;; GUD provides commands for setting and clearing breakpoints,
;; selecting stack frames, and stepping through the program.
;;
;; C-x C-a C-b
;;
;;     Set a breakpoint on the source line that point is on.
;;
;; C-x C-a C-b
;;
;;     (gud-break), when called in a source buffer, sets a debugger
;;     breakpoint on the current source line. This command is
;;     available only after starting GUD. If you call it in a buffer
;;     that is not associated with any debugger subprocess, it signals
;;     a error.
;;
;; The following commands are available both in the GUD interaction
;; buffer and globally, but with different key bindings. The keys
;; starting with C-c are available only in the GUD interaction buffer,
;; while those starting with C-x C-a are available globally. Some of
;; these commands are also available via the tool bar; some are not
;; supported by certain debuggers.
;;
;; C-c C-l
;; C-x C-a C-l
;;     (gud-refresh) Display, in another window, the last source line
;;     referred to in the GUD interaction buffer (gud-refresh).
;;
;; C-c C-s
;; C-x C-a C-s
;;     (gud-step) Execute the next single line of code (gud-step). If
;;     the line contains a function call, execution stops after
;;     entering the called function.
;;
;; C-c C-n
;; C-x C-a C-n
;;     (gud-next) Execute the next single line of code, stepping
;;     across function calls without stopping inside the functions
;;     (gud-next).
;;
;; C-c C-i
;; C-x C-a C-i
;;     (gud-stepi) Execute a single machine instruction (gud-stepi).
;;
;; C-c C-p
;; C-x C-a C-p
;;     (gud-print) Evaluate the expression at point (gud-print). If
;;     Emacs does not print the exact expression that you want, mark
;;     it as a region first.
;;
;; C-c C-r
;; C-x C-a C-r
;;     (gud-cont) Continue execution without specifying any stopping
;;     point. The program will run until it hits a breakpoint,
;;     terminates, or gets a signal that the debugger is checking for
;;     (gud-cont).
;;
;; C-c C-d
;; C-x C-a C-d
;;     (gud-remove) Delete the breakpoint(s) on the current source
;;     line, if any (gud-remove). If you use this command in the GUD
;;     interaction buffer, it applies to the line where the program
;;     last stopped.
;;
;; C-c C-t
;; C-x C-a C-t
;;     (gud-tbreak) Set a temporary breakpoint on the current source
;;     line, if any (gud-tbreak). If you use this command in the GUD
;;     interaction buffer, it applies to the line where the program
;;     last stopped.
;;
;; C-c <
;; C-x C-a <
;;     (gud-up) Select the next enclosing stack frame (gud-up). This
;;     is equivalent to the GDB command ‘up’.
;;
;; C-c >
;; C-x C-a >
;;     (gud-down)
;;     Select the next inner stack frame (gud-down). This is
;;     equivalent to the GDB command ‘down’.
;;
;; C-c C-u
;; C-x C-a C-u
;;     (gud-until) Continue execution to the current line. The program
;;     will run until it hits a breakpoint, terminates, gets a signal
;;     that the debugger is checking for, or reaches the line on which
;;     the cursor currently sits.
;;
;; C-x C-a C-w
;;     (gud-watch) Add a watch expression of the variable in the point
;;     to the speedbar. If you specify a prefix argument you can enter
;;     the variable name in the minibuffer. see
;;     https://www.gnu.org/software/emacs/manual/html_node/emacs/Watch-Expressions.html
;;
;; C-c C-f
;; C-x C-a C-f
;;     (gud-finish) Run the program until the selected stack frame
;;     returns or stops for some other reason (gud-finish).
;;
;;
;; If you are using GDB, these additional key bindings are available:
;;
;; C-x C-a C-j
;;     Only useful in a source buffer, gud-jump transfers the
;;     program’s execution point to the current line. In other words,
;;     the next line that the program executes will be the one where
;;     you gave the command. If the new execution line is in a
;;     different function from the previously one, GDB prompts for
;;     confirmation since the results may be bizarre. See the GDB
;;     manual entry regarding jump for details.
;;
;; TAB
;;     With GDB, complete a symbol name
;;     (gud-gdb-complete-command). This key is available only in the
;;     GUD interaction buffer.
;;
;; These commands interpret a numeric argument as a repeat count, when
;; that makes sense.
;;
;; Because TAB serves as a completion command, you can’t use it to
;; enter a tab as input to the program you are debugging with
;; GDB. Instead, type C-q TAB to enter a tab.

(setq gdb-many-windows t)
(setq gdb-speedbar-auto-raise t)
(defun my-gdb-hook ()
  (gud-def my-gdb-run-program "run" "" "(re)start the program")
  (gud-def my-gdb-kill-program "kill" "" "kill the program")

  (global-set-key [f5] 'my-gdb-run-program)
  (global-set-key [S-f5] 'my-gdb-kill-program)
  (global-set-key [f7] 'gud-cont)      ;; continue
  (global-set-key [f9] 'gud-break)     ;; add breakpoint
  (global-set-key [C-f9] 'gud-remove)  ;; remove breakpoint
  (global-set-key [S-f9] 'gud-print)   ;; print var under cursor or region
  (global-set-key [C-S-f9] 'gud-watch) ;; watch var under cursor or region
  (global-set-key [f10] 'gud-next)     ;; step over
  (global-set-key [C-f10] 'gud-until)  ;; execute until current line
  (global-set-key [f11] 'gud-step)     ;; step into
  (global-set-key [S-f11] 'gud-finish) ;; finish current function

  ;; make the gdb prompt sticky to its window
  ;; this assumes that the gud window is focused
  ;(sticky-buffer-mode 1)

  ;; Problems with source files opening in different windows:
  ;; http://stackoverflow.com/questions/20226626/emacs-gdb-always-display-source-in-specific-window-with-gdb-many-windows

  ;; in ubuntu this was in /usr/share/emacs/24.5/lisp/progmodes/gud.el.gz

  ;;(defadvice gud-display-line (around do-it-better activate)
  ;;  "Always use the same window to show source code."
  ;;  (let* ...
  ;;     (window (and buffer
  ;;                  (or (if (eq gud-minor-mode 'gdbmi)
  ;;                          (unless (gdb-display-source-buffer buffer)
  ;;                            (gdb-display-buffer buffer nil 'visible)))
  ;;                      (get-buffer-window buffer)
  ;;                      (display-buffer buffer))))
  ;;  ...)

  ;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
  ;; and that its line LINE is visible.
  ;; Put the overlay-arrow on the line LINE in that buffer.
  ;; Most of the trickiness in here comes from wanting to preserve the current
  ;; region-restriction if that's possible.  We use an explicit display-buffer
  ;; to get around the fact that this is called inside a save-excursion.
  (defadvice gud-display-line (true-file line)
    (let* ((last-nonmenu-event t)	 ; Prevent use of dialog box for questions.
           (buffer
            (with-current-buffer gud-comint-buffer
              (gud-find-file true-file)))
           (window (and buffer
                        (or (get-buffer-window buffer)
                            (display-buffer buffer))))
           (pos))
      (when buffer
        (with-current-buffer buffer
          (unless (or (verify-visited-file-modtime buffer) gud-keep-buffer)
            (if (yes-or-no-p
                 (format "File %s changed on disk.  Reread from disk? "
                         (buffer-name)))
                (revert-buffer t t)
              (setq gud-keep-buffer t)))
          (save-restriction
            (widen)
            (goto-char (point-min))
            (forward-line (1- line))
            (setq pos (point))
            (or gud-overlay-arrow-position
                (setq gud-overlay-arrow-position (make-marker)))
            (set-marker gud-overlay-arrow-position (point) (current-buffer))
            ;; If they turned on hl-line, move the hl-line highlight to
            ;; the arrow's line.
            (when (featurep 'hl-line)
              (cond
               (global-hl-line-mode
                (global-hl-line-highlight))
               ((and hl-line-mode hl-line-sticky-flag)
                (hl-line-highlight)))))
          (cond ((or (< pos (point-min)) (> pos (point-max)))
                 (widen)
                 (goto-char pos))))
        (when window
          (set-window-point window gud-overlay-arrow-position)
          (if (eq gud-minor-mode 'gdbmi)
              (unless (gdb-display-source-buffer buffer)
                (gdb-display-buffer buffer nil 'visible))
            (message "aqui caralho 0")
            )
          (message "aqui caralho 1")
          (get-buffer-window buffer)
          (display-buffer buffer)
          )
        ;(when window
        ;  (set-window-point window gud-overlay-arrow-position)
        ;  (if (eq gud-minor-mode 'gdbmi)
        ;      (setq gdb-source-window window))
        ;  )
        )
      )
    )
  )
(add-hook 'gdb-mode-hook 'my-gdb-hook)
(global-set-key [f5] 'gdb)

(defun edb ()
  "start a debugging session in a new frame"
  (interactive)
  (run-command-in-new-frame "" "gdb"))

;;-------------------------------------------------------------------------
;; Grepping under Emacs: https://www.gnu.org/software/emacs/manual/html_node/emacs/Grep-Searching.html#Grep-Searching

;; M-x grep
;; M-x lgrep
;;     Run grep asynchronously under Emacs, listing matching lines in the buffer named *grep*.
;; M-x grep-find
;; M-x find-grep
;; M-x rgrep
;;     Run grep via find, and collect output in the *grep* buffer.
;; M-x zrgrep
;;     Run zgrep and collect output in the *grep* buffer.
;; M-x kill-grep
;;     Kill the running grep subprocess.
;;

;; Search and replace:
;; https://www.emacswiki.org/emacs/CategorySearchAndReplace

;;-------------------------------------------------------------------------
;; https://github.com/atilaneves/cmake-ide

(defun cmake-ide--get-default-build-dir ()
  "get a default value for cmake-ide-build-dir"
  (if (and (boundp 'cmake-ide-build-dir)
           (not (string-equal 'cmake-ide-build-dir "")))
      ;; if there's a current cmake-ide-build-dir, use it
      (progn
        ;;(message "cmake-ide-build-dir already defined")
        cmake-ide-build-dir)
      ;; otherwise...
    (progn
      ;; is there a result from a previous session?
      (let ((fn (concat emacs-dir "cmake-ide-build-dir.save")))
        (if (file-exists-p fn)
          (progn
            ;; load the file into a string
            ;;(message "found a previous session at %s: %s" fn
            ;;         (with-temp-buffer (insert-file-contents fn)(buffer-string)))
            (with-temp-buffer (insert-file-contents fn)(buffer-string)))
          ;; otherwise, just use the current directory
          (progn
            ;;(message "cmake-ide-build-dir from current directory: %s"
            ;;         (setq fonix (file-name-directory (buffer-file-name))))
            (file-name-directory (buffer-file-name))
            )
          )
        )
      )
    )
  )

(defun cmake-ide--set-build-dir (dir)
  "set cmake-ide-build-dir, and store the value to a persistent file"
  (if (and (boundp 'cmake-ide-build-dir)
           (not (string-equal 'cmake-ide-build-dir "")))
      (progn (message "cmake-ide-build-dir was %s" cmake-ide-build-dir))
      (progn (message "cmake-ide-build-dir was empty"))
    )
  (setq cmake-ide-build-dir dir)
  (message "cmake-ide-build-dir is now %s" dir)
  ;; save this value to a file for use in future sessions
  (write-region cmake-ide-build-dir nil
                (concat user-emacs-directory "cmake-ide-build-dir.save"))
  )

(defun cmake-ide-set-build-dir (dir)
  (interactive
   (list (read-directory-name "Enter the cmake-ide build directory: "
                              (cmake-ide--get-default-build-dir))))
  (cmake-ide--set-build-dir dir)
  )

(defun my-cmake-ide-setup()
  (interactive)
  (require 'rtags)
  (call-interactively 'cmake-ide-set-build-dir)
  )

(use-package cmake-ide
  :defer t
  :init
;;  :bind ("s-p" . projectile-command-map)
  :commands (cmake-ide-setup)
  )

;;-------------------------------------------------------------------------
;; https://projectile.readthedocs.io/en/

(use-package projectile
  :defer t
  :init
  :bind ("s-p" . projectile-command-map)
  :config
  (projectile-global-mode)
;; (persp-mode)
;; (use-package persp-projectile
;;   :commands persp-projectile
;;   :config
;;   (add-hook 'persp-activated-hook
;;             #'(lambda ()
;;                 (persp-add-buffer
;;                  (get-buffer-create "*Messages*")))))
;; (require 'persp-projectile)
;; (setq projectile-switch-project-action 'projectile-dired)
;; (setq projectile-mode-line
;;       '(:eval (if (file-remote-p default-directory)
;;                   " Prj[*remote*]"
;;                 (format " Prj[%s]" (projectile-project-name)))))
  )

;; C-c p f 	Display a list of all files in the project. With a prefix argument it will clear the cache first.
;; C-c p F 	Display a list of all files in all known projects.
;; C-c p g 	Display a list of all files at point in the project. With a prefix argument it will clear the cache first.
;; C-c p 4 f 	Jump to a project's file using completion and show it in another window.
;; C-c p 4 g 	Jump to a project's file based on context at point and show it in another window.
;; C-c p d 	Display a list of all directories in the project. With a prefix argument it will clear the cache first.
;; C-c p 4 d 	Switch to a project directory and show it in another window.
;; C-c p 4 a 	Switch between files with the same name but different extensions in other window.
;; C-c p T 	Display a list of all test files(specs, features, etc) in the project.
;; C-c p l 	Display a list of all files in a directory (that's not necessarily a project)
;; C-c p s g 	Run grep on the files in the project.
;; M-- C-c p s g 	Run grep on projectile-grep-default-files in the project.
;; C-c p v 	Run vc-dir on the root directory of the project.
;; C-c p V 	Browse dirty version controlled projects.
;; C-c p b 	Display a list of all project buffers currently open.
;; C-c p 4 b 	Switch to a project buffer and show it in another window.
;; C-c p 4 C-o 	Display a project buffer in another window without selecting it.
;; C-c p a 	Switch between files with the same name but different extensions.
;; C-c p o 	Runs multi-occur on all project buffers currently open.
;; C-c p r 	Runs interactive query-replace on all files in the projects.
;; C-c p i 	Invalidates the project cache (if existing).
;; C-c p R 	Regenerates the projects TAGS file.
;; C-c p j 	Find tag in project's TAGS file.
;; C-c p k 	Kills all project buffers.
;; C-c p D 	Opens the root of the project in dired.
;; C-c p e 	Shows a list of recently visited project files.
;; C-c p E 	Opens the .dirs-local.el file of the project.
;; C-c p s s 	Runs ag on the project. Requires the presence of ag.el.
;; C-c p ! 	Runs shell-command in the root directory of the project.
;; C-c p & 	Runs async-shell-command in the root directory of the project.
;; C-c p c 	Runs a standard compilation command for your type of project.
;; C-c p P 	Runs a standard test command for your type of project.
;; C-c p t 	Toggle between an implementation file and its test file.
;; C-c p 4 t 	Jump to implementation or test file in other window.
;; C-c p z 	Adds the currently visited file to the cache.
;; C-c p p 	Display a list of known projects you can switch to.
;; C-c p S 	Save all project buffers.
;; C-c p m 	Run the commander (an interface to run commands with a single key).
;; C-c p ESC 	Switch to the most recently selected Projectile buffer.
;;
;; If you ever forget any of Projectile's keybindings just do a:
;;
;; C-c p C-h
;;

;;-------------------------------------------------------------------------
;;text modes

(defun my-text-hook ()
  (message "my-text-hook: entering")
  (visual-line-mode 0)
  (setq-default fill-column 77) ; https://www.emacswiki.org/emacs/FillParagraph#toc2
  (message "my-text-hook: turning on auto-fill-mode. See https://www.emacswiki.org/emacs/AutoFillMode")
  (auto-fill-mode 77)
  (turn-on-auto-fill)
  (message "my-text-hook: done.")
)
(add-hook 'text-mode-hook 'my-text-hook)

;;; LaTeX
(add-hook 'tex-mode-hook 'my-text-hook)
(add-hook 'latex-mode-hook 'my-text-hook)


;;; Restructured Text
;;see http://docutils.sourceforge.net/docs/user/emacs.html
;
;;C-c C-a   Commands to adjust the section headers and work with the hierarchy they build.
;;C-c C-c   Commands to compile the current reStructuredText document to various output formats.
;;C-c C-l   Commands to work with lists of various kinds.
;;C-c C-r   Commands to manipulate the current region.
;;C-c C-t   Commands to create and manipulate a table of contents.
;
;;At any stage of typing you may use C-h to get help on the
;;available key bindings. I.e. C-c C-h gives you help on all key
;;bindings while C-c C-r C-h gives you help on the commands for
;;regions. This is handy if you forgot a certain key binding.
(use-package rst-mode
  :defer t
  :config
  (add-hook 'rst-mode-hook 'my-text-hook)
)

;; Markdown
(use-package markdown-mode
  :defer t
  :config
  (add-hook 'markdown-mode-hook 'my-text-hook)
  )

;;-----------------------------------------------------------------------------
;;Step through historic versions of git controlled file
;;https://github.com/pidu/git-timemachine

;;Visit a git-controlled file and issue M-x git-timemachine (or bind
;;it to a keybinding of your choice). If you just need to toggle the
;;time machine you can use M-x git-timemachine-toggle.
;
;;Use the following keys to navigate historic version of the file
;
;;    p Visit previous historic version
;;    n Visit next historic version
;;    w Copy the abbreviated hash of the current historic version
;;    W Copy the full hash of the current historic version
;;    g Goto nth revision
;;    q Exit the time machine.
;
(use-package git-timemachine
  :defer t
  :commands (git-timemachine git-timemachine-toggle)
  )

;;-----------------------------------------------------------------------------
(if this-is-windows
  (progn
    ;;if in Windows run this block
    (custom-set-faces '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "unknown" :family "Consolas")))))
  )
  (progn
    ;;otherwise run this block
    (custom-set-faces '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "Inconsolata")))))
  )
)
;



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("80ceeb45ccb797fe510980900eda334c777f05ee3181cb7e19cd6bb6fc7fda7c" "8abee8a14e028101f90a2d314f1b03bed1cde7fd3f1eb945ada6ffc15b1d7d65" "c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" "ba9be9caf9aa91eb34cf11ad9e8c61e54db68d2d474f99a52ba7e87097fa27f5" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "9d91458c4ad7c74cf946bd97ad085c0f6a40c370ac0a1cbeb2e3879f15b40553" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
 '(ecb-options-version "2.40")
 '(safe-local-variable-values
   (quote
    ((eval load-file
           (concat c4stl-dir ".project.el"))
     (eval set
           (make-local-variable
            (quote c4stl-dir))
           (file-name-directory
            (let
                ((d
                  (dir-locals-find-file ".")))
              (if
                  (stringp d)
                  d
                (car d)))))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "Inconsolata"))))
 '(flymake-errline ((t (:background nil :foreground nil :inverse-video nil :underline nil :slant normal :weight normal))))
 '(highlight-indentation-face ((t (:background "gray24")))))
