;; see https://github.com/chrisdone/elisp-guide
;; see http://lisperati.com/casting.html
;; see http://ergoemacs.org/emacs/elisp_basics.html
;; see http://ergoemacs.org/emacs/emacs_make_modern.html for lots of goodies
;; see http://stackoverflow.com/questions/5795451/how-to-detect-that-emacs-is-in-terminal-mode
;; Many of the use-package calls were taken from https://github.com/ljos/.emacs.d/blob/master/init.el

;; emacs on windows:
;; https://caiorss.github.io/Emacs-Elisp-Programming/Emacs_On_Windows.html
;; https://github.com/hubisan/emacs-wsl


(setq emacs-dir (file-name-directory load-file-name))
(message (format "emacs-dir %s" emacs-dir))

;;; Emacs Load Path
(add-to-list 'load-path (concat emacs-dir "lisp"))

;;http://ikaruga2.wordpress.com/2011/04/11/testing-for-windows-in-emacs/
(defvar this-is-windows (string-match "windows" (symbol-name system-type)))

;; a shortcut for opening this file
(defun my-open-init-el ()
  "edit ~/.emacs.d/init.el"
  (interactive)
  (find-file (concat user-emacs-directory "init.el"))
  )
(defun my-open-cmany-el ()
  "edit ~/.emacs.d/cmany.el/cmany.el"
  (interactive)
  (find-file (concat user-emacs-directory "cmany.el/cmany.el"))
  )
(global-set-key (kbd "C-c u e") 'my-open-init-el)
(global-set-key (kbd "C-c u m") 'my-open-cmany-el)

;; https://edivad.wordpress.com/2007/04/03/emacs-convert-dos-to-unix-and-vice-versa/
(defun my-dos2unix()
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix)
  )
(defun my-unix2dos()
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos)
  )

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

;; save command history
(savehist-mode 1)

;;reload files automatically
;;http://www.emacswiki.org/emacs/RevertBuffer
(global-auto-revert-mode 1)

(setq scroll-preserve-screen-position t) ;; make PageUp+PageDown return to the departing pos
(setq visible-bell t)       ; Disable bell
(setq inhibit-startup-screen t) ; Disable startup screen
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ; remove toolbar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ; remove menu
;;Avoid the symlink Version-Control warning
;;http://www.gnu.org/software/emacs/manual/html_node/emacs/General-VC-Options.html
;;http://stackoverflow.com/questions/15390178/emacs-and-symbolic-links
(setq vc-follow-symlinks t)

;; quick yes: bind M-y to answer "yes" to a yes-or-no-p question
(require 'quick-yes)

;; disable C-x C-z
;; https://stackoverflow.com/questions/7243155/cant-seem-to-get-rid-of-ctrl-x-ctrl-z-key-binding-in-emacs-for-minimizing-windo
(put 'suspend-frame 'disabled t)
(global-set-key "\C-x\C-z" nil)
(global-set-key (kbd "C-x C-z") nil)

;; UTF8 everywhere
;; https://caiorss.github.io/Emacs-Elisp-Programming/Emacs_On_Windows.html
(set-terminal-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

;;-------------------------------------------------------------------------------
;; set garbage-collection threshold to 10MB to speed up flx-ido:
;; see https://github.com/lewang/flx
(setq gc-cons-threshold 10000000)
;; restore after startup
(add-hook 'after-init-hook
          #'(lambda () (setq gc-cons-threshold 800000)))

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

;;-------------------------------------------------------------------------------
;; Automatically compile and save ~/.emacs.el
;; https://nilsdeppe.com/posts/emacs-c++-ide2

(defun byte-compile-init-files (file)
  "Automatically compile FILE."
  (interactive)
  (save-restriction
    ;; Suppress the warning when you setq an undefined variable.
    (if (>= emacs-major-version 23)
        (setq byte-compile-warnings '(not free-vars obsolete))
      (setq byte-compile-warnings
            '(unresolved
              callargs
              redefine
              obsolete
              noruntime
              cl-warnings
              interactive-only)))
    (byte-compile-file (expand-file-name file)))
  )

(add-hook
 'after-save-hook
 (function
  (lambda ()
    (if (string= (file-truename "~/.emacs.el")
                 (file-truename (buffer-file-name)))
        (byte-compile-init-files (file-truename "~/.emacs.el")))
    )
  )
 )

;; Byte-compile again to ~/.emacs.elc if it is outdated
(if (file-newer-than-file-p
     (file-truename "~/.emacs.el")
     (file-truename "~/.emacs.elc"))
(byte-compile-init-files "~/.emacs.el"))


;;-------------------------------------------------------------------------------
;; re-builder: regular expression builder
;; see https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
(setq reb-re-syntax 'string)

;; C-c TAB     switch re-builder syntax mode.
;; C-c C-u     Show Error
;; C-c C-e     enter the sub-expression mode to only highlight capturing groups
;; C-c C-i     toggle the case sensitivity
;; C-c C-s     next match
;; C-c C-r     prev match
;; C-c C-b     Change Target Buffer
;; C-c C-w     Copy Regular Expression (and convert where applicable) the expression to a string format suitable for use in elisp
;; C-c C-q     Quit re-builder

;;------------------------------------------------------------------------------
;; MELPA - package installer
;; http://melpa.milkbox.net/#/getting-started
;; http://ergoemacs.org/emacs/emacs_package_system.html

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

;; https://emacs.stackexchange.com/questions/31872/how-to-update-packages-installed-with-use-package
;; Some package menu tweaks:

;; With this I can use "a" "new" to just see what packages are newly
;; available. And after hitting U to mark upgrades I can hit a for
;; an occur buffer list of the ones that were marked, in case I want
;; to dig in to details of what changed etc.

(defun package-menu-find-marks ()
  "Find packages marked for action in *Packages*."
  (interactive)
  (occur "^[A-Z]"))

;; Only in Emacs 25.1+
(defun package-menu-filter-by-status (status)
  "Filter the *Packages* buffer by status."
  (interactive
   (list (completing-read
          "Status: " '("new" "installed" "dependency" "obsolete"))))
  (package-menu-filter (concat "status:" status)))

(define-key package-menu-mode-map "s" #'package-menu-filter-by-status)
(define-key package-menu-mode-map "a" #'package-menu-find-marks)


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
;; ; bind a key to primary commands within the package. Defer loading also. Note the following:
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
;;
;; ; explicitly defer loading of the package
;; :defer t
;; ; ... or explicitly demant loading of the package
;; :demand t
;;
;; )


;;-------------------------------------------------------------------------------
;; vlf - handle open very large files
(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup))

;;-------------------------------------------------------------------------------

;; http://ergoemacs.org/emacs/emacs_hyper_super_keys.html

;; ;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
;; ;; super: left Windows key
;; (setq w32-pass-lwindow-to-system nil)
;; (setq w32-lwindow-modifier 'super) ; Left Windows key
;; ;; super: right Windows key
;; (setq w32-pass-rwindow-to-system nil)
;; (setq w32-rwindow-modifier 'super) ; Right Windows key
;; ;; make the menu/app key the hyper key
;; (setq w32-pass-apps-to-system nil)
;; (setq w32-apps-modifier 'hyper) ; Menu/App key

;; Mac OS X
;; set keys for Apple keyboard, for emacs in OS X
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper


;;-------------------------------------------------------------------------------
;; Important keys:

;; C Control
;; M Meta (Alt)
;; S shift
;; s super
;; H hyper

;;-------------------------------------------------------------------------------
;; Important shortcuts:

;; which-key: https://github.com/justbur/emacs-which-key
(use-package which-key
  :config (which-key-mode))

;; C-h m
;; F1 m
;;    describe current buffer's mode
;; C-h M-k
;;    describe-keymap
;; C-h b
;; F1 b
;;    describe bindings (all current keybindings)
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

(use-package help-fns+) ;; provides describe-keymap http://stackoverflow.com/a/7135736/5875572


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
;; C-x C-e
;;    Evaluate sexp before point; print value in the echo area.

;; F3
;; C-x (
;;    start recording macro
;; F4
;; C-x )
;;    stop recording macro

;; C-x z
;;    repeat previous command. After this, pressing z will repeat again
(global-set-key [f2] 'repeat)  ; does the same as C-x z

;; C-x ESC ESC
;;    repeat previous complex command (a command which used the minibuffer)
(global-set-key [M-f2] 'repeat-complex-command)  ; does the same as C-x ESC ESC


;; M-! cmd RET
;;     Run the shell command line cmd and display the
;;     output (shell-command).
;; M-| cmd RET
;;     Run the shell command line cmd with region contents as input;
;;     optionally replace the region with the output
;;     (shell-command-on-region).
;; M-x shell
;;     Run a subshell with input and output through an Emacs
;;     buffer. You can then give commands interactively.
;; M-x term
;;     Run a subshell with input and output through an Emacs
;;     buffer. You can then give commands interactively. Full terminal
;;     emulation is available.
;; M-x eshell
;;     Start the Emacs shell.
;;
;; C-c C-j
;;     Switch to line mode (term-line-mode). Do nothing if already in
;;     line mode.
;; C-c C-k
;;     Switch to char mode (term-char-mode). Do nothing if already in
;;     char mode.

;; The following commands are only available in char mode:
;; C-c C-c
;;     Send a literal C-c to the sub-shell.
;; C-c char
;;     This is equivalent to C-x char in normal Emacs. For example,
;;     C-c o invokes the global binding of C-x o, which is normally
;;     ‘other-window’.

;; Term mode has a page-at-a-time feature. When enabled, it makes
;; output pause at the end of each screenful:
;;
;; C-c C-q
;;     Toggle the page-at-a-time feature. This command works in both
;;     line and char modes. When the feature is enabled, the mode-line
;;     displays the word ‘page’, and each time Term receives more than
;;     a screenful of output, it pauses and displays ‘**MORE**’ in the
;;     mode-line. Type SPC to display the next screenful of output, or
;;     ? to see your other options. The interface is similar to the
;;     more program.


;; Debugging, Tracing, and Profiling (taken from http://stackoverflow.com/a/19896143/5875572)

;;(toggle-debug-on-error)
;;(toggle-debug-on-quit)

;; Debugging: https://www.emacswiki.org/emacs/SourceLevelDebugger
;; C-u C-M-x

;; M-: (info "(elisp) Debugging") RET

;; Standard debugger:
;; M-x debug-on-entry FUNCTION
;; M-x cancel-debug-on-entry &optional FUNCTION
;; debug &rest DEBUGGER-ARGS
;; M-x toggle-debug-on-error
;; M-x toggle-debug-on-quit
;; setq debug-on-signal
;; setq debug-on-next-call
;; setq debug-on-event
;; setq debug-on-message REGEXP

;; Edebug -- a source-level debugger for Emacs Lisp
;; M-x edebug-defun (C-u C-M-x) Cancel with eval-defun (C-M-x)
;; M-x edebug-all-defs -- Toggle edebugging of all definitions
;; M-x edebug-all-forms -- Toggle edebugging of all forms
;; M-x edebug-eval-top-level-form

;; Tracing:
;; M-x trace-function FUNCTION &optional BUFFER
;; M-x untrace-function FUNCTION
;; M-x untrace-all

;; Timing and benchmarking:
;; (benchmark-run &optional REPETITIONS &rest FORMS)

;; Emacs Lisp Profiler (ELP)
;; M-x elp-instrument-package
;; M-x elp-instrument-list
;; M-x elp-instrument-function
;; M-x elp-reset-*
;; M-x elp-results
;; M-x elp-restore-all
;;
;; "There's a built-in profiler called ELP. You can try something like
;; M-x elp-instrument-package, enter "vc", and then try finding a file
;; Afterwards, M-x elp-results will show you a profile report.
;; (Note that if the time is instead being spent in non-vc-related
;; functions, this technique will not show it, but you can instrument
;; further packages if you like.)" http://stackoverflow.com/a/6732810/324105

;; CPU & Memory Profiler ('Native Profiler')
;; M-x profiler-start
;; M-x profiler-report
;; M-x profiler-reset
;; M-x profiler-stop
;; M-x profiler-*

;; Dope ("DOtemacs ProfilEr. A per-sexp-evaltime profiler.")
;; https://raw.github.com/emacsmirror/dope/master/dope.el
;; M-x dope-quick-start will show a little introduction tutorial.

;; Spinning:
;; Set debug-on-quit to t
;; When the problem happens, hit C-g for a backtrace.

(defun keymap-symbol (keymap)
  "Return the symbol to which KEYMAP is bound, or nil if no such symbol exists.
  http://stackoverflow.com/questions/14489848/emacs-name-of-current-local-keymap"
  (catch 'gotit
    (mapatoms (lambda (sym)
                (and (boundp sym)
                     (eq (symbol-value sym) keymap)
                     (not (eq sym 'keymap))
                     (throw 'gotit sym))))))

;;------------------------------------------------------------------------------

(global-unset-key (kbd "C-z")) ;; disable Ctrl-z

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


;;--------------------------------------------------------------------
;; ORG-MODE

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

;; https://orgmode.org/manual/Clocking-work-time.html
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; https://stackoverflow.com/questions/22720526/set-clock-table-duration-format-for-emacs-org-mode
(setq org-duration-format (quote h:mm))

;; C-c C-x C-r     (org-clock-report) Insert a dynamic block
;; C-c C-c  or  C-c C-x C-u     (org-dblock-update) Update dynamic block at point
;; C-u C-c C-x C-u      Update all dynamic blocks
;; S-LEFT, S-RIGHT     (org-clocktable-try-shift) Shift the current :block interval and update the table.
;; C-c C-t     (org-todo) Changing the TODO state of an item to DONE automatically stops the clock if it is running in this same item.
;; C-c C-x C-i     (org-clock-in)
;; C-c C-x C-o     (org-clock-out)
;; C-c C-x C-x     (org-clock-in-last)
;; C-c C-x C-e     (org-clock-modify-effort-estimate)
;; C-c C-c  or  C-c C-y     (org-evaluate-time-range) Recompute the time interval after changing one of the timestamps. This is only necessary if you edit the timestamps directly.
;; C-S-up/down     (org-clock-timestamps-up/down)
;; S-M-up/down     (org-timestamp-up/down)
;; C-c C-x C-q     (org-clock-cancel)
;; C-c C-x C-j     (org-clock-goto)
;; C-c C-x C-d     (org-clock-display)

;; AGENDA VIEW
;; C-c [        Add current file to the list of agenda files. The file is added to the front of the list. If it was already in the list, it is moved to the front. With a prefix argument, file is added/moved to the end.
;; C-c ]        Remove current file from the list of agenda files.
;; C-,          Cycle through agenda file list, visiting one file after the other.
;; C-c a a      The calendar-like agenda (see Weekly/daily agenda).
;; C-c a t / T  A list of all TODO items (see Global TODO list).
;; C-c a m / M  A list of headlines matching a TAGS expression (see Matching tags and properties).
;; C-c a s      A list of entries selected by a boolean expression of keywords and/or regular expressions that must or must not occur in the entry.
;;
;; Motion
;; n  Next line (same as up and C-p).
;; p  Previous line (same as down and C-n).
;;
;; View/Go to Org file
;; mouse-3 / SPC  Display the original location of the item in another window. With prefix arg, make sure that the entire entry is made visible in the outline, not only the heading.
;; TAB            Go to the original location of the item in another window. Under Emacs 22, mouse-1 will also work for this.
;; RET            Go to the original location of the item and delete other windows.
;;
;; Change display
;; o               Delete other windows.
;; d / w           Switch to day/week view.
;; f and b         Go forward/backward in time to display the following org-agenda-current-span days. For example, if the display covers a week, switch to the following/previous week.
;; .               Go to today.
;; j               Prompt for a date and go there.
;; v l or short l  Toggle Logbook mode. In Logbook mode, entries that were marked DONE while logging was on (variable org-log-done) are shown in the agenda, as are entries that have been clocked on that day. When called with a C-u prefix, show all possible logbook entries, including state changes.
;; r or g          Recreate the agenda buffer, to reflect the changes.
;; s               Save all Org buffers in the current Emacs session, and also the locations of IDs.
;;
;; Secondary filtering and query editing
;; /      Filter the current agenda view with respect to a tag. You are prompted for a letter to select a tag. Press ‘-’ first to select against the tag.
;; \      Narrow the current agenda filter by an additional condition.
;;
;; Remote editing (see the manual for many more commands)
;; 0--9        Digit argument.
;; t           Change the TODO state of the item, in the agenda and in the org file.
;; C-k         Delete the current agenda item along with the entire subtree belonging to it in the original Org file.
;; C-c C-w     Refile the entry at point.
;; C-c C-x C-a  or short  a Archive the subtree corresponding to the entry at point using the default archiving command set in org-archive-default-command.
;; C-c C-x C-s  or short  $ Archive the subtree corresponding to the current headline.
;; C-c C-s     Schedule this item, with prefix arg remove the scheduling timestamp
;; C-c C-d     Set a deadline for this item, with prefix arg remove the deadline.
;; S-right and S-left Change the timestamp associated with the current line by one day.
;; I           Start the clock on the current item.
;; O / X       Stop/cancel the previously started clock.
;; J           Jump to the running clock in another window.


;; https://emacs.stackexchange.com/questions/9528/is-it-possible-to-remove-emsp-from-clock-report-but-preserve-indentation
(defun my-org-clocktable-indent-string (level)
    (if (= level 1)
        ""
      (let ((str "`"))
        (while (> level 2)
          (setq level (1- level)
                str (concat str "--")))
        (concat str "-> "))))

(advice-add 'org-clocktable-indent-string
            :override #'my-org-clocktable-indent-string)

(add-hook 'org-mode-hook #'use-snips)
(add-hook 'org-mode-hook #'hook-snips)

(defun --my-org-parse-hh:mm (tm)
  (let* ((spl (split-string tm ":"))
         (hh (string-to-number (nth 0 spl)))
         (mm (string-to-number (nth 1 spl))))
    (+ hh (/ mm 60.0))
    )
  )

(defun --my-org-time-cmp(ts fn suffix)
  (if (string-empty-p ts)
      ""
    (let* ((treal (--my-org-parse-hh:mm ts))
           (tbill (/ (fround (* value 4.0)) 4.0)
           (delta (- tbill treal)))
      (format "%5.2f vs %5.2f -> %+.2f%s"
              (funcall fn tbill) (funcall fn treal) (funcall fn delta) suffix)
      )
      )
    )
  )

(defun my-org-time-cmp(ts)
  (--my-org-time-cmp ts (lambda (arg) arg) "h")
  )

(defun my-org-bill-cmp(ts rate)
  (--my-org-time-cmp ts (lambda (arg) (* arg rate)) "€")
  )

(defun my-org-merge-times(timesheet week day)
  (if (not (string-empty-p day))
      day
    (if (not (string-empty-p week))
        week
      (if (not (string-empty-p timesheet))
          timesheet
        )
      )
    )
  )

(defun my-org-task-cost(timesheet week day)
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


(defun my-three-windows()
  "split the frame into three horizontal windows of equal size"
  (interactive)
  (if (eq 1 (length (window-list)))
      (progn
        (split-window-right)
        (split-window-right)
        (balance-windows)
        )
    (progn
      (message "not single window ; will not split")
      )
    )
  )
(global-set-key (kbd "C-<f3>") 'my-three-windows)


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
  (set-face-background 'mode-line "#404040")
  (set-face-background 'mode-line-inactive "#303030")
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
;;(global-linum-mode 1)
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

(defun enable-line-wrapping()
  (interactive)
  (visual-line-mode 1)
  (toggle-truncate-lines 0)
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
;; https://caiorss.github.io/Emacs-Elisp-Programming/Emacs_On_Windows.html
(defun udf-windows-setup ()
  (interactive)
  ;(setq git-shell-exe "C:\\Git\\bin\\sh")
  (setq git-shell-exe "C:\\Git\\bin\\bash.exe")
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

;;(if (eq system-type 'windows-nt)
;;    (udf-windows-setup))

(defun run-bash-windows ()
      (interactive)
      (let ((shell-file-name "C:\\Git\\bin\\bash.exe"))
            (shell "*bash*")))

(defun run-cmdexe-windows ()
      (interactive)
      (let ((shell-file-name "cmd.exe"))
            (shell "*cmd.exe*")))


;;-------------------------------------------------------------------------
;; IDO mode: (Interactively DO things)
;; https://www.masteringemacs.org/article/introduction-to-ido-mode

(require 'ido)
(require 'flx-ido) ; https://github.com/lewang/flx
(require 'ido-vertical-mode) ; https://github.com/creichert/ido-vertical-mode.el
(require 'ido-ubiquitous) ; https://github.com/DarwinAwardWinner/ido-ubiquitous
(ido-mode 1)
(flx-ido-mode)
(ido-everywhere 1) ; enable basic IDO support for files and buffers
(setq ido-everywhere t)
(setq ido-use-faces t)
(setq ido-vertical-show-count t)
(ido-vertical-mode 1)
(ido-ubiquitous-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;; ido binds C-x C-d to ido-list-directory, so bind to dired
(global-set-key (kbd "C-x C-d") 'ido-dired)

;; disable ido faces to see flx highlights.
;(setq ido-use-faces nil)


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

;; General-purpose IDO Commands
;;
;; To skip IDO's current suggestion and accept what's already typed-in,
;; hit C-j
;;
;; Tricks for windows:
;;  * for opening a file/dir in a different drive (eg D:), do: C-x C-f D:/
;;    and IDO will intelligently switch to D:
;;  * for opening a file/dir in the home directory do: C-X C-f ~/
;;
;; C-b Reverts to the old switch-buffer completion engine. Available in Buffers.
;; C-f Reverts to the old find-file completion engine. Available in Files
;; C-d Opens a dired buffer in the current directory. Available in Dirs / Files
;; C-a Toggles showing ignored files (see ido-ignore-files). Available in Files / Buffers
;; C-c Toggles if searching of buffer and file names should ignore case. (see
;;     ido-case-fold). Available in Dirs / Files / Buffers
;; TAB Attempt to complete the input like the normal completing read
;;     functionality. Available in Dirs / Files / Buffers
;; C-p Toggles prefix matching; when it's on the input will only match the
;;     beginning of a filename instead of any part of it.
;;
;; Files
;;
;; C-s / C-r
;;             Moves to the next and previous match, respectively. Available
;;             everywhere
;; C-t
;;             Toggles matching by Emacs regular expression.. Available everywhere
;; Backspace
;;             Deletes characters as usual or goes up one directory if it makes
;;             sense to do so.. Available everywhere
;; C-SPC / C-@
;;             Restricts the completion list to anything that matches your
;;             current input. Available everywhere
;; //
;;             Like most Linux shells two forward slashes in a path means
;;             "ignore the preceding path, and go back to the top-most
;;             directory". Works the same in Ido but it's more interactive: it
;;             will go to the root / (or the root of the current drive in
;;             Windows) Available in Files
;; ~/
;;             Jumps to the home directory. On Windows this would be typically
;;             be %USERPROFILE% or %HOME%, if it is defined. Available in
;;             Files / Dirs
;; M-d
;;             Searches for the input in all sub-directories to the directory
;;             you're in.. Available in Files
;; C-k
;;             Kills the currently focused buffer or deletes the file
;;             depending on the mode.. Available in Files / Buffers
;; M-m
;;             Creates a new sub-directory to the directory you're
;;             in. Available in Files
;;
;; OK, so you probably won't get in the habit of using all the commands;
;; that's fine, but some are more important to remember than others, like:
;; Backspace; C-s and C-r; // and ~/; and C-d.
;;
;; If Ido is getting in your way, remember the fallback commands:
;;  C-f for files; C-b for buffers.

;;-----------------------------------------------------------------------------
;; Ivy config
;; https://nilsdeppe.com/posts/emacs-c++-ide2

(use-package ivy
  :ensure t
  :config
  (require 'ivy)
  (ivy-mode t)
  (setq ivy-wrap t)
  (setq ivy-display-style 'fancy)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq magit-completing-read-function 'ivy-completing-read)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  ;; Show #/total when scrolling buffers
  (setq ivy-count-format "%d/%d ")
  )

;; (use-package swiper
;;   :ensure t
;;   :bind (("C-s" . swiper)
;;          ("C-r" . swiper))
;;   )


;;------------------------------------------------------------------
;; counsel

;; nice intro:
;; https://truthseekers.io/lessons/how-to-use-ivy-swiper-counsel-in-emacs-for-noobs/

;; using counsel-ag: very good:
;; https://sam217pa.github.io/2016/09/11/nuclear-power-editing-via-ivy-and-ag/
;;
;; C-c s k (counsel-ag) to search in the current directory.
;; C-c C-o (ivy-occur) in the search result. It opens an ivy-occur buffer.
;; C-x C-q (ivy-wgrep-change-to-wgrep-mode) on the ivy-occur buffer to edit it
;;         I can now change each variable name globally using normal search/replace.
;; C-c C-c (wgrep-finish-edit).

(use-package counsel
  :ensure t
  :bind (
         ;; ("M-x" . counsel-M-x) ;; using smex (see below)
         ;;("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ;;("<f2> i" . counsel-info-lookup-symbol)
         ;;("<f2> u" . counsel-unicode-char)
         ("C-c s f" . counsel-fzf)
         ("C-c s g" . counsel-git-grep)
         ("C-c s j" . counsel-git)
         ("C-c s k" . counsel-ag) ;; https://truthseekers.io/lessons/how-to-use-ivy-swiper-counsel-in-emacs-for-noobs/
         ("C-c s r" . counsel-rg) ;; https://truthseekers.io/lessons/how-to-use-ivy-swiper-counsel-in-emacs-for-noobs/
         ("C-c s l" . counsel-locate)
         ;; http://pragmaticemacs.com/emacs/counsel-yank-pop-with-a-tweak/
         ("M-y" . counsel-yank-pop) ;; easily show the kill-ring (goes to ivy-minibuffer-map)
         :map ivy-minibuffer-map
         ("S-M-y" . ivy-previous-line)
         ("M-y" . ivy-next-line)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-add)
         )
  :config
  (if (executable-find "rg")
      ;; use ripgrep instead of grep because it's way faster
      (setq counsel-grep-base-command
            "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
            counsel-rg-base-command
            "rg -i -M 120 --no-heading --line-number --color never %s ."
            )
    ;;(warn "\nWARNING: Could not find the ripgrep executable. Using counsel-grep defaults.")
    )
  )

;; Use universal ctags to build the tags database for the project.
;; When you first want to build a TAGS database run 'touch TAGS'
;; in the root directory of your project.
;; https://github.com/redguardtoo/counsel-etags
(use-package counsel-etags
  :ensure t
  :bind (
         ;; to save grep results to a buffer, use C-c C-o
         ("M-G" . counsel-etags-grep-symbol-at-point)
         ("M-+" . counsel-etags-find-tag-at-point)
         ("M-T" . counsel-etags-find-tag))
  :config
  ;; Ignore files above 800kb
  (setq counsel-etags-max-file-size 800)
  ;; Ignore build directories for tagging
  (add-to-list 'counsel-etags-ignore-directories '"build*")
  (add-to-list 'counsel-etags-ignore-directories '"install*")
  (add-to-list 'counsel-etags-ignore-directories '".vscode")
  (add-to-list 'counsel-etags-ignore-filenames '".clang-format")
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  ;; How many seconds to wait before rerunning tags for auto-update
  (setq counsel-etags-update-interval 180)
  ;; Set up auto-update
  (add-hook
   'prog-mode-hook
   (lambda () (add-hook 'after-save-hook
                        (lambda ()
                          (counsel-etags-virtual-update-tags))))
   )

  ;; The function provided by counsel-etags is broken (at least on Linux)
  ;; and doesn't correctly exclude directories, leading to an excessive
  ;; amount of incorrect tags. The issue seems to be that the trailing '/'
  ;; in e.g. '*dirname/*' causes 'find' to not correctly exclude all files
  ;; in that directory, only files in sub-directories of the dir set to be
  ;; ignore.
  (defun my-scan-dir (src-dir &optional force)
    "Create tags file from SRC-DIR. \
     If FORCE is t, the commmand is executed without \
     checking the timer."
    (let* ((find-pg (or
                     counsel-etags-find-program
                     (counsel-etags-guess-program "find")))
           (ctags-pg (or
                      counsel-etags-tags-program
                      (format "%s -e -L" (counsel-etags-guess-program
                                          "ctags"))))
           (default-directory src-dir)
           ;; run find&ctags to create TAGS
           (cmd (format
                 "%s . \\( %s \\) -prune -o -type f -not -size +%sk %s | %s -"
                 find-pg
                 (mapconcat
                  (lambda (p)
                    (format "-iwholename \"*%s*\"" p))
                  counsel-etags-ignore-directories " -or ")
                 counsel-etags-max-file-size
                 (mapconcat (lambda (n)
                              (format "-not -name \"%s\"" n))
                            counsel-etags-ignore-filenames " ")
                 ctags-pg))
           (tags-file (concat (file-name-as-directory src-dir) "TAGS"))
           (doit (or force (not (file-exists-p tags-file)))))
      ;; always update cli options
      (when doit
        (message "%s at %s" cmd default-directory)
        (shell-command cmd)
        (visit-tags-table tags-file t)
        )
      )
    )

  (setq counsel-etags-update-tags-backend
        (lambda ()
          (interactive)
          (let* ((tags-file (counsel-etags-locate-tags-file)))
            (when tags-file
              (my-scan-dir (file-name-directory tags-file) t)
              (run-hook-with-args
               'counsel-etags-after-update-tags-hook tags-file)
              (unless counsel-etags-quiet-when-updating-tags
                (message "%s is updated!" tags-file))))
          )
        )
  )


;;-------------------------------------------------------------------------
;; smex
;; https://github.com/nonsequitur/smex
;; use IDO for completion of commands in M-x with enhancements
;; like putting your most-used commands at the front of the list

(use-package smex
  :config
  (smex-initialize)
  :commands
  (smex smex-major-mode-commands)
  :bind
  (("M-x"   . smex)
   ("M-X"   . smex-major-mode-commands)
   ("C-M-x" . execute-extended-command)
   ;;:map ido-completion-map
   ;;("<tab>" . minibuffer-complete)
   ;;("C-h f" . smex-describe-function) ;; fails with missing symbol ido-require-match
   ;;("C-h w" . smex-where-is)
   ;;("M-."   . smex-find-function)
   ;;("C-a"   . move-beginning-of-line)
   )
  )

;;-------------------------------------------------------------------------
;; Auto complete

;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;(setq ac-comphist-f)
;(ac-config-default)
;;(require 'auto-complete-etags)

;; General Usage: Completion will start automatically after you type a
;; few letters. Use M-n and M-p to select, <return> to complete or <tab>
;; to complete the common part. Search through the completions with C-s,
;; C-r and C-o. Press M-(digit) to quickly complete with one of the
;; first 10 candidates. When the completion candidates are shown, press
;; <f1> to display the documentation for the selected candidate, or C-w
;; to see its source. Not all back-ends support this.
;;
;; The variable company-backends specifies a list of backends that
;; company-mode uses to retrieves completion candidates for you.

(defun company--my-insert-spc()   (interactive)(company-abort)(insert-char #10r32))
(defun company--my-insert-dot()   (interactive)(company-abort)(insert-char #10r46))
(defun company--my-insert-comma() (interactive)(company-abort)(insert-char #10r44))
(defun company--my-insert-equal() (interactive)(company-abort)(insert-char #10r61))
(defun company--my-setup()
  (interactive)
  (setq company-minimum-prefix-length 3)
  (setq company-idle-delay nil) ;; disable auto popup
  ;;(setq company-auto-complete t)
  (setq company-auto-complete nil)
  (setq company-show-numbers t)
  (setq company-selection-wraparound t)
  )
(use-package company
  :config
  (global-company-mode)
  (company--my-setup)
  :bind
  (("C-<tab>" . company-complete)
   :map company-active-map
   ;; for some reason this disables the M-digit shortcuts
   ;; see http://emacs.stackexchange.com/questions/31760/company-m-digit-shortcuts-not-working
   ;;("ESC" . company-abort)
   ;; ... but this doesn't!!!
   ("<escape>" . company-abort)
   ;; prevent company from completing on its own when we type regular characters
   ("SPC" . company--my-insert-spc)
   ("."   . company--my-insert-dot)
   (","   . company--my-insert-comma)
   ("="   . company--my-insert-equal)
   )
  )


;;-------------------------------------------------------------------------

(setq tramp-terminal-type "dumb")
(use-package tramp
  :defer t
  :commands tramp
  :config
  ;;(setq tramp-auto-save-directory (expand-file-name "~/.emacs.d/auto-save-list"))
  ;; # https://emacs.stackexchange.com/questions/24159/tramp-waiting-for-prompts-from-remote-shell
  ;; # fix for the error: Tramp: Waiting for prompts from remote shell:
  ;; # paste this into your local ~/.bashrc:
  ;;
  ;; case "$TERM" in
  ;;     "dumb")
  ;;         export PS1="> "
  ;;         ;;
  ;; esac
  ;;"
  (setq tramp-terminal-type "dumb")
  ;; ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Remote-shell-setup.html
  ;; (setenv "ESHELL" "bash")
  ;; (setq tramp-default-method "ssh")
  ;; ;; setting tramp-shell-prompt-pattern may be needed so that tramp understands
  ;; ;; the shell prompt; see https://www.emacswiki.org/emacs/TrampMode
  ;; (setq tramp-shell-prompt-pattern
  ;;       "\\(?:^\\|
  ;; \\)[^]#$%>\n]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")
  )

;; prevent TRAMP from connecting to hosts on startup
(defun ido-remove-tramp-from-cache nil
  "Taken from https://www.emacswiki.org/emacs/TrampMode
    Remove any TRAMP entries from `ido-dir-file-cache'.
    This stops tramp from trying to connect to remote hosts on emacs startup,
    which can be very annoying."
  (interactive)
  (setq ido-dir-file-cache
        (cl-remove-if
         (lambda (x)
           (string-match "/\\(rsh\\|ssh\\|telnet\\|su\\|sudo\\|sshx\\|krlogin\\|ksu\\|rcp\\|scp\\|rsync\\|scpx\\|fcp\\|nc\\|ftp\\|smb\\|adb\\):" (car x)))
         ido-dir-file-cache)))
;; redefine `ido-kill-emacs-hook' so that cache is cleaned before being saved

(defun ido-kill-emacs-hook ()
  (ido-remove-tramp-from-cache)
  (ido-save-history))


(setq --my-tramp-open--user "")
(setq --my-tramp-open--host "")
(setq --my-tramp-open--port "22")
(setq --my-tramp-open--file "~")
(defun my-tramp-open (user host port file)
  "interactively open a file on another host"
  (interactive (list
                (read-string "user: " --my-tramp-open--user)
                (read-string "host: " --my-tramp-open--host)
                (read-string "port: " --my-tramp-open--port)
                (read-string "file: " --my-tramp-open--file)))
  (let* ((method "ssh") ;; (method (if tramp-default-method tramp-default-method "ssh"))
         (fn (concat "/" method ":" user "@" host "#" port ":" file)))
    (message "my-tramp-open: opening file: %s" fn)
    (setq --my-tramp-open--user user)
    (setq --my-tramp-open--host host)
    (setq --my-tramp-open--port port)
    (setq --my-tramp-open--file file)
    (find-file fn)
    )
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

;; smooth horizontal scrolling
;; https://stackoverflow.com/questions/20628878/changing-horizontal-scrolling-in-emacs
(setq hscroll-margin 0)
(setq hscroll-step 1)

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

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;=========================================================================
;; EDITING

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


;;------------------------------------------------------------------
(require 'iedit)
;; iedit. https://github.com/victorhge/iedit
;;
;;   * Highlight certain contents - by press C-; All occurrences of a symbol,
;;     string or a region in the buffer may be highlighted corresponding to
;;     current mark, point and prefix argument. Refer to the document of
;;     ‘iedit-mode’ for details.
;;   * Edit one of the occurrences The change is applied to other occurrences
;;     simultaneously.
;;   * Finish - by pressing C-; again
;;
;; This package also provides rectangle support with visible
;; rectangle highlighting, which is similar with cua mode rectangle
;; support. But it’s lighter weight and uses Iedit mechanisms.
;;
;; You can also use Iedit mode as a quick way to temporarily show
;; only the buffer lines that match the current text being
;; edited. This gives you the effect of a temporary ‘keep-lines’ or
;; ‘occur’. To get this effect, hit C-’ when in Iedit mode - it
;; toggles hiding non-matching lines.
;;
;; Renaming refactoring is convenient in Iedit mode:
;;
;;   * The symbol under point is selected as occurrence by default and only
;;     complete symbols are matched
;;   * With digit prefix argument 0, only symbols in current function are
;;     matched
;;   * Restricting symbols in current region can be done by pressing C-;
;;     again
;;   * Last renaming refactoring is remembered and can be applied to other
;;     buffers later
;;
;; There are also some other facilities you may never think about. Refer to
;; the document of function ‘iedit-mode’ (C-h f iedit-mode RET) for more
;; details.

;;------------------------------------------------------------------
;; multiple-cursors. https://github.com/magnars/multiple-cursors.el

;; To get out of multiple-cursors-mode, press <return> or C-g. The latter
;; will first disable multiple regions before disabling multiple cursors. If
;; you want to insert a newline in multiple-cursors-mode, use C-j.

(use-package multiple-cursors
  :commands (
             mc/edit-lines
             mc/mark-next-like-this
             mc/mark-previous-like-this
             mc/mark-all-like-this
             )
  :bind
  ;; When you have an active region that spans multiple lines, the following
  ;; will add a cursor to each line:
  (
   ("M-m" . mc/edit-lines)
   ("M-M" . mc/mark-all-in-region)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-ç a" . mc/mark-all-like-this)
   )
)

;;------------------------------------------------------------------
;; bicycle. https://github.com/tarsius/bicycle
;; code folding

(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
              ("C-c <C-return>" . bicycle-cycle)
              ("C-c <C-S-return>" . bicycle-cycle-global)))

(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'prog-mode-hook 'hs-minor-mode))


;;------------------------------------------------------------------
;; https://github.com/k-talo/volatile-highlights.el
;; brings visual feedback to some operations by highlighting portions
;; relating to the operations.
;;(require 'volatile-highlights)
;;(volatile-highlights-mode t)

;;------------------------------------------------------------------
;; https://github.com/jscheid/dtrt-indent
;; A minor mode that guesses the indentation offset originally used
;; for creating source code files and transparently adjusts the
;; corresponding settings in Emacs, making it more convenient to edit
;; foreign files.
(use-package dtrt-indent
  :config (message "loading dtrt-indent")
  :commands (dtrt-indent-mode)
  )

;;------------------------------------------------------------------
;; drag line/selection up or down
;; http://emacs.stackexchange.com/questions/13941/move-selected-lines-up-and-down

(use-package drag-stuff
  :commands (drag-stuff-up drag-stuff-down)
  :bind
  (("M-<up>" . drag-stuff-up)
   ("M-<down>" . drag-stuff-down))
)

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
(defun my-newline-shortcuts()
  ;; create line after current and indent
  (global-set-key (kbd "<S-return>") "\C-e\C-m")
  (global-set-key (kbd "S-RET"     ) "\C-e\C-m")
  ;; create line before current and indent
  (global-set-key (kbd "<S-C-return>") "\C-p\C-e\C-m")
  (global-set-key (kbd "S-C-RET"     ) "\C-p\C-e\C-m")
  )
(my-newline-shortcuts)

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

;;-------------------------------------------------------------------------

;;;; a minor mode that provides many features for manipulating
;;;; pairs. Pair can be simple as parentheses or brackets, or can be
;;;; programming tokens such as if...fi or if...end in many
;;;; languages. The most basic and essential feature is automatic closing
;;;; of a pair when user inserts an opening one.
;;(use-package smartparens
;;  :commands (smartparens-mode
;;             smartparens-strict-mode
;;             sp-with-modes)
;;  :bind (:map smartparens-strict-mode-map
;;              ("C-}" . sp-forward-slurp-sexp)
;;              ("M-s" . sp-backward-unwrap-sexp)
;;              ("C-c [" . sp-select-next-thing)
;;              ("C-c ]" . sp-select-next-thing-exchange))
;;  :config
;;  (require 'smartparens-config)
;;  (show-smartparens-global-mode +1)
;;  (smartparens-global-mode 1)
;;  )
;;;; something else interacted with this. Manually enabling seems to fix it.
;;(smartparens-global-mode 1)

;; try this instead of smartparens:
;; https://github.com/joaotavora/autopair

;; this was superseded by electric-pair-mode
(electric-pair-mode)


(use-package highlight-symbol
  :commands highlight-symbol-mode
  :config
  (setq highlight-symbol-idle-delay 0.2)
  (add-hook 'highlight-symbol-mode-hook
            (function
             (lambda ()
               (highlight-symbol-nav-mode +1)
               (message "highlight porra"))
             )
            )
  )

;(use-package smart-mode-line
;; :init
;; (setq-default sml/vc-mode-show-backend t
;		sml/theme 'respectful)
;; (sml/setup))

;;-------------------------------------------------------------------------
;; string inflection
;; https://github.com/akicho8/string-inflection
;;
;; (string-inflection-underscore-function "EmacsLisp")           ; => "emacs_lisp"
;; (string-inflection-pascal-case-function "emacs_lisp")         ; => "EmacsLisp"
;; (string-inflection-camelcase-function "emacs_lisp")           ; => "emacsLisp"
;; (string-inflection-upcase-function "emacs_lisp")              ; => "EMACS_LISP"
;; (string-inflection-kebab-case-function "emacs_lisp")          ; => "emacs-lisp"
;; (string-inflection-capital-underscore-function "emacs_lisp")  ; => "Emacs_Lisp"
;;
;; (string-inflection-pascal-case-p "EmacsLisp")                 ; => t
;; (string-inflection-pascal-case-p "emacs_lisp")                ; => nil

(use-package string-inflection
  :defer t
  :commands string-inflection-all-cycle
  :config
  (message "loading string-inflection")
  )
(global-set-key (kbd "C-c C-u") 'string-inflection-all-cycle)


;;-------------------------------------------------------------------------
;;SNIPPETS

(defun use-snips()
  (use-package yasnippet
    :config
    )
)

(defun hook-snips()
  (interactive)
  (message "enabling YASnippet...")
  ;; insert our dir at the front of the default snippets
  ;(setq sdir (concat emacs-dir "snippets"))
  ;(message (format "sdir=%s" sdir))
  ;(message (format "snippet_dirs=%s" yas-snippet-dirs))
  ;(setq yas-snippet-dirs (add-to-list 'yas-snippet-dirs sdir))
  (message "yas-snippet-dirs=%s" yas-snippet-dirs)
  (yas-reload-all)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; bind yas-expand to C-S-tab
  (define-key yas-minor-mode-map (kbd "<C-S-iso-lefttab>") 'yas-expand)  ;; this doesn't work in windows
  (define-key yas-minor-mode-map (kbd "<C-s-tab>") 'yas-expand)  ;; ... so use this with the windows key
  (define-key yas-minor-mode-map (kbd "<C-S-tab>") 'yas-expand)  ;; ... so use this with the windows key
  (define-key yas-minor-mode-map (kbd "C-+") 'yas-expand)
  (yas-minor-mode-on)
  (message "enabling YASnippet: done")
)

(defun yasl()
  (interactive)
  (yas-describe-tables)
)

;;-------------------------------------------------------------------------
;; https://projectile.readthedocs.io/en/

(use-package projectile
  ;;:defer t
  :init
  (setq projectile-enable-caching t)
  ;; use git or find or fd (if available) to index projects
  (setq projectile-indexing-method 'alien)
  ;;:bind ("s-p" . projectile-command-map)
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;;(persp-mode)
  ;;(use-package persp-projectile
  ;;  :commands persp-projectile
  ;;  :config
  ;;  (add-hook 'persp-activated-hook
  ;;            #'(lambda ()
  ;;                (persp-add-buffer
  ;;                 (get-buffer-create "*Messages*")))))
  ;;(require 'persp-projectile)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-mode-line
        '(:eval (if (file-remote-p default-directory)
                    " Prj[*remote*]"
                  (format " Prj[%s]" (projectile-project-name)))))
  (push ".ccls-cache" projectile-globally-ignored-directories)
  (push ".clangd" projectile-globally-ignored-directories)
  (push ".cquery_cached_index" projectile-globally-ignored-directories)
  )

;;
;; If you ever forget any of Projectile's keybindings just do a:
;;
;; C-c p C-h
;;
;; C-c p f 	  Display a list of all files in the project. With a prefix argument it will clear the cache first.
;; C-c p F 	  Display a list of all files in all known projects.
;; C-c p g 	  Display a list of all files at point in the project. With a prefix argument it will clear the cache first.
;; C-c p 4 f 	  Jump to a project's file using completion and show it in another window.
;; C-c p 4 g 	  Jump to a project's file based on context at point and show it in another window.
;; C-c p d 	  Display a list of all directories in the project. With a prefix argument it will clear the cache first.
;; C-c p 4 d 	  Switch to a project directory and show it in another window.
;; C-c p 4 a 	  Switch between files with the same name but different extensions in other window.
;; C-c p T 	  Display a list of all test files(specs, features, etc) in the project.
;; C-c p l 	  Display a list of all files in a directory (that's not necessarily a project)
;; C-c p s g 	  Run grep on the files in the project.
;; M-- C-c p s g  Run grep on projectile-grep-default-files in the project.
;; C-c p v 	  Run vc-dir on the root directory of the project.
;; C-c p V 	  Browse dirty version controlled projects.
;; C-c p b 	  Display a list of all project buffers currently open.
;; C-c p 4 b 	  Switch to a project buffer and show it in another window.
;; C-c p 4 C-o 	  Display a project buffer in another window without selecting it.
;; C-c p a 	  Switch between files with the same name but different extensions.
;; C-c p o 	  Runs multi-occur on all project buffers currently open.
;; C-c p r 	  Runs interactive query-replace on all files in the projects.
;; C-c p i 	  Invalidates the project cache (if existing).
;; C-c p R 	  Regenerates the projects TAGS file.
;; C-c p j 	  Find tag in project's TAGS file.
;; C-c p k 	  Kills all project buffers.
;; C-c p D 	  Opens the root of the project in dired.
;; C-c p e 	  Shows a list of recently visited project files.
;; C-c p E 	  Opens the .dirs-local.el file of the project.
;; C-c p s s 	  Runs ag on the project. Requires the presence of ag.el.
;; C-c p ! 	  Runs shell-command in the root directory of the project.
;; C-c p & 	  Runs async-shell-command in the root directory of the project.
;; C-c p c 	  Runs a standard compilation command for your type of project.
;; C-c p P 	  Runs a standard test command for your type of project.
;; C-c p t 	  Toggle between an implementation file and its test file.
;; C-c p 4 t 	  Jump to implementation or test file in other window.
;; C-c p z 	  Adds the currently visited file to the cache.
;; C-c p p 	  Display a list of known projects you can switch to.
;; C-c p S 	  Save all project buffers.
;; C-c p m 	  Run the commander (an interface to run commands with a single key).
;; C-c p ESC 	  Switch to the most recently selected Projectile buffer.
;;

;;-------------------------------------------------------------------------
;; https://github.com/atilaneves/cmake-ide

;;(defun cmake-ide--get-default-build-dir ()
;;  "get a default value for cmake-ide-build-dir"
;;  (if (and (boundp 'cmake-ide-build-dir)
;;           (not (string-equal 'cmake-ide-build-dir "")))
;;      ;; if there's a current cmake-ide-build-dir, use it
;;      (progn
;;        ;;(message "cmake-ide-build-dir already defined")
;;        cmake-ide-build-dir)
;;      ;; otherwise...
;;    (progn
;;      ;; is there a result from a previous session?
;;      (let ((fn (concat emacs-dir "cmake-ide-build-dir.save")))
;;        (if (file-exists-p fn)
;;          (progn
;;            ;; load the file into a string
;;            ;;(message "found a previous session at %s: %s" fn
;;            ;;         (with-temp-buffer (insert-file-contents fn)(buffer-string)))
;;            (with-temp-buffer (insert-file-contents fn)(buffer-string)))
;;          ;; otherwise, just use the current directory
;;          (progn
;;            ;;(message "cmake-ide-build-dir from current directory: %s"
;;            ;;         (setq fonix (file-name-directory (buffer-file-name))))
;;            (file-name-directory (buffer-file-name))
;;            )
;;          )
;;        )
;;      )
;;    )
;;  )
;;
;;(defun cmake-ide--set-build-dir (dir)
;;  "set cmake-ide-build-dir, and store the value to a persistent file"
;;  (if (and (boundp 'cmake-ide-build-dir)
;;           (not (string-equal 'cmake-ide-build-dir "")))
;;      (progn (message "cmake-ide-build-dir was %s" cmake-ide-build-dir))
;;      (progn (message "cmake-ide-build-dir was empty"))
;;    )
;;  (setq cmake-ide-build-dir dir)
;;  (message "cmake-ide-build-dir is now %s" dir)
;;  ;; save this value to a file for use in future sessions
;;  (write-region cmake-ide-build-dir nil
;;                (concat user-emacs-directory "cmake-ide-build-dir.save"))
;;  )
;;
;;(defun cmake-ide-set-build-dir (dir)
;;  (interactive
;;   (list (read-directory-name "Enter the cmake-ide build directory: "
;;                              (cmake-ide--get-default-build-dir))))
;;  (cmake-ide--set-build-dir dir)
;;  )
;;
;;(defun my-cmake-ide-setup()
;;  (interactive)
;;  (require 'rtags)
;;  (call-interactively 'cmake-ide-set-build-dir)
;;  )
;;
;;(use-package cmake-ide
;;  :defer t
;;  :init
;;;;  :bind ("s-p" . projectile-command-map)
;;  :commands (cmake-ide-setup)
;;  )

;;-------------------------------------------------------------------------
;;MODES
;;to enable a mode at runtime, type M-x the-mode-name

;;Turn off tab character
(setq-default indent-tabs-mode nil)

;;Indent size
(setq standard-indent 4)

;;Auto-close bracket pairs
;(electric-pair-mode 1)

;; Prevent electric-indent-mode from indenting the current line
;; https://emacs.stackexchange.com/questions/20896/change-the-behaviour-of-ret-with-electric-indent-to-only-indent-the-new-line
(setq-default electric-indent-inhibit t)

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

(use-package open-in-msvs
  :defer t
  :commands open-in-msvs)

;;; C/C++
(load "my-cppsetup") ; needs cleanup

;; cmany
(add-to-list 'load-path (concat emacs-dir "cmany.el"))
(load "cmany")
;;(global-cmany-mode 1)
;;(add-hook 'c-mode-common-hook 'cmany-mode)


;;; PHP
(use-package php-mode
  :defer t
  :config (define-abbrev php-mode-abbrev-table "ex" "extends")
  (use-snips)(add-hook 'php-mode-hook #'hook-snips)
)


;; C#
;; https://github.com/OmniSharp/omnisharp-emacs
(defun my-csharp-mode-setup()
  (flycheck-mode)
  (csharp-mode 1)
  (c-set-offset 'substatement-open 0)
  (add-to-list 'company-backends 'company-omnisharp)
  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  )
(use-package omnisharp-mode
  :config
  (use-package csharp-mode)
  (use-snips)(add-hook 'csharp-mode-hook #'hook-snips)
  (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)
  :mode (("\\.cs\\'" . omnisharp-mode)
         ("\\.CS\\'" . omnisharp-mode)))

;; F#
(use-package fsharp-mode
  :config
  (c-set-offset 'substatement-open 0)
  (use-snips)
  (add-hook 'fsharp-mode-hook #'hook-snips)
  ;; https://github.com/fsharp/emacs-fsharp-mode
  (when this-is-windows
    (let* ((pat "C:/Program Files (x86)/Microsoft Visual Studio/%d/Community/Common7/IDE/CommonExtensions/Microsoft/FSharp/")
           (fsvs2017 (format pat 2017))
           (fsvs2019 (format pat 2019))
           (fsdir (if (file-directory-p fsvs2019) fsvs2019 fsvs2017))
          )
      (setq inferior-fsharp-program (concat fsdir "fsi.exe"))
      (setq fsharp-compiler         (concat fsdir "fsc.exe"))
      )
    )
  :mode (("\\.fs\\'" . fsharp-mode)
         ("\\.FS\\'" . fsharp-mode)
         ("\\.fsx\\'" . fsharp-mode)
         ("\\.FSX\\'" . fsharp-mode)
         )
  )


;; Python
;; https://elpy.readthedocs.io/en/
;; https://github.com/jorgenschaefer/elpy
(defun my-pdb-send-cmd (cmd)
  (interactive (list (read-string "command to send to pdb: " "")))
  (let ((b (current-buffer)))
    (message "pdb: sending command %s" cmd)
    (switch-to-buffer (get-buffer "*gud-main.py*"))
    (end-of-buffer)
    (insert cmd)
    (execute-kbd-macro "\C-m")
    (switch-to-buffer b)
    )
  )


(defun my-python-hook ()
  (win-nav-rsz)
  (require 'gud)
  (require 'gdb-mi)
  (require 'tooltip)
  (subword-mode 1)
  (company--my-setup)
  )
(use-package python
  :defer t
  :mode ("\\.py" . python-mode)
  :config
  (use-package elpy
    :commands elpy-enable
    :config
    (message "python mode: elpy config")
    ;; http://emacs.stackexchange.com/questions/16637/how-to-set-up-elpy-to-use-python3
    ;; requires sudo pip3 install rope_py3k jedi importmagic autopep8 flake8
    (setq elpy-rpc-python-command "python"
;;         elpy-modules (dolist (elem '(elpy-module-highlight-indentation
;;                                     elpy-module-yasnippet))
;;                        (remove elem elpy-modules))
         )
    ;;(elpy-use-ipython "ipython3")
    (add-hook 'python-mode-hook #'my-python-hook)
    (add-hook 'gud-mode-hook #'my-pdb-hook)
    (message "python mode: elpy config finished")
    :bind (:map elpy-mode-map
                ("M-." . elpy-goto-definition)
                ("C-<up>" . backward-paragraph)
                ("C-<down>" . forward-paragraph)
                ("M-<up>" . drag-stuff-up)
                ("M-<down>" . drag-stuff-down)
                ("C-<right>" . subword-forward)
                ("C-<left>" . subword-backward)
                )
    )
  (message "python mode: config")
  (elpy-enable)
  (use-snips)
  (add-hook 'python-mode-hook #'hook-snips)
  (setq gud-pdb-command-name "pdb3")
  ;(add-hook 'python-mode-hook #'smartparens-strict-mode)
  (company--my-setup)
  (message "python mode: ready")
  )
(use-package cython-mode
  :mode (("\\.py[xdi]" . cython-mode)))

;;; Haskell
;; http://www.mew.org/~kazu/proj/ghc-mod/en/preparation.html
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook
          (lambda ()
            (ghc-init))
          )

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

;;; HTML
;; http://web-mode.org/
(use-package web-mode
  :init
  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\."))
        )
  :mode (("\\.html?\\'" . web-mode)
         ("\\.phtml\\'" . web-mode))
  :config
  (subword-mode 1)
  (my-newline-shortcuts)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-block-padding 0)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  )

;;; JavaScript
(setq js-indent-level 2)

;;; XML
;; http://superuser.com/questions/383520/how-to-efficiently-type-in-a-pair-of-xml-tags-in-emacs
;; C-c <right> / C-c <left> move to end of matching tag
;; C-M-n / C-M-p jump to begin/end of tag
(use-package nxml
  :init (setq nxml-slash-auto-complete-flag t)
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.xsd\\'" . nxml-mode)
         ("\\.xslt\\'" . nxml-mode)
         ("\\.xsl\\'" . nxml-mode)
         ("\\.rng\\'" . nxml-mode)
         ("\\.dtllp\\'" . nxml-mode)
         ("\\.xaml\\'" . nxml-mode)
         )
  )
(defun unhtml (start end)
  "escape html characters: &, <, >, \".
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
      (goto-char (point-min))
      (replace-string "\"" "&quot;")
      )))


;;; YAML
(defun my-yaml-hook()
  (message "my-yaml-hook: enter")
  (use-snips)
  (hook-snips)
  (auto-fill-mode 0)
  (disable-line-wrapping)
  (message "my-yaml-hook: exit")
  )
(use-package yaml-mode
  :config (add-hook 'yaml-mode-hook #'my-yaml-hook)
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode))
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


;;; Arduino
(use-package arduino-mode
  :config (use-snips)(add-hook 'arduino-mode-hook #'hook-snips)
  :mode ("\\.ino\\'" . arduino-mode)
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
  (use-snips)
  (add-hook 'octave-mode-hook #'hook-snips)
  (setq inferior-octave-program "octave-cli")
  :mode ("\\.m$" . octave-mode)
  )

;;; AutoHotKey
(use-package xahk-mode
  :mode ("\\.ahk$" . xahk-mode)
  )

;;; PowerShell
(use-package powershell
  :mode ("\\.ps1$" . powershell-mode)
  )

;;; Robot Framework
(use-package robot-mode
  :defer t
  ;;:init (message "robot framework init!!")
  :config (message "robot framework config!!")
  :mode ("\\.robot$" . robot-mode)
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
;; Running Compilations under Emacs: https://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation.html
;;
;; M-x compile
;;     Run a compiler asynchronously under Emacs, with error messages going to the *compilation* buffer.
;; M-x recompile
;;     Invoke a compiler with the same command as in the last invocation of M-x compile.
;; M-x kill-compilation
;;     Kill the running compilation subprocess.

;; Compilation mode commands: https://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation-Mode.html#Compilation-Mode
;;
;; M-g M-n
;; M-g n
;; C-x `
;;     Visit the locus of the next error message or match (next-error).
;; M-g M-p
;; M-g p
;;     Visit the locus of the previous error message or match (previous-error).
;; M-n
;;     Move point to the next error message or match, without visiting its locus (compilation-next-error).
;; M-p
;;     Move point to the previous error message or match, without visiting its locus (compilation-previous-error).
;; M-}
;;     Move point to the next error message or match occurring in a different file (compilation-next-file).
;; M-{
;;     Move point to the previous error message or match occurring in a different file (compilation-previous-file).
;; C-c C-f
;;     Toggle Next Error Follow minor mode, which makes cursor motion in the compilation buffer produce automatic source display.

(require 'compile)
;;; Shut up compile saves
(setq compilation-ask-about-save nil)
;;; Don't save *anything*
(setq compilation-save-buffers-predicate '(lambda () nil))
;;see https://stackoverflow.com/questions/4657142/how-do-i-encourage-emacs-to-follow-the-compilation-buffer
(setq compilation-scroll-output 'first-error)
;; add VS error regexes
(add-to-list 'compilation-error-regexp-alist 'visual_studio)
(add-to-list 'compilation-error-regexp-alist-alist
             '(visual_studio
               "^[ \t]*\\([-A-Za-z0-9:_/\\\\. (),]+\\)(\\([0-9]+\\)\\(,[0-9]+\\)?)+: +\\(fatal error +C[0-9]+\\|error +C[0-9]+\\|warning +C[0-9]+\\|message +\\):"
               1 2)
             )

;; always kill compile buffer http://user42.tuxfamily.org/compilation-always-kill/index.html
(autoload 'compilation-always-kill-mode "compilation-always-kill" nil t)
(eval-after-load "compile" '(compilation-always-kill-mode 1))
(defun my-compilation-mode-hook()
  (company-mode -1)
  (ivy-mode -1)
  (undo-tree-mode -1)
  (linum-mode 0)
  (disable-line-wrapping)
  (local-set-key (kbd "w") 'visual-line-mode)
  (local-set-key (kbd "k") 'my-kill-compilation)
  )
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(defvar my-compilation-exit-code nil)
(defun my-compilation-exit-message-function (status_ code message)
  (setq my-compilation-exit-code code)
  (cons message code)
  )
(setq compilation-exit-message-function 'my-compilation-exit-message-function)


(defun my-call-compile-or-recompile(which)
  (setq my-compilation-buffer-exists
        (not (eq nil (get-buffer "*compilation*"))))
  (setq my-compilation-buffer-is-visible nil)
  (setq my-compilation-buffer-is-current nil)
  (when my-compilation-buffer-exists
    (setq my-compilation-buffer-is-visible
          (not (eq nil (get-buffer-window-list "*compilation*"))))
    (setq my-compilation-buffer-is-current
          (eq (selected-window) (get-buffer-window "*compilation*")))
    )
  (message "compilation buffer exists: %s" my-compilation-buffer-exists)
  (message "compilation buffer is visible: %s" my-compilation-buffer-is-visible)
  (message "compilation buffer is current: %s" my-compilation-buffer-is-current)
  (setq my-compilation-curr-window (selected-window))
  (call-interactively which)
  (setq my-compilation-comp-window (get-buffer-window "*compilation*"))
  (select-window my-compilation-comp-window)
  ;;(when (not my-compilation-buffer-exists)
  ;;  (setq h (window-height w))
  ;;  (shrink-window (- h 10))
  ;;)
  (previous-buffer)
  (setq my-compilation-comp-buffer (current-buffer))
  (next-buffer)
  (select-window my-compilation-curr-window)
  )

;;see https://www.emacswiki.org/emacs/CompilationMode#toc4
(defun my-compile()
  "run compile"
  (interactive)
  (my-call-compile-or-recompile 'compile)
  )
(defun my-recompile()
  "run recompile"
  (interactive)
  (my-call-compile-or-recompile 'recompile)
  )
(defun my-compilation-hook()
  )
(defun my-after-compilation-hook(buffer desc)
  ;; https://emacs.stackexchange.com/questions/14187/run-code-right-after-compilation
  ;; https://emacs.stackexchange.com/questions/9949/how-to-access-the-original-buffer-when-running-m-x-compile-and-friends?rq=1
  ;;(message "Buffer %s: %s ---- exit code=%d" buffer desc my-compilation-exit-code)
  (if (= my-compilation-exit-code 0)
      (progn
        ;(message "Compilation succeeded!")
        (when (not my-compilation-buffer-is-visible)
          (select-window my-compilation-comp-window)
          (switch-to-buffer my-compilation-comp-buffer)
          (select-window my-compilation-curr-window)
          )
        (when my-compilation-buffer-is-visible
          (select-window my-compilation-comp-window)
          ;;(recenter-top-bottom)
          (move-to-window-line -1)
          (select-window my-compilation-curr-window)
          )
        )
      (progn
        ;(message "Compilation failed...")
        ;;(select-window my-compilation-comp-window)
        ;;(next-error)
        )
      )
  )
(defun my-kill-process-interactive ()
  (interactive)
  (let ((pname (ido-completing-read "Process Name: "
                    (mapcar 'process-name (process-list)))))
    (delete-process (get-process pname))
    )
  )
(defun my-kill-compilation()
  "Kill the process made by the \\[compile] or \\[grep] commands. The original kill-compilation uses (interrupt-process), and doesn't work in windows"
  (interactive)
  (let ((buffer (compilation-find-buffer)))
    (if (get-buffer-process buffer)
	(delete-process (get-buffer-process buffer))
      (error "The %s process is not running" (downcase mode-name))
      )
    )
  )
(add-hook 'compilation-mode-hook 'my-compilation-hook)
(add-hook 'compilation-finish-functions 'my-after-compilation-hook)
(global-set-key [C-pause] 'my-kill-compilation)
(global-set-key [C-f7] 'my-kill-compilation)
(global-set-key [S-f6] 'my-compile)
(global-set-key [f6] 'my-recompile)
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
(gud-tooltip-mode 1)
(defun my-pdb-hook ()
  (interactive)

  (global-set-key [f5]     (lambda ()(interactive) (my-pdb-send-cmd "run")))
  (global-set-key [S-f5]   (lambda ()(interactive) (my-pdb-send-cmd "restart")))
  (global-set-key [f7]     (lambda ()(interactive) (my-pdb-send-cmd "continue")))   ;; continue
  (global-set-key [f9]     (lambda ()(interactive) (my-pdb-send-cmd (format "break %s:%d" (buffer-file-name) (line-number-at-pos)))))  ;; add breakpoint
  (global-set-key [C-f9]   (lambda ()(interactive) (my-pdb-send-cmd (format "clear %s:%d" (buffer-file-name) (line-number-at-pos))))) ;; remove breakpoint
  (global-set-key [S-f9]   (lambda ()(interactive) (my-pdb-send-cmd "pp")))  ;; print var under cursor or region
  (global-set-key [C-S-f9] (lambda ()(interactive) (my-pdb-send-cmd "pp")))  ;; watch var under cursor or region
  (global-set-key [f10]    (lambda ()(interactive) (my-pdb-send-cmd "next")))   ;; step over
  ;;(global-set-key [C-f10]  (lambda ()(interactive) (my-pdb-send-cmd "run")))  ;; execute until current line
  (global-set-key [f11]    (lambda ()(interactive) (my-pdb-send-cmd "step")))   ;; step into
  (global-set-key [S-f11]  (lambda ()(interactive) (my-pdb-send-cmd "return"))) ;; finish current function
  )
(defun my-gdb-hook ()
  (interactive)
  (gud-def my-gdb-run-program "run" "" "(re)start the program")
  (gud-def my-gdb-kill-program "kill" "" "kill the program")

  (global-set-key [f5]     'my-gdb-run-program)
  (global-set-key [S-f5]   'my-gdb-kill-program)
  (global-set-key [f7]     'gud-cont)   ;; continue
  (global-set-key [f9]     'gud-break)  ;; add breakpoint
  (global-set-key [C-f9]   'gud-remove) ;; remove breakpoint
  (global-set-key [S-f9]   'gud-print)  ;; print var under cursor or region
  (global-set-key [C-S-f9] 'gud-watch)  ;; watch var under cursor or region
  (global-set-key [f10]    'gud-next)   ;; step over
  (global-set-key [C-f10]  'gud-until)  ;; execute until current line
  (global-set-key [f11]    'gud-step)   ;; step into
  (global-set-key [S-f11]  'gud-finish) ;; finish current function

  ;; make the gdb prompt sticky to its window
  ;; this assumes that the gud window is focused
  ;;(sticky-buffer-mode 1)

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
  ;; (defadvice gud-display-line (true-file line)
  ;;   (let* ((last-nonmenu-event t)	 ; Prevent use of dialog box for questions.
  ;;          (buffer
  ;;           (with-current-buffer gud-comint-buffer
  ;;             (gud-find-file true-file)))
  ;;          (window (and buffer
  ;;                       (or (get-buffer-window buffer)
  ;;                           (display-buffer buffer))))
  ;;          (pos))
  ;;     (when buffer
  ;;       (with-current-buffer buffer
  ;;         (unless (or (verify-visited-file-modtime buffer) gud-keep-buffer)
  ;;           (if (yes-or-no-p
  ;;                (format "File %s changed on disk.  Reread from disk? "
  ;;                        (buffer-name)))
  ;;               (revert-buffer t t)
  ;;             (setq gud-keep-buffer t)))
  ;;         (save-restriction
  ;;           (widen)
  ;;           (goto-char (point-min))
  ;;           (forward-line (1- line))
  ;;           (setq pos (point))
  ;;           (or gud-overlay-arrow-position
  ;;               (setq gud-overlay-arrow-position (make-marker)))
  ;;           (set-marker gud-overlay-arrow-position (point) (current-buffer))
  ;;           ;; If they turned on hl-line, move the hl-line highlight to
  ;;           ;; the arrow's line.
  ;;           (when (featurep 'hl-line)
  ;;             (cond
  ;;              (global-hl-line-mode
  ;;               (global-hl-line-highlight))
  ;;              ((and hl-line-mode hl-line-sticky-flag)
  ;;               (hl-line-highlight)))))
  ;;         (cond ((or (< pos (point-min)) (> pos (point-max)))
  ;;                (widen)
  ;;                (goto-char pos))))
  ;;       (when window
  ;;         (set-window-point window gud-overlay-arrow-position)
  ;;         (if (eq gud-minor-mode 'gdbmi)
  ;;             (unless (gdb-display-source-buffer buffer)
  ;;               (gdb-display-buffer buffer nil 'visible))
  ;;           (message "aqui fonix 0")
  ;;           )
  ;;         (message "aqui fonix 1")
  ;;         (get-buffer-window buffer)
  ;;         (display-buffer buffer)
  ;;         )
  ;;       ;(when window
  ;;       ;  (set-window-point window gud-overlay-arrow-position)
  ;;       ;  (if (eq gud-minor-mode 'gdbmi)
  ;;       ;      (setq gdb-source-window window))
  ;;       ;  )
  ;;       )
  ;;     )
  ;;   )
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


;; rg.el: emacs wrapper for ripgrep: https://github.com/dajva/rg.el
;; ripgrep recursively searches directories for a regex pattern
;; https://github.com/BurntSushi/ripgrep
(use-package rg
  :defer t
  :commands
  ;; This works the same way as M-x rgrep, i.e. you get an interactive
  ;; prompt to enter search details. Universal argument can be used as for
  ;; rgrep
  rg
  ;; M-x rg-literal is a non regexp version of rg
  rg-literal
  ;; M-x rg-project searches in a project defined by projectile,
  ;; find-file-in-project or a vc-backend
  rg-project
  ;; M-x rg-dwim searches for thing at point in a project in all files with
  ;; the same type alias as the current buffer file.
  rg-dwim
  ;; A search result buffer can be saved by invoking rg-save-search or
  ;; rg-save-search-as-name. The former will give the saved buffer a unique
  ;; name and the latter will prompt the user for a name. The
  ;; rg-list-searches command will open a buffer with all active rg-mode
  ;; buffers showing basic information about each search.
  rg-save-search
  rg-save-search-as-name
  rg-list-searches
  :init
  ;; M-s r 	rg
  ;; M-s d 	rg-dwim
  ;; M-s k 	rg-kill-saved-searches
  ;; M-s l 	rg-list-searches
  ;; M-s p 	rg-project
  ;; M-s s 	rg-save-search
  ;; M-s S 	rg-save-search-as-name
  ;; M-s t 	rg-literal
  ;;(rg-enable-default-bindings (kbd "M-s"))
  (add-hook 'rg-mode-hook 'wgrep-ag-setup)
  :config
  (setq rg-show-columns t)
  )

;; this is needed for rg (see above)
(use-package wgrep-ag
  :defer t
  :commands wgrep-ag-setup
  :init
  )


;;------------------------------------------------------------------
;; wgrep
;; wgrep allows you to edit a grep buffer and apply those changes to the file buffer.
;; You can edit the text in the *grep* buffer after typing `C-c C-p` .
;; After that the changed text is highlighted.
;; The following keybindings are defined:
;;
;; * `C-c C-e`: Apply the changes to file buffers.
;; * `C-c C-u`: All changes are unmarked and ignored.
;; * `C-c C-d`: Mark as delete to current line (including newline).
;; * `C-c C-r`: Remove the changes in the region (these changes are not
;;   applied to the files. Of course, the remaining
;;   changes can still be applied to the files.)
;; * `C-c C-p`: Toggle read-only area.
;; * `C-c C-k`: Discard all changes and exit.
;; * `C-x C-q`: Exit wgrep mode.
;;
;; * To save all buffers that wgrep has changed, run
;;
;;     M-x wgrep-save-all-buffers
;;
;; * To save buffer automatically when `wgrep-finish-edit'.
;;
;;     (setq wgrep-auto-save-buffer t)
;;
;; * You can change the default key binding to switch to wgrep.
;;
;;     (setq wgrep-enable-key "r")
;;
;; * To apply all changes wheather or not buffer is read-only.
;;
;;     (setq wgrep-change-readonly-file t)


;; wgrep allows you to edit all files in a grep result. For example,
;; you can use C-c g or C-c r to search all files in a project, then
;; use C-c C-o to enter ivy-occur mode, followed by 'w' to make
;; the grep results buffer editable, then you can edit the results
;; however you wish.
(use-package wgrep
:ensure t)

;;-------------------------------------------------------------------------

(setq --my-grep--pattern "")
(setq --my-grep--target nil)
(defun my-grep()
  (interactive)
  (let* ((pattern (read-string "grep pattern: " --my-grep--pattern))
         (_target (if --my-grep--target --my-grep--target buffer-file-name))
         (dn (file-name-directory _target))
         (bn (file-name-base _target))
         (target (ido-read-file-name
                  "grep target: " dn bn nil bn))
         )                              ;
    (message "grepping for pattern: '%s' at target '%s'" pattern target)
    (setq --my-grep--pattern pattern)
    (setq --my-grep--target target)
    (grep (format "grep -rnH '%s' %s" pattern target))
    )
  )


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
(defun my-latex-hook()
  (message "my-latex-hook: enter")
  (my-text-hook)
  (message "my-latex-hook: exit")
  )
(add-hook 'tex-mode-hook 'my-latex-hook)
(add-hook 'latex-mode-hook 'my-latex-hook)

;; save image from clipboard:
(load-file (concat emacs-dir "imgsave/imgsave.el"))
;; https://github.com/robinchenyu/image-paste/blob/master/bin/imagepaste.py
;; https://stackoverflow.com/questions/17435995/paste-an-image-on-clipboard-to-emacs-org-mode-file-without-saving-it
;; https://emacs.stackexchange.com/questions/41016/how-can-i-yank-images-from-emacs
;; https://github.com/dnxbjyj/pasteex-mode
;; https://github.com/mooreryan/markdown-dnd-images


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
  (add-hook 'rst-mode-hook 'hook-snips)
)

;; Markdown
;; https://jblevins.org/projects/markdown-mode/
;; to preview using the github style: https://github.com/joeyespo/grip
(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (add-hook 'markdown-mode-hook 'my-text-hook)
  (add-hook 'markdown-mode-hook 'hook-snips)
  (add-hook 'gfm-mode-hook 'my-text-hook)
  (add-hook 'gfm-mode-hook 'hook-snips)
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
;; MAGIT https://magit.vc/manual/magit/Getting-Started.html#Getting-Started
;;
;;     C-x g ('magit-status) to see git status, and in the status buffer:
;;
;;     C-TAB to toggle section visibility
;;     TAB to toggle item visibility
;;     RET to open items
;;     n go to next
;;     p go to prev
;;     g refresh status buffer
;;     s to stage hunk/file/selection
;;     u to unstage hunk/file/selection
;;     ? open the dispatch popup
;;     h open the dispatch popup
;;
;;     c to commit (type c, type the message then C-c C-c to actually commit)
;;     b b to switch to another branch
;;
;; Other handy keys:
;;
;;     P u to do a git push
;;     F u to do a git pull
;;
;; git config --global alias.lola "log --graph --decorate --pretty=oneline --abbrev-commit --all"
;;

(use-package magit
  :init
  (setq magit-refresh-status-buffer nil)
  ;; https://magit.vc/manual/magit/Listing-Submodules.html
  ;; https://emacs.stackexchange.com/questions/13659/displaying-branch-descriptions-in-magit
  (setq magit-module-sections-nested t)
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-overview
                          nil t)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpulled-from-upstream
                          nil t)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpushed-to-upstream
                          nil t)
  :commands (magit-status)
  :bind
  (
   ("C-x g" . magit-status)
   ("C-x C-g" . my-magit-status)
   ("C-x M-g" . magit-dispatch-popup))
  )

(defun my-magit-status ()
  (interactive)
  (let* ((prompt "repo root: ")
         (rd (car (split-string (shell-command-to-string "git rev-parse --show-toplevel"))))
         (.. (message "repodir=%s" rd))
         (dn (file-name-directory rd))
         (.. (message "repodirname=%s" dn))
         (bn (file-name-base rd))
         (.. (message "repobasename=%s" bn))
         (r (ido-read-directory-name prompt dn bn nil bn))
         (.. (message "read=%s" r))
         (r (file-truename r))
         (.. (message "truename=%s" r))
         (r (file-name-as-directory r))
         (.. (message "finalname=%s" r))
         )
    (magit-status r)
    )
  )


;;-----------------------------------------------------------------------------
;; google-this
;; http://pragmaticemacs.com/emacs/google-search-from-inside-emacs/

;; C-c / SPC 	google-this-region
;; C-c / a 	google-this-ray
;; C-c / c 	google-this-translate-query-or-region
;; C-c / e 	google-this-error
;; C-c / f 	google-this-forecast
;; C-c / g 	google-this-lucky-search
;; C-c / i 	google-this-lucky-and-insert-url
;; C-c / l 	google-this-line
;; C-c / m 	google-maps
;; C-c / n 	google-this-noconfirm
;; C-c / r 	google-this-cpp-reference
;; C-c / s 	google-this-symbol
;; C-c / t 	google-this
;; C-c / w 	google-this-word
;; C-c / <return> 	google-this-search

(use-package google-this
  :commands google-this
  ;; :config
  ;; (google-this-mode 1)
  )


;;--------------------------------------------------------------------
;; Persistent scratch - save the scratch file
;; http://dorophone.blogspot.com/2011/11/how-to-make-emacs-scratch-buffer.html

(setq initial-major-mode 'fundamental-mode) ;; https://emacsredux.com/blog/2014/07/25/configure-the-scratch-buffers-mode/
(setq initial-scratch-message "# Scratch buffer

This buffer is for notes you don't want to save.
If you want to create a file, visit that file with `C-x C-f`,
then enter the text in that file's own buffer.

This buffer contains markdown text, but it is in fundamental-mode
to make startup faster. Run `M-x markdown-mode` to enable markdown
mode.
")

(defvar persistent-scratch-backup-directory
    (concat user-emacs-directory "scratch-persist/")
    "Location of backups of the *scratch* buffer contents for
    persistent-scratch.")

(defvar persistent-scratch-filename
    (concat persistent-scratch-backup-directory "current.txt")
    "Location of *scratch* file contents for persistent-scratch.")

(defun persistent-scratch-get-backup-name ()
  "Create a filename to backup the current scratch file by
  concatenating persistent-scratch-backup-directory with the
  current date and time."
  (concat persistent-scratch-backup-directory
          ;; TODO use the file modification time instead
          "scratch." (format-time-string "%Y%m%d_%H%M%S") ".txt"))

(defun persistent-scratch-save ()
  (interactive)
  "write the contents of *scratch* to the file name
  persistent-scratch-filename, making a backup copy in
  persistent-scratch-backup-directory."
  (unless (file-directory-p persistent-scratch-backup-directory)
    (make-directory persistent-scratch-backup-directory))
  (with-current-buffer (get-buffer "*scratch*")
    (if (file-exists-p persistent-scratch-filename)
        (let ((bn (persistent-scratch-get-backup-name)))
          (if (file-exists-p bn) (delete-file bn))
          (copy-file persistent-scratch-filename bn)))
    (write-region (point-min) (point-max)
                  persistent-scratch-filename)))

(defun persistent-scratch-load ()
  (interactive)
  "Load the contents of PERSISTENT-SCRATCH-FILENAME into the
  scratch buffer, clearing its contents first."
  (if (file-exists-p persistent-scratch-filename)
      (with-current-buffer (get-buffer "*scratch*")
        (delete-region (point-min) (point-max))
        (shell-command (format "cat %s" persistent-scratch-filename) (current-buffer)))))

(persistent-scratch-load)
(push #'persistent-scratch-save kill-emacs-hook)


;;-----------------------------------------------------------------------------

;; https://www.reddit.com/r/emacs/comments/981khz/emacs_music_player_with_emms/
(use-package emms
  :defer t
  :commands ()
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Dropbox/muzzik/tmp/")
  (setq emms-playlist-buffer-name "*Music*")
  (setq emms-info-asynchronously t)
  ;; https://emacs.stackexchange.com/questions/45938/install-emms-and-missing-executable-emms-print-metadata
  ;;(require 'emms-info-libtag) ;;; load functions that will talk to emms-print-metadata which in turn talks to libtag and gets metadata
  ;;(setq emms-info-functions '(emms-info-libtag)) ;;; make sure libtag is the only thing delivering metadata
  (require 'emms-mode-line)
  (emms-mode-line 1)
  (require 'emms-playing-time)
  (emms-playing-time 1))


;;-----------------------------------------------------------------------------
(if this-is-windows
  (progn
    ;;if in Windows run this block
    (set-face-attribute 'default nil :font "Consolas-10")
    ;;(custom-set-faces '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :foundry "unknown"
    ;;                                          :height 100 :family "Consolas")))))
  )
  (progn
    ;;otherwise run this block
    ;; https://askubuntu.com/questions/690427/how-can-i-get-emacs24-fonts-to-smooth-like-in-the-terminal
    (set-face-attribute 'default nil :font "Consolas-11")
    ;;(set-face-attribute 'default nil :font "Inconsolata-12")
    ;;(set-face-attribute 'default nil :font "Consolas-11")
    ;;(set-face-attribute 'default nil :font "Ubuntu Mono-12")
    ;;(custom-set-faces '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :foundry "unknown"
    ;;                                          ;;:height 120 :family "Inconsolata")))))
    ;;                                          ;;:height 120 :family "Consolas")))))
    ;;                                          :height 100 :family "Monospace")))))
  )
)


;;-----------------------------------------------------------------------------

(let ((fn (concat emacs-dir "local.el")))
  (if (file-exists-p fn)
      (progn
        (message "loading local config file: %s" fn)
        (load-file fn)
        )
      )
  )


;;-----------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("80ceeb45ccb797fe510980900eda334c777f05ee3181cb7e19cd6bb6fc7fda7c" "8abee8a14e028101f90a2d314f1b03bed1cde7fd3f1eb945ada6ffc15b1d7d65" "c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" "ba9be9caf9aa91eb34cf11ad9e8c61e54db68d2d474f99a52ba7e87097fa27f5" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "9d91458c4ad7c74cf946bd97ad085c0f6a40c370ac0a1cbeb2e3879f15b40553" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
 '(ecb-options-version "2.40")
 '(package-selected-packages
   (quote
    (
     ag
     anzu
     arduino-mode
     async-await
     babel
     babel-repl
     bicycle
     ccls
     clang-format
     clean-aindent-mode
     cmake-mode
     company-c-headers
     counsel-etags
     company-lsp
     company-rtags
     company-ycmd
     counsel-projectile
     cquery
     csharp-mode
     cython-mode
     dirtree
     drag-stuff
     dtrt-indent
     elisp-slime-nav
     elpy
     emms
     emms-info-mediainfo
     emms-mark-ext
     emms-mode-line-cycle
     emms-player-simple-mpv
     emms-state
     ess
     find-file-in-project
     find-file-in-repository
     firebelly-theme
     flycheck
     flymake-cppcheck
     flx-ido
     fsharp-mode
     fzf
     git-timemachine
     glsl-mode
     ghc
     google-this
     help-fns+
     hemisu-theme
     highlight-symbol
     hungry-delete
     ido-ubiquitous
     ido-vertical-mode
     iedit
     ivy
     levenshtein
     lsp-mode
     lsp-ui
     magit
     man-commands
     markdown-mode
     mc-extras
     modern-cpp-font-lock
     monokai-theme
     multiple-cursors
     omnisharp
     open-in-msvs
     org-babel-eval-in-repl
     paradox
     pdb-mode
     persp-mode
     persp-projectile
     php-mode
     powershell
     protobuf-mode
     pylint
     pytest-pdb-break
     realgud
     realgud-ipdb
     ripgrep
     rg
     rtags
     rtags-xref
     seq
     slime
     smart-mode-line
     smartparens
     smex
     solarized-theme
     string-inflection
     syntax-subword
     tango-plus-theme
     term-run
     transient
     undo-tree
     use-package
     vlf
     volatile-highlights
     web-mode
     wgrep
     wgrep-ag
     which-key
     window-number
     ws-butler
     xahk-mode
     yaml-mode
     yasnippet-classic-snippets
     zenburn-theme
     )
    )
   )
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
 '(flymake-error ((t (:background nil :foreground nil :inverse-video nil :underline nil :slant normal :weight normal))))
 '(highlight-indentation-face ((t (:background "gray24")))))


(set-face-attribute 'ido-vertical-first-match-face nil :background "#777777" :foreground "orange")
(set-face-attribute 'ido-vertical-only-match-face nil :background nil :foreground nil)
(set-face-attribute 'ido-vertical-match-face nil :foreground nil)
(put 'downcase-region 'disabled nil)
