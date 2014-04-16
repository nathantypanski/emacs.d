;; Disable toolbars and splash screens.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Hide startup messages
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

;; Line numbers!
(nlinum-mode 1)

;; Disable vertical scrollbars in all frames.
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Disable the menu bar in console emacs.
(unless (display-graphic-p) (menu-bar-mode -1))

;; Set the default font (only matters in graphical mode).
(set-default-font "Terminus-10")

;; Ediff with horizontal splits.
(setq ediff-split-window-function 'split-window-horizontally)

;; disable backup
(setq backup-inhibited t)

;; disable auto save
(setq auto-save-default nil)

;; Only scroll one line when near the bottom of the screen, instead
;; of jumping the screen around.
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t)

;; Let me write `y` or `n` even for important stuff that would normally require
;; me to fully type `yes` or `no`.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable the mouse in terminal mode.
(xterm-mouse-mode 1)

;; UTF-8 everything!
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; This isn't a typewriter (even if it is a terminal); one space after sentences,
;; please.
(setq sentence-end-double-space nil)

;; Flash the frame to represent a bell.
(setq visible-bell t)

;; The default of 16 is too low. Give me a 64-object mark ring.
;; Across all files, make it 128.
(setq mark-ring-max 64)
(setq global-mark-ring-max 128)

;; Display the current function name in the modeline.
(which-function-mode 1)

;; Show me the new saved file if the contents change on disk when editing.
(global-auto-revert-mode 1)

;; Typing newlines triggers indentation.
(electric-indent-mode 1)

;; Ignoring electric indentation
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode"
  (if (equal major-mode 'python-mode)
      `no-indent'
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

;; Enter key executes newline-and-indent
(defun set-newline-and-indent ()
  "Map the return key with `newline-and-indent'"
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-newline-and-indent)

;; Turn word-wrap on and redefine certain (simple) commands to work on visual
;; lines, not logical lines.
;;
;; Also, show trailing whitespace.
(add-hook 'find-file-hook (lambda ()
                            (visual-line-mode)
                            (setq show-trailing-whitespace t)))

(random t) ;; seed

(plist-put minibuffer-prompt-properties
           'point-entered 'minibuffer-avoid-prompt)

(setq-default indent-tabs-mode nil)

(setq-default c-default-style "linux"
              c-basic-offset 8
              tab-width 8)

(global-set-key [remap eval-expression] 'pp-eval-expression)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

(linum-mode)

(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory "config"))
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(provide 'my-globals)
