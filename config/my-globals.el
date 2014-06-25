;; Don't show those horrible buttons
(tool-bar-mode -1)

;; break long lines at word boundaries
(visual-line-mode 1)

;; lockfiles are evil.
(setq create-lockfiles nil)

;; also tabs are evil
(setq-default indent-tabs-mode nil)

;; number columns
(column-number-mode)

;'Woman' > 'man'.
(defalias 'man 'woman)

;; Disable toolbars and splash screens.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; for `dotimes', `push' (Emacs 21)
(eval-when-compile (require 'cl))

(defmacro after (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

;; make sure $PATH is set correctly
(use-package exec-path-from-shell
  :ensure exec-path-from-shell)

(ignore-errors ;; windows
  (exec-path-from-shell-initialize))

;; Hide startup messages
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

;; Line numbers!
;; Disable vertical scrollbars in all frames.
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Disable menu bar for all (trying this out)
(menu-bar-mode -1)

;; Disable the menu bar in console emacs.
(unless (display-graphic-p) (menu-bar-mode -1))

;; Ediff with horizontal splits.
(setq ediff-split-window-function 'split-window-horizontally)

;; I know what I'm doing; don't litter my fscking tree!

(defvar my-auto-save-folder "~/.emacs.d/.saves/")
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))
(setq auto-save-file-name-transforms
      `((".*" ,my-auto-save-folder t)))

;; (setq make-backup-files nil)

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
(which-function-mode 0)

;; Show me the new saved file if the contents change on disk when editing.
(global-auto-revert-mode 1)

;; Turn word-wrap on and redefine certain (simple) commands to work on visual
;; lines, not logical lines.
;;
;; Also, show trailing whitespace.
(add-hook 'find-file-hook (lambda ()
                            (visual-line-mode)
                            (setq show-trailing-whitespace t)
                            (electric-indent-mode 1)
                            )
          )

(random t) ;; seed

(plist-put minibuffer-prompt-properties
           'point-entered 'minibuffer-avoid-prompt)


(global-set-key [remap eval-expression] 'pp-eval-expression)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

;;(add-to-list 'load-path (concat user-emacs-directory "config"))
;;(add-to-list 'load-path (concat user-emacs-directory "elisp"))

;; Set the default font (only matters in graphical mode).
(when window-system
  (progn
    (set-face-attribute 'default nil :font "Terminus-10")
    (set-frame-font "Terminus-10" nil t)
))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x g") 'my-google)
(global-set-key (kbd "C-c e") 'my-eval-and-replace)

;; have no use for these default bindings
(global-unset-key (kbd "C-x m"))

(provide 'my-globals)
