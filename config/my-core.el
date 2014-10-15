;; Don't show those horrible buttons
(tool-bar-mode -1)

;; break long lines at word boundaries
(visual-line-mode 1)

;; lockfiles are evil.
(setq create-lockfiles nil)

;; also tabs are evil
(setq-default indent-tabs-mode nil)

;; number columns in the status bar
(column-number-mode)

;; require a trailing newline
(setq require-final-newline t)

;; I never look at right-side fringes. Do you?
(set-fringe-style '(8 . 0))

;; don't put intitial text in scratch buffer
(setq initial-scratch-message nil)

;'Woman' > 'man'.
(defalias 'man 'woman)

;; Disable toolbars and splash screens.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; from <https://github.com/bling/dotemacs/>
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
;; nevermind that's annoying
(setq ring-bell-function 'ignore)

;; The default of 16 is too low. Give me a 64-object mark ring.
;; Across all files, make it 128.
(setq mark-ring-max 64)
(setq global-mark-ring-max 128)

;; Display the current function name in the modeline.
(which-function-mode 0)

;; Show me the new saved file if the contents change on disk when editing.
(global-auto-revert-mode 1)

;; Repurposed from
;; <https://github.com/bling/dotemacs/blob/master/config/init-core.el>
(defun my-find-file-check-large-file ()
  "Check the size of files when loading, and don't let me break them."
  (when (> (buffer-size) (* 1024 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(defun my-setup-file-defaults ()
  "Set the defaults for a new file opening."
  (my-find-file-check-large-file)
  (setq show-trailing-whitespace t)
  (electric-indent-mode 1)
)

;; Adapted from
;; <https://github.com/bling/dotemacs/blob/master/config/init-core.el>
(defun my-do-not-kill-scratch-buffer ()
  "Don't let the scratch buffer die."
  (if (member (buffer-name (current-buffer)) '("*scratch*" "*Messages*"))
      (progn
        (bury-buffer)
        nil)
    t))

(add-hook 'kill-buffer-query-functions 'my-do-not-kill-scratch-buffer)

(add-hook 'find-file-hook 'my-setup-file-defaults)

(random t) ;; seed

(plist-put minibuffer-prompt-properties
           'point-entered 'minibuffer-avoid-prompt)

(defun my-spawn-terminal-here ()
  "Open a terminal in the current buffer's directory"
  (interactive)
  (start-process "urxvt" nil "urxvt"))

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

(define-prefix-command 'my-prefix)
(global-set-key (kbd "C-a") 'my-prefix)

(after 'evil
    (global-set-key (kbd "C-a h") 'evil-window-left)
    (global-set-key (kbd "C-a j") 'evil-window-down)
    (global-set-key (kbd "C-a k") 'evil-window-up)
    (global-set-key (kbd "C-a l") 'evil-window-right)
  )
(global-set-key (kbd "C-a -") 'split-window-vertically)
(global-set-key (kbd "C-a |") 'split-window-horizontally)
(global-set-key (kbd "C-a x") 'kill-this-window)

;; have no use for these default bindings
(global-unset-key (kbd "C-x m"))

;; easy increment? Good enough ...
(global-set-key (kbd "C-c +") 'increment-number-at-point)

(provide 'my-core)
