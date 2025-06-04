;; my-core.el   -*- lexical-binding:t; -*-
;;
;; The big, beating heart of my little corner of Emacs.
;; General, mostly-plugin-independent settings go here.

(defvar my-terminal-emulator "foot"
  "Terminal emulator to be spawned with my-spawn-terminal-here.")

(defvar my-graphical-font
  (cond
    ((eq system-type 'darwin) "Terminus 10")
    ((eq system-type 'gnu/linux) "Terminus (TTF)-10"))
  "Font used for graphical editing sessions.")

(tool-bar-mode -1)
(menu-bar-mode -1)

;; don't break long lines at word boundaries
(visual-line-mode nil)

;; split at end of buffer in programming mode buffers
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;; lockfiles are evil.
(setq create-lockfiles nil)

;; bar cursor
(setq cursor-type 'box)

;; also tabs are evil
(setq-default indent-tabs-mode nil)

;; number columns in the status bar
(column-number-mode)

;; require a trailing newline
(setq require-final-newline t)

;; I never look at right-side fringes. Do you?
;; (if (fboundp 'set-fringe-style) (set-fringe-style '(8 . 0)))

;; don't put intitial text in scratch buffer
(setq initial-scratch-message nil)

;'Woman' > 'man'.
(defalias 'man 'woman)

;; Disable toolbars and splash screens.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(defmacro after (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(with-eval-after-load ,feature
     ,@body))

(ignore-errors ;; windows
  (exec-path-from-shell-initialize))

;; Hide startup messages
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

;; Line numbers!
;; Disable vertical scrollbars in all frames.
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Ediff with horizontal splits.
(after 'ediff
  (setq ediff-split-window-function 'split-window-horizontally))

(defun my-home-path (&rest components)
  "Join COMPONENTS relative to `user-home-directory`, even if some start with '/'."
  (expand-file-name
   (string-join (seq-remove #'string-empty-p
                            (mapcar (lambda (s) (string-remove-prefix "/" s))
                                    components))
                "/")
   (getenv "HOME")))


(defvar my-auto-save-folder (my-home-path "/.emacs.d/.saves/")
  "Directory used for Emacs backups.")

(setq backup-directory-alist `(("." . ,my-auto-save-folder)))
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name my-auto-save-folder) t)))

;; Only scroll one line when near the bottom of the screen, instead
;; of jumping the screen around.
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t)

;; Let me write `y` or `n` even for important stuff that would normally require
;; me to fully type `yes` or `no`.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable the mouse in terminal mode.
(xterm-mouse-mode 1)

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

;; Thanks
;; http://www.jesshamrick.com/2013/03/31/macs-and-emacs/
(defun my-system-is-mac ()
  "t when systm is a mac, else nil."
  (interactive)
  (string-equal system-type "darwin"))

(defun my-system-is-linux ()
  "t when systm is linux, else nil."
  (interactive)
   (or (string-equal system-type "gnu/linux")
       (string-equal system-type "linux")))

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
  (electric-indent-mode 1))

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

;; name the title based on the file
(defun my-update-emacs-title ()
  "Update the Emacs title based on the current buffer.

If the current buffer is associated with a filename, that filename will be
used to tile the window. Otherwise, the window will be titled based upon the
name of the buffer."
  (if (buffer-file-name (current-buffer))
      (setq frame-title-format "Emacs - %f")
      (setq frame-title-format "Emacs - %b")))

(cl-dolist (hook '(buffer-list-update-hook
                   change-major-mode-hook
                   find-file-hook))
  (add-hook hook 'my-update-emacs-title))

(defun my-spawn-terminal-here ()
  "Open a terminal in the current buffer's directory"
  (interactive)
  (start-process my-terminal-emulator nil my-terminal-emulator))

(defun my-set-window-font (font)
  "Set the frame font to FONT.
FONT is the name of a xft font, like `Monospace-10'."
  (interactive "sFont: ")
  ;; (set-face-attribute 'default nil :height 125 :family "Fira Mono"))
  (set-face-attribute 'fixed-pitch nil :font font)
  (set-face-attribute 'default nil :font font)
  ;; (set-face-attribute 'variable-pitch nil :font font)
  ;; (set-face-attribute 'fringe nil :family "Terminus")
  (set-frame-font font nil t))


(defun my-use-default-font (&optional frame)
  "Set the frame font to the font name in the variable my-graphical-font.
This command only has an effect on graphical frames."
  (interactive)
  (if (my-system-is-mac)
    (set-face-attribute 'default nil
       :family "Monaco" :height 100 :weight 'normal))
  (when window-system
    (my-set-window-font my-graphical-font)))

(when (display-graphic-p)
    (add-hook 'after-make-frame-functions 'my-use-default-font)
    (setq pop-up-frames nil)
    (setq display-buffer-alist ()
          ;;'(("\\*Help\\*" display-buffer-pop-up-frame)
          ;;  ("\\*Completions\\*" display-buffer-pop-up-frame)
            ;; Add more rules as desired
            )

    (my-use-default-font))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x g") 'my-google)
(global-set-key (kbd "C-c e") 'my-eval-and-replace)

(when (my-system-is-mac)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

(defun my-enable-line-numbers ()
  "Enable line numbers in a smart way."
  (interactive)
  (unless (or (minibufferp)
              (member major-mode '(org-mode
                                   eshell-mode
                                   shell-mode
                                   term-mode
                                   vterm-mode
                                   eshell-mode)))
    (display-line-numbers-mode)))

(add-hook 'prog-mode-hook #'my-enable-line-numbers)
(add-hook 'text-mode-hook #'my-enable-line-numbers)

;; Set undo-limit to a large value (e.g., 100 MB of changes)
(setq undo-limit 100000000) ; 100,000,000 characters

;; Set undo-outer-limit to an even larger value (e.g., 500 MB of changes)
;; This acts as a hard cap. Make it large enough for your needs.
(setq undo-outer-limit 500000000) ; 500,000,000 characters

;; tell indent-for-tab-command to complete when indent has no effect
(setq tab-always-indent 'complete)

;; 1. Ensure margins exist
(setq-default left-margin-width  0
              right-margin-width 0)

(setq fill-column 80)
(global-display-fill-column-indicator-mode)

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

(setq-default scroll-margin 5)

;; try to make scrolling smooth in terminal
(setq scroll-preserve-screen-position t)

(set-fringe-mode 0)  ; Removes fringe, which can cause flickering

(provide 'my-core)
