;; my-core.el   -*- lexical-binding:t; -*-
;;
;; The big, beating heart of my little corner of Emacs.
;; General, mostly-plugin-independent settings go here.

;; ===================================================================
;; ## Performance
;; -------------------------------------------------------------------
;; 268,435,456
(defvar my-gc-cons-threshold (* 256 1024 1024))

(setq
 gc-cons-threshold my-gc-cons-threshold
 gc-cons-percentage 0.1)

(add-hook
 'minibuffer-setup-hook
 (lambda ()
   (setq gc-cons-threshold most-positive-fixnum)))

(add-hook
 'minibuffer-exit-hook
 (lambda ()
   (garbage-collect)
   (setq gc-cons-threshold my-gc-cons-threshold)))

(setq copy-region-blink-delay 0)
;; -------------------------------------------------------------------

;; responsiveness
(setq echo-keystrokes 0.1)

(setq backup-by-copying t)

;; Suppress lexical binding warnings for dependencies
(setq warning-suppress-types '((files)))
(setq warning-suppress-log-types '((files)))
(setq warning-minimum-level :error)
(setq warning-minimum-log-level :error)

;; keep more messages (default 1000)
(setq message-log-max 10000)

;; Security: Disable file-local variables and eval headers
;; This prevents malicious files from executing code when opened
(setq enable-local-variables nil)      ; Disable all file-local variables
(setq enable-local-eval nil)           ; Never eval code from files
(setq enable-dir-local-variables nil)  ; Disable .dir-locals.el files

;; Always delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Prompt about reverting buffers when files change on disk
(setq auto-revert-verbose t)                     ; Show messages when reverting
(setq revert-without-query nil)                  ; Always prompt before reverting
(setq auto-revert-avoid-polling t)               ; Use file system notifications
(setq auto-revert-check-vc-info t)               ; Check version control info when reverting
(global-auto-revert-mode 1)                     ; Enable auto-revert globally (with prompting)

;; Enable syntax highlighting globally
(global-font-lock-mode 1)

;; Find file at point configuration
(require 'ffap)
(ffap-bindings)

(defvar my-terminal-emulator
  "Terminal emulator to be spawned with my-spawn-terminal-here."
  (cond
    ((eq system-type 'gnu/linux) "foot")
    ((eq system-type 'darwin) "open -a Terminal")))

(defun my-macos-setup ()
  "Do macos-specific setup. Caller must ensure `my-system-is-mac'."
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

;; split at end of buffer in programming mode buffers
(defun my-enable-truncate-lines ()
  "Enable truncate-lines in programming modes."
  (setq truncate-lines t))

(add-hook 'prog-mode-hook 'my-enable-truncate-lines)

;; lockfiles are evil.
(setq create-lockfiles nil)

;; also tabs are evil
(setq-default indent-tabs-mode nil)

;; require a trailing newline
(setq require-final-newline t)

;; don't put intitial text in scratch buffer
(setq initial-scratch-message nil)

;; 'Woman' > 'man'.
;;
;; no longer! from woman:
;;
;; Note that `M-x woman' doesn’t yet support the latest features of
;; modern man pages, so we recommend using `M-x man' if that is
;; available on your system."
;; (defalias 'man 'woman)

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

;; Ediff with horizontal splits
(setq ediff-split-window-function 'split-window-horizontally)

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

;; Keep all backups in .emacs.d, including tramp
(setq backup-directory-alist `((".*" . ,my-auto-save-folder)))
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name my-auto-save-folder) t)))
;; Force tramp to use the same backup directory
(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-auto-save-directory my-auto-save-folder)

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

;; Fix minibuffer messages disappearing too quickly
(setq minibuffer-message-timeout 10)     ; Keep messages for 10 seconds
(setq eldoc-idle-delay 2.0)              ; Wait 2 seconds before showing eldoc (longer delay)
(setq eldoc-echo-area-display-truncation-message nil) ; Don't truncate
(setq eldoc-print-after-edit nil)        ; Don't show eldoc while typing

;; Prevent eldoc from clearing important messages
(defvar my-important-message-active nil
  "Flag to indicate an important message is being displayed.")

(defun my-protect-important-messages (orig-fun &rest args)
  "Advice to prevent eldoc from clearing important messages."
  (let ((msg (and args (car args))))
    ;; Mark important messages
    (when (and msg (stringp msg)
               (or (string-match-p "changed on disk" msg)
                   (string-match-p "really edit" msg)
                   (string-match-p "revert" msg)
                   (string-match-p "(y, n, r" msg)))
      (setq my-important-message-active t)
      ;; Clear the flag after a reasonable time
      (run-with-timer 30 nil (lambda () (setq my-important-message-active nil))))
    ;; Don't let eldoc clear important messages
    (unless (and my-important-message-active
                 (or (null msg) (string-empty-p msg)))
      (apply orig-fun args))))

(advice-add 'message :around #'my-protect-important-messages)

;; Debug function to track what's clearing the minibuffer
(defun my-debug-minibuffer-clear ()
  "Debug function to see what's clearing the minibuffer."
  (interactive)
  (let ((original-message (symbol-function 'message)))
    (advice-add 'message :around
                (lambda (orig-fun &rest args)
                  (when (and args (stringp (car args)))
                    (message "[DEBUG] Message: %s" (car args)))
                  (apply orig-fun args)))
    (message "Minibuffer debugging enabled. Run again to disable.")
    (run-with-timer 30 nil
                    (lambda ()
                      (advice-remove 'message 'my-debug-minibuffer-clear)
                      (message "Minibuffer debugging disabled")))))

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

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x g") 'my-google)
(global-set-key (kbd "C-c e") 'my-eval-and-replace)

;; Set undo-limit to a large value (e.g., 100 MB of changes)
(setq undo-limit 100000000) ; 100,000,000 characters

;; Set undo-outer-limit to an even larger value (e.g., 500 MB of changes)
;; This acts as a hard cap. Make it large enough for your needs.
(setq undo-outer-limit 500000000) ; 500,000,000 characters

;; tell indent-for-tab-command to complete when indent has no effect
(setq tab-always-indent 'complete)

;; Enable savehist for minibuffer history
(savehist-mode 1)
(setq savehist-additional-variables
      '(search-ring regexp-search-ring kill-ring compile-history))

;; Enable recentf for recent files
(recentf-mode 1)
(setq recentf-max-saved-items 100)

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

(if (my-system-is-mac) (my-macos-setup))

(defun my-pair-delim (&optional ARG)
  "Set up pairing delim, e.g. disable `electiric-pair-mode' and eventually
will hook in smartparens."
  (interactive)
  ;; disable electric pair
  (electric-pair-mode -1))


(provide 'my-core)
