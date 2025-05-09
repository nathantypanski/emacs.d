;; my-core.el
;;
;; The big, beating heart of my little corner of Emacs.
;; General, mostly-plugin-independent settings go here.

(eval-when-compile (require 'cl-lib))

(defvar my-terminal-emulator "foot"
  "Terminal emulator to be spawned with my-spawn-terminal-here.")

(defvar my-graphical-font
  (cond
    ((eq system-type 'darwin) "Terminus 10")
    ((eq system-type 'gnu/linux) "Terminus 16"))
  "Font used for graphical editing sessions.")

;; evil-collection requires this set before loading evil
(setq evil-want-keybinding nil)

;; Don't show those horrible buttons
(tool-bar-mode -1)

;; break long lines at word boundaries
(visual-line-mode 1)

;; lockfiles are evil.
(setq create-lockfiles nil)

;; bar cursor
(setq cursor-type 'bar)
;; cursor-shape control for foot
(defun my/tty-cursor-update ()
  "Block cursor by default; beam cursor in Evil Insert state."
  (when (not (display-graphic-p))              ; TTY frames only
    (send-string-to-terminal
     (if (evil-insert-state-p)
         "\e[6 q"   ;; Ps = 6  → steady beam (vertical bar)
       "\e[2 q")))) ;; Ps = 2  → steady block

;; also tabs are evil
(setq-default indent-tabs-mode nil)

;; number columns in the status bar
(column-number-mode)

;; require a trailing newline
(setq require-final-newline t)

;; I never look at right-side fringes. Do you?
(if (fboundp 'set-fringe-style) (set-fringe-style '(8 . 0)))

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
  :ensure exec-path-from-shell
  :config
  (progn
    (exec-path-from-shell-copy-env "PATH")
    (exec-path-from-shell-copy-env "PYTHONPATH")
))

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

(defvar my-auto-save-folder "~/.emacs.d/.saves/"
  "Directory used for Emacs backups.")

(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))
(setq auto-save-file-name-transforms
      `((".*" ,my-auto-save-folder t)))

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

;; Thanks
;; http://www.jesshamrick.com/2013/03/31/macs-and-emacs/
;; for the my-system-is-x functions below.
(defun my-system-is-mac ()
  (interactive)
  (string-equal system-type "darwin"))

(defun my-system-is-linux ()
  (interactive)
  (string-equal system-type "gnu/linux"))

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

;; (defun my-spawn-emacs-in-terminal-here ()
;;   "" (interactive)
;;   (start-process "foot-emacsclient" nil "foot" "-e" "sh" "-c" "emacsclient -nw -c -a \"\""))


(defun my-set-window-font (font)
  "Set the frame font to FONT.
FONT is the name of a xft font, like `Monospace-10'."
  (interactive "sFont: ")
  ;; (set-face-attribute 'default nil :height 125 :family "Fira Mono"))
    (set-face-attribute 'default nil :height 120 :font font)
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

(add-hook 'after-make-frame-functions 'my-use-default-font)
(my-use-default-font)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x g") 'my-google)
(global-set-key (kbd "C-c e") 'my-eval-and-replace)

(defun my-setup-help-mode ()
  "Setup help mode the way I like it."
  (set-fill-column 80))

(add-hook 'help-mode-hook 'my-setup-help-mode)

(when (my-system-is-mac)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))


;; (global-linum-mode 1)
(defun my-enable-line-numbers ()
  "Enable line numbers in a smart way."
  (interactive)
  (unless (or (minibufferp)
              (member major-mode '(org-mode eshell-mode
                                            shell-mode term-mode
                                            vterm-mode eshell-mode)))
    (display-line-numbers-mode)))

(add-hook 'prog-mode-hook #'my-enable-line-numbers)
(add-hook 'text-mode-hook #'my-enable-line-numbers)

(defun my-home-path (&rest components)
  "Join COMPONENTS relative to `user-home-directory`, even if some start with '/'."
  (expand-file-name
   (string-join (seq-remove #'string-empty-p
                            (mapcar (lambda (s) (string-remove-prefix "/" s))
                                    components))
                "/")
   (getenv "HOME")))

;;(advice-add 'company-mode :override (lambda (&rest _) (message "❌ company-mode blocked")))

(provide 'my-core)
