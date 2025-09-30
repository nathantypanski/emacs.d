;; Which-key: discoverable keybindings -*- lexical-binding: t; -*-
(use-package which-key
  :ensure which-key
  :custom
  (which-key-idle-delay 0.4)
  (which-key-side-window-location 'top)
  (which-key-side-window-max-height 0.3)
  (which-key-min-display-lines 1)
  (which-key-popup-type (if (display-graphic-p) 'side-window 'minibuffer))
  (which-key-allow-evil-operators t)
  (which-key-show-prefix 'top)
  ;; Pretty appearance
  (which-key-separator " → ")
  (which-key-prefix-prefix "◉ ")
  (which-key-add-column-padding 2)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-show-remaining-keys t)
  (setq which-key-max-display-columns 5)
  :config
  (which-key-mode)
  ;; Replace common keys with prettier symbols (conditional on display type)
  (if (display-graphic-p)
      ;; GUI mode - use simpler symbols that work with more fonts
      (progn
        (push '((nil . "RET") . (nil . "RET")) which-key-replacement-alist)
        (push '((nil . "TAB") . (nil . "TAB")) which-key-replacement-alist)
        (push '((nil . "SPC") . (nil . "SPC")) which-key-replacement-alist)
        (push '((nil . "DEL") . (nil . "DEL")) which-key-replacement-alist)
        (push '((nil . "ESC") . (nil . "ESC")) which-key-replacement-alist))
    ;; Terminal mode - use Unicode symbols that work well in terminals
    (progn
      (push '((nil . "RET") . (nil . "↵")) which-key-replacement-alist)
      (push '((nil . "TAB") . (nil . "↹")) which-key-replacement-alist)
      (push '((nil . "SPC") . (nil . "⎵")) which-key-replacement-alist)
      (push '((nil . "DEL") . (nil . "⌫")) which-key-replacement-alist)
      (push '((nil . "ESC") . (nil . "⎋")) which-key-replacement-alist))

    ;; Disable Ctrl+mouse wheel font scaling
    (global-unset-key (kbd "C-<wheel-up>"))
    (global-unset-key (kbd "C-<wheel-down>"))
    (global-unset-key (kbd "C-<mouse-4>"))
    (global-unset-key (kbd "C-<mouse-5>")))

  ;; Leader key descriptions (comma as leader) - conditional on display type
  (which-key-add-key-based-replacements
    "," "leader"
    ",a" "AI/GPT"
    ",ai" "claude-ide"
    ",g" "magit"
    ",p" "project"
    ",q" "quit"
    ",s" "search"
    ",t" "tabs"
    ",o" "org"
    ",c" "code"
    ",j" "jump"))

(use-package key-chord
  :ensure key-chord
  :config
  (progn
    (key-chord-mode 1)))

;; Keep Messages buffer visible
(defun my-show-messages-buffer ()
  "Display *Messages* buffer in a side window."
  (interactive)
  (display-buffer-in-side-window
   (get-buffer-create "*Messages*")
   '((side . bottom) (window-height . 0.25))))

(defun my-show-messages-transient ()
  "Show *Messages* buffer temporarily."
  (interactive)
  (let ((win (display-buffer-in-side-window
              (get-buffer "*Messages*")
              '((side . bottom) (window-height . 0.4))))
        (map (make-sparse-keymap)))
    ;; Any key closes it
    (define-key map [t] (lambda () (interactive) (delete-window win)))
    (set-transient-map map t)))

(use-package general
  :ensure t
  :demand t
  :config
  (general-define-key
   :override nil
   "C-c m" 'my-show-messages-transient
   "C-c d" 'describe-symbol               ; Full documentation with descriptions
   "M-`"   'tmm-menubar
   "C-c M" 'my-show-messages-buffer)


  ;; Set up keybindings for dired
  (after 'general
    (general-define-key
     :keymap dired-mode-map
     :states '(normal)
     "j" 'my-dired-next-line
     "k" 'my-dired-prev-line))
  (message "loaded general.el"))

  ;; Clean undo system - no external packages needed

(defun my-configure-gestures ()
  "Setup mouse gestures.

- Disable text-scale-pinch (too easy to trigger by accident)."
  (interactive)

  ;; undefine <pinch>
  (global-unset-key (kbd "<pinch>"))
  (global-unset-key [pinch]))

(my-configure-gestures)

(provide 'my-keys)
