;; Which-key: discoverable keybindings -*- lexical-binding: t; -*-
(use-package which-key
  :ensure which-key
  :custom
  (which-key-idle-delay 0.5)
  (which-key-side-window-location 'top)
  (which-key-side-window-max-height 0.2)
  (which-key-popup-type 'side-window)
  (which-key-allow-evil-operators t)
  (which-key-show-prefix 'top)
  :config (which-key-mode))

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
   "C-c d" 'my-show-eldoc-transient)
  )

(provide 'my-keys)
