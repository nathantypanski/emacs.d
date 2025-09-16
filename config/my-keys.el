;; Which-key: discoverable keybindings -*- lexical-binding: t; -*-
(use-package which-key
  :ensure which-key
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
   "C-c m" 'my-show-messages-transient)
  )

(provide 'my-keys)
