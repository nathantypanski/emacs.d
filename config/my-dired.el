(put 'dired-find-alternate-file 'disabled nil)

(use-package saveplace
  :config
  (progn
    (setq save-place-file (concat user-emacs-directory ".cache/places"))
    (setq-default save-place t)
    )
  )

(use-package savehist
  :idle
  :config
    (progn
        (setq savehist-file (concat user-emacs-directory ".cache/savehist")
            savehist-additional-variables '(search ring regexp-search-ring)
            savehist-autosave-interval 60)
        (savehist-mode t)
    )
)

(use-package recentf
  :config
  (progn
    (setq recentf-save-file (concat user-emacs-directory ".cache/recentf")
          recentf-max-saved-items 1000
          recentf-max-menu-items 500)
    (recentf-mode +1)
    )
)

(require 'dired-x)
(use-package dired-x
  :commands dired
  :init
  (progn
    (defun my-load-dired-x ()
      "Load dired-x; for use on dired-load-hook"
        (load "dired-x")
        ;; Set dired-x global variables here.  For example:
        ;; (setq dired-guess-shell-gnutar "gtar")
        ;; (setq dired-x-hands-off-my-keys nil)
        )
      )
    )

(setq dired-listing-switches "-aBhl  --group-directories-first")

(defun my-dired-up-directory ()
  "Take dired up one directory, but behave like dired-find-alternate-file"
  (interactive)
  (let ((old (current-buffer)))
    (dired-up-directory)
    (kill-buffer old)
    ))

(defun my-dired-next-line (count)
  "Move to next line, always staying on the dired filename."
  (interactive "p")
  (dired-next-line count)
  (dired-move-to-filename)
  )

(defun my-dired-previous-line (count)
  "Move to previous line, always staying on the dired filename."
  (interactive "p")
  (dired-previous-line count)
  (dired-move-to-filename)
  )

(after 'evil
  (evil-define-key 'normal dired-mode-map "h" 'my-dired-up-directory)
  (evil-define-key 'normal dired-mode-map "l" 'dired-find-alternate-file)
  (evil-define-key 'normal dired-mode-map "a" 'ag-dired)
  (evil-define-key 'normal dired-mode-map "o" 'dired-sort-toggle-or-edit)
  (evil-define-key 'normal dired-mode-map "v" 'dired-toggle-marks)
  (evil-define-key 'normal dired-mode-map "m" 'dired-mark)
  (evil-define-key 'normal dired-mode-map "u" 'dired-unmark)
  (evil-define-key 'normal dired-mode-map "U" 'dired-unmark-all-marks)
  (evil-define-key 'normal dired-mode-map "c" 'dired-create-directory)
  (evil-define-key 'normal dired-mode-map "q" 'kill-this-buffer)
  (evil-define-key 'normal dired-mode-map "/" 'evil-search-forward)
  (evil-define-key 'normal dired-mode-map "n" 'evil-search-next)
  (evil-define-key 'normal dired-mode-map "N" 'evil-search-previous)
  (evil-define-key 'normal dired-mode-map "j" 'my-dired-next-line)
  (evil-define-key 'normal dired-mode-map "k" 'my-dired-previous-line)
  )

(provide 'my-dired)
