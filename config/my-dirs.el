(use-package saveplace
  :config
  (progn
    (setq save-place-file (concat user-emacs-directory ".cache/places"))
    (setq-default save-place t)
    )
  )

(use-package savehist
  :config
    (progn
        (setq savehist-file (concat user-emacs-directory ".cache/savehist")
            savehist-additional-variables '(search ring regexp-search-ring)
            savehist-autosave-interval 60)
        (savehist-mode +1)
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
  :init
  (progn
    (add-hook 'dired-load-hook
              (lambda ()
                (load "dired-x")
                ;; Set dired-x global variables here.  For example:
                ;; (setq dired-guess-shell-gnutar "gtar")
                ;; (setq dired-x-hands-off-my-keys nil)
                ))
    (add-hook 'dired-mode-hook
              (lambda ()
                ;; Set dired-x buffer-local variables here.  For example:
                ;; (diredbomit-mode 1)
                )
              )
    )
  )

(provide 'my-dirs)
