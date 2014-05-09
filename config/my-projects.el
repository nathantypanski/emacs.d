;; my-projects.el
;;
;; Stuff related to maintaining and navigating around projects.

(use-package etags-select
  :ensure etags-select
  :init
  (setq etags-select-go-if-unambiguous t)
  )

(use-package projectile
  :ensure projectile
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-cache-file (concat user-emacs-directory ".cache/projectile.cache"))
    (setq projectile-known-projects-file (concat user-emacs-directory ".cache/projectile-bookmarks.eld"))
    (add-to-list 'projectile-globally-ignored-directories "elpa")
    (add-to-list 'projectile-globally-ignored-directories ".cache")
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (projectile-global-mode 1)
    )
  )

(use-package project-explorer
  :ensure project-explorer
  :commands (progn project-explorer project-explorer-open pe/show-file)
  :config
  (progn
    (setq pe/omit-regex (concat pe/omit-regex "\\|^node_modules$"))
    (after 'project-explorer-autoloads
      (after 'project-explorer
        (after 'evil
          (define-key project-explorer-mode-map (kbd "C-l") 'evil-window-right)))
      (global-set-key [f2] 'project-explorer-open)
      (global-set-key [f3] 'pe/show-file))
    )
  )

(use-package flycheck
  :ensure flycheck
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
    (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
    (global-flycheck-mode 1)
    (after 'evil (add-hook 'flycheck-error-list-mode-hook (lambda () (evil-mode 0))))
    )
  )

(provide 'my-projects)
