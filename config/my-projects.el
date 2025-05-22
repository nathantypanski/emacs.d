;; my-projects.el -*- lexical-binding: t; -*-
;;
;; Stuff related to maintaining and navigating around projects.
;; projectile, etc.

;;(use-package etags-select
;;  :ensure etags-select
;;  :init
;;  (setq etags-select-go-if-unambiguous t)
;;  )

(use-package projectile
  :ensure projectile
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-cache-file (concat user-emacs-directory ".cache/projectile.cache"))
    (setq projectile-known-projects-file (concat user-emacs-directory "projectile-bookmarks.eld"))
    (add-to-list 'projectile-globally-ignored-directories "elpa")
    (add-to-list 'projectile-globally-ignored-directories ".cache")
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (projectile-global-mode 1)
    ;; automatically dired in projectile-switch-project
    (setq projectile-switch-project-action 'projectile-dired)
    (setq projectile-completion-system 'default)
    (setq projectile-enable-caching t)
    ;; use ripgrep/fd if available
    (setq projectile-indexing-method 'alien)
    (setq projectile-globally-ignored-directories
          '(".idea"
            ".eunit"
            ".git"
            ".hg"
            ".fslckout"
            ".bzr"
            "_darcs"
            ".tox"
            ".svn"
            "build"))))

(provide 'my-projects)
