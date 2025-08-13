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
          "build")))

(require 'desktop)
;; Enable desktop-save-mode for session persistence
(setq desktop-dirname user-emacs-directory)
(setq desktop-path (list desktop-dirname))
(setq desktop-load-locked-desktop t)
(setq desktop-auto-save-timeout 600) ; Auto-save every 10 minutes

;; Add error handling to prevent broken desktop files
(setq desktop-save 'ask-if-new)
(setq desktop-restore-eager 5) ; Only restore first 5 buffers immediately

;; Desktop mode is disabled - remove problematic save hook
(desktop-save-mode -1)

  ;; Don't save scratch and other special buffers
  (setq desktop-buffers-not-to-save
        (concat "\\("
                "^nn\\.a[0-9]+\\|^\\*.*\\*\\|^\\s-*$"
                "\\|\\*compilation\\*\\|\\*Completions\\*"
                "\\|\\*scratch\\*\\|\\*Messages\\*"
                ;; buffers starting and ending with a *
                "\\|\\*\\*.*\\*\\)"))
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  (after 'gptel (add-to-list 'desktop-modes-not-to-save 'gptel-mode))

(provide 'my-projects)
