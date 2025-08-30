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



;; Use a whitelist approach - only save these safe modes
(setq desktop-modes-not-to-save nil) ; Clear the default blacklist
(setq desktop-buffers-not-to-save "^$") ; Only exclude empty buffer names

;; Define safe modes that work well with desktop restoration
(setq desktop-modes-to-save
      '(text-mode
        org-mode
        markdown-mode
        emacs-lisp-mode
        python-mode
        js-mode
        css-mode
        html-mode
        prog-mode)) ; Add other programming modes you use

;; Override desktop's mode saving to use whitelist
(defun my-desktop-save-buffer-p (buffer-name buffer-mode)
  "Only save buffers with modes in our whitelist."
  (memq buffer-mode desktop-modes-to-save))

(setq desktop-save-buffer 'my-desktop-save-buffer-p)

;; Conservative settings
(setq desktop-dirname user-emacs-directory)
(setq desktop-load-locked-desktop t)
(setq desktop-restore-eager 3) ; Only restore first 3 buffers
(setq desktop-auto-save-timeout nil) ; Disable auto-save to prevent corruption

;; Enable desktop-save-mode for session persistence
(setq desktop-dirname user-emacs-directory)
(setq desktop-path (list desktop-dirname))
(setq desktop-load-locked-desktop t)
(setq desktop-auto-save-timeout 600) ; Auto-save every 10 minutes

;; Add error handling to prevent broken desktop files
(setq desktop-save 'ask)
(setq desktop-restore-eager 5) ; Only restore first 5 buffers immediately

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
