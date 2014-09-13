;; my-projects.el
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
    (setq projectile-known-projects-file (concat user-emacs-directory ".cache/projectile-bookmarks.eld"))
    (add-to-list 'projectile-globally-ignored-directories "elpa")
    (add-to-list 'projectile-globally-ignored-directories ".cache")
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (projectile-global-mode 1)
    ;; automatically dired in projectile-switch-project
    (setq projectile-switch-project-action 'projectile-dired)
    (after 'evil-leader
      (evil-leader/set-key    "P"   'projectile-switch-project)
      (evil-leader/set-key  "cc"  'projectile-compile-project)
    )
    )
  )

(use-package project-explorer
  :ensure project-explorer
  :commands (progn project-explorer project-explorer-open pe/show-file)
  :config
  (progn
    (setq pe/omit-regex (concat pe/omit-regex "\\|^node_modules$"))
    (after 'project-explorer-autoloads
      (global-set-key [f2] 'project-explorer-open)
      (global-set-key [f3] 'pe/show-file)
    (after 'evil
      (evil-define-key 'normal project-explorer-mode-map
        (kbd "+") 'pe/create-file
        (kbd "-") 'pe/delete-file
        (kbd "d") 'pe/delete-file
        (kbd "u") 'pe/up-element
        (kbd "a") 'pe/goto-top
        (kbd "TAB") 'pe/tab
        (kbd "<backtab>") 'pe/backtab
        (kbd "M-}") 'pe/forward-element
        (kbd "M-{") 'pe/backward-element
        (kbd "]") 'pe/forward-element
        (kbd "[") 'pe/backward-element
        (kbd "n") 'next-line
        (kbd "p") 'previous-line
        (kbd "j") 'next-line
        (kbd "k") 'previous-line
        (kbd "l") 'forward-char
        (kbd "h") 'backward-char
        (kbd "RET") 'pe/return
        (kbd "<mouse-2>") 'pe/middle-click
        (kbd "<mouse-1>") 'pe/left-click
        (kbd "q") 'pe/quit
        (kbd "s") 'pe/change-directory
        (kbd "r") 'pe/rename-file
        (kbd "c") 'pe/copy-file
        (kbd "f") 'pe/find-file
        (kbd "w") 'pe/copy-file-name-as-kill
        (kbd "M-k") 'pe/ack-and-a-half
        (kbd "M-l") 'pe/set-filter-regex
        (kbd "M-o") 'pe/toggle-omit
        )
      )
    )
  ))


(use-package nav)
(use-package fiplr
  :ensure fiplr
  :config
  (progn
    (setq fiplr-ignored-globs '((directories (".git" ".svn"))
                                (files ("*.jpg" "*.png" "*.zip" "*~"))))
    (after 'evil
      (define-key evil-normal-state-map (kbd "C-p") 'fiplr-find-file)
      )
    )
  )

(provide 'my-projects)
