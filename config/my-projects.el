;; my-projects.el
;;
;; Stuff related to maintaining and navigating around projects.

;;(use-package etags-select
;;  :ensure etags-select
;;  :init
;;  (setq etags-select-go-if-unambiguous t)
;;  )

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
  :init
  (progn
        (add-hook 'after-init-hook #'global-flycheck-mode))
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
    (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))

    (defun my-flycheck-list-errors ()
      "Jump to flycheck errors and switch to the errorlist buffer"
      (interactive)
      (flycheck-list-errors)
      (switch-to-buffer-other-window "*Flycheck errors*" t)
      )

    (after 'evil
      (evil-define-key 'normal flycheck-error-list-mode-map "k" #'flycheck-error-list-previous-error)
      (evil-define-key 'normal flycheck-error-list-mode-map "j" #'flycheck-error-list-next-error)
      (evil-define-key 'normal flycheck-error-list-mode-map "K" #'evil-previous-line)
      (evil-define-key 'normal flycheck-error-list-mode-map "J" #'evil-next-line)
      (evil-define-key 'normal flycheck-error-list-mode-map (kbd "RET") #'flycheck-error-list-goto-error)
      (evil-define-key 'normal flycheck-error-list-mode-map "q" 'quit-window)

      )
    )
  )

(use-package ctags-update
  :ensure ctags-update
  :init
  (progn
    (add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
    (add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode)
    (autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)
    ))

(use-package nav)
(use-package fiplr
  :ensure fiplr
  :config
  (progn
    (setq fiplr-ignored-globs '((directories (".git" ".svn"))
                                (files ("*.jpg" "*.png" "*.zip" "*~"))))
    )
  )

(provide 'my-projects)
