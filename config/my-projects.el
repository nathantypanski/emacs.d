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
