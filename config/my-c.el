(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))
(defun my-set-evil-shift-width ()
  "Set Evil's shift width for C editing."
  (after 'evil
    (setq evil-shift-width 8)
    )
  )

(add-hook 'c-initialization-hook 'my-set-evil-shift-width)

(c-set-offset 'case-label '+)

(use-package semantic
  :ensure semantic
  :init
  (progn
    (global-semanticdb-minor-mode 1)
    )
)
(use-package cedet
  :ensure cedet
  :config
  (progn
    (after 'evil
      (evil-define-key 'insert c-mode-map (kbd "TAB") 'c-indent-line-or-region)
      )
    (semantic-mode)
))



(provide 'my-c)
