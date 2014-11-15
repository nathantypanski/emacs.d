;; my-markdown.el
;;
;; Settings for editing Markdown files.

(use-package markdown-mode
  :commands markdown-mode
  :ensure markdown-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.page\\'" . markdown-mode))
    (add-hook 'markdown-mode-hook 'visual-line-mode)
    )
  )

(provide 'my-markdown)
