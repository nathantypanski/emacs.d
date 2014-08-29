(use-package ggtags
  :ensure ggtags
  :init
  (progn
    (defun my-setup-ggtags ()
      "Configure the modes I want to use ggtags."
        (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1)))
    (add-hook 'c-mode-common-hook 'my-setup-ggtags)
    (after 'evil-leader
      ;; (evil-leader/set-key (kbd ".") 'ggtags-find-tag-dwim)
    )
   )
  )

(provide 'my-gtags)
