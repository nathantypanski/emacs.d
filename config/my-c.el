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
(provide 'my-c)
