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

(use-package cedet
  :ensure cedet
  :config
  (progn
    (after 'evil
      (evil-define-key 'insert c-mode-map (kbd "TAB") 'c-indent-line-or-region)
      )
    (semantic-mode)
    ;; show semantic summary in minibuffer when I idle over a function
    (global-semantic-idle-summary-mode)
    ))

(after 'ac-etags
  ;; ac-etags setup for C code.
  ;; See ~/.emacs.d/config/my-autocomplete.el
  (defun my-c-mode-common-hook ()
    (add-to-list 'ac-sources 'ac-source-etags))
  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
  )

(provide 'my-c)
