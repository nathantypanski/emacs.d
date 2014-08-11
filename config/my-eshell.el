(after 'evil
  (defun my-eshell-evil-insert ()
    "If the eshell prompt is before point, enter insert state. Otherwise, insert after the prompt"
    (interactive)
    (if (> (comint-line-beginning-position) (point))
        (eshell-bol)
        ())
    (evil-insert-state)
    )

  (defun my-eshell-evil-append (count &optional vcount skip-empty-lines)
  "Switch to Insert state just after point.
    The insertion will be repeated COUNT times and repeated once for
    the next VCOUNT - 1 lines starting at the same column.  If
    SKIP-EMPTY-LINES is non-nil, the insertion will not be performed
    on lines on which the insertion point would be after the end of
    the lines."
    "If the comint prompt is before point, just do evil-append. Otherwise, insert after the prompt"
    (interactive
    (list (prefix-numeric-value current-prefix-arg)
          (and (evil-visual-state-p)
               (memq (evil-visual-type) '(line block))
               (save-excursion
                 ;; go to upper-left corner temporarily so
                 ;; `count-lines' yields accurate results
                 (evil-visual-rotate 'upper-left)
                 (count-lines evil-visual-beginning evil-visual-end)))))
    (if (> (eshell-line-beginning-position) (point))
        (progn
          (eshell-bol)
          (evil-insert-state))
        (evil-append count vcount skip-empty-lines)))

  (evil-set-initial-state 'eshell-mode 'insert)

  (evil-define-key 'normal eshell-mode-map (kbd "RET") 'eshell-send-input)
  (evil-define-key 'insert eshell-mode-map (kbd "RET") 'eshell-send-input)
  (evil-define-key 'normal eshell-mode-map (kbd "i") 'my-eshell-evil-insert)
  (evil-define-key 'visual eshell-mode-map (kbd "i") 'my-eshell-evil-insert)
  (evil-define-key 'normal eshell-mode-map (kbd "a") 'my-eshell-evil-append)
  (evil-define-key 'visual eshell-mode-map (kbd "a") 'my-eshell-evil-append)
  )

(provide 'my-eshell)
