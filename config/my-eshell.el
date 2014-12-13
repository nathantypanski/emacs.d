;; my-eshell.el
;;
;; Configure Eshell nicely for Evil.

(after 'evil

  (defun my-eshell-hop-to-bottom ()
    "Go to the end of the buffer and eshell prompt."
    (interactive)
    (evil-goto-line)
    (eshell-bol))

  (defun my-is-eshell-before-prompt ()
    "Non-nil when point is before the eshell prompt."
    (let ((oldpt (point))
          (eol   (line-end-position)))
      (goto-char (line-beginning-position))
      (if (and (looking-at eshell-prompt-regexp)
               (<= (match-end 0) eol)
               (>  (match-end 0) oldpt))
          1
        (progn
          (goto-char oldpt)
          ;; Silence the return value of goto-char.
          ;; Is there a better way of doing this?
          nil)
        )))

  (defun my-is-eshell-above-last-prompt ()
    "Non-nil when eshell is above the very last prompt."
    (interactive)
    (or (> eshell-last-output-end (point))))

  (defun my-eshell-evil-insert ()
    "If the eshell prompt is before point, enter insert state.

Otherwise, insert after the prompt"
    (interactive)
    (if (or (my-is-eshell-before-prompt) (my-is-eshell-above-last-prompt))
          (my-eshell-hop-to-bottom))
    (evil-insert-state))

  (defun my-eshell-evil-append (count &optional vcount skip-empty-lines)
    "Perform an evil-append if point is after the last prompt.

Otherwise, jump to the prompt in insert state."
    (interactive
     (list (prefix-numeric-value current-prefix-arg)
           (and (evil-visual-state-p)
                (memq (evil-visual-type) '(line block))
                (save-excursion
                  ;; go to upper-left corner temporarily so
                  ;; `count-lines' yields accurate results
                  (evil-visual-rotate 'upper-left)
                  (count-lines evil-visual-beginning evil-visual-end)))))
    (if (or (my-is-eshell-before-prompt) (my-is-eshell-above-last-prompt))
        (progn
          (my-eshell-hop-to-bottom)
          (evil-insert-state))
      (evil-append count vcount skip-empty-lines)))

  (evil-set-initial-state 'eshell-mode 'insert)

  (defun my-setup-eshell ()
    "Set the eshell bindings and extra triggers.

This is done every time eshell is started, since for some reason doing it
globally breaks Evil's keybindings."
    (evil-define-key 'normal eshell-mode-map (kbd "RET") 'eshell-send-input)
    (evil-define-key 'insert eshell-mode-map (kbd "RET") 'eshell-send-input)
    (evil-define-key 'normal eshell-mode-map (kbd "i") 'my-eshell-evil-insert)
    (evil-define-key 'visual eshell-mode-map (kbd "i") 'my-eshell-evil-insert)
    (evil-define-key 'normal eshell-mode-map (kbd "a") 'my-eshell-evil-append)
    (evil-define-key 'visual eshell-mode-map (kbd "a") 'my-eshell-evil-append)
    (evil-define-key 'normal eshell-mode-map (kbd "G") 'my-eshell-hop-to-bottom)
    (evil-define-key 'normal eshell-mode-map (kbd "G") 'my-eshell-hop-to-bottom))

  (add-hook 'eshell-mode-hook 'my-setup-eshell))

(provide 'my-eshell)
