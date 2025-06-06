;; -*- lexical-binding: t; -*-
(after 'evil
  (defun my-comint-evil-insert ()
    "If the comint prompt is before point, enter insert state. Otherwise, insert after the prompt"
    (interactive)
    (if (> (comint-line-beginning-position) (point))
        (comint-bol)
        ())
    (evil-insert-state)
    )
  (defun my-comint-evil-append (count &optional vcount skip-empty-lines)
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
    (if (> (comint-line-beginning-position) (point))
        (progn
          (comint-bol)
          (evil-insert-state))
        (evil-append count vcount skip-empty-lines)))

  (evil-define-motion my-comint-beginning-of-line ()
    "Move the cursor to the beginning of the current line, but before the prompt."
    :type exclusive
    (comint-bol))

  (defun my-comint-next-input (n)
    "Get the next matching input and move to the end of line"
    (interactive "*p")
    (comint-bol)
    (comint-previous-input (- n))
    (evil-end-of-line)
    (forward-char 1)
    )

  (defun my-comint-previous-input (n)
    "Get the previous matching input and move to the end of line"
    (interactive "*p")
    (comint-bol)
    (comint-previous-input n)
    (evil-end-of-line)
    (evil-forward-char 1))

  (add-hook 'R-mode-hook 'ansi-color-for-comint-mode-on)
  (setq comint-prompt-read-only t)
  (evil-set-initial-state 'comint-mode 'normal)
  (evil-define-key 'insert comint-mode-map (kbd "RET") 'comint-send-input)
  (evil-define-key 'normal comint-mode-map "i" 'my-comint-evil-insert)
  (evil-define-key 'visual comint-mode-map "i" 'my-comint-evil-insert)
  (evil-define-key 'normal comint-mode-map "a" 'my-comint-evil-append)
  (evil-define-key 'visual comint-mode-map "a" 'my-comint-evil-append)
  (evil-define-key 'motion comint-mode-map "0" 'my-comint-beginning-of-line)
  (evil-define-key 'normal comint-mode-map (kbd "<up>") 'my-comint-next-input)
  (evil-define-key 'normal comint-mode-map (kbd "<down>") 'my-comint-previous-input)
  (evil-define-key 'insert comint-mode-map (kbd "<up>") 'my-comint-next-input)
  (evil-define-key 'insert comint-mode-map (kbd "<down>") 'my-comint-previous-input)
  (evil-define-key 'normal comint-mode-map "\e\C-l" 'comint-show-output)
  (evil-define-key 'normal comint-mode-map "\C-m" 'comint-send-input)
  (evil-define-key 'normal comint-mode-map "\C-d" 'comint-delchar-or-maybe-eof)
  (evil-define-key 'normal comint-mode-map [delete]    'delete-forward-char)
  (evil-define-key 'normal comint-mode-map (kbd "S-<RET>") 'comint-accumulate)
  (evil-define-key 'normal comint-mode-map "\C-c\C-x"    'comint-get-next-from-history)
  (evil-define-key 'normal comint-mode-map "\C-c\C-a"    'comint-bol-or-process-mark)
  (evil-define-key 'normal comint-mode-map "\C-c\C-u"    'comint-kill-input)
  (evil-define-key 'normal comint-mode-map "\C-c\C-w"    'backward-kill-word)
  (evil-define-key 'normal comint-mode-map "\C-c\C-c"    'comint-interrupt-subjob)
  (evil-define-key 'normal comint-mode-map "\C-c\C-z"    'comint-stop-subjob)
  (evil-define-key 'normal comint-mode-map "\C-c\C-\\"   'comint-quit-subjob)
  (evil-define-key 'normal comint-mode-map "\C-c\C-m"    'comint-copy-old-input)
  (evil-define-key 'normal comint-mode-map "\C-c\C-o"    'comint-delete-output)
  (evil-define-key 'normal comint-mode-map "\C-c\C-r"    'comint-show-output)
  (evil-define-key 'normal comint-mode-map "\C-c\C-e"    'comint-show-maximum-output)
  (evil-define-key 'normal comint-mode-map "\C-c\C-l"    'comint-dynamic-list-input-ring)
  (evil-define-key 'normal comint-mode-map "\C-c\C-n"    'comint-next-prompt)
  (evil-define-key 'normal comint-mode-map "\C-c\C-p"    'comint-previous-prompt)
  (evil-define-key 'normal comint-mode-map "\C-c\C-d"    'comint-send-eof)
  (evil-define-key 'normal comint-mode-map "\C-c\C-s"    'comint-write-output)
  (evil-define-key 'normal comint-mode-map "\C-c."    'comint-insert-previous-argument)
  ;; Mouse Buttons:
  (evil-define-key 'normal comint-mode-map [mouse-2]     'comint-insert-input))

(provide 'my-comint)
