;; my-haskell.el
;;
;; Haskell packages, etc.

(use-package haskell-mode
  :ensure haskell-mode
  :commands haskell-mode
  :config
  (progn
    (setq haskell-process-show-debug-tips nil)
    (setq haskell-process-type 'cabal-repl)
    (defun my-haskell-autoloads ()
      "Autoloads for entering Haskell-mode."
      (turn-on-haskell-doc-mode)
      (after 'evil
        (setq evil-auto-indent nil)
        )
      (turn-on-haskell-indentation))
    (add-hook 'haskell-mode-hook 'my-haskell-autoloads)

    (defun my-haskell-interactive-evil-bol ()
      "If λ prompt is before point, go to bol. Otherwise, go to λ."
      (interactive)
      (if (> haskell-interactive-mode-prompt-start (point))
          (evil-beginning-of-line)
        (my-haskell-interactive-jump-to-prompt)))

    (defun my-haskell-interactive-jump-to-prompt ()
      "Go to the prompt."
      (goto-char haskell-interactive-mode-prompt-start))

    (defun my-haskell-interactive-maybe-jump-to-prompt ()
      "Go to the prompt if it's after POINT. Otherwise do nothing."
        (if (> haskell-interactive-mode-prompt-start (point))
            (my-haskell-interactive-jump-to-prompt)))

    (defun my-haskell-interactive-evil-insert ()
      "If the λ prompt is before point, enter insert state. Otherwise, insert after the prompt"
      (interactive)
      (my-haskell-interactive-maybe-jump-to-prompt)
      (evil-insert-state))

    (defun my-haskell-interactive-evil-append (count &optional vcount skip-empty-lines)
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
      (if (> haskell-interactive-mode-prompt-start (point))
          (my-haskell-interactive-jump-to-prompt))
      (evil-append count vcount skip-empty-lines))

    (defun my-haskell-interactive-evil-append-line (count &optional vcount)
      "Go to the end of the prompt if before it.

       Otherwise, behave like a normal Evil append line."
      (interactive "p")
      (if (> haskell-interactive-mode-prompt-start (point))
          (progn
            (goto-char (point-max))
            (evil-insert-state))
          (evil-append-line count vcount)))

    (defun my-haskell-interactive-history-previous (arg)
      "Go to the prompt if we're before it. Then cycle through previous history."
      (interactive "*p")
      (my-haskell-interactive-maybe-jump-to-prompt)
      (haskell-interactive-mode-history-previous arg))

    (defun my-haskell-interactive-history-next (arg)
      "Go to the prompt if we're before it. Then cycle through next history."
      (interactive "*p")
      (my-haskell-interactive-maybe-jump-to-prompt)
      (haskell-interactive-mode-history-next arg))

    (after 'evil
      (evil-set-initial-state 'haskell-error-mode 'emacs)
      (evil-define-key 'insert haskell-interactive-mode-map
        (kbd "<up>")          'my-haskell-interactive-history-previous
        (kbd "<down>")        'my-haskell-interactive-history-next
        (kbd "RET")           'haskell-interactive-mode-return
        (kbd "TAB")           'company-complete)
      (evil-define-key 'normal haskell-interactive-mode-map
        (kbd "<up>")          'my-haskell-interactive-history-previous
        (kbd "<down>")        'my-haskell-interactive-history-next
        (kbd "0")             'my-haskell-interactive-evil-bol
        (kbd "A")             'my-haskell-interactive-evil-append-line
        (kbd "a")             'my-haskell-interactive-evil-append
        (kbd "i")             'my-haskell-interactive-evil-insert
        (kbd "RET")           'haskell-interactive-mode-return)
      (evil-define-key 'normal haskell-mode-map (kbd "C-x C-d") nil)
      (evil-define-key 'normal haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
      (evil-define-key 'normal haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
      (evil-define-key 'normal haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
      (evil-define-key 'normal haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
      (evil-define-key 'normal haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
      (evil-define-key 'normal haskell-mode-map (kbd "C-c M-.") nil)
      (evil-define-key 'normal haskell-mode-map (kbd "C-c C-d") nil))
    )
  )

(provide 'my-haskell)
