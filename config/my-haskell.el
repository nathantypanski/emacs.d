;; my-haskell.el
;;
;; Haskell packages, etc.

(use-package haskell-mode
  :ensure haskell-mode
  :commands haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :config
  (progn
    (setq haskell-process-show-debug-tips nil)
    (defun my-haskell-autoloads ()
      "Autoloads for entering Haskell-mode."
      (turn-on-haskell-doc-mode)
      (after 'evil
        (setq evil-auto-indent nil)
        )
      (turn-on-haskell-indentation))
    (add-hook 'haskell-mode-hook 'my-haskell-autoloads)

    (defun my-haskell-interactive-evil-insert ()
        "If the λ prompt is before point, enter insert state. Otherwise, insert after the prompt"
        (interactive)
        (if (> (haskell-interactive-mode-prompt-start) (point))
            (goto-char (haskell-interactive-mode-prompt-start))
            ())
        (evil-insert-state)
        )

    (evil-set-initial-state 'haskell-error-mode 'emacs)
    (evil-define-key 'normal haskell-interactive-mode-map (kbd "i")
      'my-haskell-interactive-evil-insert)
    (evil-define-key 'insert haskell-interactive-mode-map (kbd "RET")
      'haskell-interactive-mode-return)
    (after 'evil
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
