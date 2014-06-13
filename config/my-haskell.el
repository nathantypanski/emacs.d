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
    (evil-define-key 'normal haskell-mode-map (kbd "C-x C-d") nil)
    (evil-define-key 'normal haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (evil-define-key 'normal haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
    (evil-define-key 'normal haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
    (evil-define-key 'normal haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
    (evil-define-key 'normal haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
    (evil-define-key 'normal haskell-mode-map (kbd "C-c M-.") nil)
    (evil-define-key 'normal haskell-mode-map (kbd "C-c C-d") nil))
  )

(provide 'my-haskell)
