;; my-haskell.el
;;
;; Haskell packages, etc.

(use-package haskell-mode
  :ensure haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :config
  (progn
    (define-key haskell-mode-map (kbd "C-x C-d") nil)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
    (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c M-.") nil)
    (define-key haskell-mode-map (kbd "C-c C-d") nil)
    (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
    (add-hook 'haskell-mode-hook
              (lambda ()
                (turn-on-haskell-doc-mode)
                (after 'evil
                  (setq evil-auto-indent nil))
                (turn-on-haskell-indentation)
                (ghc-init)
                (purty-mode)
                )
              )
    (require 'haskell-mode-autoloads)
    ;;(use-package inf-haskell)
    ;;(use-package haskell-cabal
    ;;  :init
    ;;  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile)
    ;;  )
    )
  )

(provide 'my-haskell)
