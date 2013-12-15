
;; Load the packages
(require-package 'haskell-mode)
(require 'haskell-mode)

;; Modules
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(require 'haskell-mode-autoloads)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; Use GHCI instead of `inferior-haskell-mode`. From:
;; <http://haskell.github.io/haskell-mode/manual/latest/#haskell_002dinteractive_002dmode>
(eval-after-load "haskell-mode"
  '(progn
    (define-key haskell-mode-map (kbd "C-x C-d") nil)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
    (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c M-.") nil)
    (define-key haskell-mode-map (kbd "C-c C-d") nil)
    (define-key haskell-mode-map (kbd "C-c C-c") nil)
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)))

;; Compile bindings
(eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(eval-after-load "haskell-cabal"
    '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

; do proper indenting and stuff
; from <https://github.com/prooftechnique/.emacs.d/blob/6d08779adb8fe67acbe9ab82fe25e78a7fc40eb8/config/jhenahan-haskell.el>
(add-hook 'haskell-mode-hook
          (lambda ()
            (turn-on-haskell-doc-mode)
            (setq evil-auto-indent nil)
            (turn-on-haskell-indentation)
            (ghc-init)))

(provide 'init-haskell)
