;; my-nix.el -*- lexical-binding: t; -*-
;;
;; Settings for the Nix programming language.

(require 'man)
(when (eq Man-header-file-path t)
    (setq Man-header-file-path nil))

(use-package nix-haskell-mode
  :commands (nix-haskell-mode)
  :ensure nix-haskell-mode)

(use-package nix-mode
  :commands nix-mode
  :ensure nix-mode
  :mode "\\.nix\\'")

;; (use-package nix-repl
;;   :ensure nix-mode
;;   :commands (nix-repl))

(provide 'my-nix)
