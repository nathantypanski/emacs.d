;; my-nix.el -*- lexical-binding: t; -*-
;;
;; Settings for the Nix programming language.

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

;; (use-package nix-shell
;;   :ensure nix-mode
;;   :commands (nix-shell-unpack nix-shell-configure nix-shell-build))

(provide 'my-nix)
