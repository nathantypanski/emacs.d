;; my-nix.el
;;
;; Settings for the Nix programming language.

(use-package nix-haskell-mode
  :commands (nix-haskell-mode)
  :ensure nix-haskell-mode)

(use-package nix-mode
  :commands nix-mode
  :ensure nix-mode
  :mode "\\.nix\\'")

(provide 'my-nix)
