;; -*- lexical-binding: t; -*-

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra")
      ispell-alternate-dictionary (expand-file-name "~/.nix-profile/lib/aspell/en-common.rws"))

(provide 'my-spelling)
