;; my-ledger.el  -*- lexical-binding:t; -*-
;;
;; ledger-cli emacs mode + other finance nonsense

(use-package ledger-mode
  :ensure ledger-mode
  :config
  :mode "\\.ledger\\'")

(provide 'my-finances)
