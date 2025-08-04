;; my-ledger.el  -*- lexical-binding:t; -*-
;;
;; ledger-cli emacs mode + other finance nonsense

(use-package ledger-mode
  :ensure ledger-mode
  :config
  :mode "\\.ledger\\'")

(use-package beancount
  :ensure beancount
  :config
  :mode "\\.beancount\\'")


(provide 'my-finances)
