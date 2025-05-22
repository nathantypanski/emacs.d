;; my-keychain.el -*- lexical-binding: t; -*-
;;
;; Load GPG/SSH keys automatically with keychain.


(use-package keychain-environment
  :ensure keychain-environment
  :init
  (keychain-refresh-environment))


(provide 'my-keychain)
