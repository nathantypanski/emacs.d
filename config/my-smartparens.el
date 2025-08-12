;; -*- lexical-binding: t; -*-
;; Smartparens for structured editing of Lisp and other languages

(use-package smartparens
  :ensure t
  :hook ((prog-mode . smartparens-mode)
         (emacs-lisp-mode . smartparens-strict-mode)
         (lisp-mode . smartparens-strict-mode)
         (scheme-mode . smartparens-strict-mode)
         (clojure-mode . smartparens-strict-mode)
         (cider-repl-mode . smartparens-strict-mode))
  :config
  ;; Load default smartparens config with sane defaults
  (require 'smartparens-config)
  
  ;; Evil-collection integration (no manual setup needed)
  (after 'evil-collection
    (add-to-list 'evil-collection-mode-list 'smartparens)
    (evil-collection-init '(smartparens))))

(provide 'my-smartparens)
