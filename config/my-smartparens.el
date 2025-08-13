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

  ;; Basic Evil integration for smartparens
  (after 'evil
    ;; Make smartparens work well with Evil
    (sp-use-paredit-bindings)
    ;; Optional: Add some Evil-friendly bindings
    (define-key evil-normal-state-map (kbd "M-(") 'sp-wrap-round)
    (define-key evil-normal-state-map (kbd "M-[") 'sp-wrap-square)

(provide 'my-smartparens)
