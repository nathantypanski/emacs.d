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
  :custom
  (sp-autoskip-closing-pair 'always)
  :config
  ;; Load default smartparens config with sane defaults
  (require 'smartparens-config)

  (show-paren-mode -1)            ;; conflict
  (show-paren-local-mode -1)      ;; conflict
  (show-smartparens-mode -1)         ;; alternative to show-paren-mode

  ;; (setq sp-autoinsert-pair t)
  ;; (setq sp-autoskip-closing-pair 'always)
  ;; (setq sp-cancel-autoskip-on-backward-movement t)
  ;; (setq sp-autoescape-string-quote t)
  ;; (setq sp-autoinsert-quote-if-followed-by-closing-pair t)

  ;; (setq sp-autodelete-pair t)
  ;; (setq sp-autodelete-wrap t)
  ;; (setq sp-autodelete-closing-pair t)
  ;; (setq sp-autodelete-opening-pair t)
  )


(provide 'my-smartparens)
