;; -*- lexical-binding: t; -*-
;; Evil-friendly delimiter pairing using electric-pair-mode
;;
;; This configuration uses electric-pair-mode exclusively for all languages,
;; replacing smartparens which doesn't work well with Evil mode's paradigm
;; of manual delimiter insertion in normal mode.

;; Use built-in electric-pair-mode for all modes (works great with Evil)
(use-package electric
  :straight (:type built-in)
  :hook ((ruby-mode . electric-pair-local-mode)
         (ruby-ts-mode . electric-pair-local-mode)
         (python-mode . electric-pair-local-mode)
         (python-ts-mode . electric-pair-local-mode)
         (go-mode . electric-pair-local-mode)
         (rust-mode . electric-pair-local-mode)
         (js-mode . electric-pair-local-mode)
         (typescript-mode . electric-pair-local-mode)
         (nix-mode . electric-pair-local-mode)
         (emacs-lisp-mode . electric-pair-local-mode)
         (lisp-mode . electric-pair-local-mode)
         (scheme-mode . electric-pair-local-mode)
         (clojure-mode . electric-pair-local-mode)
         (cider-repl-mode . electric-pair-local-mode))
  :config
  ;; Show matching parens
  (show-paren-mode 1)
  (setq show-paren-delay 0.1)
  (setq show-paren-style 'parenthesis))

(provide 'my-parens)
