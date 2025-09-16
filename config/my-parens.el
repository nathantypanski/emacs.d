;; -*- lexical-binding: t; -*-
;; Modern pairing solution: electric-pair-mode (Evil-friendly)

;; Use built-in electric-pair-mode for most modes (works great with Evil)
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
         (nix-mode . electric-pair-local-mode))
  :config
  ;; Show matching parens
  (show-paren-mode 1)
  (setq show-paren-delay 0.1)
  (setq show-paren-style 'parenthesis)

  ;; Ruby-specific electric-pair configuration
  (defun my-ruby-electric-setup ()
    "Configure electric-pair-mode for Ruby - less aggressive for symbols."
    ;; Don't pair single quotes (used for symbols like :foo)
    ;; Be smarter about when to pair braces
    (setq-local electric-pair-inhibit-predicate
                (lambda (c)
                  (cond
                   ;; Don't pair single quotes in Ruby (symbols)
                   ((char-equal c ?\') t)
                   ;; Use conservative pairing for other cases
                   (t (electric-pair-conservative-inhibit c))))))

  (add-hook 'ruby-mode-hook #'my-ruby-electric-setup)
  (add-hook 'ruby-ts-mode-hook #'my-ruby-electric-setup))

;; Keep smartparens ONLY for Lisp modes where it's genuinely better
(use-package smartparens
  :ensure t
  :hook ((emacs-lisp-mode . smartparens-strict-mode)
         (lisp-mode . smartparens-strict-mode)
         (scheme-mode . smartparens-strict-mode)
         (clojure-mode . smartparens-strict-mode)
         (cider-repl-mode . smartparens-strict-mode))
  :config
  ;; Only load minimal smartparens, not the problematic language configs
  (require 'smartparens)
  ;; Disable show-smartparens to avoid conflicts with show-paren-mode
  (show-smartparens-mode -1))

(provide 'my-parens)