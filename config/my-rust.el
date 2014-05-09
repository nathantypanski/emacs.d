(add-to-list 'load-path "~/devel/rust/rust/etc/emacs")

(use-package rust-mode
  :commands rust-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))
  :config
    (add-hook 'rust-mode-hook (lambda ()
    setq tab-width 4)))

(provide 'my-rust)
