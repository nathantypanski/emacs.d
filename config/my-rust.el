(add-to-list 'load-path "~/devel/rust/rust/etc/emacs")

(use-package rust-mode
  :commands rust-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))
  :config
  (progn
    (defun my-rust-electric-rbrace (arg)
      "Insert a rbrace, and then indent the line properly"
      (interactive "*P")
      (insert "}")
      (rust-mode-indent-line)
      )

    (defvar rust-mode-map () "Keymap used in Rust mode.")
    (if rust-mode-map nil
      (setq rust-mode-map (make-sparse-keymap))
      (define-key rust-mode-map "}" 'my-rust-electric-rbrace)
      (evil-define-key 'insert rust-mode-map "}" 'my-rust-electric-rbrace)
    )
    (defun my-rust-setup ()
      "Make rust do things the way I like it."
      (interactive)
      (setq tab-width 4)
      (use-local-map rust-mode-map))

    (add-hook 'rust-mode-hook 'my-rust-setup)
    )
  )

(provide 'my-rust)
