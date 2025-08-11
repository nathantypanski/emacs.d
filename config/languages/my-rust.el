;; -*- lexical-binding: t; -*-
(use-package rust-mode
  :ensure rust-mode
  :commands rust-mode
  :config
  (progn
    (defun my-rust-electric-rbrace (&optional _)
      "Insert a rbrace, and then indent the line properly"
      (interactive "*P")
      (insert "}")
      (rust-mode-indent-line)
      )
    (after 'evil
      (defvar rust-mode-map () "Keymap used in Rust mode.")
      (defun my-rust-setup ()
        "Make rust do things the way I like it."
        (interactive)
        (setq tab-width 4)
        (evil-define-key 'insert rust-mode-map "}" 'my-rust-electric-rbrace)
        (use-local-map rust-mode-map)
        (flycheck-mode 0))
      (add-hook 'rust-mode-hook 'my-rust-setup))))

(provide 'my-rust)
