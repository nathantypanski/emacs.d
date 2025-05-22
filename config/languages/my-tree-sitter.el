;; -*- lexical-binding: t; -*-
(use-package tree-sitter
  :commands tree-sitter
  :ensure tree-sitter
  :config
  (progn
    (global-tree-sitter-mode)))

(use-package tree-sitter-langs
  :after tree-sitter
  :ensure tree-sitter-langs)

(provide 'my-tree-sitter)
