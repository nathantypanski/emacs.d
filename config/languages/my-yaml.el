;; my-yaml.el -*- lexical-binding: t; -*-
;;
;; Yaml editing.

(use-package yaml-mode
  :ensure yaml-mode
  :commands yaml-mode
  :mode "\\.yml\\'")

(provide 'my-yaml)
