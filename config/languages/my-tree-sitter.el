;; -*- lexical-binding: t; -*-

(use-package treesit
  :straight (:type built-in)  ; treesit is built into Emacs 29+
  :when (treesit-available-p)
  :custom
  (treesit-auto-install-grammar nil)
  :config
  ;; Uncomment to use github-cloned sources
  (setq treesit-language-source-alist ())
        ;; '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        ;;   (c "https://github.com/tree-sitter/tree-sitter-c")
        ;;   (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        ;;   (python "https://github.com/tree-sitter/tree-sitter-python")
        ;;   (rust "https://github.com/tree-sitter/tree-sitter-rust")
        ;;   (go "https://github.com/tree-sitter/tree-sitter-go")
        ;;   (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        ;;   (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        ;;   (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; Auto-install missing grammars
  (defun my/treesit-install-all-languages ()
    "Install all configured tree-sitter languages."
    (interactive)
    (dolist (lang-config treesit-language-source-alist)
      (let ((lang (car lang-config)))
        (unless (treesit-language-available-p lang)
          (message "Installing tree-sitter grammar for %s" lang)
          (treesit-install-language-grammar lang)))))

  ;; Set up auto-mode-alist for tree-sitter modes
  (setq major-mode-remap-alist
        '((bash-mode . bash-ts-mode)
          (sh-mode . bash-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (python-mode . python-ts-mode)
          (rust-mode . rust-ts-mode)
          (go-mode . go-ts-mode)
          (js-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (yaml-mode . yaml-ts-mode)))

  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode)))

(provide 'my-tree-sitter)
