;; zsh shell script mode
(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

(provide 'my-filetypes)
