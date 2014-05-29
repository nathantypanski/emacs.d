;; verilog
;;(add-to-list 'auto-mode-alist '("\\.v\\'" . verilog-mode))
;;(add-hook verilog-mode-hook (progn (setq verilog-indent-level 8)
;;                                   (setq verilog-indent-level-module 8)
;;                                   (setq verilog-indent-level-declaration 8)
;;                                   (setq verilog-tab-always-indent t)
;;                                   )
;;          )

;; zsh shell script mode
(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

(provide 'my-filetypes)
