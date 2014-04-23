;; verilog
;;(add-to-list 'auto-mode-alist '("\\.v\\'" . verilog-mode))
;;(add-hook verilog-mode-hook (progn (setq verilog-indent-level 8)
;;                                   (setq verilog-indent-level-module 8)
;;                                   (setq verilog-indent-level-declaration 8)
;;                                   (setq verilog-tab-always-indent t)
;;                                   )
;;          )

;; zsh shell script mode
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
(require 'rust-mode)

(add-hook 'rust-mode-hook (lambda ()
   setq tab-width 4))
(provide 'my-filetypes)