;; my-scss.el
;;
;; for things related to scss

(use-package scss-mode
  :ensure scss-mode
  :config
  (progn
    (autoload 'scss-mode "scss-mode")
    (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
    ))

(provide 'my-scss)
