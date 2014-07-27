;; my-scss.el
;;
;; for things related to scss

(use-package css-mode
  :commands css-mode
  :ensure css-mode
  :mode "\\.css\\'"
  )

(use-package scss-mode
  :commands scss-mode
  :ensure scss-mode
  :mode "\\.scss\\'"
)

(provide 'my-scss)
