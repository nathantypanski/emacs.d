;; my-scss.el -*- lexical-binding: t; -*-
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
  :config
  (progn
    (setq scss-compile-at-save nil)
    )
)



(provide 'my-scss)
