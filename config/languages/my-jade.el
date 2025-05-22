;; my-jade.el -*- lexical-binding: t; -*-
;;
;; Jade (HTML templating) editing.

(use-package jade-mode
  :ensure jade-mode
  :commands jade-mode
  :mode "\\.jade\\'")

(provide 'my-jade)
