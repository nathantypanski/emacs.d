;; my-magit.el    -*- lexical-binding:t; -*-
;;
;; Magit is an Emacs mode for Git.
;; <https://github.com/magit/magit>
;;
;; This file contains my personal customizations - mostly keybindings - for it.

(use-package magit
  :ensure magit
  :custom
  (magit-auto-revert-mode nil))

(provide 'my-magit)
