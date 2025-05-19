;; my-magit.el    -*- lexical-binding:t; -*-
;;
;; Magit is an Emacs mode for Git.
;; <https://github.com/magit/magit>
;;
;; This file contains my personal customizations - mostly keybindings - for it.

(use-package diff-hl
  :ensure t
  :custom
  ;; turn on line-change highlighting everywhere
  (global-diff-hl-mode 1)
  :config
  ;; and in terminals, use the margin instead of the fringe
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1)))

(use-package magit
  :ensure magit
  :custom
  (magit-auto-revert-mode nil))

(provide 'my-magit)
