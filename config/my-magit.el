;; my-magit.el
;;
;; Magit is an Emacs mode for Git.
;; <https://github.com/magit/magit>
;;
;; This file contains my personal customizations - mostly keybindings - for it.

(use-package magit
  :ensure magit
  :config
  (progn
    (setq magit-auto-revert-mode nil)
    ;; tell magit not to complain about auto-revert-mode being a new setting
    (setq magit-last-seen-setup-instructions "1.4.0")
    ))

(provide 'my-magit)
