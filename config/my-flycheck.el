;; my-flycheck.el
;;
;; Syntax checkers.

(use-package flycheck
  :ensure flycheck
  :init
  (progn
        (add-hook 'after-init-hook #'global-flycheck-mode))
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
    (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
    (setq flycheck-rust-cargo-executable "/usr/bin/cargo")

    (defun my-flycheck-list-errors ()
      "Jump to flycheck errors and switch to the errorlist buffer"
      (interactive)
      (flycheck-list-errors)
      (switch-to-buffer-other-window "*Flycheck errors*" t))

    (after 'evil
      (evil-define-key 'normal flycheck-error-list-mode-map
        "q" 'quit-window
        "j" #'flycheck-error-list-next-error
        "k" #'flycheck-error-list-previous-error
        "K" #'evil-previous-line
        "J" #'evil-next-line
        (kbd "RET") #'flycheck-error-list-goto-error))))

(provide 'my-flycheck)
