;;; my-flycheck.el --- flycheck customization -*- lexical-binding:t; -*-
;;; Commentary:
;;
;; Syntax checkers.
;;
;;; Code:

(use-package flycheck
  :ensure flycheck
  :demand flycheck
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-display-errors-function nil)
  (flycheck-wrap-around t)
  :config
  (setq-default flycheck-indication-mode 'left-margin)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; remove elisp checkdoc
  (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))

  (defun my-flycheck-list-errors ()
    "Jump to flycheck errors and switch to the errorlist buffer"
    (interactive)
    (flycheck-list-errors)
    (switch-to-buffer-other-window "*Flycheck errors*" t))

  (add-hook 'flycheck-error-list-mode-hook ;; Error List only
            (lambda () (setq-local truncate-lines nil) ; allow wrapping
              (visual-line-mode 1))) ; soft-wrap at window edge

  (after 'evil
    (evil-define-key 'normal flycheck-error-list-mode-map
      "q" 'quit-window
      "j" #'flycheck-error-list-next-error
      "k" #'flycheck-error-list-previous-error
      "K" #'evil-previous-line
      "J" #'evil-next-line
      (kbd "RET") #'flycheck-error-list-goto-error)))

(provide 'my-flycheck)
;;; my-flycheck.el ends here
