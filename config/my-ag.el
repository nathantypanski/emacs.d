;; -*- lexical-binding: t; -*-

(use-package ag
  :ensure ag
  :commands (ag ag-mode ag-files ag-regexp)
  :hook ((age-mode-hook . my-setup-ag))
  :custom
  (ag-highlight-search t)
  (ag-reuse-buffers t)
  :init
    (defun my-setup-ag ()
      "Function called to set my ag stuff up."
      (toggle-truncate-lines t)
      (switch-to-buffer-other-window "*ag search*"))
    (add-hook 'ag-mode-hook 'my-setup-ag)
    (after 'evil
      (evil-set-initial-state 'ag-mode 'normal))
  :config
      (define-key ag-mode-map (kbd "k") 'nil)
      (evil-define-key 'normal ag-mode-map (kbd "k") 'nil)
      (evil-define-key 'motion ag-mode-map (kbd "k") 'compilation-previous-error)
      (evil-define-key 'motion ag-mode-map (kbd "j") 'compilation-next-error))

(provide 'my-ag)
