(use-package ag
  :ensure ag
  :commands (ag ag-mode ag-files ag-regexp)
  :init
  (progn
    (setq ag-highlight-search t)
    (setq ag-reuse-buffers t)
    (defun my-setup-ag ()
      "Function called to set my ag stuff up."
      (toggle-truncate-lines t)
      (linum-mode 0)
      (switch-to-buffer-other-window "*ag search*")
      )
    (add-hook 'ag-mode-hook 'my-setup-ag)
    (after 'evil
      (evil-set-initial-state 'ag-mode 'normal)
      ;; unbind kill window
      )
    )
  :config
  (progn
      (define-key ag-mode-map (kbd "k") 'nil)
      (evil-define-key 'normal ag-mode-map (kbd "k") 'nil)
      (evil-define-key 'motion ag-mode-map (kbd "k") 'compilation-previous-error)
      (evil-define-key 'motion ag-mode-map (kbd "j") 'compilation-next-error)
    )
  )

(provide 'my-ag)
