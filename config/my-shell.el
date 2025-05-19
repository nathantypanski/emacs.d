(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

(after 'evil
  ;; ensure RET sends the command
  (evil-define-key 'insert comint-mode-map (kbd "RET") 'comint-send-input))

(provide 'my-shell)
