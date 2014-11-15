;; my-help.el
;;
;; Make help-mode more convenient to work with inside of Evil.
;; Give it Vimmy keybindings, but retain conveniences like "q to quit".

(after 'evil
  (evil-set-initial-state 'help-mode-map 'normal)
  (evil-define-key 'normal help-mode-map
    (kbd "j") 'evil-next-line
    (kbd "k") 'evil-previous-line
    (kbd "h") 'evil-backward-char
    (kbd "l") 'evil-forward-char
    (kbd "q") 'quit-window
    (kbd "H") 'help-go-back
    (kbd "L") 'help-go-forward
    (kbd "RET") 'help-follow))

(provide 'my-help)
