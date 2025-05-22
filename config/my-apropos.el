;; # my-apropos.el -*- lexical-binding: t; -*-
;;
;; Apropos customizations for Evil.
;;
;; ## Notes:
;;
;; * Maybe it's not strictly necessary to do these bindings in evil-mode.
;;   Could I just define them in apropos-mode-map and be done with it?

(after 'evil
  (evil-set-initial-state 'apropos-mode 'normal)
  (evil-define-key 'normal apropos-mode-map
    (kbd "j") 'forward-button
    (kbd "RET") 'apropos-follow
    (kbd "q") 'quit-window
    (kbd "k") 'backward-button)
  )

(provide 'my-apropos)
