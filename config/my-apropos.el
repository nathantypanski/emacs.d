;; # my-apropos.el -*- lexical-binding: t; -*-
;;
;; Apropos customizations for Evil.
;;
;; ## Notes:
;;
;; * Maybe it's not strictly necessary to do these bindings in evil-mode.
;;   Could I just define them in apropos-mode-map and be done with it?

(eval-when-compile (require 'my-core))

(after 'evil
  (after 'general
    (evil-set-initial-state 'apropos-mode 'normal)
    (general-define-key
     :states 'normal
     :keymaps 'apropos-mode-map
     "j"   'forward-button
     "RET" 'apropos-follow
     "q"   'quit-window
     "k"   'backward-button)))

(provide 'my-apropos)
