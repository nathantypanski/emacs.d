;; Which-key: discoverable keybindings -*- lexical-binding: t; -*-
(use-package which-key
  :ensure which-key
  :config (which-key-mode))

(use-package key-chord
  :ensure key-chord
  :config
  (progn
    (key-chord-mode 1)))

(use-package general
  :ensure t
  :demand t)

(provide 'my-keys)
