;; -*- lexical-binding: t; -*-
(use-package zenburn-theme
  :ensure zenburn-theme
  :config
    (unless noninteractive
      (load-theme 'zenburn t)))

(global-hl-line-mode t)

;; Color derived from zenburn, but I want the current focus to be obvious.
(set-face-background 'hl-line "#444444")

;; GPT: any other similar customizations I might make? Like line number bg?

;; highlight entire expression when matching paren is not visible;
;; otherwise just highlight matching paren
(set-variable show-paren-style 'mixed)

;; Show parentheses
(show-paren-mode 1)

(require 'whitespace)
(setq whitespace-style '(trailing missing-newline-at-eof face))

(global-whitespace-mode 1)

(use-package smart-mode-line
  :ensure smart-mode-line
  :custom
  (sml/theme 'automatic)
  (sml/mode-width 10)
  :config
    (sml/setup)
    (smart-mode-line-enable))

(use-package rainbow-mode
  :ensure rainbow-mode)

(use-package rainbow-delimiters
  :ensure rainbow-delimiters
  :init
  (progn
    (rainbow-delimiters-mode-enable)))

(defun my-coding-mode-eyecandy ()
  "Eyecandy specific to programming text editing modes."
  (rainbow-delimiters-mode-enable))

(provide 'my-eyecandy)
