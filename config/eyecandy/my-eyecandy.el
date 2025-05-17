
(use-package zenburn-theme
  :ensure zenburn-theme
  :config
  (progn
    (unless noninteractive
      (load-theme 'zenburn t))))

(global-hl-line-mode t)

;; Color derived from zenburn, but I want the current focus to be obvious.
(set-face-background 'hl-line "#2a2e2e")
;; GPT: any other similar customizations I might make? Like line number bg?

;; Show parentheses
(show-paren-mode 1)

;; highlight entire expression when matching paren is not visible;
;; otherwise just highlight matching paren
(setq show-paren-style 'mixed)

(setq whitespace-style '(trailing))
(global-whitespace-mode 1)

(use-package smart-mode-line
  :ensure smart-mode-line
  :config
  (progn
    (setq sml/theme 'dark)
    (setq sml/mode-width 30)
    (sml/setup)
    ))

(use-package rainbow-mode
  :ensure rainbow-mode)

(use-package rainbow-delimiters
  :ensure rainbow-delimiters
  :init
  (progn
    (rainbow-delimiters-mode-enable)
    ))

(defun my-coding-mode-eyecandy ()
  "Eyecandy specific to programming text editing modes."
  (rainbow-delimiters-mode-enable))

(provide 'my-eyecandy)
