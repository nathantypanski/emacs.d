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

;; show whitespace
(require 'whitespace)
(setq whitespace-style '(trailing))
(global-whitespace-mode 1)

(use-package smart-mode-line
  :ensure smart-mode-line
  :custom
  (sml/theme 'automatic)
  (sml/show-frame-identification t)
  (sml/show-client t)
  (sml/use-projectile-p t)
  (sml/mode-width 'right)
  (sml/shorten-modes t)
  (sml/shorten-directory t)
  (sml/hidden-modes '(".*"))
  (mode-line-format
   (cons '(:eval (when (bound-and-true-p evil-local-mode)
                   (my/evil-state-indicator)))
         mode-line-format))
  :config

  (after 'evil
    (defun my/evil-state-indicator ()
      (propertize
       (concat "[" (upcase (symbol-name evil-state)) "]")
       'face
       (cond
        ((eq evil-state 'normal)
         '(:foreground "green" :weight bold))
        ((eq evil-state 'insert)
         '(:foreground "orange" :weight bold))
        ((eq evil-state 'visual)
         '(:foreground "magenta" :weight bold))
        ((eq evil-state 'emacs)
         '(:foreground "cyan" :weight bold))
        (t '(:foreground "red" :weight bold))))))
  (sml/setup))

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
