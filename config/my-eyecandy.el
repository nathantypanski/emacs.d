;; -*- lexical-binding: t; -*-

(use-package zenburn-theme
  :ensure zenburn-theme
  :config
  (unless noninteractive
    (load-theme 'zenburn t)))

;; I never look at right-side fringes. Do you?
(if (fboundp 'set-fringe-style) (set-fringe-style '(8 . 0)))

(global-hl-line-mode t)

;; Color derived from zenburn, but I want the current focus to be obvious.
(set-face-background 'hl-line "#444444")

;; GPT: any other similar customizations I might make? Like line number bg?

;; highlight entire expression when matching paren is not visible;
;; otherwise just highlight matching paren
(set-variable show-paren-style 'mixed)

;; show whitespace
(require 'whitespace)
(setq whitespace-style '(trailing))
(global-whitespace-mode 1)

(use-package smart-mode-line
  :ensure smart-mode-line
  :if (not noninteractive)  ; Skip in batch mode
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

;; half-riffed from
;; https://gist.github.com/b7r6/23cfacbf181c9b0447841c798345a793
(defun my-clean-window-dividers-with-fringe ()
  "Set up clean Unicode window dividers while keeping fringe for diagnostics."
  (interactive)
  ;; Clean vertical borders
  (custom-set-faces '(vertical-border nil))

  ;; Clean up fringe indicators (from first function)
  (setq-default fringe-indicator-alist '())

  ;; Keep fringe for diagnostics but make it subtle
  (fringe-mode '(8 . 8))

  ;; Remove mode-line position (from first function)
  (setq-default
   mode-line-format
   (remove 'mode-line-position mode-line-format))

  ;; Unicode divider character
  (when (boundp 'standard-display-table)
    (unless standard-display-table
      (setq standard-display-table (make-display-table)))
    (set-display-table-slot
     standard-display-table 'vertical-border
     (make-glyph-code ?│))))

(use-package idle-highlight-mode
  :ensure t
  :straight t
  :config
  (global-idle-highlight-mode t))

(use-package highlight-parentheses
  :ensure t
  :straight t
  :config

  ;; disable show-paren-mode
  (show-paren-mode -1)
  (global-highlight-parentheses-mode 1)
  (add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup))

(use-package rainbow-delimiters
  :ensure rainbow-delimiters
  :init
  (rainbow-delimiters-mode-enable))

(use-package highlight-escape-sequences
  :ensure t
  :straight t
  :config
  (hes-mode t))

(defun my-coding-mode-eyecandy ()
  "Eyecandy specific to programming text editing modes."
  (interactive)
  (rainbow-delimiters-mode-enable)
  (show-paren-mode -1)
  (rainbow-delimiters-mode t)
  (highlight-parentheses-mode 1)
  (turn-on-hes-mode))

(add-hook 'prog-mode-hook #'my-coding-mode-eyecandy)

(my-clean-window-dividers-with-fringe)

(provide 'my-eyecandy)
