;; -*- lexical-binding: t; -*-

(defvar my-graphical-font
  (cond
    ((eq system-type 'darwin) "Monaco 12")
    ((eq system-type 'gnu/linux) "Terminus (TTF)-12"))
  "Font used for graphical editing sessions.")

(use-package zenburn-theme
  :ensure zenburn-theme
  :config
  (unless noninteractive
    ;; Disable any existing themes first
    (mapc #'disable-theme custom-enabled-themes)
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

(defun my-enable-line-numbers ()
  "Enable line numbers in a smart way."
  (interactive)
  (unless (or (minibufferp)
              (member major-mode '(org-mode
                                   eshell-mode
                                   shell-mode
                                   term-mode
                                   vterm-mode
                                   eshell-mode)))
    (display-line-numbers-mode)))

(add-hook 'prog-mode-hook #'my-enable-line-numbers)
(add-hook 'text-mode-hook #'my-enable-line-numbers)

(defun my-set-window-font (font)
  "Set the frame font to FONT.
FONT is the name of a xft font, like `Monospace-10'."
  (interactive "sFont: ")
  ;; (set-face-attribute 'default nil :height 125 :family "Fira Mono"))
  (set-face-attribute 'fixed-pitch nil
                      :font font
                      :height 100)
  (set-face-attribute 'default nil
                      :font font
                      :height 100)
  (set-face-attribute 'variable-pitch nil
                      :font font
                      :height 100)
  (set-frame-font font nil t))


(defun my-use-default-font (&optional frame)
  "Set the frame font to the font name in the variable my-graphical-font.
  This command only has an effect on graphical frames."
  (interactive)
  (my-set-window-font my-graphical-font)
  ;; Configure font fallbacks with matching metrics for Unicode
  (when (display-graphic-p)
    ;; Use monospace fonts with similar metrics to Terminus for Unicode fallback
    (let ((font "DepartureMono Nerd Font Mono Regular-12"))
      ;; Ensure box-drawing characters use consistent metrics
      (let ((font "DepartureMono Nerd Font Mono Regular"))
        (set-fontset-font t '(#x2500 . #x257F) font nil 'prepend)
        ;; Arrows and symbols
        (set-fontset-font t '(#x2190 . #x21FF) font nil 'prepend)
        ;; Geometric shapes
        (set-fontset-font t '(#x25A0 . #x25FF) font nil 'prepend)))))

(defun my-display-gui-p ()
  "Return t if running in GUI mode, nil if terminal."
  (display-graphic-p))

(defun my-general-ui-setup ()
  "Do setup for both terminal and non-graphical modes."
  (tab-bar-mode 1)
  ;; don't break long lines at word boundaries
  (global-visual-line-mode 1)
  ;; bar cursor
  (setq cursor-type 'box)
  ;; number columns in the status bar
  (column-number-mode)
  ;; Put ediff control panels in a separate frame in graphical mode.
  ;; In terminal mode, ediff will do everything in one frame.
  (setq ediff-window-setup-function 'ediff-setup-windows-default)
  (setq-default scroll-margin 5))

(defun my-graphical-ui-setup ()
  "Do setup for graphical terminals, like enabling the menu bar."
  (interactive)
  (message "running graphical setup")
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  ;; Basic ediff setup - specific configurations are in my-gpt.el
  (setq ediff-window-setup-function 'ediff-setup-windows-default)
  (add-hook 'after-make-frame-functions 'my-use-default-font)
  (setq display-buffer-alist ()
        ;;'(("\\*Help\\*" display-buffer-pop-up-frame)
        ;;  ("\\*Completions\\*" display-buffer-pop-up-frame)
        ;; Add more rules as desired
        )
  ;; Complete removal of title bar
  (add-to-list 'default-frame-alist '(undecorated . t))
  (setq x-gtk-use-system-tooltips nil)
  (setq x-gtk-use-old-file-dialog t)
  ;; Most importantly - disable GTK menu bar
  (setq x-gtk-use-system-menu-bar nil)
  (my-use-default-font))

(defun my-terminal-ui-setup ()
  "Do setup for non-graphical terminals, like disabling the toolbar."
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  ;; try to make scrolling smooth in terminal
  (setq scroll-preserve-screen-position t))

(my-general-ui-setup)
(if (display-graphic-p)
    ;; graphical mode
    (my-graphical-ui-setup)
  ;; terminal mode
  (my-terminal-ui-setup))

(provide 'my-eyecandy)
