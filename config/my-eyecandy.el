;; -*- lexical-binding: t; -*-

(defvar my-graphical-font
  (cond
    ((my-system-is-mac) "Monaco 12")
    ((my-system-is-linux) "Terminus (TTF)-12"))
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
                   (my-evil-state-indicator)))
         mode-line-format))
  :config

  (after 'evil
    (defun my-evil-state-indicator ()
      (propertize
       (cond
        ;; Special case: vterm copy mode
        ((and (boundp 'vterm-copy-mode) vterm-copy-mode)
         "[VTERM-COPY]")
        ;; Regular evil states
        (t (concat "[" (upcase (symbol-name evil-state)) "]")))
       'face
       (cond
        ;; Special highlighting for vterm copy mode
        ((and (boundp 'vterm-copy-mode) vterm-copy-mode)
         '(:foreground "yellow" :weight bold))
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
  (set-face-attribute 'info-menu-header nil
                      :height 100)
  (set-frame-font font nil t))

(defun my-setup-consistent-fonts (pixel-size)
  "Set up fonts with consistent heights using direct pixel sizing.
Preserves org-mode DepartureMono configuration for code blocks."
  (interactive "nPixel size: ")
  (let* ((base-font "Terminus (TTF)")
         (font-spec (format "%s:pixelsize=%d" base-font pixel-size)))

    ;; Set frame font at the desired size
    (set-frame-font font-spec nil t)

    ;; Configure fontset for consistent symbol rendering
    (set-fontset-font t 'unicode font-spec nil 'prepend)

    ;; Handle specific script ranges
    (dolist (range '(latin greek cyrillic symbol unicode))
      (set-fontset-font t range font-spec nil 'prepend))

    ;; Fallback fonts at same pixel size
    (let ((fallback-fonts '("DejaVu Sans Mono" "Liberation Mono" "Courier New")))
      (dolist (fallback fallback-fonts)
        (when (find-font (font-spec :name fallback))
          (let ((fallback-spec (format "%s:pixelsize=%d" fallback pixel-size)))
            (set-fontset-font t 'unicode fallback-spec nil 'append)))))

    ;; Clear the problematic rescale alist
    (setq face-font-rescale-alist nil)

    ;; Clear height overrides to let pixel size take precedence
    ;; BUT preserve org-mode block faces that should use DepartureMono
    (dolist (face '(default fixed-pitch variable-pitch))
      (set-face-attribute face nil :height 'unspecified))

    ;; Re-trigger org-mode face setup to preserve DepartureMono for blocks
    (when (fboundp 'my-setup-org-faces)
      (my-setup-org-faces))

    (message "Consistent font setup: %s at %dpx (preserving org-mode DepartureMono)" base-font pixel-size)))

(defun my-use-default-font (&optional frame)
  "Set the frame font to the font name in the variable my-graphical-font.
  This command only has an effect on graphical frames."
  (interactive)
  (my-enforce-consistent-font-height)
  (my-set-window-font my-graphical-font)
  ;; Essential font fallback prevention - prevent wrong-sized fonts
  (when (display-graphic-p)
    ;; Prevent problematic fonts from being used as fallbacks
    (dolist (bad-font '("Droid Sans" "Adwaita" "Cantarell" "DejaVu Sans"))
      (when (find-font (font-spec :name bad-font))
        ;; Override fallbacks to these fonts with our preferred font at correct size
        (set-fontset-font t 'unicode "Terminus (TTF):pixelsize=12" nil 'prepend)))

    ;; Scale down problematic fonts if they ever appear
    (when (boundp 'face-font-rescale-alist)
      (setq face-font-rescale-alist '(("Droid Sans" . 0.75)
                                      ("Adwaita" . 1.0)
                                      ("DejaVu Sans" . 1.0))))))

(defun my-enforce-consistent-font-height-ultimate ()
  "Aggressively enforce consistent font height, including hooks to prevent changes"
  (interactive)
  (let ((base-height (face-attribute 'default :height)))
    ;; First, set all face heights
    (dolist (face (face-list))
      (set-face-attribute face nil :height 'unspecified))

    ;; Specifically target org faces
    (dolist (face '(org-code org-block org-verbatim org-table
                    org-inline-src-block org-block-begin-line
                    org-block-end-line org-meta-line))
      (when (facep face)
        (set-face-attribute face nil :height 1.0)))

    ;; Add advice to prevent height changes
    (advice-add 'set-face-attribute :before
                (lambda (face frame &rest args)
                  (when (and (plist-member args :height)
                            (not (equal (plist-get args :height) 'unspecified)))
                    (message \"Blocking height change for %s\" face)
                    (plist-put args :height 'unspecified)))
                '((name . enforce-font-height)))

    (message "Enforced consistent font heights and installed protection")))

(defun my-get-faces-with-custom-sizes ()
  "Return list of faces with non-default font sizes."
  (let ((faces-with-sizes '()))
    (dolist (face (face-list))
      (let ((height (face-attribute face :height nil t))
            (family (face-attribute face :family nil t)))
        (when (and (numberp height) (not (eq height 'unspecified)))
          (push (list face height family) faces-with-sizes))))
    (sort faces-with-sizes
          (lambda (a b) (string< (symbol-name (car a))
                                (symbol-name (car b)))))))

(defun my-insert-customize-link (face)
  "Insert a clickable link to customize FACE."
  (insert-button (format "%s" face)
                 'face 'link
                 'action `(lambda (_) (customize-face ',face))
                 'help-echo (format "Click to customize %s" face)))

(defun my-insert-source-link (face)
  "Insert a clickable link to view source of FACE."
  (insert-button "source"
                 'face 'link
                 'action `(lambda (_) (find-function-other-window ',face))
                 'help-echo (format "Click to view source of %s" face)))

(defun my-list-font-faces-with-sizes ()
  "Display faces with custom font sizes with clickable links."
  (interactive)
  (let ((faces-with-sizes (my-get-faces-with-custom-sizes)))
    (with-current-buffer (get-buffer-create "*Font Faces*")
      (erase-buffer)
      (insert "Font Faces with Custom Sizes\n")
      (insert "============================\n\n")
      (dolist (face-info faces-with-sizes)
        (let ((face (car face-info))
              (height (cadr face-info))
              (family (caddr face-info)))
          ;; Customize link
          (my-insert-customize-link face)
          (insert (format ": height=%s" height))
          (when (not (eq family 'unspecified))
            (insert (format " family=%s" family)))
          (insert "  (")
          ;; Source link
          (my-insert-source-link face)
          (insert ")\n")))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun my-display-gui-p ()
  "Return t if running in GUI mode, nil if terminal."
  (display-graphic-p))

(defun my-enforce-consistent-font-height ()
  "Force ALL faces to use the same height as default face, preserving
other styling."
  (interactive)
  (let ((base-height (face-attribute 'default :height)))
    ;; Only change height, preserve all other face attributes
    (dolist (face (face-list))
      (let ((current-height (face-attribute face :height nil t)))
        (when (and current-height
                   (not (eq current-height 'unspecified))
                   (not (equal current-height base-height)))
          ;; Only set :height, don't touch other attributes
          (set-face-attribute face nil :height base-height))))
    (message "Enforced height %s on all faces" base-height)))

(defun my-reset-to-inherited-fonts ()
  "Reset most faces to inherit from base faces instead of having explicit heights."
  (interactive)
  (dolist (face (face-list))
    (unless (member face '(default fixed-pitch variable-pitch))
      (set-face-attribute face nil :height 'unspecified :family 'unspecified)))
  (message "Reset font inheritance"))

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
  (my-use-default-font)
  (set-fontset-font t '(#x2700 . #x27BF) "DepartureMono Nerd Font"))

(defun my-terminal-ui-setup ()
  "Do setup for non-graphical terminals, like disabling the toolbar."
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  ;; try to make scrolling smooth in terminal
  (setq scroll-preserve-screen-position t))

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
  (setq ediff-window-setup-function 'ediff-setup-windows-multiframe)
  (setq-default scroll-margin 5)

  ;; run graphic or terminal specific setup
  (if (display-graphic-p)
      ;; graphical mode
      (my-graphical-ui-setup)
    ;; terminal mode
    (my-terminal-ui-setup)))

(my-general-ui-setup)

(provide 'my-eyecandy)
