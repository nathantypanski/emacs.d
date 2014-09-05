(defvar my-use-color-theme t
  "If nil, use color-theme to provide styling. Otherwise, default
to zenburn.")
(setq my-use-color-theme nil)

;; Colors!
(if my-use-color-theme
    (use-package color-theme
      :ensure color-theme
      :init
      (progn
        (color-theme-initialize)
        (require 'color-theme-jellybeans)
        (color-theme-jellybeans)
        ))
;;  (use-package zenburn-theme
;;    :ensure zenburn-theme
;;    :config
;;    (progn
;;      (load-theme 'zenburn t)
;;      ))
  )

(global-hl-line-mode t)
(set-face-background 'hl-line "#3e4446")

;; Show parentheses
(show-paren-mode 1)

;; highlight entire expression when matching paren is not visible;
;; otherwise just highlight matching paren
(setq show-paren-style 'mixed)

(setq whitespace-style '(trailing))
(global-whitespace-mode 1)

(use-package smart-mode-line
  :ensure smart-mode-line
  :idle
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
  :idle
  :init
  (progn
    (global-rainbow-delimiters-mode)
    )
  )

(use-package windsize
  :ensure windsize
  :init
  (progn
    (setq windsize-cols 16)
    (setq windsize-rows 8)
    (windsize-default-keybindings)
    )
  )

;; mouse scrolling in terminal
;; is this eyecandy?
(unless (display-graphic-p)
  (global-set-key [mouse-4] (bind (scroll-down 1)))
  (global-set-key [mouse-5] (bind (scroll-up 1))))

;; restore my blood pressure to normal: stop having fill-column=3 in help mode

(add-hook 'help-mode-hook
          (lambda ()
            (set-fill-column 80)))

(require 'my-modeline)
(provide 'my-eyecandy)
