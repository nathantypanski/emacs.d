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
  (use-package zenburn-theme
    :ensure zenburn-theme
    :config
    (progn
      (load-theme 'zenburn t)
      ))
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

(use-package powerline
  :ensure powerline
  :init
  (progn
    (use-package powerline-evil
      :ensure powerline-evil
      :init
      (progn
        (setq powerline-arrow-shape 'arrow14)
        (set-face-foreground 'mode-line "#fff")
        (set-face-background 'mode-line "#000")
        (setq-default mode-line-format '("%e"
                                         (:eval
                                          (let*
                                              ((active
                                                (powerline-selected-window-active))
                                               (mode-line
                                                (if active 'mode-line 'mode-line-inactive))
                                               (face1
                                                (if active 'powerline-active1 'powerline-inactive1))
                                               (face2
                                                (if active 'powerline-active2 'powerline-inactive2))
                                               (separator-left
                                                (intern
                                                 (format "powerline-%s-%s" powerline-default-separator
                                                         (car powerline-default-separator-dir))))
                                               (separator-right
                                                (intern
                                                 (format "powerline-%s-%s" powerline-default-separator
                                                         (cdr powerline-default-separator-dir))))
                                               (lhs
                                                (list
                                                 (let
                                                     ((evil-face
                                                       (powerline-evil-face)))
                                                   (if (and evil-mode
                                                            (eq (current-buffer) (car (buffer-list))))
                                                       (powerline-raw
                                                        (powerline-evil-tag)
                                                        evil-face)))
                                                 (powerline-buffer-id
                                                  `(mode-line-buffer-id ,mode-line)
                                                  'l)
                                                 (powerline-raw "[" mode-line 'l)
                                                 (powerline-major-mode mode-line)
                                                 (powerline-process mode-line)
                                                 (powerline-raw "]" mode-line)
                                                 (when
                                                     (buffer-modified-p)
                                                   (powerline-raw "[+]" mode-line))
                                                 (when buffer-read-only
                                                   (powerline-raw "[RO]" mode-line))
                                                 (powerline-raw "[%z]" mode-line)
                                                 (when
                                                     (and
                                                      (boundp 'which-func-mode)
                                                      which-func-mode)
                                                   (powerline-raw which-func-format nil 'l))
                                                 (when
                                                     (boundp 'erc-modified-channels-object)
                                                   (powerline-raw erc-modified-channels-object face1 'l))
                                                 ;; (powerline-raw "[" mode-line 'l)
                                                 ;; (powerline-minor-modes mode-line)
                                                 ;; (powerline-raw "%n" mode-line)
                                                 ;; (powerline-raw "]" mode-line)
                                                 (when
                                                     (and vc-mode buffer-file-name)
                                                   (let
                                                       ((backend
                                                         (vc-backend buffer-file-name)))
                                                     (when backend
                                                       (concat
                                                        (powerline-raw "[" mode-line 'l)
                                                        (powerline-raw
                                                         (format "%s / %s" backend
                                                                 (vc-working-revision buffer-file-name backend)))
                                                        (powerline-raw "]" mode-line)))))))
                                               (rhs
                                                (list
                                                 (powerline-raw
                                                  '(10 "%i"))
                                                 (powerline-raw global-mode-string mode-line 'r)
                                                 (powerline-raw "%l," mode-line 'l)
                                                 (powerline-raw
                                                  (format-mode-line
                                                   '(10 "%c")))
                                                 (powerline-raw
                                                  (replace-regexp-in-string "%" "%%"
                                                                            (format-mode-line
                                                                             '(-3 "%p")))
                                                  mode-line 'r))))
                                            (concat
                                             (powerline-render lhs)
                                             (powerline-fill mode-line
                                                             (powerline-width rhs))
                                             (powerline-render rhs))))))
        )
      )
    )
  )


(provide 'my-eyecandy)
