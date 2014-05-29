(require 'my-globals)
(require 'my-functions)
(require 'my-dirs)
(require 'my-buffers)
(require 'my-imenu)
(require 'my-c)
(require 'my-autocomplete)
(require 'my-org)
(require 'my-projects)
(require 'my-interaction)
(require 'my-scss)
(require 'my-haskell)
(require 'my-python)
(require 'my-unbound-keys)
(require 'my-eyecandy)
(require 'my-shell)
(require 'my-filetypes)

(use-package expand-region
  :ensure expand-region)

(after 'comint
  (define-key comint-mode-map [up] 'comint-previous-input)
  (define-key comint-mode-map [down] 'comint-next-input))

(after 'expand-region-autoloads
  (global-set-key (kbd "C-=") 'er/expand-region))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x g") 'my-google)
(global-set-key (kbd "C-c e") 'my-eval-and-replace)

;; have no use for these default bindings
(global-unset-key (kbd "C-x m"))

(use-package guide-key
  :ensure guide-key
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x" "C-c"))
    (setq guide-key/recursive-key-sequence-flag t)
    (guide-key-mode 1)
    (setq guide-key/idle-delay 2.5)
    )
  )

(use-package ace-jump-mode
  :ensure ace-jump-mode
  :config
  (progn
    (after 'evil
      ;; Not sure if the `after` here is necessary, but anyway:
      (after 'ace-jump-mode-autoloads
        (define-key evil-normal-state-map (kbd "SPC j") 'ace-jump-char-mode)
        (define-key evil-motion-state-map (kbd "SPC") 'evil-ace-jump-char-mode)
        (define-key evil-motion-state-map (kbd "S-SPC") 'evil-ace-jump-line-mode)
        )
      ;; These will definitely work:
      (after 'key-chord-mode
        (key-chord-define evil-normal-state-map ";w" 'ace-jump-word-mode)
        (key-chord-define evil-normal-state-map ";c" 'ace-jump-char-mode)
        (key-chord-define evil-normal-state-map ";l" 'ace-jump-line-mode)
        )
      )
    )
  )

(require 'my-evil)
(provide 'my-startup)
