(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless flex))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (unless (not (display-graphic-p))  ; marginalia generally works in tty too
    (marginalia-mode)))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("M-y" . consult-yank-pop)
         ("C-x b" . consult-buffer)))

(use-package consult-lsp
  :ensure t
  :after lsp-mode)

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (tab-always-indent 'complete)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-cycle t)
  (corfu-preselect-first nil)
  (corfu-quit-no-match 'separator)
  :config
  (when (display-graphic-p)
    (corfu-popupinfo-mode -1))
  (global-set-key (kbd "M-TAB") #'corfu-complete))

(unless (display-graphic-p)
  (use-package corfu-terminal
    :ensure t
    :after corfu
    :config
    (corfu-terminal-mode +1)
    (setq corfu-count 5) ; reduce candidates to prevent overlap
    (setq corfu-scroll-margin 0)
    ;; disable inline echo to avoid overlap in echo area
    (corfu-echo-mode -1)))

(setq eldoc-echo-area-prefer-doc-buffer t)

(use-package cape
  :ensure t
  :config
  (progn
    (after 'lsp-mode
      (add-hook 'lsp-mode-hook
                (lambda ()
                  (setq-local completion-at-point-functions
                              (list (cape-capf-buster #'lsp-completion-at-point))))))))

(provide 'my-completion)
