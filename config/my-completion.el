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

;; annotate completion candidates, e.g. in M-x
(use-package marginalia
  :ensure t
  :config
  (progn
    (marginalia-mode)
))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("M-y" . consult-yank-pop)
         ("C-x b" . consult-buffer))
  :config
  (progn
    (setq completion-in-region-function #'consult-completion-in-region)))

(use-package consult-lsp
  :ensure t
  :after lsp-mode)

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)))

(when (bound-and-true-p semantic-mode)
  (semantic-mode -1))

;; (defun my-auto-complete-at-point ()
;;   (when (and (not (active-minibuffer-window))
;;              (not (eq this-command 'completion-at-point))
;;              (memq major-mode '(rust-mode go-mode c-mode python-mode)))
;;     (completion-at-point)))

;; (defun my-enable-auto-minibuffer-completion ()
;;   (add-hook 'post-self-insert-hook #'my-auto-complete-at-point nil t))

;; (add-hook 'prog-mode-hook #'my-enable-auto-minibuffer-completion)
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-preview-current t)
  (corfu-cycle t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match 'separator)
  :config
  (progn
    ;; (after 'marginalia-mode
    ;; (add-hook 'corfu-mode-hook (lambda () (marginalia-mode -1))))
))
  (use-package corfu-terminal
    :ensure t
    :custom
    (corfu-terminal-position-right-margin 5)
    :config
    (progn
      (corfu-terminal-mode +1)))

(provide 'my-completion)
