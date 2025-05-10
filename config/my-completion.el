(use-package vertico
  :ensure t
  :after consult
  :init
  (vertico-mode)
  :config
  (defun my-vertico-toggle-capf ()
  "Use `consult-completion-in-region` when Vertico is on, else default."
  (setq-local completion-in-region-function
              (if vertico-mode
                  #'consult-completion-in-region
                #'completion--in-region)))

  (setq vertioc-cycle t))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless flex))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; annotate completion candidates, e.g. in M-x
(use-package marginalia
  :ensure t
  :init
  (setq completion-in-region-function #'consult-completion-in-region)
  :config
  (progn
    (marginalia-mode)))

(use-package consult
  :ensure t
  :bind
  (("C-s" . consult-line)
   ("M-y" . consult-yank-pop)
   ("C-x b" . consult-buffer)
   )
  :init
  (progn)
  :config
  (progn))

(use-package consult-lsp
  :ensure t
  :after lsp-mode)

(use-package embark-consult
  :ensure t
  :bind (("C-." . embark-act)))

(when (bound-and-true-p semantic-mode)
  (semantic-mode -1))
(setq completion-auto-help t)

(provide 'my-completion)
