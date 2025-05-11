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

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

;; BUG: Hop to a new buffer, enter insert state, start typing ("k" was 2nd char in "ok")
;;
;; Debugger entered--Lisp error: (wrong-type-argument stringp nil)
;;   company-insertion-on-trigger-p("k")
;;   company--continue()
;;   company--perform()
;;   company-post-command()
(use-package company
  :after yasnippet
  :ensure t
  :hook (after-init . global-company-mode)
  :custom
  (company-backends '(company-capf company-yasnippet))
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-auto-commit nil)
  (company-auto-commit-chars nil)
  :config
  (define-key company-active-map (kbd "TAB")   #'company-select-next)
  (define-key company-active-map (kbd "<tab>") #'company-select-next)
  (define-key company-active-map (kbd "S-TAB") #'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") #'company-select-previous)
  (define-key company-active-map (kbd "RET")   #'company-complete-selection))

(provide 'my-completion)
