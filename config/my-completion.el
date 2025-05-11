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

(use-package company
  :ensure t
  :hook ((prog-mode . company-mode))
  :custom
  (company-backends '(company-capf))
  (company-idle-delay 0.8)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  ;; (company-auto-commit nil)
  ;; (company-auto-commit-chars nil)
  :config
  (define-key company-active-map (kbd "TAB")   #'company-select-next)
  (define-key company-active-map (kbd "<tab>") #'company-select-next)
  (define-key company-active-map (kbd "S-TAB") #'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") #'company-select-previous)
  (define-key company-active-map (kbd "RET")   #'company-complete-selection)

  ;; BUG: Hop to a new buffer, enter insert
  ;;
  ;;     (wrong-type-argument stringp nil)
  ;;     company-insertion-on-trigger-p(")")
  ;;     company--continue()
  ;;     company--perform()
  ;;     company-post-command()
  ;;
  ;; Constant headache (for `)`, but also real characters like `k`! Company:
  ;;
  ;; This is a known bug pattern in company-mode, often triggered by
  ;; non-character input, manual typing after rejecting a completion, or certain
  ;; backends misbehaving (especially when returning nil or incomplete data
  ;; during post-command processing).
  ;;
  ;; The key part of the trace:
  ;;
  ;;     company-insertion-on-trigger-p(")")
  ;;
  ;; suggests it's trying to check whether `)` is a trigger character — but
  ;; something inside company-insertion-on-trigger-p is still trying to handle a
  ;; nil somewhere (most likely `company--prefix` or backend-provided data is
  ;; nil).
  ;;
  ;; HACK Fix: Patch company-insertion-on-trigger-p to guard nil
  ;;
  ;; This is the cleanest and safest workaround for now:
  ;;
  ;; (advice-add 'company-insertion-on-trigger-p :around
  ;;             (lambda (orig char)
  ;;               (when (and char (stringp char))
  ;;                 (funcall orig char))))

  ;; Disable company autopopup for completions if point is in a comment.
  (defun my-company-inhibit-in-comments (fun &rest args)
    "Inhibit company idle completion in comments."
    (if (nth 4 (syntax-ppss))
        ;; If inside a comment, require manual completion
        (let ((company-idle-delay nil))
          (apply fun args))
      ;; Else, follow normal behavior
      (apply fun args)))

  (advice-add 'company-idle-begin :around #'my-company-inhibit-in-comments))

(provide 'my-completion)
