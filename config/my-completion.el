;; my-ido.el
;;
(use-package vertico
  :ensure vertico
  :init (vertico-mode))

(use-package orderless
  :ensure orderless
  :custom
  (completion-styles '(orderless flex)))

;; Rich annotations
(use-package marginalia
  :init (marginalia-mode))

;; Better commands
(use-package consult
  :bind (("C-s" . consult-line)
         ("M-y" . consult-yank-pop)
         ("C-x b" . consult-buffer)))

;; Context-sensitive actions (like Helm's TAB preview)
(use-package embark
  :bind (("C-." . embark-act)))

;; Optional: in-buffer popup completion
(use-package corfu
  :init (global-corfu-mode))

(provide 'my-completion)
