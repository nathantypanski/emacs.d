
(use-package vertico
  :ensure vertico
  :init (vertico-mode))

(use-package orderless
  :ensure orderless
  :custom
  (completion-styles '(orderless flex)))

;; Rich annotations
(use-package marginalia
  :ensure marginalia
  :init
  (marginalia-mode))

;; Better commands
(use-package consult
  :ensure consult
  :bind (("C-s" . consult-line)
         ("M-y" . consult-yank-pop)
         ("C-x b" . consult-buffer)))

;; Context-sensitive actions (like Helm's TAB preview)
(use-package embark
  :ensure embark
  :bind (("C-." . embark-act)))

(use-package corfu
  :ensure t
  :config
  ;; enable Corfu globally
  (progn
    (global-corfu-mode))
  ;; TAB completes, fallback to indent
  (setq tab-always-indent 'complete)
  :custom
  ;; auto popup after delay
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  ;; wrap around candidates
  (corfu-cycle t)
  ;; don’t preselect exact match
  (corfu-preselect-first nil)
  (corfu-quit-no-match 'separator)
  ;; disable floating doc popup (bad in terminal)
  (corfu-popupinfo-mode nil)
  :config
  ;; in terminal frames, use corfu-terminal and echo fallback
  (unless (display-graphic-p)
    (use-package corfu-terminal
      :ensure t
      :config
      (corfu-terminal-mode +1))
    (corfu-echo-mode +1)))

(global-set-key (kbd "M-TAB") #'corfu-complete)

(provide 'my-completion)
