;; ---------- Corfu (core) ----------
(use-package corfu
  :ensure t
  :init
  ;; enable Corfu globally
  (global-corfu-mode)
  ;; TAB completes, fallback to indent
  (setq tab-always-indent 'complete)
  :custom
  ;; auto popup after delay
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-cycle t)                 ;; wrap around candidates
  (corfu-preselect-first nil)     ;; don’t preselect exact match
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
    (corfu-echo-mode +1)))        ;; show docs in echo area

;; optional: M-TAB to manually trigger completion
(global-set-key (kbd "M-TAB") #'corfu-complete)

(provide 'my-autocomplete)
