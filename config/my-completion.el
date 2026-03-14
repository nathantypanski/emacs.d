;;; my-completion.el --- completion framework -*- lexical-binding:t; -*-
;;; Commentary:
;;
;;; Code:

(require 'tempo)

(use-package vertico
  :ensure t
  :demand t
  :after (consult general)
  :init
  (vertico-mode)
  :custom
  ;; cycle from next to prev in vertico
  (vertico-cycle t)
  (general-define-key :keymaps 'vertico-map
   "<TAB>"    #'vertico-next
   "<tab>"    #'vertico-next
   "<backtab>"#'vertico-previous
   "<RET>"    #'vertico-exit
   "C-<RET>"  #'vertico-exit-input
   "C-j"      #'vertico-exit-input
   "S-<TAB>"  #'vertico-previous
   "<ESC>"    #'vertico-exit-input)
  :config
  (defun my-vertico-toggle-capf ()
    "Use `consult-completion-in-region` when Vertico is on, else default."
    (setq-local completion-in-region-function
                (if (bound-and-true-p vertico-mode)
                    #'consult-completion-in-region
                  #'completion--in-region)))

  )


(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless flex))
  (completion-category-overrides '((file (styles partial-completion))))
  :config
  (setq completion-category-defaults nil))

;; annotate completion candidates, e.g. in M-x
(use-package marginalia
  :ensure t
  :init
  (setq completion-in-region-function #'consult-completion-in-region)
  (marginalia-mode))

(use-package consult
  :ensure t
  :demand t
  :custom
  (consult-async-min-input 0)
  :config
  (after 'general
    (general-define-key
     :keymaps 'evil-normal-state-map
     (kbd "P") 'consult-yank-from-kill-ring
     (kbd "SPC `") 'consult-mark)))

(use-package embark
  :ensure t
  :commands (embark-act embark-act-all)
  :after ('general 'evil)
  :config
   (general-define-key :keymaps 'embark-collect-mode-map
                       "C-n" nil
                       "C-p" nil))

(use-package embark-consult
  :ensure t
  :after ('consult 'embark)
  :bind (("C-." . embark-act)))

(when (bound-and-true-p semantic-mode)
  (semantic-mode -1))
(setq completion-auto-help t)

;; better sorting for minibuffer completion
(use-package prescient
  :straight t
  :config (prescient-persist-mode 1))

(use-package corfu
  :straight t
  :custom
  ;; Auto completion is now safe since file-local variables are disabled
  (corfu-auto t)
  (corfu-cycle t) ; Enable cycling for completions
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match 'separator)
  (setq corfu-preview-current 'insert)
  (global-corfu-modes '((not org-mode) t))
  ;; Popupinfo settings for docstrings
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :bind (:map corfu-map
         ("C-n" . corfu-next)
         ("C-p" . corfu-previous)
         ("TAB" . corfu-next)
         ("<tab>" . corfu-next)
         ("S-TAB" . corfu-previous)
         ("<backtab>" . corfu-previous)
         ("RET" . corfu-complete)
         ("<escape>" . corfu-quit))
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode 1))

(use-package corfu-terminal
  :straight t
  :after corfu
  :config
  ;; Enable corfu-terminal for TTY support
  (unless (my-display-gui-p)
    (corfu-terminal-mode +1)))

;; Global pcomplete settings - less aggressive
(setq completion-ignore-case t)
(setq pcomplete-autolist nil)
(setq pcomplete-cycle-completions nil)


;; Disable ALL completion in git commit messages - it's useless there
(defun my-disable-completion-in-git-commit (&optional buffer)
  "Disable all completion in git-commit buffers.
If BUFFER is provided, operate on that buffer. Otherwise use current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq-local completion-at-point-functions nil)
    (when (bound-and-true-p company-mode)
      (company-mode -1))
    (when (bound-and-true-p corfu-mode)
      (corfu-mode -1))
    (message "Disabled completion in buffer %s" (buffer-name))))
(add-hook 'git-commit-mode-hook 'my-disable-completion-in-git-commit)

(use-package helpful)

(defun my-disable-completion (&optional buffer)
  "Disable all completion in buffer.
If BUFFER is provided, operate on that buffer. Otherwise use current buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq-local completion-at-point-functions nil)
    (when (bound-and-true-p company-mode)
      (company-mode -1))
    (when (bound-and-true-p corfu-mode)
      (corfu-mode -1))
    (message "Disabled completion in buffer %s" (buffer-name))))

(provide 'my-completion)
;;; my-completion.el ends here
