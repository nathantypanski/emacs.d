;;; my-completion.el --- flycheck customization -*- lexical-binding:t; -*-
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
                (if vertico-mode
                    #'consult-completion-in-region
                  #'completion--in-region))))


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
  :config
  (marginalia-mode))

(use-package consult
  :ensure t
  :demand t
  :bind
  (("C-s" . consult-line)
   ("M-y" . consult-yank-pop)
   ("C-x b" . consult-buffer))
  :custom
  (consult-async-min-input 0)
  :config
  (after 'evil
    (define-key evil-normal-state-map (kbd "P") 'consult-yank-from-kill-ring)
    (define-key evil-normal-state-map (kbd "SPC `") 'consult-mark)))

(use-package embark-consult
  :ensure t
  :after (consult)
  :bind (("C-." . embark-act)))

(when (bound-and-true-p semantic-mode)
  (semantic-mode -1))
(setq completion-auto-help t)

(use-package company
  :straight nil
  :ensure t
  :demand t
  :hook (prog-mode elisp-mode slime-mode)
  :custom
  (company-backends '(company-capf))
  (company-idle-delay nil)
  (company-minimum-prefix-length 1) ; keyboard-triggered
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-auto-commit nil)
  (company-auto-commit-chars nil)
  :config
  (define-key company-active-map (kbd "TAB")   #'company-select-next)
  (define-key company-active-map (kbd "<tab>") #'company-select-next)
  (define-key company-active-map (kbd "S-TAB") #'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") #'company-select-previous)
  (define-key company-active-map (kbd "RET")   #'company-complete-selection)

  ;; Disable company autopopup for completions if point is in a comment.
  (defun my-company-inhibit-in-comments (fun &rest args)
    "Inhibit company idle completion in comments."
    (if (nth 4 (syntax-ppss))
        ;; If inside a comment, require manual completion
        (let ((company-idle-delay nil))
          (apply fun args))
      ;; Else, follow normal behavior
      (apply fun args)))
  (advice-add 'company-idle-begin :around #'my-company-inhibit-in-comments)
  (global-company-mode -1))

;; better sorting for both minibuffer & company
(use-package prescient
  :straight t
  :config (prescient-persist-mode 1))

(use-package company-prescient
  :straight t
  :after (company prescient)
  :config (company-prescient-mode 1))

(use-package corfu
  :straight t
  :custom
  ;; Auto completion is now safe since file-local variables are disabled
  (corfu-auto t)
  (corfu-cycle t) ; Enable cycling for completions
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  (global-corfu-modes '((not org-mode) t))
  :config
  (global-corfu-mode))

(use-package corfu-terminal
  :straight t
  :after corfu
  :config
  ;; Enable corfu-terminal for TTY support
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

;; Global pcomplete settings - less aggressive
(setq pcomplete-ignore-case t)
(setq pcomplete-autolist nil)
(setq pcomplete-cycle-completions nil)

;; Disable ALL completion in git commit messages - it's useless there
(defun my-disable-completion-in-git-commit ()
  "Disable all completion in git-commit buffers."
  (setq-local completion-at-point-functions nil)
  (when (bound-and-true-p company-mode)
    (company-mode -1))
  (when (bound-and-true-p corfu-mode)
    (corfu-mode -1))
  (message "Disabled completion in git-commit buffer"))

(add-hook 'git-commit-mode-hook 'my-disable-completion-in-git-commit)

(provide 'my-completion)
;;; my-completion.el ends here
