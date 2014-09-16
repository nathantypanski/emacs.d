(require 'google-c-style)
;;(c-add-style "Google" google-c-style t)

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
;;                        (c++-mode . "Google") ;; makes things bug
                        (other . "linux")))

(defun my-set-evil-shift-width ()
  "Set Evil's shift width for C editing."
  (after 'evil
    (setq evil-shift-width 8)
    )
  )

(add-hook 'c-initialization-hook 'my-set-evil-shift-width)

(setq c-hungry-delete-key t)

(defun my-c-mode-common-hook ()
  (c-set-offset 'case-label '+)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(after 'evil
    (evil-define-key 'insert c-mode-map (kbd "TAB") 'c-indent-line-or-region)
    (evil-define-key 'normal c-mode-map (kbd "K")   'my-woman-entry)
    (evil-define-key 'insert c-mode-map (kbd "<backspace>") 'c-electric-backspace)
    (evil-define-key 'normal c++-mode-map (kbd "K")   'my-woman-entry)
    (evil-define-key 'insert c++-mode-map (kbd "<backspace>") 'c-electric-backspace)
)

(use-package semantic
  :disabled t
  :ensure semantic
  ;; :commands
  ;; (semantic-ia-fast-jump
  ;;  semantic-complete-jump-local
  ;;  semantic-complete-jump
  ;;  semantic-complete-jump-local-members
  ;;  semantic-symref-symbol
  ;;  semantic-symref
  ;;  semantic-complete-analyze-inline
  ;;  senator-kill-tag
  ;;  senator-copy-tag
  ;;  senator-yank-tag
  ;;  senator-copy-tag-to-register
  ;;  semantic-force-refresh
  ;;  senator-transpose-tags-up
  ;;  senator-transpose-tags-down
  ;;  semantic-analyze-possible-completions
  ;;  semantic-mode)
  :init
  (progn
    (global-semanticdb-minor-mode 1)
    )
  :config
  (progn
    (setq semanticdb-default-save-directory
        (expand-file-name "semanticdb" "~/.emacs.d/.semantic"))
    )
  )

(use-package cedet
  :disabled t
  :ensure cedet
  :commands (
             eassist-switch-h-cpp
             )
  :init
  (progn
    (after 'evil
        (evil-define-key 'normal c++-mode-map (kbd "SPC o") 'eassist-switch-h-cpp)
        (evil-define-key 'normal c-mode-map   (kbd "SPC o") 'eassist-switch-h-c)
    ))
  :config
  (progn
    (require 'eassist)
    (semantic-mode)
    (semanticdb-enable-gnu-global-databases 'c-mode)
    ;; show semantic summary in minibuffer when I idle over a function
    (global-semantic-idle-summary-mode)
    )
    )

(use-package helm-gtags
  :disabled t
  :ensure helm-gtags
  :commands (
             helm-gtags-mode
             helm-gtags-find-tag
             helm-gtags-find-rtag
             helm-gtags-find-symbol
             helm-gtags-parse-file
             helm-gtags-previous-history
             helm-gtags-next-history
             helm-gtags-pop-stack
             )
  :config
  :init
  (progn
       ;; (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
       ;; (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
       ;; (define-key helm-gtags-mode-map (kbd "M-s")    'helm-gtags-find-symbol)
       ;; (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
       ;; (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
       ;; (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
       ;; (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
        (add-hook 'c-mode-hook 'helm-gtags-mode)
        (add-hook 'c++-mode-hook 'helm-gtags-mode)
        (add-hook 'asm-mode-hook 'helm-gtags-mode)
    ))

(provide 'my-c)
