;;(require 'google-c-style)
;;(c-add-style "Google" google-c-style t)

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
 ;;                       (c++-mode . "Google")
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

(use-package cedet
  :ensure cedet
  :config
  (progn
    (after 'evil
      (evil-define-key 'insert c-mode-map (kbd "TAB") 'c-indent-line-or-region)
      (evil-define-key 'normal c-mode-map (kbd "K") 'my-woman-entry)

      (evil-define-key 'insert c++-mode-map (kbd "<backspace>") 'c-electric-backspace)
      (evil-define-key 'normal c++-mode-map (kbd "SPC o") 'eassist-switch-h-cpp)
      )
    (semantic-mode)
    (after 'evil-leader
      ;; toggle between a function declaration and its implementation
      (evil-leader/set-key-for-mode 'c-mode   "d" 'semantic-analyze-proto-impl-toggle)
      (evil-leader/set-key-for-mode 'c++-mode "d" 'semantic-analyze-proto-impl-toggle)
      (evil-leader/set-key-for-mode 'c-mode   "." 'semantic-ia-fast-jump)
      (evil-leader/set-key-for-mode 'c++-mode "." 'semantic-ia-fast-jump)
      )
    (require 'eassist)
    ;; show semantic summary in minibuffer when I idle over a function
    (global-semantic-idle-summary-mode)
    )
    )

(provide 'my-c)
