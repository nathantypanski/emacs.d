(require 'google-c-style)
(c-add-style "Google" google-c-style t)

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c++-mode . "Google")
                        (other . "linux")))

(defun my-set-evil-shift-width ()
  "Set Evil's shift width for C editing."
  (after 'evil
    (setq evil-shift-width 8)
    )
  )

(add-hook 'c-initialization-hook 'my-set-evil-shift-width)

(c-set-offset 'case-label '+)
(setq c-hungry-delete-key t)

(use-package cedet
  :ensure cedet
  :config
  (progn
    (after 'evil
      (evil-define-key 'insert c-mode-map (kbd "TAB") 'c-indent-line-or-region)
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
    (evil-define-key 'normal c++-mode-map (kbd "SPC o") 'eassist-switch-h-cpp)
    ;; show semantic summary in minibuffer when I idle over a function
    (global-semantic-idle-summary-mode)
    ))

(after 'ac-etags
  ;; ac-etags setup for C code.
  ;; See ~/.emacs.d/config/my-autocomplete.el
  (defun my-c-mode-common-hook ()
    (add-to-list 'ac-sources 'ac-source-etags))
  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
  )
(after 'evil
  (evil-define-key 'insert c++-mode-map (kbd "<backspace>") 'c-electric-backspace)

(provide 'my-c)
