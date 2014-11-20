(require 'google-c-style)

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c++-mode . "google")
                        (other . "linux")))



(defun my-set-evil-shift-width ()
  "Set Evil's shift width for C editing."
  (after 'evil
    (setq evil-shift-width 8)))

(add-hook 'c-initialization-hook 'my-set-evil-shift-width)

(setq c-hungry-delete-key t)

(defun my-c-mode-common-setup ()
  "Setup C/C++-mode common configurations."
  (c-set-offset 'case-label '+))

(add-hook 'c-mode-common-hook 'my-c-mode-common-setup)

(after 'evil
    (evil-define-key 'insert c-mode-map (kbd "TAB") 'c-indent-line-or-region)
    (evil-define-key 'normal c-mode-map (kbd "K")   'my-woman-entry)
    (evil-define-key 'insert c-mode-map (kbd "<backspace>") 'backward-delete-char-untabify)
    (evil-define-key 'normal c++-mode-map (kbd "K")   'my-woman-entry)
    (evil-define-key 'insert c++-mode-map (kbd "<backspace>") 'backward-delete-char-untabify))

(use-package semantic
  :ensure semantic
  :commands
  (semantic-ia-fast-jump
   semantic-complete-jump-local
   semantic-complete-jump
   semantic-complete-jump-local-members
   semantic-symref-symbol
   semantic-symref
   semantic-complete-analyze-inline
   senator-kill-tag
   senator-copy-tag
   senator-yank-tag
   senator-copy-tag-to-register
   semantic-force-refresh
   senator-transpose-tags-up
   senator-transpose-tags-down
   semantic-analyze-possible-completions
   semantic-mode)
  :init
  (progn
    (global-semanticdb-minor-mode 1))
  :config
  (progn
    (setq semanticdb-default-save-directory
        (expand-file-name "semanticdb" "~/.emacs.d/.semantic"))))

(use-package cedet
  :ensure cedet
  :commands (eassist-switch-h-cpp)
  :init
  (progn
    (after 'evil
        (evil-define-key 'normal c++-mode-map (kbd "SPC o") 'eassist-switch-h-cpp)
        (evil-define-key 'normal c-mode-map   (kbd "SPC o") 'eassist-switch-h-c)
        (evil-set-initial-state 'eieio-custom-mode 'emacs)))
  :config
  (progn
    (require 'eassist)
    (semantic-mode)
    (semanticdb-enable-gnu-global-databases 'c-mode)
    ;; show semantic summary in minibuffer when I idle over a function
    (global-semantic-idle-summary-mode)))

(provide 'my-c)
