;; Always eldoc in lispy modes.
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(use-package slime
  :ensure slime)

(use-package elisp-slime-nav
  :ensure elisp-slime-nav
  :commands my-jump-to-elisp-docs
  :diminish elisp-slime-nav-mode
  :init
  (progn
    (defun my-lisp-hook ()
      (progn
        (elisp-slime-nav-mode)
        (turn-on-eldoc-mode)
        )
      )
    (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
    (add-hook 'lisp-interaction-mode-hook 'my-lisp-hook)
    (add-hook 'ielm-mode-hook 'my-lisp-hook)
    (defun my-jump-to-elisp-docs (sym-name)
      "Jump to a pane and do elisp-slime-nav-describe-elisp-thing-at-point"
      (interactive (list (elisp-slime-nav--read-symbol-at-point)))
      (help-xref-interned (intern sym-name)))
    )
  :config
  (progn
    (after 'evil
      (evil-define-key 'normal emacs-lisp-mode-map (kbd "K")
        'my-jump-to-elisp-docs))))

(defun my-setup-elisp-mode ()
  "Commands to be run at the start of Emacs Lisp mode."
  (eldoc-mode)
  (my-coding-mode-eyecandy))

(add-hook 'emacs-lisp-mode-hook 'my-setup-elisp-mode)

(defun my-electric-lisp-comment ()
    "Autocomment things for lisp."
  (interactive)
  ;; we can get away with autocommenting on empty lines.
  ;; not so much on regular ones - that's more likely to be a mistake.
  (if (my-is-this-line-empty)
      (insert ";; ")
    (insert ";")))

(after 'evil
  (evil-define-key 'insert emacs-lisp-mode-map ";" 'my-electric-lisp-comment)
  (evil-define-key 'normal emacs-lisp-mode-map "\C-c\C-c" 'eval-defun))

(provide 'my-elisp)
