;; Always eldoc in lispy modes. -*- lexical-binding: t; -*-

(require 'my-functions)

(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(when (not (display-graphic-p))
  (setq eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p nil
        eldoc-echo-area-prefer-doc-buffer t))

(use-package slime
  :straight nil
  :ensure slime)

(use-package elisp-slime-nav
  :after slime
  :ensure elisp-slime-nav
  :commands my-jump-to-elisp-docs
  :diminish elisp-slime-nav-mode
  :init
    (defun my-lisp-hook ()
        (elisp-slime-nav-mode)
        (turn-on-eldoc-mode))
    (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
    (add-hook 'lisp-interaction-mode-hook 'my-lisp-hook)
    (add-hook 'ielm-mode-hook 'my-lisp-hook)
    (defun my-jump-to-elisp-docs (sym-name)
      "Jump to a pane and do elisp-slime-nav-describe-elisp-thing-at-point"
      (interactive (list (elisp-slime-nav--read-symbol-at-point)))
      (help-xref-interned (intern sym-name))))

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

(use-package slime-company
  :straight t
  :after (slime company)
  :custom
  (slime-company-completion 'fuzzy)
  (slime-company-after-completion 'slime-company-just-one-space))

(provide 'my-elisp)
