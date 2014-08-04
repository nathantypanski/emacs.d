;; Always eldoc in lispy modes.
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(use-package elisp-slime-nav
  :ensure elisp-slime-nav
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :commands my-jump-to-elisp-docs
  :diminish elisp-slime-nav-mode
  :init (progn
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
            (help-xref-interned (intern sym-name))
            (switch-to-buffer-other-window "*Help*" t))
          )
  :config
  (progn
    (after 'evil
      (evil-define-key 'normal emacs-lisp-mode-map (kbd "K")
        'my-jump-to-elisp-docs)
      )
    )
  )

(defun my-electric-lisp-comment ()
    "Autocomment things for lisp."
  (interactive)
  (insert ";; "))

(after 'evil
  (evil-define-key 'insert emacs-lisp-mode-map ";" 'my-electric-lisp-comment)
  (evil-define-key 'normal emacs-lisp-mode-map "\C-c\C-c" 'eval-defun)
  (paredit-mode)
  (use-package evil-paredit
    :ensure evil-paredit
    :commands enable-paredit-mode
    :init
    (progn
      (autoload 'enable-paredit-mode "paredit"
        "Turn on pseudo-structural editing of Lisp code." t)
      (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
      (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
      (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
      (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
      (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
      (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
      )
    :config
    (progn
      (evil-define-key 'normal emacs-lisp-mode-map "\M-q" 'paredit-reindent-defun)
      )
    )
  )
(provide 'my-elisp)
