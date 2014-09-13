;; Always eldoc in lispy modes.
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(use-package elisp-slime-nav
  :ensure elisp-slime-nav
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
          (after 'company
            ;; slime-company completions
            (slime-setup '(slime-company))
            )
          (defun my-jump-to-elisp-docs (sym-name)
            "Jump to a pane and do elisp-slime-nav-describe-elisp-thing-at-point"
            (interactive (list (elisp-slime-nav--read-symbol-at-point)))
            (help-xref-interned (intern sym-name))
            (switch-to-buffer-other-window "*Help*" t))

          (after 'evil-leader
            (evil-leader/set-key-for-mode 'emacs-lisp-mode
              "." 'elisp-slime-nav-find-elisp-thing-at-point)
            )
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
  ;; we can get away with autocommenting on empty lines.
  ;; not so much on regular ones - that's more likely to be a mistake.
  (if (my-is-this-line-empty)
      (insert ";; ")
    (insert ";")))

(require 'lispy)
(use-package lispy
  :disabled t
  :init
  (progn
    (defun my-lispy-mode-enable ()
      "Enable lispy-mode."
      (lispy-mode 1)
      )
    (add-hook 'emacs-lisp-mode-hook 'my-lispy-mode-enable)
    (setq lispy-use-ctrl-digits nil)
    )
  :config
  )

(after 'evil
  (evil-define-key 'insert emacs-lisp-mode-map ";" 'my-electric-lisp-comment)
  (evil-define-key 'normal emacs-lisp-mode-map "\C-c\C-c" 'eval-defun)
  (use-package paredit
               :ensure paredit
               :config
               (progn
  (paredit-mode)
  (use-package evil-paredit
    :ensure evil-paredit
    :disabled t
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
    )))
  )
(provide 'my-elisp)
