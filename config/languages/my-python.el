
;; The package is python" but the mode is "python-mode":

(setq py-install-directory (concat user-emacs-directory "elisp/python-mode"))
(add-to-list 'load-path py-install-directory)

(defvar my-official-python-mode nil
  "Use official python mode when non-nil.")

(if my-official-python-mode
    (progn
      (setq python-indent-offset 4)
      (setq python-indent-trigger-commands nil))
  (progn
    (when (featurep 'python) (unload-feature 'python t))
    (autoload 'python-mode "python-mode" "Python editing mode." t)
    (use-package python-mode
      :commands python-mode
      :mode "\\.py\\'"
      :init
      (progn
        (add-to-list 'auto-mode-alist '("/PYDOCS\\'" . help-mode))
        (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
        (add-to-list 'interpreter-mode-alist '("python" . python-mode))
        )
      :config
      (progn
        (setq py-electric-comment-p nil)
        (setq py-max-help-buffer-p nil)
        (setq py-electric-comment-add-space-p nil)
        (setq py-tab-indent nil)
        (setq py-smart-indentation t)
        (setq py-return-key 'py-newline-and-indent)
        (setq py-complete-function nil)
        (setq py-empty-line-closes-p nil)
        (evil-define-key 'insert python-mode-map (kbd "RET") 'py-newline-and-indent)

        (defun py-shell-complete (&optional shell debug beg end word)
          "Silence python-mode's complete word before point function by overwriting."
          (interactive))
        ;; Set pylint from venv. See my question at:
        ;;
        ;; https://answers.launchpad.net/python-mode/+question/250108

        (defvar py-pylint-default (executable-find "pylint"))

        (defun my-set-pylint-from-venv ()
          "Change flycheck pylint executable to virtualenv executable"
          ;; virtualenv-name might be nil
          (when (and (boundp 'virtualenv-name)
                     (stringp virtualenv-name)
                     (virtualenv-p (py--normalize-directory
                                    virtualenv-name)))
            (let ((pylintpath
                   (concat (py--normalize-directory virtualenv-name)
                           "bin/pylint")))
              (setq flycheck-python-pylint-executable pylintpath))))

        ;; IMO the inverse is needed also
        (defun my-reset-pylint ()
          "Set flycheck `pylint' executable to default value. "
          (interactive)
          (setq flycheck-python-pylint-executable py-pylint-default))

        (defadvice virtualenv-activate (after my-set-pylint-from-venv activate)
          (my-set-pylint-from-venv))
        (ad-activate 'virtualenv-activate)

        (defadvice virtualenv-deactivate (after my-reset-pylint activate)
          (my-reset-pylint))
        (ad-activate 'virtualenv-deactivate)


        (defun my-python-nav-backward-end-of-block ()
          "Move to the end of the previous block."
          (interactive "^")
          (let ((old-point (point)))
            (python-nav-backward-block)
            (python-nav-end-of-block)
            ;; If we're at the same spot as before, call (python-nav-backward-block)
            ;; twice to get back to the *previous* previous block, then move to the
            ;; end of that.
            ;;
            ;; for some reason this reports point as incorrectly having moved one
            ;; position over when no movement happens.
            (if (= old-point (- (point) 1))
                (progn
                  (python-nav-backward-block 2)
                  (python-nav-end-of-block)))))
        (defun py-help-at-point (&optional debug)
          "Print help on symbol at point.

If symbol is defined in current buffer, jump to it's definition
Optional \\[universal-argument] used for debugging, will prevent deletion of temp file. "
          (interactive "P")
          ())
        )
      )
    ))



(use-package jedi
  :commands jedi:setup
  :ensure jedi
  :init
  (progn
    (add-hook 'python-mode-hook 'jedi:setup)
    )
  :config
  (progn
    (setq jedi:complete-on-dot t)
    (setq jedi:doc-mode 'help-mode.)
    (defun my-jump-to-python-docs (w)
      "Jump to a pane and do py-documentation"
      (interactive (list (let* ((word (thing-at-point 'word)))
                           word)))
      (jedi:show-doc)
      (switch-to-buffer-other-window "*jedi:doc*" t))

    (after 'evil
      (evil-define-key 'normal python-mode-map (kbd "K") 'my-jump-to-python-docs))
    (after 'evil-leader
      (evil-leader/set-key-for-mode 'python-mode "." 'jedi:goto-definition)
      )
    ))


(use-package ein
  :ensure ein
  :config (progn))

(provide 'my-python)
