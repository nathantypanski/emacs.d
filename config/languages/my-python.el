
;; The package is python" but the mode is "python-mode":

(setq py-install-directory (concat user-emacs-directory "elisp/python-mode"))
(add-to-list 'load-path py-install-directory)
(require 'python-mode)

(use-package python-mode
  :commands python-mode
  :mode "\\.py\\'"
  :init
  (progn
    (when (featurep 'python) (unload-feature 'python t))
    (autoload 'python-mode "python-mode" "Python editing mode." t)
    (add-to-list 'auto-mode-alist '("/PYDOCS\\'" . help-mode))
    (setenv "PYTHONPATH"
            (shell-command-to-string ". ~/.zshrc; echo -n $PYTHONPATH")
            )
    )
  :config
  (progn
    (setq py-electric-comment-p 0)

    (setq py-electric-comment-add-space-p 1)
    (setq py-tab-indent nil)
    (setq py-return-key 'py-newline-and-indent)

    (defun my-python-maybe-indent ()
      "Indent to python-mode's computed indentation for empty lines,
       but do nothing for lines with content."
      (indent-to (if (empty-line-p) (py-compute-indentation) 0)))

    (setq py-complete-function 'ipython-complete)

    (after 'evil
        (evil-define-key 'insert python-mode-map (kbd "RET") 'py-newline-and-indent)

    (defun my-python-no-evil-indent ()
      "Remove indent hook from Evil insert"
      (after 'evil
        (progn
          (setq-local indent-line-function 'my-python-maybe-indent)
          ;; (bind-key (kbd "RET") 'evil-newline-indent)
          (evil-define-key 'insert python-mode-map (kbd "RET") 'py-newline-and-indent)
          )
        )
      )
    (add-hook 'python-mode-hook 'my-python-no-evil-indent))

    (defun my-disable-electric-indent ()
      "Disable electric indent. Buffer-local."
      (electric-indent-local-mode -1)
      )

    ;; Set pylint from venv. See my question at:
    ;;
    ;; https://answers.launchpad.net/python-mode/+question/250108

    (defvar py-pylint-default (executable-find "pylint"))

    (defun my-set-pylint-from-venv ()
      "Change flycheck pylint executable to virtualenv executable"
      ;; virtualenv-name might be nil
      (when (and (boundp 'virtualenv-name)(stringp virtualenv-name)
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

    (setq py-empty-line-closes-p nil)

    (add-hook 'python-mode-hook 'my-disable-electric-indent)

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
                (python-nav-end-of-block)
                )
          )
        )
    )

    ;;(use-package pylint
    ;;  :ensure pylint
    ;;  :init
    ;;  (progn
    ;;    (add-to-list 'flycheck-disabled-checkers 'python-flake8)
    ;;    (add-to-list 'flycheck-disabled-checkers 'python-pyflake)
    ;;    (add-hook 'python-mode-hook 'flycheck-mode)
    ;;    )
    ;;  :config
    ;;  (progn
    ;;    )
    ;;  )
    (use-package jedi
      :commands jedi:setup
      :ensure jedi
      :init
      (progn
        (add-hook 'python-mode-hook 'jedi:setup)
        (add-hook 'python-mode-hook 'jedi:ac-setup)
        )
      :config
      (progn
        (setq jedi:complete-on-dot t)
        (defun my-jump-to-python-docs (w)
          "Jump to a pane and do py-documentation"
          (interactive (list (let* ((word (thing-at-point 'word)))
                               word)))
          (jedi:show-doc)
          (switch-to-buffer-other-window "*jedi:doc*" t)
          )

        (after 'evil
          (evil-define-key 'normal python-mode-map (kbd "K") 'my-jump-to-python-docs))

        )
      )
    ))

(provide 'my-python)
