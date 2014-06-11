
;; The package is python" but the mode is "python-mode":

(setq py-install-directory (concat user-emacs-directory "elisp/python-mode"))
(add-to-list 'load-path py-install-directory)
(require 'python-mode)

(use-package python-mode
  :mode ("\\.py\\'" . python3-mode)
  :init
  (progn
    (when (featurep 'python) (unload-feature 'python t))
    (autoload 'python-mode "python3-mode" "Python editing mode." t)
    )
  :config
  (progn
    (setq py-electric-comment-p 1)
    (setq py-electric-comment-add-space-p 1)
    (setq py-tab-indent nil)
    (setq py-return-key 'newline)
    (setq py-complete-function 'ipython-complete)
    (define-key python-mode-map (kbd "RET") 'py-newline-and-indent)
    (defun my-python-maybe-indent ()
      "Indent to python-mode's computed indentation for empty lines,
       but do nothing for lines with content."
      (indent-to (if (empty-line-p) (py-compute-indentation) 0)))

    (defun my-python-no-evil-indent ()
      "Remove indent hook from Evil insert"
      (after 'evil
        (progn
          (setq-local indent-line-function 'my-python-maybe-indent)
          (bind-key (kbd "RET") 'evil-newline-indent)
          )
        )
      )

    (defun my-disable-electric-indent ()
      "Disable electric indent. Buffer-local."
      (electric-indent-local-mode -1)
      )

    (setq py-empty-line-closes-p nil)

    (add-hook 'python-mode-hook 'my-python-no-evil-indent)
    (add-hook 'python-mode-hook 'my-disable-electric-indent)

    (use-package python-pylint
      :ensure python-pylint
      :init
      (progn
        (add-to-list 'flycheck-disabled-checkers 'python-flake8)
        (add-to-list 'flycheck-disabled-checkers 'python-pyflake)
        (setq flycheck-python-pylint-executable "pylint3")
        (add-hook 'python-mode-hook 'flycheck-mode)
        )
      :config
      (progn
        )
      )
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
        (setq jedi:environment-root "jedi")  ; or any other name you like
        (setq jedi:environment-virtualenv
              (append python-environment-virtualenv
                      '("--python" "/usr/bin/python3")))
        )
      )
    )
  )
(provide 'my-python)
