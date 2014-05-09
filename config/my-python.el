
;; The package is python" but the mode is "python-mode":


(use-package python-mode
  :mode ("\\.py\\'" . python3-mode)
  :init
  (progn
    (when (featurep 'python) (unload-feature 'python t))

    )
  :config
  (progn
    (autoload 'python-mode "python-mode" "Python editing mode." t)
    (use-package flycheck-pyflakes
      :ensure flycheck-pyflakes
      :commands python-mode
      :init
      (progn (add-hook 'python-mode-hook 'flycheck-mode)
             (add-to-list 'flycheck-disabled-checkers 'python-flake8)
             (add-to-list 'flycheck-disabled-checkers 'python-pylint)
      )
      :config
      (progn
        ))
    (add-hook 'python-mode-hook 'flycheck-mode)
    (use-package jedi
      :ensure jedi
      :init
      (progn
        (add-hook 'python-mode-hook 'jedi:setup)
        )
      :config
      (progn
        (setq jedi:complete-on-dot t)
        )
      )
    )
  )

(provide 'my-python)
