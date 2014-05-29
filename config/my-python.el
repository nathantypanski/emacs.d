
;; The package is python" but the mode is "python-mode":


(use-package python-mode
  :mode ("\\.py\\'" . python3-mode)
  :init
  (progn
    (when (featurep 'python) (unload-feature 'python t))
    (autoload 'python-mode "python3-mode" "Python editing mode." t)
    )
  :config
  (progn
    (use-package python-pylint
      :ensure python-pylint
      :init
      (progn
        (add-to-list 'flycheck-disabled-checkers 'python-flake8)
        (add-to-list 'flycheck-disabled-checkers 'python-pyflake)
        (setq flycheck-python-pylint-executable "pylint3")
        ;(add-hook 'python3-mode-hook 'flycheck-mode)
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
