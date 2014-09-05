(use-package ess
  :ensure ess
  :mode ("\\.R\\'" . R-mode)
  :config

  (progn
    (defadvice ess-help-mode (after ess-help activate) (read-only-mode))
    )
  )

(provide 'my-r)
