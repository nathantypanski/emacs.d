;; -*- lexical-binding: t; -*-

;; The package is python" but the mode is "python-mode":

(use-package python
  :ensure nil  ; builtin package
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . (lambda ()
                         (electric-indent-local-mode -1))))

(provide 'my-python)
