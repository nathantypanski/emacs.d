;; -*- lexical-binding: t; -*-

;; The package is python" but the mode is "python-mode":

(use-package python
  :ensure nil  ; builtin package
  :mode ("\\.py\\'" . python-mode)
  :hook ((python-mode . my-disable-insert-indent)
         (python-ts-mode . my-disable-insert-indent)
         (python-ts-mode . my-python-ts-mode-setup))
  :config
  ;; Configure python-ts-mode indentation
  (when (treesit-available-p)
    (setq python-ts-mode-indent-offset 4))

  ;; Setup function for python-ts-mode
  (defun my-python-ts-mode-setup ()
    "Configure python-ts-mode settings."
    (setq-local python-indent-offset 4)
    (setq-local python-indent-guess-indent-offset-verbose nil)
    (setq-local indent-tabs-mode nil)
    ;; Disable evil auto-indent for python-ts-mode
    (my-disable-insert-indent))

  ;; Debug function to check if insert indent is disabled
  (defun my-debug-python-indent ()
    "Debug python indentation settings."
    (interactive)
    (message "my-should-insert-indent: %s, major-mode: %s"
             my-should-insert-indent major-mode)))

(provide 'my-python)
