(use-package nlinum
  :ensure nlinum
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'nlinum-mode)
    ))
