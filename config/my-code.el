;; my-code.el
;;
;; General stuff related to coding. Put more language-specific things
;; in their respective files, e.g.:
;;
;; * my-haskell.el
;; * my-c.el

(use-package semantic
  :ensure semantic
  :init
  (progn
    (global-semanticdb-minor-mode 1)
    )
  :config
  (progn
    (setq semanticdb-default-save-directory
        (expand-file-name "semanticdb" "~/.emacs.d/.semantic"))
    )
  )

(provide 'my-code)
