(after 'comint
  (define-key comint-mode-map [up] 'comint-previous-input)
  (define-key comint-mode-map [down] 'comint-next-input))
(use-package multi-term
  :commands multi-term
  :init (progn
  (setq multi-term-buffer-name "term"
        multi-term-program "/bin/zsh")
          )
  :commands
  (progn)
)
(provide 'my-term)
