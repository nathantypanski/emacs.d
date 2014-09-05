(use-package clojure-mode
  :ensure clojure-mode
  :config
  (progn
    )
)

(use-package cider
  :ensure cider
  :config
  (progn
    ;; always eldoc in cider mode
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-mode-hook '(paredit-mode +1))

    (use-package ac-nrepl
      :ensure ac-nrepl)

    (after 'evil
    (defun my-evil-cider-repl-insert ()
      "Enter insert mode at the prompt, if we're behind the prompt."
      (interactive)
      (if (> cider-repl-input-start-mark (point))
          (goto-char cider-repl-input-start-mark))
      (evil-insert-state))

    (defun my-evil-cider-repl-append ()
      "Enter insert mode at the prompt, if we're behind the prompt."
      (interactive)
      (if (> cider-repl-input-start-mark (point))
          (goto-char cider-repl-input-start-mark))
      (evil-append))

    (defun my-cider-on-prompt-line ()
      "Returns t if point is on the same line as the current prompt."
      (my-same-line (point) (cider-repl-input-start-mark)))

    (defun my-evil-cider-repl-bol ()
      "Perform cider-bol, but never go before the prompt."
      (interactive)
      (cider-bol)
      (if (> cider-repl-input-start-mark (point))
          (goto-char cider-repl-input-start-mark)))

    ;; http://blog.jenkster.com/2013/12/a-cider-excursion.html
    (defun cider-namespace-refresh ()
     "Reset the Clojure namespace."
      (interactive)
      (cider-interactive-eval
       "(require 'clojure.tools.namespace.repl)
        (clojure.tools.namespace.repl/refresh)"))


    (setq nrepl-hide-special-buffers t)
    (evil-define-key 'insert cider-repl-mode-map
        (kbd "<up>") 'cider-repl-previous-input
        (kbd "<down>") 'cider-repl-next-input
        (kbd "C-c C-d") 'cider-doc-map
        (kbd "M-.") 'cider-jump
        (kbd "M-,") 'cider-jump-back
        (kbd "C-c M-.") 'cider-jump-to-resource
        (kbd "RET") 'cider-repl-return
        (kbd "TAB") 'cider-repl-tab
        (kbd "C-<return>") 'cider-repl-closing-return
        (kbd "C-j") 'cider-repl-newline-and-indent
        (kbd "C-c C-o") 'cider-repl-clear-output
        (kbd "C-c M-o") 'cider-repl-clear-buffer
        (kbd "C-c M-n") 'cider-repl-set-ns
        (kbd "C-c C-u") 'cider-repl-kill-input
        (kbd "C-a") 'cider-repl-bol
        (kbd "C-S-a") 'cider-repl-bol-mark
        [home] 'cider-repl-bol
        [S-home] 'cider-repl-bol-mark
        (kbd "C-<up>") 'cider-repl-backward-input
        (kbd "C-<down>") 'cider-repl-forward-input
        (kbd "M-p") 'cider-repl-previous-input
        (kbd "M-n") 'cider-repl-next-input
        (kbd "M-r") 'cider-repl-previous-matching-input
        (kbd "M-s") 'cider-repl-next-matching-input
        (kbd "C-c C-n") 'cider-repl-next-prompt
        (kbd "C-c C-p") 'cider-repl-previous-prompt
        (kbd "C-c C-b") 'cider-interrupt
        (kbd "C-c C-c") 'cider-interrupt
        (kbd "C-c C-m") 'cider-macroexpand-1
        (kbd "C-c M-m") 'cider-macroexpand-all
        (kbd "C-c C-z") 'cider-switch-to-last-clojure-buffer
        (kbd "C-c M-s") 'cider-selector
        (kbd "C-c M-f") 'cider-load-fn-into-repl-buffer
        (kbd "C-c C-q") 'cider-quit
        (kbd "C-c M-i") 'cider-inspect
        (kbd "C-c M-t") 'cider-toggle-trace
        (kbd "C-c C-x") 'cider-refresh
        (kbd "C-x C-e") 'cider-eval-last-sexp
        (kbd "C-c C-r") 'cider-eval-region
        )
    (evil-define-key 'visual cider-repl-mode-map
        (kbd "0") 'cider-repl-bol
      )
    (evil-define-key 'normal cider-repl-mode-map
        (kbd "i") 'my-evil-cider-repl-insert
        (kbd "<up>") 'cider-repl-previous-input
        (kbd "<down>") 'cider-repl-next-input
        (kbd "C-c C-d") 'cider-doc-map
        (kbd "M-.") 'cider-jump
        (kbd "M-,") 'cider-jump-back
        (kbd "C-c M-.") 'cider-jump-to-resource
        (kbd "RET") 'cider-repl-return
        (kbd "TAB") 'cider-repl-tab
        (kbd "C-<return>") 'cider-repl-closing-return
        (kbd "C-j") 'cider-repl-newline-and-indent
        (kbd "C-c C-o") 'cider-repl-clear-output
        (kbd "C-c M-o") 'cider-repl-clear-buffer
        (kbd "C-c M-n") 'cider-repl-set-ns
        (kbd "C-c C-u") 'cider-repl-kill-input
        (kbd "C-a") 'cider-repl-bol
        (kbd "C-S-a") 'cider-repl-bol-mark
        (kbd "0") 'cider-repl-bol
        ;; [S-home] 'cider-repl-bol-mark
        (kbd "C-<up>") 'cider-repl-backward-input
        (kbd "C-<down>") 'cider-repl-forward-input
        (kbd "M-p") 'cider-repl-previous-input
        (kbd "M-n") 'cider-repl-next-input
        (kbd "M-r") 'cider-repl-previous-matching-input
        (kbd "M-s") 'cider-repl-next-matching-input
        (kbd "C-c C-n") 'cider-repl-next-prompt
        (kbd "C-c C-p") 'cider-repl-previous-prompt
        (kbd "C-c C-b") 'cider-interrupt
        (kbd "C-c C-c") 'cider-interrupt
        (kbd "C-c C-m") 'cider-macroexpand-1
        (kbd "C-c M-m") 'cider-macroexpand-all
        (kbd "C-c C-z") 'cider-switch-to-last-clojure-buffer
        (kbd "C-c M-s") 'cider-selector
        (kbd "C-c M-f") 'cider-load-fn-into-repl-buffer
        (kbd "C-c C-q") 'cider-quit
        (kbd "C-c M-i") 'cider-inspect
        (kbd "C-c M-t") 'cider-toggle-trace
        (kbd "C-c C-x") 'cider-refresh
        (kbd "C-x C-e") 'cider-eval-last-sexp
        (kbd "C-c C-r") 'cider-eval-region
        )
    ))
)
(provide 'my-clojure)
