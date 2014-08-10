;; my-evil.el
;;
;; Configuration for ostracizing me from both the Emacs and Vim communities.

(use-package evil-leader
      :commands (evil-leader-mode)
      :ensure evil-leader
      :demand evil-leader
      :init
      (global-evil-leader-mode)
      :config
      (progn
	(evil-leader/set-leader ",")
        (evil-leader/set-key "w" 'save-buffer)
        (evil-leader/set-key "q" 'kill-buffer-and-window)
        (evil-leader/set-key "e" 'pp-eval-last-sexp)
        (evil-leader/set-key "g s" 'magit-status)
        (evil-leader/set-key "g l" 'magit-log)
        (evil-leader/set-key "g d" 'magit-diff)
        (evil-leader/set-key "h" 'dired-jump)
        (evil-leader/set-key "v" 'split-window-right)
        (evil-leader/set-key "e" 'pp-eval-last-sexp)
        (evil-leader/set-key "TAB" 'my-hop-around-buffers)
        (evil-leader/set-key "," 'other-window)
        (evil-leader/set-key "a" 'ag-regexp)
        (evil-leader/set-key "f" 'dired-jump)
        (evil-leader/set-key "F" 'helm-find-files)
        (evil-leader/set-key "B" 'helm-buffers-list)
        (evil-leader/set-key "x" 'helm-M-x)
        (evil-leader/set-key "b" 'ibuffer)
        )
      )
(use-package evil
  :ensure evil
  :config
  (progn
    (evil-mode 1)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-w-in-emacs-state t)
    (setq evil-search-module 'isearch)
    (setq evil-magic 'very-magic)
    (setq evil-emacs-state-cursor '("#dfaf8f" box))
    (setq evil-normal-state-cursor '("#f8f893" box))
    (setq evil-insert-state-cursor '("#f8f893" bar))
    (setq evil-replace-state-cursor '("#cc9393" box))
    (setq evil-want-fine-undo t)

    (defun my-hop-around-buffers ()
      "Swap the current buffer with the previous one."
      (interactive)
      (switch-to-buffer (other-buffer)))

    (use-package evil-nerd-commenter
      :ensure evil-nerd-commenter
      :config
      (progn
        (define-key evil-normal-state-map (kbd "gc") 'evilnc-comment-or-uncomment-lines)
        )

      (use-package evil-matchit
        :ensure evil-matchit
        :commands evilmi-jump-items
        :init
        (progn
          (define-key evil-normal-state-map "%" 'evilmi-jump-items))
        )
      (use-package surround
        :ensure evil-surround
        :config
        (progn
          (global-surround-mode 1)
          )
        )

      (evil-set-initial-state 'flycheck-error-list-mode 'emacs)
      (evil-set-initial-state 'package-men-mode 'emacs)
      (evil-set-initial-state 'help-mode 'emacs)
      (evil-set-initial-state 'eshell-mode 'emacs)
      (evil-set-initial-state 'shell-mode 'emacs)
      (evil-set-initial-state 'esup-mode 'emacs)
      (evil-set-initial-state 'diff-mode 'emacs)
      (evil-set-initial-state 'haskell-interactive-mode 'emacs)
      (evil-set-initial-state 'term-mode 'emacs)
      (evil-set-initial-state 'multi-term-mode 'emacs)

      (use-package key-chord
        :ensure key-chord
        :diminish key-chord-mode
        :config
        (progn
          (key-chord-mode 1)
          )
        )

      (evil-define-text-object my-evil-next-match (count &optional beg end type)
        "Select next match."
        (evil-ex-search-previous 1)
        (evil-ex-search-next count)
        (list evil-ex-search-match-beg evil-ex-search-match-end))

      (evil-define-text-object my-evil-previous-match (count &optional beg end type)
        "Select previous match."
        (evil-ex-search-next 1)
        (evil-ex-search-previous count)
        (list evil-ex-search-match-beg evil-ex-search-match-end))

      (define-key minibuffer-local-map [escape] 'my-minibuffer-keyboard-quit)
      (define-key minibuffer-local-ns-map [escape] 'my-minibuffer-keyboard-quit)
      (define-key minibuffer-local-completion-map [escape] 'my-minibuffer-keyboard-quit)
      (define-key minibuffer-local-must-match-map [escape] 'my-min )

      (defun my-append-and-indent ()
        "Moves to end of line, enters insert mode, and also indents the line."
        (interactive)
        (evil-append-line 0)
        (indent-according-to-mode)
        )

      (define-key evil-insert-state-map (kbd "RET") 'evil-ret-and-indent)
      (define-key evil-normal-state-map (kbd "RET") 'my-append-and-indent)
      (define-key evil-normal-state-map (kbd "<S-return>") 'my-append-and-indent)

      (defun my-delete-trailing-whitespace-at-point ()
        "Delete trailing whitespace on the current line only."
        (let ((begin (line-beginning-position))
              (end   (line-end-position)))
          (delete-trailing-whitespace begin end)
          ))

      ;; exiting insert mode -> delete trailing whitespace
      (remove-hook 'evil-insert-state-exit-hook 'my-delete-trailing-whitespace-at-point)

      ;; entering insert mode -> indent according to mode
      ;; (add-hook 'evil-insert-state-entry-hook 'indent-according-to-mode)
      ;; Normal Evil bindings
      (define-key evil-insert-state-map (kbd "<S-backspace>")
        'my-backward-delete-word)

      (define-key evil-normal-state-map (kbd "SPC a") 'ag)
      (define-key evil-normal-state-map (kbd "SPC A") 'apropos)
      (define-key evil-normal-state-map (kbd "SPC X") 'helm-M-x)

      (define-key evil-normal-state-map (kbd "C-p") 'fiplr-find-file)
      (define-key evil-normal-state-map (kbd "C-q") 'universal-argument)

      (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
      (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
      (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
      (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

      (define-key evil-normal-state-map (kbd "-") (kbd "dd"))

      (define-key evil-normal-state-map "j" 'evil-next-visual-line)
      (define-key evil-normal-state-map "k" 'evil-previous-visual-line)
      (define-key evil-normal-state-map "$" 'my-smart-end)
      (define-key evil-normal-state-map "0" 'my-smart-home)

      (define-key evil-motion-state-map "j" 'evil-next-visual-line)
      (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
      (define-key evil-motion-state-map "$" 'evil-end-of-line)
      (define-key evil-motion-state-map "0" 'evil-beginning-of-line)

      (define-key evil-normal-state-map "/" 'evil-search-forward)
      (define-key evil-normal-state-map (kbd "SPC /") 'helm-swoop)
      (define-key evil-motion-state-map "/" 'evil-search-forward)
      (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))


      ;; butter fingers
      (evil-ex-define-cmd "Q"  'evil-quit)
      (evil-ex-define-cmd "Qa" 'evil-quit-all)
      (evil-ex-define-cmd "QA" 'evil-quit-all)
      (evil-add-hjkl-bindings package-menu-mode-map 'emacs
        "h" 'evil-backward-word-begin
        "l" 'evil-forward-word-begin
        "/" 'evil-search-forward
        "?" 'evil-search-backward
        ))

    (defun my-evil-modeline-change (default-color)
      "changes the modeline color when the evil mode changes"
      (let ((color (cond ((evil-insert-state-p) '("#002233" . "#ffffff"))
                         ((evil-visual-state-p) '("#330022" . "#ffffff"))
                         ((evil-normal-state-p) default-color)
                         (t '("#440000" . "#ffffff")))))
        (set-face-background 'mode-line (car color))
        (set-face-foreground 'mode-line (cdr color))))
))

(provide 'my-evil)
