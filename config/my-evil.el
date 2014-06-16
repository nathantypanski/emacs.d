;; my-evil.el
;;
;; Configuration for ostracizing me from both the Emacs and Vim communities.

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

    (use-package evil-leader
      :commands (evil-leader-mode)
      :ensure evil-leader
      :demand evil-leader
      :init
      (global-evil-leader-mode)
      :config
      (progn
	(evil-leader/set-leader ",")
	(evil-leader/set-key
	  "w"     'save-buffer
	  "e"     (kbd "C-x C-e")
	  "E"     (kbd "C-M-x")
	  "C"     'customize-group
	  "b d"   'kill-this-buffer
	  "v"     (kbd "C-w v C-w l")
	  "s"     (kbd "C-w s C-w j")
	  "g s"   'magit-status
	  "g b"   'magit-status
	  "g l"   'magit-log
	  "g d"   'vc-diff
	  "P"     'package-list-packages
	  "h"     help-map
	  "h h"   'help-for-help-internal)
	)
      )

    (use-package evil-nerd-commenter
      :ensure evil-nerd-commenter
      :config
      ;; Type 'gcc' to comment the current line.
      (progn (setq evilnc-hotkey-comment-operator "gc")
             )
      )
    (use-package evil-matchit
      :ensure evil-matchit
      :commands evilmi-jump-items
      :init
      (progn
        (define-key evil-normal-state-map "%" 'evilmi-jump-items))
      )
    (use-package surround
      :ensure surround
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
    (evil-set-initial-state 'magit-mode 'emacs)
    (evil-set-initial-state 'comint-mode 'emacs)
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
    (define-key evil-normal-state-map (kbd "RET") 'my-insert-and-indent)
    (define-key evil-normal-state-map (kbd "<S-return>") 'my-append-and-indent)

    (defun my-delete-trailing-whitespace-at-point ()
      "Delete trailing whitespace on the current line only."
      (let ((begin (line-beginning-position))
            (end   (line-end-position)))
        (delete-trailing-whitespace begin end)
        ))

    ;; exiting insert mode -> delete trailing whitespace
    (add-hook 'evil-insert-state-exit-hook 'my-delete-trailing-whitespace-at-point)

    ;; entering insert mode -> indent according to mode
    ;; (add-hook 'evil-insert-state-entry-hook 'indent-according-to-mode)
    ;; Normal Evil bindings
    (define-key evil-insert-state-map (kbd "<S-backspace>")
      'my-backward-delete-word)

    (define-key evil-normal-state-map (kbd ", k") 'kill-buffer)
    (define-key evil-normal-state-map (kbd "SPC a") 'ag)
    (define-key evil-normal-state-map (kbd "SPC A") 'apropos)

    (define-key evil-normal-state-map (kbd "SPC F") 'ffap)
    (define-key evil-visual-state-map (kbd "SPC F") 'ffap)

    (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
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

    (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

    (define-key evil-visual-state-map (kbd ", e") 'eval-region)

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
)

(provide 'my-evil)
