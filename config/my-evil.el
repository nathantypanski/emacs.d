;; my-evil.el
;;
;; Configuration for ostracizing me from both the Emacs and Vim communities.

(use-package evil-leader
  :commands (evil-leader-mode global-evil-leader-mode)
  :ensure evil-leader
  :demand evil-leader
  :init
  (progn
    (evil-leader/set-leader ",")
    (global-evil-leader-mode t)
    )
  :config
  )

(use-package evil
  :ensure evil
  :config
  (progn
    (evil-mode 1)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-w-in-emacs-state t)
    (setq evil-search-module        'isearch)
    (setq evil-magic                'very-magic)
    (setq evil-emacs-state-cursor   '("#dfaf8f" box))
    (setq evil-normal-state-cursor  '("#f8f893" box))
    (setq evil-insert-state-cursor  '("#f8f893" bar))
    (setq evil-replace-state-cursor '("#cc9393" box))
    (setq evil-want-fine-undo t)
    (setq evil-want-change-word-to-end t)

    (use-package evil-nerd-commenter
      :ensure evil-nerd-commenter
      :commands (evilnc-comment-or-uncomment-lines))

      (use-package evil-matchit
        :ensure evil-matchit
        :commands evilmi-jump-items
        :init
        (progn
          (setq global-evil-matchit-mode t)
          (define-key evil-normal-state-map "%" 'evilmi-jump-items))
        )
      (use-package evil-surround
        :ensure evil-surround
        :config
        (progn
          (global-evil-surround-mode 1)
          )
        )

      (evil-set-initial-state 'flycheck-error-list-mode 'normal)
      (evil-set-initial-state 'git-commit-mode 'insert)
      (evil-set-initial-state 'shell-mode 'emacs)
      (evil-set-initial-state 'esup-mode 'emacs)
      (evil-set-initial-state 'diff-mode 'emacs)
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
      (define-key minibuffer-local-must-match-map [escape] 'my-minibuffer-keyboard-quit)

      (defun my-append-and-indent ()
        "Moves to end of line, enters insert mode, and also indents the line."
        (interactive)
        (evil-append-line 0)
        (indent-according-to-mode)
        )

      (define-key evil-insert-state-map (kbd "RET")        'evil-ret-and-indent)
      (define-key evil-normal-state-map (kbd "RET")        'my-append-and-indent)
      (define-key evil-normal-state-map (kbd "<S-return>") 'my-append-and-indent)
      (define-key evil-normal-state-map (kbd "C-w }") 'evil-window-rotate-downwards)
      (define-key evil-normal-state-map (kbd "C-w {") 'evil-window-rotate-upwards)

      (defun my-what-line ()
        "Get the line, without printing the word 'line' before it."
        (1+ (count-lines 1 (point)))
        )

      (defun my-where-beginning-of-visual-line ()
        "Calculate the difference between the beginning
of the current visual line and point."
        (interactive)
        (let ((old-point (point))
              (bovl (save-excursion (beginning-of-visual-line)
                                    (point))))
          (- old-point bovl)))

      (defun my-current-line-is-empty ()
        (save-excursion (beginning-of-line) (looking-at "\\s-+$")))

      (defun my-delete-trailing-whitespace-at-line ()
        "Delete trailing whitespace on the current line only."
        (interactive)
        (let ((begin (line-beginning-position))
              (end   (line-end-position)))
          (delete-trailing-whitespace begin end)
          ))

     (defun my-electric-append-with-indent (count &optional vcount)
        "Indent the current line if it is empty. Otherwise, just do a normal append-line."
        (interactive "p")
        (if (and (= (point) (line-beginning-position))
                 (my-is-this-line-empty))
            (indent-according-to-mode))
        (evil-append-line count vcount)
        )
      ;; exiting insert mode -> delete trailing whitespace
      (remove-hook 'evil-insert-state-exit-hook 'my-exit-insert-state)

      ;; entering insert mode -> indent according to mode
      ;; Normal Evil bindings
      (define-key evil-insert-state-map (kbd "<S-backspace>")
        'my-backward-delete-word)

      (define-key evil-normal-state-map (kbd "SPC a") 'ag)
      (define-key evil-normal-state-map (kbd "SPC SPC") 'helm-M-x)

      (define-key evil-normal-state-map (kbd "C-q")   'universal-argument)

      (define-key evil-normal-state-map (kbd "C-h")   'evil-window-left)
      (define-key evil-normal-state-map (kbd "C-j")   'evil-window-down)
      (define-key evil-normal-state-map (kbd "C-k")   'evil-window-up)
      (define-key evil-normal-state-map (kbd "C-l")   'evil-window-right)
      (define-key evil-normal-state-map (kbd "-") (kbd "dd"))

      (define-key evil-normal-state-map "a"           'evil-append)
      (define-key evil-normal-state-map "A"           'my-electric-append-with-indent)
      (define-key evil-normal-state-map "$"           'my-smart-end)
      (define-key evil-normal-state-map "0"           'my-smart-home)

      (define-key evil-motion-state-map "h"           'evil-backward-char)
      (define-key evil-motion-state-map "j"           'evil-next-visual-line)
      (define-key evil-motion-state-map "k"           'evil-previous-visual-line)
      (define-key evil-motion-state-map "l"           'evil-forward-char)
      (define-key evil-motion-state-map "$"           'evil-end-of-line)
      (define-key evil-motion-state-map "0"           'evil-beginning-of-line)

      (define-key evil-normal-state-map "/"           'evil-search-forward)
      (define-key evil-normal-state-map (kbd "SPC /") 'helm-swoop)
      (define-key evil-motion-state-map "/"           'evil-search-forward)
      (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

      (evil-ex-define-cmd "Q"  'evil-quit)
      (evil-ex-define-cmd "Qa" 'evil-quit-all)
      (evil-ex-define-cmd "QA" 'evil-quit-all)

    (evil-define-key 'motion python-mode-map "]]" 'python-nav-forward-block)
    (evil-define-key 'motion python-mode-map "][" 'python-nav-end-of-block)
    (evil-define-key 'motion python-mode-map "[[" 'python-nav-backward-block)
    (evil-define-key 'motion python-mode-map "[]" 'my-python-nav-backward-end-of-block)
    (evil-define-key 'motion python-mode-map "[(" 'evil-previous-open-paren)
    (evil-define-key 'motion python-mode-map "])" 'evil-next-close-paren)
    (evil-define-key 'motion python-mode-map "[{" 'evil-previous-open-brace)
    (evil-define-key 'motion python-mode-map "]}" 'evil-next-close-brace)
 ))

(use-package evil-jumper
  :ensure evil-jumper
  :init
  ;; C-i and C-o don't work unless we do this binding here.
  (require 'evil-jumper)
  )

(provide 'my-evil)
