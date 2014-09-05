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
    (evil-leader/set-key    "w"   'save-buffer)
    (evil-leader/set-key    "qq"  'kill-this-buffer)
    (evil-leader/set-key    "qW"  'kill-buffer-and-window)
    (after 'evil
      (evil-leader/set-key  "qw"  'evil-window-delete)
    )
    (evil-leader/set-key    "q"   'nil)
    (evil-leader/set-key    "Q"   'kill-buffer-and-window)
    (evil-leader/set-key    "e"   'pp-eval-last-sexp)
    (evil-leader/set-key    "h"   'dired-jump)

    ;; window splits
    ;;
    ;; mnemonic:
    ;;
    ;;    |       vertical split      (technically it's the `\` key)
    ;;    -       horizontal split
    ;;
    (evil-leader/set-key    "\\"  'split-window-horizontally)
    (evil-leader/set-key    "-"   'split-window-vertically)
    (evil-leader/set-key    "e"   'pp-eval-last-sexp)
    (evil-leader/set-key    "TAB" 'my-hop-around-buffers)
    (evil-leader/set-key    ","   'other-window)

    ;; s -> "search"
    (evil-leader/set-key  "sr"  'ag-regexp)
    (evil-leader/set-key  "sf"  'ag-dired-regexp)
    (evil-leader/set-key  "ss"  'helm-swoop)

    (evil-leader/set-key-for-mode 'emacs-lisp-mode
      "." 'elisp-slime-nav-find-elisp-thing-at-point)

    (evil-leader/set-key-for-mode 'c-mode
      "." 'semantic-ia-fast-jump)

    ;; toggle between a function declaration and its implementation
    (evil-leader/set-key-for-mode 'c-mode
      "d" 'semantic-analyze-proto-impl-toggle)

    ;; g -> "git"
    (evil-leader/set-key  "gs" 'magit-status)
    (evil-leader/set-key  "gl" 'magit-log)
    (evil-leader/set-key  "gd" 'magit-diff)

    ;; j -> "jump"
    (evil-leader/set-key    "jf"  'ffap)
    (evil-leader/set-key    "p"   'project-explorer-open)
    (evil-leader/set-key  "l"   'helm-semantic-or-imenu)
    (after 'evil-nerd-commenter
      (evil-leader/set-key  "/"   'evilnc-comment-or-uncomment-lines)
      )
    (evil-leader/set-key    "cl"  'my-flycheck-list-errors)
    (after 'projectile
      (evil-leader/set-key  "cc"  'projectile-compile-project)
    )
    (evil-leader/set-key    "f"   'helm-find-files)
    (evil-leader/set-key    "x"   'helm-M-x)
    )
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

    (use-package evil-nerd-commenter
      :ensure evil-nerd-commenter
      )

      (use-package evil-matchit
        :ensure evil-matchit
        :commands evilmi-jump-items
        :init
        (progn
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

      (defun my-delete-trailing-whitespace-at-line (line)
        "Delete trailing whitespace on the current line only."
        (save-excursion
        (let ((begin (line-beginning-position))
              (end   (line-end-position)))
          (delete-trailing-whitespace begin end)
          )))

    (defun my-next-line-with-smart-delete (count)
        "Delete trailing whitespace on the previous line before moving
but only if we just exited insert state from an indented blank line
and are moving down.
TODO: make this work properly with visual lines, then start using it!"
        (interactive "p")
        (let ((line-move-visual t)
              (old-spot (my-where-beginning-of-visual-line))
              (old-line (my-what-line)))
          (if (my-current-line-is-empty)
            (progn (my-delete-trailing-whitespace-at-point)
                   (evil-next-visual-line count)
                   (if (< (my-where-beginning-of-visual-line) old-spot)
                       (progn
                         (evil-beginning-of-visual-line)
                          (evil-forward-char old-spot nil t)
                          (print old-spot)
                         )))
            (evil-next-visual-line count))
          ))

      (defun my-previous-line-with-smart-delete (count)
        "Delete trailing whitespace on the previous line before moving
but only if we just exited insert state from an indented blank line
and are moving down.
TODO: make this work properly with visual lines, then start using it!"
        (interactive "p")
        (let ((line-move-visual t)
              (old-spot (my-where-beginning-of-visual-line))
              (old-line (my-what-line)))
          (if (my-current-line-is-empty)
            (progn (my-delete-trailing-whitespace-at-point)
                   (evil-previous-visual-line count)
                   (if (< (my-where-beginning-of-visual-line) old-spot)
                       (progn
                         (evil-beginning-of-visual-line)
                         (evil-forward-char old-spot nil t)
                          (print old-spot)
                         )))
            (progn (evil-previous-visual-line count)
                   nil
             )
          )))

      (defun my-electric-append-with-indent (count &optional vcount)
        "Indent the current line if it is empty. Otherwise, just do a normal append-line."
        (interactive "p")
        ;; (if (= (point) (line-beginning-position))
        ;;     (indent-according-to-mode))
        (evil-append-line count vcount)
        )

      ;; (defun my-evil-forward-char ()
      ;;   (evil-forward-char)
      ;;  )

      ;; exiting insert mode -> delete trailing whitespace
      ;; (remove-hook 'evil-insert-state-exit-hook 'my-delete-trailing-whitespace-at-point)

      ;; entering insert mode -> indent according to mode
      ;; Normal Evil bindings
      (define-key evil-insert-state-map (kbd "<S-backspace>")
        'my-backward-delete-word)

      (define-key evil-normal-state-map (kbd "SPC a") 'ag)
      (define-key evil-normal-state-map (kbd "SPC A") 'apropos)
      (define-key evil-normal-state-map (kbd "SPC X") 'helm-M-x)

      (define-key evil-normal-state-map (kbd "C-q")   'universal-argument)

      (define-key evil-normal-state-map (kbd "C-h")   'evil-window-left)
      (define-key evil-normal-state-map (kbd "C-j")   'evil-window-down)
      (define-key evil-normal-state-map (kbd "C-k")   'evil-window-up)
      (define-key evil-normal-state-map (kbd "C-l")   'evil-window-right)
      (define-key evil-normal-state-map (kbd "-") (kbd "dd"))

      (define-key evil-normal-state-map "a"           'evil-append)
      (define-key evil-normal-state-map "A"           'my-electric-append-with-indent)
      (define-key evil-normal-state-map "j"           'evil-next-visual-line)
      (define-key evil-normal-state-map "k"           'evil-previous-visual-line)
      (define-key evil-normal-state-map "$"           'my-smart-end)
      (define-key evil-normal-state-map "0"           'my-smart-home)

      (define-key evil-motion-state-map "j"           'evil-next-visual-line)
      (define-key evil-motion-state-map "k"           'evil-previous-visual-line)
      (define-key evil-motion-state-map "$"           'evil-end-of-line)
      (define-key evil-motion-state-map "0"           'evil-beginning-of-line)

      (define-key evil-normal-state-map "/"           'evil-search-forward)
      (define-key evil-normal-state-map (kbd "SPC /") 'helm-swoop)
      (define-key evil-motion-state-map "/"           'evil-search-forward)
      (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

      (evil-ex-define-cmd "Q"  'evil-quit)
      (evil-ex-define-cmd "Qa" 'evil-quit-all)
      (evil-ex-define-cmd "QA" 'evil-quit-all)
      (evil-add-hjkl-bindings package-menu-mode-map 'emacs
        "h" 'evil-backward-word-begin
        "l" 'evil-forward-word-begin
        "/" 'evil-search-forward
        "?" 'evil-search-backward
        )

    (defun my-evil-modeline-change (default-color)
      "changes the modeline color when the evil mode changes"
      (let ((color (cond ((evil-insert-state-p) '("#002233" . "#ffffff"))
                         ((evil-visual-state-p) '("#330022" . "#ffffff"))
                         ((evil-normal-state-p) default-color)
                         (t '("#440000" . "#ffffff")))))
        (set-face-background 'mode-line (car color))
        (set-face-foreground 'mode-line (cdr color))))

    (defun my-evil-insert-modeline ()
      "Changes the modeline to insert color."
      (set-face-background 'mode-line "#440000")
      )
    (defun my-evil-normal-modeline ()
      "Changes the modeline to normal color."
      (set-face-background 'mode-line "#330022")
      )
))

(use-package evil-jumper
  :ensure evil-jumper
  )

(provide 'my-evil)
