;; my-evil.el
;;
;; Configuration for ostracizing me from both the Emacs and Vim communities.

(use-package evil
  :ensure evil
  :config
  (progn
    (evil-mode 1)
    (setq evil-want-C-u-scroll t
	  evil-want-C-w-in-emacs-state t
	  evil-search-module 'isearch
	  evil-magic 'very-magic
	  evil-emacs-state-cursor '("#dfaf8f" box)
	  evil-normal-state-cursor '("#f8f893" box)
	  evil-insert-state-cursor '("#f8f893" bar)
          evil-replace-state-cursor '("#cc9393" box)
	  )

    (use-package evil-leader
      :ensure evil-leader
      :config
      (progn
	(global-evil-leader-mode)
	(evil-leader/set-leader ",")
	(setq my-eshell-buffer-count 0)
	(evil-leader/set-key
	  "w" 'save-buffer
	  "e" (kbd "C-x C-e")
	  "E" (kbd "C-M-x")
	  "c" (bind
	       (evil-window-split)
	       (setq my-eshell-buffer-count (+ 1 my-eshell-buffer-count))
	       (eshell my-eshell-buffer-count))
	  "C" 'customize-group
	  "b d" 'kill-this-buffer
	  "v" (kbd "C-w v C-w l")
	  "s" (kbd "C-w s C-w j")
	  "g s" 'magit-status
	  "g l" 'magit-log
	  "g d" 'vc-diff
	  "P" 'package-list-packages
	  "h" help-map
	  "h h" 'help-for-help-internal)
	)
      )
    (use-package evil-nerd-commenter
      :ensure evil-nerd-commenter
      :config
      ;; Type 'gcc' to comment the current line.
      (progn (setq evilnc-hotkey-comment-operator "gc")))
    (use-package evil-indent-textobject
      :ensure evil-indent-textobject)
    (use-package evil-visualstar
      :ensure evil-visualstar)
    (use-package evil-matchit
      :ensure evil-matchit
      :config
      (progn
	(after 'evil-matchit
	  (define-key evil-normal-state-map "%" 'evilmi-jump-items))
	))
    (use-package surround
      :ensure surround
      :config
      (progn
	(global-surround-mode 1)
	)
      )

    (dolist (mode '(eshell-mode
		    shell-mode
		    term-mode
		    terminal-mode
		    comint-mode
		    skewer-repl-mode
		    profiler-report-mode
		    erc-mode weechat-mode
		    direx:direx-mode
		    project-explorer-mode))
      (evil-set-initial-state mode 'emacs))

    (use-package key-chord
      :ensure key-chord
      :diminish key-chord-mode
      :config
      (progn
        (key-chord-mode 1)))

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

    (define-key evil-motion-state-map "gN" 'my-evil-previous-match)
    (define-key evil-motion-state-map "gN" 'my-evil-previous-match)

    (defadvice evil-ex-search-next
      (after advice-for-evil-ex-search-next activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (defadvice evil-ex-search-previous
      (after advice-for-evil-ex-search-previous activate)
      (evil-scroll-line-to-center
       (line-number-at-pos)))

     ;;; esc quits
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)

    ;; paragraph bindings
    (dolist (key '("\M-k" "\M-j" "\M-h" "\M-l"))
      (global-unset-key key))

    (after 'git-gutter+-autoloads
      (define-key evil-normal-state-map (kbd "[ h") 'git-gutter+-previous-hunk)
      (define-key evil-normal-state-map (kbd "] h") 'git-gutter+-next-hunk)
      (define-key evil-normal-state-map (kbd ", g a") 'git-gutter+-stage-hunks)
      (define-key evil-normal-state-map (kbd ", g r") 'git-gutter+-revert-hunks)
      (evil-ex-define-cmd "Gw" (bind (git-gutter+-stage-whole-buffer))))

    ;; Normal Evil bindings

    (define-key evil-normal-state-map (kbd ", k") 'kill-buffer)
    (define-key evil-normal-state-map (kbd "SPC F") 'dired-at-point)

    (define-key evil-normal-state-map (kbd "[ SPC") (bind (evil-insert-newline-above) (forward-line)))
    (define-key evil-normal-state-map (kbd "] SPC") (bind (evil-insert-newline-below) (forward-line -1)))
    (define-key evil-normal-state-map (kbd "[ e") (kbd "ddkP"))
    (define-key evil-normal-state-map (kbd "] e") (kbd "ddp"))
    (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
    (define-key evil-normal-state-map (kbd "] b") 'next-buffer)
    (define-key evil-normal-state-map (kbd "[ q") 'previous-error)
    (define-key evil-normal-state-map (kbd "] q") 'next-error)

    (define-key evil-normal-state-map (kbd "g p") (kbd "` [ v ` ]"))

    (after 'etags-select
      (define-key evil-normal-state-map (kbd "g ]") 'etags-select-find-tag-at-point))

    (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
    (define-key evil-normal-state-map (kbd "C-q") 'universal-argument)

    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

    (define-key evil-motion-state-map "j" 'evil-next-visual-line)
    (define-key evil-motion-state-map "k" 'evil-previous-visual-line)

    (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

    (define-key evil-visual-state-map (kbd ", e") 'eval-region)

    ;; emacs lisp
    (after 'elisp-slime-nav-autoloads
      ;; TODO: how do I close this file afterwards (for an easy way to jump back to where I was working?)
      (evil-define-key 'normal emacs-lisp-mode-map (kbd "g d") 'elisp-slime-nav-find-elisp-thing-at-point)
      ;; TODO: find a way to make this automatically switch to the buffer it opens
      (evil-define-key 'normal emacs-lisp-mode-map (kbd "K") 'elisp-slime-nav-describe-elisp-thing-at-point))

    (after 'ag-autoloads
      (define-key evil-normal-state-map (kbd "SPC /") 'ag-regexp-project-at-point))

    (after 'multiple-cursors
      (define-key evil-visual-state-map (kbd "C->") 'mc/mark-all-like-this)
      (define-key evil-normal-state-map (kbd "C->") 'mc/mark-next-like-this)
      (define-key evil-normal-state-map (kbd "C-<") 'mc/mark-previous-like-this))

    (after 'magit
      (define-key magit-status-mode-map (kbd "C-n") 'magit-goto-next-sibling-section)
      (define-key magit-status-mode-map (kbd "C-p") 'magit-goto-previous-sibling-section)
      (evil-add-hjkl-bindings magit-status-mode-map 'emacs
	"K" 'magit-discard-item
	"l" 'magit-key-mode-popup-logging
	"h" 'magit-toggle-diff-refine-hunk))

    ;; butter fingers
    (evil-ex-define-cmd "Q" 'evil-quit)
    (evil-ex-define-cmd "Qa" 'evil-quit-all)
    (evil-ex-define-cmd "QA" 'evil-quit-all)
    )
  )

(after 'evil
    (define-minor-mode evil-org-mode
        "Buffer local minor mode for evil-org"
        :init-value nil
        :diminsh evil-org-mode
        :lighter " EvilOrg"
        :keymap (make-sparse-keymap) ; defines evil-org-mode-map
        :group 'evil-org)
  (add-hook 'org-mode-hook 'evil-org-mode) ;; only load with org-mode
  (setq evil-auto-indent nil)
  (defun always-insert-item ()
    "Force insertion of org item"
    (if (not (org-in-item-p))
        (insert "\n- ")
      (org-insert-item))
    )

  (defun evil-org-eol-call (fun)
    "Go to end of line and call provided function"
    (end-of-line)
    (funcall fun)
    (evil-append nil)
    )
  ;; normal state shortcuts
  (evil-define-key 'normal evil-org-mode-map
    "gh" 'outline-up-heading
    "gj" (if (fboundp 'org-forward-same-level) ;to be backward compatible with older org version
             'org-forward-same-level
           'org-forward-heading-same-level)
    "gk" (if (fboundp 'org-backward-same-level)
             'org-backward-same-level
           'org-backward-heading-same-level)
    "gl" 'outline-next-visible-heading
    "t" 'org-todo
    "T" '(lambda () (interactive) (evil-org-eol-call '(org-insert-todo-heading nil)))
    "H" 'org-beginning-of-line
    "L" 'org-end-of-line
    ";t" 'org-show-todo-tree
    "o" '(lambda () (interactive) (evil-org-eol-call 'always-insert-item))
    "O" '(lambda () (interactive) (evil-org-eol-call 'org-insert-heading))
    "$" 'org-end-of-line
    "^" 'org-beginning-of-line
    "<" 'org-metaleft
    ">" 'org-metaright
    "&" 'org-edit-src-code
    ";a" 'org-agenda
    "-" 'org-cycle-list-bullet
    (kbd "TAB") 'org-cycle)

  ;; normal & insert state shortcuts.
  (mapc (lambda (state)
          (evil-define-key state evil-org-mode-map
            (kbd "M-l") 'org-metaright
            (kbd "M-h") 'org-metaleft
            (kbd "M-k") 'org-metaup
            (kbd "M-j") 'org-metadown
            (kbd "M-L") 'org-shiftmetaright
            (kbd "M-H") 'org-shiftmetaleft
            (kbd "M-K") 'org-shiftmetaup
            (kbd "M-J") 'org-shiftmetadown
            (kbd "M-o") '(lambda () (interactive)
                           (evil-org-eol-call
                            '(lambda()
                               (org-insert-heading)
                               (org-metaright))))
            (kbd "M-t") '(lambda () (interactive)
                           (evil-org-eol-call
                            '(lambda()
                               (org-insert-todo-heading nil)
                               (org-metaright))))
            ))
        '(normal insert)))

(define-key minibuffer-local-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'my-minibuffer-keyboard-quit)

(after 'package
  (after 'evil
    (evil-add-hjkl-bindings package-menu-mode-map 'emacs))
  )

(provide 'my-evil)
