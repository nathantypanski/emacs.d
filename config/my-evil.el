;; my-evil.el   -*- lexical-binding:t; -*-
;;
;; Settings for ostracizing me from both the Emacs and Vim communities.
;; a.k.a. evil-mode

;; In order to work properly, we need to load evil-leader-mode before we load
;; evil-mode.

;; evil-collection requires this set before loading evil
(setq evil-want-keybinding nil)

(use-package undo-tree
  :ensure t
  :config
   (setq undo-tree-history-directory-alist
         '(("." . "~/.emacs.d/undo-tree-history")))
   (setq undo-tree-auto-save-history nil)

  (global-undo-tree-mode))

(use-package smartparens
  :demand t
  :ensure t
  :hook ((prog-mode . electric-pair-mode))
  :straight t)


;; Here's what we've all been waiting for.
;; Recreate Vim inside Emacs.
(use-package evil
  :ensure evil
  :demand t
  :after (consult key-chord general smartparens)
  :init
  (setq evil-emacs-state-cursor   '("#dfaf8f" box)
        evil-normal-state-cursor  '("#f8f893" box)
        evil-insert-state-cursor  '("#f8f893" bar)
        evil-replace-state-cursor '("#cc9393" hbar))
  :hook
  ((evil-mode . my-tty-cursor-update)
   (evil-local-mode . my-tty-cursor-update))
   :custom
  ;; evil-collection requires this set before loading evil
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete nil)
  (evil-want-C-w-in-emacs-state t)
  (evil-want-keybinding nil)
  (evil-magic 'very-magic)
  (evil-search-module 'isearch)
  (evil-want-fine-undo t)
  (evil-want-change-word-to-end t)
  :config
  (evil-mode 1)

  (evil-set-undo-system 'undo-tree)

  ;; DECSCUSR escapes  (CSI Ps SP q)  → block 2, underline 4, bar 6
  (defconst my-tty-cursor-escape-table
    '((box  . "\e[2 q")
      (hbar . "\e[4 q")
      (bar  . "\e[6 q")))

  ;; Emacs starts with shape 'box, so start with nil to trigger an
  ;; update at launch.
  (defvar my-tty--frame-shape nil
    "Shape we last sent to THIS frame.
Not buffer-local, so it really is per frame.")

  (defun my-evil--shape ()
    "Return symbol box/bar/hbar for current evil-state, defaulting to box."
    (let* ((sym  (intern (format "evil-%s-state-cursor" evil-state)))
           (spec (and (boundp sym) (symbol-value sym))))
      (if (and (consp spec) (memq (cadr spec) '(box bar hbar)))
          (cadr spec)
        'box)))

  (defun my-tty-cursor-update (&rest _)
    "Apply correct DECSCUSR escape in TTY frames after every command."
    (interactive)
    (when (and (not (display-graphic-p))
               (bound-and-true-p evil-local-mode)
               ;; Don't send escapes during potentially problematic states
               (not (and (boundp 'transient--prefix) transient--prefix))
               (not (minibuffer-window-active-p (minibuffer-window)))
               (not executing-kbd-macro)
               (not defining-kbd-macro))
      (let ((shape (my-evil--shape)))
        ;; 1. Let Emacs know (covers 28+ which translate cursor-type themselves)
        (setq-local cursor-type shape)
        ;; 2. For Emacs ≤27 we still have to push the escape.  Deduplicate it
        (unless (eq shape my-tty--frame-shape)
          (setq my-tty--frame-shape shape)
          (when-let ((esc (cdr (assq shape my-tty-cursor-escape-table))))
            (send-string-to-terminal esc))))))

  (add-hook 'evil-mode-hook #'my-tty-cursor-update)
  (add-hook 'evil-local-mode-hook #'my-tty-cursor-update)
  (add-hook 'post-command-hook #'my-tty-cursor-update)
  (add-hook 'after-make-frame-functions #'my-tty-cursor-update)
  (add-hook 'focus-in-hook #'my-tty-cursor-update)

  (evil-set-initial-state 'flycheck-error-list-mode 'normal)
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'shell-mode 'normal)
  (evil-set-initial-state 'esup-mode 'emacs)
  (evil-set-initial-state 'diff-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'multi-term-mode 'emacs)
  (evil-set-initial-state 'transient-mode 'emacs)

  ;; Ensure transient menus work properly with Evil
  (add-hook 'transient-setup-hook
            (lambda ()
              ;; Force emacs state for transient menus - but only if not already in it
              (when (and (boundp 'evil-local-mode) evil-local-mode
                         (not (evil-emacs-state-p)))
                (evil-emacs-state))
              ;; Also disable the cursor update hook during transient to prevent interference
              (remove-hook 'post-command-hook #'my-tty-cursor-update t)))

  ;; Re-enable cursor updates when transient exits
  (add-hook 'transient-exit-hook
            (lambda ()
              (add-hook 'post-command-hook #'my-tty-cursor-update)))

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

  (general-define-key
   :keymaps '(minibuffer-local-map
              minibuffer-local-ns-map
              minibuffer-local-completion-map
              minibuffer-local-must-match-map)
   [escape] 'my-minibuffer-keyboard-quit)

  (defun my-delete-trailing-whitespace-at-line ()
    "Delete trailing whitespace on the current line only."
    (interactive)
    (let ((begin (line-beginning-position))
          (end   (line-end-position)))
      (delete-trailing-whitespace begin end)))

  (evil-define-command my-ret-and-indent (count)
  "Like RET+indent in insert state, or click a button if on one."
  (interactive "p")
  (let ((b (button-at (point))))
    (cond
     (b (push-button (point)))
     (t
      (my-delete-trailing-whitespace-at-line)
      (evil-ret-gen count nil)
      (when (my-sensible-to-indent-p)
        (indent-according-to-mode))))))

  (defun my-electric-append-with-indent (count &optional vcount)
    "Indent the current line if it is empty. Otherwise, just do a normal append-line."
    (interactive "p")
    (if (and (= (point) (line-beginning-position))
             (my-is-this-line-empty))
        (indent-according-to-mode))
    (evil-append-line count vcount))

  (evil-define-motion my-append-and-indent (count)
    "Move to end of line, enter insert mode, then indent (or complete)."
    :type line
    (interactive "p")
    (evil-append-line count)
    (indent-for-tab-command))

  (defun my-exit-insert-state ()
    "Function to be run when Evil exits insert state."
    (if (my-current-line-is-empty)
        (delete-horizontal-space t)))

  (defvar-local my-should-insert-indent t
    "When non-nil, disable automatic indentation on insert state entry.")

  (defun my-disable-insert-indent ()
    "disable insert indentation"
    (interactive)
    (setq-local my-should-insert-indent nil))

  (defun my-enter-insert-state ()
    "Function to be run when Evil enters insert state."
    (interactive)
    (when (and my-should-insert-indent
               (my-sensible-to-indent-p))
      (indent-according-to-mode)))

  (defun enable-tabs ()
    "Enable tabs in a file."
    (interactive)
    (setq indent-tabs-mode t)
    (setq indent-tabs-mode nil)
    (define-key evil-insert-state-map (kbd "TAB") 'indent-for-tab-command))

  (defun disable-tabs ()
    "Enable tabs in a file."
    (interactive)
    (setq indent-tabs-mode nil)
    (define-key evil-insert-state-map (kbd "TAB") 'indent-for-tab-command))

  ;; exiting insert mode -> delete trailing whitespace
  (add-hook 'evil-insert-state-exit-hook 'my-exit-insert-state)
  (add-hook 'evil-insert-state-entry-hook 'my-enter-insert-state)

  (evil-ex-define-cmd "Q"  'evil-quit)
  (evil-ex-define-cmd "Qa" 'evil-quit-all)
  (evil-ex-define-cmd "QA" 'evil-quit-all)

  ;; depends on my-copy
  (defun my-wl-copy-operator (beg end &optional _type)
    "Copy region to clipboard using wl-copy. Works with Evil and M-x."
    (interactive
     (if (use-region-p)
         (list (region-beginning) (region-end))
       (error "No active region")))
    (my-wl-copy-region beg end))

  ;; Optional global keybindings (non-Evil)
  (global-set-key (kbd "C-c w c") #'my-wl-copy-operator)
  (global-set-key (kbd "C-c w v") #'my-wl-paste-insert)

  ;; Evil-specific bindings
  ;; Evil operator for visual mode: `yg`
  (evil-define-operator my-wl-copy-evil-operator (beg end type)
    "Evil operator wrapper for `my-wl-copy-operator` (use with `yg`)."
    (interactive "<R>")
    (my-wl-copy-operator beg end type))

  (defun my-wl-paste-evil ()
    "Paste clipboard contents at point using wl-paste, Evil-style."
    (interactive)
    (let ((text (string-trim (shell-command-to-string my-paste-command))))
      (evil-with-undo
        (cond
         ((evil-visual-state-p)
          ;; Replace visual region
          (delete-region (region-beginning) (region-end))
          (evil-normal-state)
          (insert text))
         ((evil-normal-state-p)
          ;; Paste after point
          (forward-char)
          (insert text))
         (t
          ;; Fallback (e.g. insert or emacs state)
          (insert text))))))

  (general-define-key
   :states '(visual)
   "C-y" #'my-wl-copy-evil-operator)

  (general-define-key
   :states '(normal insert)
   "C-p" #'my-wl-paste-evil)

  (after 'lsp-mode
    (defun my-lsp-doc-no-completion (&optional pos)
      "Show LSP docs in the help window *and* select that window.
With a prefix argument, prompt for a buffer position to describe.
If LSP isn’t active here, signal a user‑friendly error."
      (interactive
       (list (if current-prefix-arg
                 (read-number "Describe at buffer position: " (point))
               (point))))
      (my-with-suppressed-capf
       (lambda ()
         (let ((help-window-select t))
           (save-excursion
             (goto-char pos)
             (if (and (fboundp #'lsp-describe-thing-at-point)
                      (bound-and-true-p lsp-mode))
                 (lsp-describe-thing-at-point)
               (user-error "No LSP available to describe here"))))))))

  (general-define-key
   :states '(insert)
   :keymaps 'evil-insert-state-map
   "TAB" #'indent-for-tab-command)

  (general-define-key
   :states 'normal
   "RET"            'my-electric-append-with-indent
   "<S-return>"     'my-append-and-indent
   "C-w }"          'evil-window-rotate-downwards
   "C-w {"          'evil-window-rotate-upwards
   "SPC SPC"        'execute-extended-command
   "SPC p"          'execute-extended-command
   "C-q"            'universal-argument
   "C-h"            'evil-window-left
   "C-j"            'evil-window-down
   "C-k"            'evil-window-up
   "C-l"            'evil-window-right
   "-"              'evil-delete-whole-line
   "a"              'evil-append
   "A"              'my-electric-append-with-indent
   "$"              'my-smart-end
   "0"              'my-smart-home
   "/"              'evil-search-forward
   "Y"              "y$"
   "P"              'consult-yank-from-kill-ring)

  (general-define-key
   :states 'insert
   "RET"             'my-ret-and-indent
   "<S-backspace>"   'backward-delete-char-untabify
   "<S-return>"      'electric-indent-just-newline)

  (general-define-key
   :states 'motion
   "h" 'evil-backward-char
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line
   "l" 'evil-forward-char
   "$" 'evil-end-of-line
   "0" 'evil-beginning-of-line
   "/" 'evil-search-forward)

  (general-define-key
   :states '(normal)
   "K" 'my-doc-at-point)

  (general-define-key
   :states '(insert)
   "M-/" 'company-complete)

  ;; TODO: figure out how to make these load in =my-org.el=

  ;; Bind <TAB> to org-tempo-expand in Evil insert mode
  )

(use-package evil-leader
  :ensure t
  :demand t
  :after (evil)
  :custom (evil-want-integration t
                                 ;; https://github.com/emacs-evil/evil-collection/issues/60
                                 evil-want-keybinding nil) ; let us load evil-collection separately
  :config
  (progn
    (evil-leader/set-leader ",")
    (global-evil-leader-mode t)))

(use-package evil-nerd-commenter
  :ensure evil-nerd-commenter
  :commands (evilnc-comment-or-uncomment-lines))

(use-package evil-matchit
  :after evil
  :ensure t
  :commands evilmi-jump-items
  :init
  (progn
    (setq global-evil-matchit-mode t)
    (define-key evil-normal-state-map "%" 'evilmi-jump-items)))

(use-package evil-surround
  :ensure evil-surround
  :after evil
  :config
  (progn
    (global-evil-surround-mode 1)))

(use-package evil-mc
  :straight t
  :after evil
  :demand t
  :config
  (global-evil-mc-mode 1)

  ;;— Optional cursor tweaks —;;
  (setq evil-mc-cursor-state  'hollow
        evil-mc-cursor-colors '("#ff5555" "#50fa7b" "#f1fa8c"))

  ;;— Define a “g”-prefixed definer for normal/visual —;;
  (general-create-definer my-leader-def
    :states  '(normal visual)
    :prefix  "g"
    :non-normal-prefix "M-g")

  ;;— Bind your evil-mc commands under “g” —;;
  (my-leader-def
    "M" #'evil-mc-make-cursor-here
    "n" #'evil-mc-make-and-goto-next-cursor
    "p" #'evil-mc-make-and-goto-prev-cursor
    "a" #'evil-mc-make-all-cursors
    "q" #'evil-mc-undo-all-cursors))

(use-package evil-collection
  :ensure evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init))

(use-package paredit
  :straight nil
  :ensure paredit)

(provide 'my-evil)
