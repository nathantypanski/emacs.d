;; my-evil.el   -*- lexical-binding:t; -*-
;;
;; Settings for ostracizing me from both the Emacs and Vim communities.
;; a.k.a. evil-mode

;; In order to work properly, we need to load evil-leader-mode before we load
;; evil-mode.

;; evil-collection requires this set before loading evil
(setq evil-want-keybinding nil)

;; undo visualization tool
(use-package vundo
  :ensure t
  :demand t)

;; https://github.com/emacs-evil/evil-collection/issues/60
(setq evil-want-integration t)
;; let us load evil-collection separately
(setq evil-want-keybinding nil)
;; Here's what we've all been waiting for.
;; Recreate Vim inside Emacs.
(use-package evil
  :ensure evil
  :demand t
  :after (consult key-chord general)
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
  (evil-want-C-w-delete nil)
  (evil-want-C-w-in-emacs-state t)
  (evil-want-keybinding nil)
  (evil-magic 'very-magic)
  (evil-search-module 'isearch)
  (evil-want-fine-undo t)
  (evil-want-change-word-to-end t)
  :config
  (evil-mode 1)

  ;; Use Emacs 28+ built-in undo-redo system (cleaner than undo-tree)
  (evil-set-undo-system 'undo-redo)

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
               (not (minibuffer-window-active-p (minibuffer-window)))
               (not executing-kbd-macro)
               (not defining-kbd-macro)
               ;; Also check if we're in the middle of key sequence processing
               (not (and (boundp 'this-command-keys-vector)
                         (vectorp this-command-keys-vector)
                         (> (length this-command-keys-vector) 0)
                         (let ((last-key (aref this-command-keys-vector
                                               (1- (length this-command-keys-vector)))))
                           ;; Don't update cursor if last key was an escape sequence
                           (and (numberp last-key) (= last-key 27))))))
      (let ((shape (my-evil--shape)))
        ;; 1. Let Emacs know (covers 28+ which translate cursor-type themselves)
        (setq-local cursor-type shape)
        ;; 2. For Emacs ≤27 we still have to push the escape.  Deduplicate it
        (unless (eq shape my-tty--frame-shape)
          (setq my-tty--frame-shape shape)
          (when-let ((esc (cdr (assq shape my-tty-cursor-escape-table))))
            (send-string-to-terminal esc))))))

  ;; Re-enable cursor updates for key state changes only
  (my-add-hook 'evil-mode-hook #'my-tty-cursor-update)
  (my-add-hook 'evil-local-mode-hook #'my-tty-cursor-update)
  (my-add-hook 'after-make-frame-functions #'my-tty-cursor-update)
  (my-add-hook 'focus-in-hook #'my-tty-cursor-update)

  ;; Hook into major state changes (but not every command)
  (advice-add 'evil-insert-state :after #'my-tty-cursor-update)
  (advice-add 'evil-normal-state :after #'my-tty-cursor-update)
  (advice-add 'evil-replace-state :after #'my-tty-cursor-update)
  (advice-add 'evil-visual-state :after #'my-tty-cursor-update)

  ;; when the selected window changes, update the cursor.
  (my-add-hook 'window-selection-change-functions #'my-tty-cursor-update)
  ;; when window layout changes in a major way
  (my-add-hook 'window-configuration-change-hook #'my-tty-cursor-update)
  ;; Catch buffer switches
  (my-add-hook 'buffer-list-update-hook #'my-tty-cursor-update)
  ;; Catch when new windows are created/deleted
  (my-add-hook 'window-size-change-functions #'my-tty-cursor-update)

  ;; God-mode integration
  (with-eval-after-load 'god-mode
    (my-add-hook 'god-mode-enabled-hook #'my-tty-cursor-update)
    (my-add-hook 'god-mode-disabled-hook #'my-tty-cursor-update))

  (evil-set-initial-state 'flycheck-error-list-mode 'normal)
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'shell-mode 'normal)
  (evil-set-initial-state 'esup-mode 'emacs)
  (evil-set-initial-state 'diff-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'multi-term-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (evil-set-initial-state 'vterm-copy-mode 'normal)
  (evil-set-initial-state 'transient-mode 'emacs)
  (evil-set-initial-state 'embark-collect-mode 'emacs)
  (evil-set-initial-state 'embark-export-mode 'emacs)

  (defun my-mode-line-update ()
    "Update the modeline."
    (force-mode-line-update))

  (add-hook 'vterm-copy-mode-hook 'my-mode-line-update)

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

  ;; Escape bindings for minibuffer
  (with-eval-after-load 'minibuffer
    (general-define-key
     :keymaps '(minibuffer-local-map
                minibuffer-local-ns-map
                minibuffer-local-completion-map
                minibuffer-local-must-match-map)
     [escape] 'my-minibuffer-keyboard-quit))

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
               (my-sensible-to-indent-p)
               ;; Don't indent if we're at the end of a non-empty line
               (not (and (> (line-end-position) (line-beginning-position))
                         (= (point) (line-end-position)))))
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
  (my-add-hook 'evil-insert-state-exit-hook 'my-exit-insert-state)
  (my-add-hook 'evil-insert-state-entry-hook 'my-enter-insert-state)

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
    (let ((text (string-trim (shell-command-to-string my-paste-program))))
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
   "a"              'evil-append
   "A"              'my-electric-append-with-indent
   "$"              'my-smart-end
   "0"              'my-smart-home
   "/"              'evil-search-forward
   "Y"              "y$"
   "P"              'consult-yank-from-kill-ring
   "gQ"             'fill-region)

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

  (general-define-key
   :states '(normal)
   :keymaps 'dired-mode-map
   "k" 'my-dired-prev-line
   "j" 'my-dired-next-line)

  ;; TODO: figure out how to make these load in =my-org.el=

  ;; Bind <TAB> to org-tempo-expand in Evil insert mode
  )

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

(use-package evil-leader
  :ensure t
  :demand t
  :after (evil)
  :config
  (evil-leader-mode t)
  (evil-leader/set-leader ",")
  (global-evil-leader-mode t))

(use-package evil-matchit
  :ensure evil-matchit
  :after evil
  :config
  (progn
    (global-evil-matchit-mode 1)))

(use-package evil-surround
  :ensure evil-surround
  :after evil
  :config
  (progn
    (global-evil-surround-mode 1)))

(use-package evil-collection
  :ensure evil-collection
  :after evil
  :demand t
  :config
  ;; Include vterm in evil-collection
  (add-to-list 'evil-collection-mode-list 'vterm)
  (evil-collection-init)
  (evil-collection-init 'embark)
  (evil-collection-init 'consult)
  ;; corfu should be loaded because my-completion comes first.
  (evil-collection-corfu-setup))


(provide 'my-evil)
