;; my-evil.el
;;
;; Settings for ostracizing me from both the Emacs and Vim communities.
;; a.k.a. evil-mode

;; In order to work properly, we need to load evil-leader-mode before we load
;; evil-mode.

;; evil-collection requires this set before loading evil
(setq evil-want-keybinding nil)
;; Here's what we've all been waiting for.
;; Recreate Vim inside Emacs.
(use-package evil
  :ensure evil
  :demand t
  :after consult
  :init
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

  (evil-set-undo-system 'undo-redo)

  (setq evil-emacs-state-cursor   '("#dfaf8f" box)
        evil-normal-state-cursor  '("#f8f893" box)
        evil-insert-state-cursor  '("#f8f893" bar)
        evil-replace-state-cursor '("#cc9393" hbar))

  ;; DECSCUSR escapes  (CSI Ps SP q)  → block 2, underline 4, bar 6
  (defconst my-tty-cursor-escape-table
    '((box  . "\e[2 q")
      (hbar . "\e[4 q")
      (bar  . "\e[6 q")))

  (defvar my-tty--frame-shape 'box
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
    (when (and (not (display-graphic-p)) (bound-and-true-p evil-local-mode))
      (let ((shape (my-evil--shape)))
        ;; 1. Let Emacs know (covers 28+ which translate cursor-type themselves)
        (setq-local cursor-type shape)
        ;; 2. For Emacs ≤27 we still have to push the escape.  Deduplicate it
        (unless (eq shape my-tty--frame-shape)
          (setq my-tty--frame-shape shape)
          (when-let ((esc (cdr (assq shape my-tty-cursor-escape-table))))
            (send-string-to-terminal esc))))))

  ;; Run AFTER every command – guarantees we always win the last cursor race
  (add-hook 'post-command-hook #'my-tty-cursor-update)

  ;; New TTY frames and focus changes can reset the hardware cursor
  (add-hook 'after-make-frame-functions (lambda (_f) (my-tty-cursor-update)))
  (add-hook 'focus-in-hook              #'my-tty-cursor-update)

  (my-tty-cursor-update)

  (evil-set-initial-state 'flycheck-error-list-mode 'normal)
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'shell-mode 'normal)
  (evil-set-initial-state 'esup-mode 'emacs)
  (evil-set-initial-state 'diff-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'multi-term-mode 'emacs)

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

  (defun my-delete-trailing-whitespace-at-line ()
    "Delete trailing whitespace on the current line only."
    (interactive)
    (let ((begin (line-beginning-position))
          (end   (line-end-position)))
      (delete-trailing-whitespace begin end)))

  (evil-define-command my-ret-and-indent (count)
    "Like RET+indent in insert state, or click a widget/button if on one."
    (interactive "p")
    (let ((w (widget-at (point)))
          (b (button-at (point))))
      (cond
       (w (widget-button-press (point)))
       (b (push-button (point)))
       (t
        (my-delete-trailing-whitespace-at-line)
        (evil-ret-gen count nil)
        (when (my-sensible-to-indent-p)
          (indent-according-to-mode))))))

  (defun my-electric-append-with-indent (count &optional vcount)
    "Indent the current line if it is empty.

Otherwise, just do a normal append-line."
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
    (indent-for-tab-command))  ; <— piggybacks on tab-always-indent=’complete

  (defun my-exit-insert-state ()
    "Function to be run when Evil exits insert state."
    (if (my-current-line-is-empty)
        (delete-horizontal-space t)))

  (defun my-enter-insert-state ()
    "Function to be run when Evil enters insert state.

Loads indent data from my-sensible-to-indent-p and uses that to determine
whether to call indent-according-to-mode."
    (interactive)
    (if (my-sensible-to-indent-p)
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

  (define-key evil-normal-state-map (kbd "RET") 'my-electric-append-with-indent)
  (define-key evil-normal-state-map (kbd "<S-return>") 'my-append-and-indent)
  (define-key evil-normal-state-map (kbd "C-w }") 'evil-window-rotate-downwards)
  (define-key evil-normal-state-map (kbd "C-w {") 'evil-window-rotate-upwards)
  (define-key evil-insert-state-map (kbd "RET") 'my-ret-and-indent)
  (define-key evil-insert-state-map (kbd "<S-backspace>")
              'backward-delete-char-untabify)
  (define-key evil-insert-state-map (kbd "<S-return>")
              'electric-indent-just-newline)
  (define-key evil-normal-state-map (kbd "<S-return>")
              'electric-indent-just-newline)

  (define-key evil-normal-state-map (kbd "SPC a") 'ag)
  (define-key evil-normal-state-map (kbd "SPC SPC") 'execute-extended-command)

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
  (define-key evil-normal-state-map (kbd "P") 'consult-yank-from-kill-ring)

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
    (let ((text (string-trim (shell-command-to-string "wl-paste -n"))))
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

  (define-key evil-visual-state-map (kbd "C-y") #'my-wl-copy-evil-operator)
  (define-key evil-normal-state-map (kbd "C-p") #'my-wl-paste-evil)
  (define-key evil-insert-state-map (kbd "C-p") #'my-wl-paste-evil)
  (define-key evil-insert-state-map (kbd "C-SPC") #'completion-at-point)


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

  ;; (defun my-evil-complete-or-indent ()
  ;;   "Try `completion-at-point`; otherwise indent."
  ;;   (interactive)
  ;;   (if (my-sensible-to-indent-p)
  ;;         (indent-according-to-mode)
  ;;     (completion-at-point)))

  ;; (define-key evil-insert-state-map (kbd "TAB") #'my-evil-complete-or-indent)

  ;; make sure TAB in insert state calls indent-for-tab-command
  (define-key evil-insert-state-map (kbd "TAB") #'indent-for-tab-command)

  (defun my-with-suppressed-capf (fn)
    "Temporarily restore the raw CAPF handler around FN."
    (let ((completion-in-region-function #'completion--in-region))
      (funcall fn)))

  (defun my-doc-at-point (&optional pos)
    "Describe the thing at POS via man, LSP hover, or Woman, and select its window.
With `C-u` prefix, prompt for a position; otherwise use point."
    (interactive "d")  ; reads POS or prompts if prefix given
    (let* ((symbol (save-excursion
                     (goto-char pos)
                     (thing-at-point 'symbol t)))
           (man-spec (and symbol (concat "1 " symbol))))
      (cond
       ;; 1) Shell-script or interactive shell → pop man(1)
       ((and symbol
             (derived-mode-p 'sh-mode
                             'shell-mode 'eshell-mode
                             'term-mode  'comint-mode))
        (if (executable-find "man")
            (let ((buf (man man-spec)))
              (pop-to-buffer buf))
          (user-error "No ‘man’ executable found to look up %s" symbol)))

       ;; 2) LSP hover → help buffer + select
       ((and (fboundp #'lsp-describe-thing-at-point)
             (bound-and-true-p lsp-mode))
        (my-with-suppressed-capf
         (lambda ()
           (let ((help-window-select t))
             (save-excursion
               (goto-char pos)
               (lsp-describe-thing-at-point))))))

       ;; 3) Woman fallback → select
       ((and symbol (fboundp #'woman-manual-entry))
        (pop-to-buffer (woman-manual-entry symbol)))

       ;; 4) Nothing found → friendly error
       (t
        (user-error "No documentation available for %s"
                    (or symbol "<nothing>"))))))


  (define-key evil-normal-state-map (kbd "K") 'my-doc-at-point))

(use-package evil-leader
  :commands (evil-leader-mode global-evil-leader-mode)
  :ensure evil-leader
  :demand evil-leader
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
  :ensure evil-matchit
  :commands evilmi-jump-items
  :init
  (progn
    (setq global-evil-matchit-mode t)
    (define-key evil-normal-state-map "%" 'evilmi-jump-items)))

(use-package evil-surround
  :ensure evil-surround
  :config
  (progn
    (global-evil-surround-mode 1)))

(use-package evil-collection
  :ensure evil-collection
  :after (evil)
  :demand t
  :config
  (evil-collection-init))

(use-package paredit
  :straight nil
  :ensure paredit)

(use-package evil-paredit
  :ensure evil-paredit
  :after (evil paredit))

(provide 'my-evil)
