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
  (progn
    (evil-set-undo-system 'undo-redo)
    (setq evil-emacs-state-cursor   '("#dfaf8f" box))
    (setq evil-normal-state-cursor  '("#f8f893" box))
    (setq evil-insert-state-cursor  '("#f8f893" bar))
    (setq evil-replace-state-cursor '("#cc9393" box))

    (evil-set-initial-state 'flycheck-error-list-mode 'normal)
    (evil-set-initial-state 'git-commit-mode 'insert)
    (evil-set-initial-state 'shell-mode 'emacs)
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

    (defvar my-evil-last-insertion-end 0
"The distance between point at the time of insert and beginning of line.

This tracks the saved value of the last insertion so we can figure out whether
to indent for it.")

    (defvar my-evil-last-insertion-distance 0
"The distance between point at the time of insert and beginning of line.

This tracks the saved value of the last insertion so we can figure out whether
to indent for it.")

    (evil-define-motion my-append-and-indent (count)
      "Moves to end of line, enters insert mode, and also indents the line."
      (evil-append-line count)
      (when (my-sensible-to-indent-p)
          (indent-according-to-mode)))

(defun my-sensible-to-indent-p ()
  "Determine whether it's sensible to indent the current line automagically.

Using the data stored from my-exit-insert-state, this function determines
whether or not it makes sense to indent the following line. The point of this
is to ensure we don't indent lines after the user has manually tabbed point to
the beginning of a line, but we do indent lines if there was already an
indentation from the last insert state.

A potential future improvement is to (rather than blindly indenting according
to mode, which is a potshot) indent intelligently to the saved state of point."
  (interactive)
  (and (> my-evil-last-insertion-distance 0)
       (my-current-line-is-empty)))

    (defun my-save-insert-state-state ()
      "Save information about the state of insert state.

This does the following:

- Sets my-evil-last-insertion-end to the character position at the end of the last
  insert state.
- Sets my-evil-last-insertion-line to the position at the beginning of the line from
  the last insert state.
- Sets my-evil-last-insertion-distance to the distance between those two points.
- Deletes trailing whitespace to the left of point.

The intent of this is to save the state of the insertion environment, so we can
make smart decisions based on it later."
      (interactive)
      (setq my-evil-last-insertion-end
            (save-excursion
              (if (not (my-current-line-is-empty))
                  (beginning-of-line-text))
              (point)))
      (setq my-evil-last-insertion-line
            (save-excursion
              (goto-char my-evil-last-insertion-end)
              (line-beginning-position)))
      (setq my-evil-last-insertion-distance
            (- my-evil-last-insertion-end my-evil-last-insertion-line)))

    (evil-define-motion my-ret-and-indent (count)
      "Move the cursor COUNT lines down.
If point is on a widget or a button, click on it.
In Insert state, insert a newline and indent."
      :type line
      (my-save-insert-state-state)
      (my-delete-trailing-whitespace-at-line)
      (evil-ret-gen count nil)
      (when (my-sensible-to-indent-p)
               (indent-according-to-mode)))

    (defun my-what-line ()
      "Get the line, without printing the word 'line' before it."
      (1+ (count-lines 1 (point))))

    (defun my-where-beginning-of-visual-line ()
      "Calculate the difference between the beginning
of the current visual line and point."
      (interactive)
      (let ((old-point (point))
            (bovl (save-excursion (beginning-of-visual-line)
                                  (point))))
        (- old-point bovl)))

    (defun my-electric-append-with-indent (count &optional vcount)
      "Indent the current line if it is empty.

Otherwise, just do a normal append-line."
      (interactive "p")
      (if (and (= (point) (line-beginning-position))
               (my-is-this-line-empty))
          (indent-according-to-mode))
      (evil-append-line count vcount))

    (defun my-exit-insert-state ()
      "Function to be run when Evil exits insert state."
      (my-save-insert-state-state)
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

    ;; give us a bar cursor
    (add-hook 'evil-normal-state-entry-hook #'my/tty-cursor-update)
    (add-hook 'evil-insert-state-entry-hook #'my/tty-cursor-update)
    (add-hook 'evil-insert-state-exit-hook  #'my/tty-cursor-update))

    (define-key evil-normal-state-map (kbd "RET") 'my-append-and-indent)
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

    (defun my-evil-complete-or-indent ()
      "Try `completion-at-point`; otherwise indent."
      (interactive)
      (if (my-sensible-to-indent-p)
            (indent-according-to-mode)
        (completion-at-point)))

    (define-key evil-insert-state-map (kbd "TAB") #'my-evil-complete-or-indent)

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
