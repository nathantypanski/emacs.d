(require 'cl)
(defun require-package (package)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))
(require-package 'use-package)
(require 'use-package)
(defmacro after (feature &rest body)
"After FEATURE is loaded, evaluate BODY."
(declare (indent defun))
`(eval-after-load ,feature
    '(progn ,@body)))

;; make sure $PATH is set correctly
(require-package 'exec-path-from-shell)
(ignore-errors ;; windows
  (exec-path-from-shell-initialize))

(require 'my-functions)
(require 'my-dirs)
(require 'my-buffers)

(use-package auto-complete
  :ensure auto-complete
  :disabled t
  :diminish auto-complete-mode
  :config
  (progn
    (setq
     ac-auto-show-menu 0.8
     ac-auto-start 2
     ac-comphist-file (expand-file-name ".cache/ac-comphist.dat" user-emacs-directory)
     ac-delay 0.1
     ac-quick-help-delay nil
     ac-use-fuzzy t
     ac-show-menu-immediately-on-auto-complete nil)
    (dolist (mode '(vimrc-mode))
      (add-to-list 'ac-modes mode))
    (after 'linum
      (ac-linum-workaround))
    (use-package auto-complete-config
      :config
      (progn
        (ac-config-default)
        )
      )
    )
  )

(use-package org
  :config
  (progn
    (global-set-key (kbd "C-c c") 'org-capture)
    (global-set-key (kbd "C-c a") 'org-agenda)
    (setq org-default-notes-file "~/.notes.org" org-log-done t)
    (defface org-block-begin-line '((t ( org-meta-line :background "gray27" :overline "gray20" :underline "gray20" :height 0.8)))
    "Face used for the line delimiting the begin of source blocks.")

    (defface org-block-background
    '((t (:background "#FFFFEA")))
    "Face used for the source block background.")

    (defface org-block-end-line
    '((t ( org-meta-line :background "gray27" :overline "gray20" :underline "gray20" :height 0.8)))
    "Face used for the line delimiting the end of source blocks.")

  (setq org-src-fontify-natively t)
  (use-package ob
    :config
    (progn
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((R . t)
         (emacs-lisp . t)
         (python . t)
         ))
      )
    )
  )
)

;; Colors!
(use-package zenburn-theme
  :ensure zenburn-theme
  :config
  (progn
    (load-theme 'zenburn t)))

(global-hl-line-mode t)

(set-face-background 'hl-line "#3e4446")

(use-package smart-mode-line
  :ensure smart-mode-line
  :config
  (progn
    (setq sml/theme 'dark)
    (sml/setup)
  ))

(use-package purty-mode
  :ensure purty-mode
  :config
  (progn
    (purty-mode)
    (purty-add-pair '("::" . "::"))
    (purty-add-pair '("=>" . "⇒"))
    (purty-add-pair '("forall" . "∀"))
    (purty-add-pair '("->" . "→"))
    (purty-add-pair '("<-" . "←"))
    (purty-add-pair '(\"Xi\" . \"Ξ\"))
    ))


;;      (lambda ()
;;                  (setq-local linum-format-fmt
;;                              (let ((w (length (number-to-string
;;                                                (count-lines (point-min) (point-max))))))
;;                                (concat "%" (number-to-string w) "d"))))))
;;    (defun linum-format-func (line)
;;      (concat
;;       (propertize (format linum-format-fmt line) 'face 'linum)
;;       (propertize " " 'face 'mode-line)))
;;    (unless window-system
;;      (setq linum-format 'linum-format-func))
;;    (setq linum-format "%4d "
;;          linum-delay t)
;;    )
;;  )

(use-package rainbow-mode
  :ensure rainbow-mode)

(use-package rainbow-delimiters
  :ensure rainbow-delimiters
  :init
  (progn
    (global-rainbow-delimiters-mode)
  )
  )

;; 80 column
(setq whitespace-style '(trailing))
(global-whitespace-mode 1)
(diminish 'global-whitespace-mode)

(use-package windsize
  :ensure windsize
  :init
  (progn
    (setq windsize-cols 16)
    (setq windsize-rows 8)
    (windsize-default-keybindings)
    )
  )

; Basic copy-paste setup. From wiki.
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

; Brilliant working copy-paste (even in Evil mode!) ripped from:
; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
(unless window-system
    (when (getenv "DISPLAY")
      ;; Callback for when user cuts
      (defun xsel-cut-function (text &optional push)
        ;; Insert text to temp-buffer, and "send" content to xsel stdin
        (with-temp-buffer
          (insert text)
          ;; I prefer using the "clipboard" selection (the one the
          ;; typically is used by c-c/c-v) before the primary selection
          ;; (that uses mouse-select/middle-button-click)
          (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
      ;; Call back for when user pastes
      (defun xsel-paste-function()
        ;; Find out what is current selection by xsel. If it is different
        ;; from the top of the kill-ring (car kill-ring), then return
        ;; it. Else, nil is returned, so whatever is in the top of the
        ;; kill-ring will be used.
        (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
          (unless (string= (car kill-ring) xsel-output)
            xsel-output )))
      ;; Attach callbacks to hooks
      (setq interprogram-cut-function 'xsel-cut-function)
      (setq interprogram-paste-function 'xsel-paste-function)
      ;; Idea from
      ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
      ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
))

(use-package etags-select
  :ensure etags-select
  :init
    (setq etags-select-go-if-unambiguous t)
  )

(use-package projectile
  :ensure projectile
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-cache-file (concat user-emacs-directory ".cache/projectile.cache"))
    (setq projectile-known-projects-file (concat user-emacs-directory ".cache/projectile-bookmarks.eld"))
    (add-to-list 'projectile-globally-ignored-directories "elpa")
    (add-to-list 'projectile-globally-ignored-directories ".cache")
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (projectile-global-mode 1)
    )
  )

(use-package helm
  :ensure helm
  :config
  (progn
    (setq helm-command-prefix-key "C-c h")
    (setq helm-quick-update t)
    (use-package helm-swoop
      :ensure helm-swoop)
    (after 'helm-autoloads
      (global-set-key (kbd "C-x C-m") 'helm-M-x)
      (global-set-key (kbd "C-c C-m") 'helm-M-x)
      (after 'evil
        (define-key evil-visual-state-map (kbd "SPC SPC") 'smex)
        (define-key evil-normal-state-map (kbd "SPC SPC") 'smex)
        (define-key evil-normal-state-map (kbd "SPC o")   'helm-imenu)
        (define-key evil-normal-state-map (kbd "SPC e")   'helm-recentf)
        (define-key evil-normal-state-map (kbd "SPC t")   'helm-etags-select)
        (define-key evil-normal-state-map (kbd "SPC l")   'helm-swoop)
        (define-key evil-normal-state-map (kbd "SPC y")   'helm-show-kill-ring)
        (define-key evil-normal-state-map [f5] 'helm-mini)))
    (after 'projectile
      (use-package helm-projectile
        :ensure helm-projectile))
    )
  )

(use-package ido
  :config
  (progn
    (ido-mode 1)
    (ido-everywhere 1)
    (setq ido-enable-prefix nil)
    (setq ido-use-virtual-buffers t)
    (setq ido-enable-flex-matching t)
    (setq ido-create-new-buffer 'always)
    (setq ido-show-dot-for-dired t)
    (setq ido-confirm-unique-completion nil)
    (setq ido-enable-last-directory-history nil)
    (setq ido-use-filename-at-point 'guess)
    (setq ido-save-directory-list-file
          (concat user-emacs-directory ".cache/ido.last"))
    (use-package ido-ubiquitous
      :ensure ido-ubiquitous)
    (add-hook
     'ido-setup-hook
     (lambda()
       ;; On ido-find-file, let `~` mean `~/` for fastness.
       (define-key ido-file-dir-completion-map "~"
         (lambda ()(interactive)
           (ido-set-current-directory "~/")
           (setq ido-exit 'refresh)
           (exit-minibuffer)))))
    (use-package ido-ubiquitous
      :config
      (progn
        (ido-ubiquitous-mode 1)
        )
      )
    (use-package flx-ido
      :ensure flx-ido
      :defines (ido-cur-item ido-default-item ido-cur-list)
      :config
      (progn
        (flx-ido-mode 1)
        )
      )
    (use-package ido-vertical-mode
      :ensure ido-vertical-mode
      :config
      (progn
        (ido-vertical-mode)
        )
      )
    (after 'evil
      (define-key evil-normal-state-map (kbd "SPC b") 'ibuffer)
      (define-key evil-normal-state-map (kbd "SPC k") 'ido-kill-buffer)
      (define-key evil-normal-state-map (kbd "SPC f") 'ido-find-file)
      )
    )
  )

(use-package smex
  :ensure smex
  :config
  (progn
    (global-set-key (kbd "M-x") 'smex)
    (setq smex-save-file (concat user-emacs-directory ".cache/smex-items"))
    (smex-initialize)
))

(use-package scss-mode
  :ensure scss-mode
  :config
  (progn
    (autoload 'scss-mode "scss-mode")
    (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
    )
  )

(use-package haskell-mode
  :ensure haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :config
  (progn
    (define-key haskell-mode-map (kbd "C-x C-d") nil)

    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
    (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c M-.") nil)
    (define-key haskell-mode-map (kbd "C-c C-d") nil)
    (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
    (add-hook 'haskell-mode-hook
              (lambda ()
                (turn-on-haskell-doc-mode)
                (after 'evil
                  (setq evil-auto-indent nil))
                (turn-on-haskell-indentation)
                (ghc-init)
                (purty-mode)
                )
              )
    (use-package haskell-mode-autoloads)
    (use-package inf-haskell)
    (use-package haskell-cabal
      :init
      (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile)
      )
    (use-package hs-lint)
    )
  )

(use-package ag
  :ensure ag
  :commands (ag ag-mode ag-files ag-regexp-at-point)
  :init
  (progn
    (setq ag-highlight-search t)
    (add-hook 'ag-mode-hook (lambda () (toggle-truncate-lines t)))
    (add-hook 'ag-mode-hook (lambda () (linum-mode 0)))
  )
)

(use-package project-explorer
  :ensure project-explorer
  :commands (progn project-explorer project-explorer-open pe/show-file)
  :config
  (progn
    (setq pe/omit-regex (concat pe/omit-regex "\\|^node_modules$"))
    )
  )

(use-package flycheck
   :ensure flycheck
   :diminsh flycheck-mode 
   :config
   (progn
     (setq flycheck-check-syntax-automatically '(save mode-enabled))
     (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
     (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
     (global-flycheck-mode 1)
    (after 'evil (add-hook 'flycheck-error-list-mode-hook (lambda () (evil-mode 0))))
    )
)


(show-paren-mode 1)

(add-to-list 'load-path "~/devel/rust/rust/etc/emacs")
(use-package rust-mode)

(use-package elisp-slime-nav
  :ensure elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :config
  (progn
    (defun my-lisp-hook ()
      (progn
        (elisp-slime-nav-mode)
        (turn-on-eldoc-mode)))
    (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
    (add-hook 'lisp-interaction-mode-hook 'my-lisp-hook)
    (add-hook 'ielm-mode-hook 'my-lisp-hook)
    )
  )

(use-package key-chord
  :ensure key-chord
  :diminish key-chord-mode
  :config
  (progn
    (key-chord-mode 1)))

(use-package undo-tree
   :ensure undo-tree
   :diminish undo-tree-mode
   :init
   (progn
     (use-package evil
       :ensure evil
       :config
       (progn
         (evil-mode 1)
         (setq evil-want-C-u-scroll t
             evil-want-C-w-in-emacs-state t
             evil-search-module 'evil-search
             evil-magic 'very-magic
             evil-emacs-state-cursor '("red" box)
             evil-normal-state-cursor '("green" box)
             evil-insert-state-cursor '("orange" bar)
             )
         (setq evil-replace-state-cursor '("red" box))

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

;  (define-minor-mode evil-org-src-mode
;    "Buffer local minor mode for evil-org-src"
;    :init-value nil
;    :lighter " EvilOrgSrc"
;    :keymap (make-sparse-keymap) ; defines evil-org-mode-map
;    :group 'evil-org
;    )

  (add-hook 'org-mode-hook 'evil-org-mode) ;; only load with org-mode
;  (add-hook 'org-src-mode-hook 'evil-org-src-mode)
;  (add-hook 'org-src-mode-hook (lambda () ((diminish 'evil-org-src-mode))))

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

;  (evil-define-key 'normal evil-org-src-mode-map
;    "&" 'org-edit-src-exit)

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
        '(normal insert))
  )

(use-package expand-region
  :ensure expand-region)

(define-key minibuffer-local-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'my-minibuffer-keyboard-quit)

(after 'package
  (after 'evil
    (evil-add-hjkl-bindings package-menu-mode-map 'emacs))
  )

(after 'project-explorer-autoloads
  (after 'project-explorer
    (after 'evil
      (define-key project-explorer-mode-map (kbd "C-l") 'evil-window-right)))

(global-set-key [f2] 'project-explorer-open)
  (global-set-key [f3] 'pe/show-file))

(after 'comint
  (define-key comint-mode-map [up] 'comint-previous-input)
  (define-key comint-mode-map [down] 'comint-next-input))

(after 'auto-complete
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous))

(after 'expand-region-autoloads
  (global-set-key (kbd "C-=") 'er/expand-region))

;; mouse scrolling in terminal
(unless (display-graphic-p)
  (global-set-key [mouse-4] (bind (scroll-down 1)))
  (global-set-key [mouse-5] (bind (scroll-up 1))))

(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x g") 'my-google)
(global-set-key (kbd "C-c e") 'my-eval-and-replace)

;; have no use for these default bindings
(global-unset-key (kbd "C-x m"))

(eval-when-compile (require 'cl))       ; for `dotimes', `push' (Emacs 21)

(defcustom unbound-modifiers '(control meta shift)
"Modifiers to consider when searching for unbound keys."
:type '(set (const control) (const meta) (const shift)
           (const super) (const hyper) (const alt)))

(defvar unbound-key-list
(let (keys)
 (dotimes (i (- ?\d ?\  -1))
   (push (+ i ?\ ) keys))
 (dotimes (i 12)
   (push (intern (format "f%s" (1+ i))) keys))
 (append '(?\t ?\r ?\e) (nreverse keys)
         '(insert delete home end prior next up down left right)))
"Keys to consider when searching for unbound keys.")

(defun key-complexity (key)
"Return a complexity score for key sequence KEY.
Currently KEY must be of the [(control shift ?s) ...] format."
(let ((ret 0))
 (dotimes (i (length key) ret)
   (setq ret (+ ret (* i 2) (key-complexity-1 (aref key i)))))))

;; This is somewhat biased for US keyboards.
(defun key-complexity-1 (key)           ; key:=(modifiers... key)
(+ (if (memq 'control key) 1 0)
  (if (memq 'meta key) 2 0)
  (if (memq 'shift key) 3 0)
  (if (memq 'super key) 4 0)
  (if (memq 'hyper key) 4 0)
  (if (memq 'alt key) 3 0)
  (* 2 (1- (length key)))
  (progn
    (setq key (car (last key)))
    (if (integerp key)
        (cond ((and (>= key ?a) (<= key ?z)) 0)
              ((and (>= key ?A) (<= key ?Z)) 6) ; capitals are weird
              ((and (>= key ?0) (<= key ?9)) 2)
              ((memq key '(?\b ?\r ?\ )) 1)
              ;; Unshifted punctuation (US keyboards)
              ((memq key '(?` ?- ?= ?\t ?[ ?] ?\\ ?\; ?' ?, ?. ?/)) 3)
              ;; Other letters -- presume that one's keyboard has them if
              ;; we're going to consider binding them.
              ((let (case-fold-search)
                 (string-match
                  "[016A]" (category-set-mnemonics
                            (char-category-set key)))) 2)
              (t 5))
      7))))


(use-package guide-key
  :ensure guide-key
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x" "C-c"))
    (setq guide-key/recursive-key-sequence-flag t)
    (guide-key-mode 1)
    ; I'm relatively new to emacs, so having a short delay is beneficial.
    (setq guide-key/idle-delay 0.1)
    )
  )

(use-package ace-jump-mode
  :ensure ace-jump-mode
  :config
  (progn
    (after 'evil
      ; Not sure if the `after` here is necessary, but anyway:
      (after 'ace-jump-mode-autoloads
        (define-key evil-normal-state-map (kbd "SPC j") 'ace-jump-char-mode)
        (define-key evil-motion-state-map (kbd "SPC") 'evil-ace-jump-char-mode)
        (define-key evil-motion-state-map (kbd "S-SPC") 'evil-ace-jump-line-mode))
      ; These will definitely work:
      (key-chord-define evil-normal-state-map "jw" 'ace-jump-word-mode)
      (key-chord-define evil-normal-state-map "jc" 'ace-jump-char-mode)
      (key-chord-define evil-normal-state-map "jl" 'ace-jump-line-mode))
    ))

(provide 'my-startup)