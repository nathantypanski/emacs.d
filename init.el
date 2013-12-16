(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))

(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory "config"))
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(require 'cl)

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)

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

(require-package 'diminish)
(use-package diminish
  :init
  (progn
    (after 'diminish-autoloads
      (diminish 'visual-line-mode)
      (after 'autopair (diminish 'autopair-mode))
      (after 'eldoc (diminish 'eldoc-mode))
      (after 'smartparens (diminish 'smartparens-mode))
      (after 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
      (after 'git-gutter+ (diminish 'git-gutter+-mode)))
    ))

(defmacro bind (&rest commands)
  "Convience macro which creates a lambda interactive command."
  `(lambda ()
     (interactive)
     ,@commands))

(defun my-window-killer ()
  "closes the window, and deletes the buffer if it's the last window open."
  (interactive)
  (if (> buffer-display-count 1)
      (if (= (length (window-list)) 1)
          (kill-buffer)
        (delete-window))
    (kill-buffer-and-window)))


(defun my-minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))


(defun set-transparency (alpha)
  "Sets the transparency of the current frame."
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha alpha))


(defun my-google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search Google: "))))))

(defun my-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(defun my-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))


(defun my-delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))


;; make sure $PATH is set correctly
(require-package 'exec-path-from-shell)
(ignore-errors ;; windows
  (exec-path-from-shell-initialize))


(defun my-terminal-config (&optional frame)
  "Establish settings for the current terminal."
  (if (not frame) ;; The initial call.
      (xterm-mouse-mode 1)
    ;; Otherwise called via after-make-frame-functions.
    (if xterm-mouse-mode
        ;; Re-initialise the mode in case of a new terminal.
        (xterm-mouse-mode 1))))
;; Evaluate both now (for non-daemon emacs) and upon frame creation
;; (for new terminals via emacsclient).
(my-terminal-config)
(add-hook 'after-make-frame-functions 'my-terminal-config)
;; Hide startup messages
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; move cursor to the last position upon open
(use-package saveplace
  :config
  (progn
    (setq save-place-file (concat user-emacs-directory ".cache/places"))
    (setq-default save-place t)
    )
  )

;; minibuffer history
(use-package savehist
  :config
    (progn
        (setq savehist-file (concat user-emacs-directory ".cache/savehist")
            savehist-additional-variables '(search ring regexp-search-ring)
            savehist-autosave-interval 60)
        (savehist-mode +1)
    )
)

;; recent files
(use-package recentf
  :config
  (progn
    (setq recentf-save-file (concat user-emacs-directory ".cache/recentf")
          recentf-max-saved-items 1000
          recentf-max-menu-items 500)
    (recentf-mode +1)
    ))

;; erc
(setq erc-log-channels-directory (concat user-emacs-directory ".cache/erc/logs"))

;; vc
(setq vc-make-backup-files t)


;; narrowing
(put 'narrow-to-region 'disabled nil)


;; dired
(require 'dired-x)


;; ediff
(setq ediff-split-window-function 'split-window-horizontally)


;; store most files in the cache
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory ".cache/backups")))
      auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory ".cache/backups") t))
      auto-save-list-file-prefix
      (concat user-emacs-directory ".cache/auto-save-list/.saves-"))


;; better scrolling
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t)


;; better buffer names for duplicates
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-ignore-buffers-re "^\\*" ; leave special buffers alone
      uniquify-after-kill-buffer-p t)


(defalias 'yes-or-no-p 'y-or-n-p)

(xterm-mouse-mode 1)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


(setq sentence-end-double-space nil
      delete-by-moving-to-trash t
      visible-bell t
      mark-ring-max 64
      global-mark-ring-max 128)


(which-function-mode 1)
(blink-cursor-mode -1)
(global-auto-revert-mode 1)
(electric-indent-mode 1)


(setq-default
 indent-tabs-mode nil)


(add-hook 'find-file-hook (lambda ()
                            (visual-line-mode)
                            (setq show-trailing-whitespace t)))

(random t) ;; seed


(require-package 'auto-complete)
(use-package auto-complete
  :diminish auto-complete-mode
  :config
  (progn
    (setq
     ac-auto-show-menu 0.01
     ac-auto-start 2
     ac-comphist-file (expand-file-name ".cache/ac-comphist.dat" user-emacs-directory)
     ac-delay 0.01
     ac-quick-help-delay 0.5
     ac-use-fuzzy t
     ac-show-menu-immediately-on-auto-complete t)
     (dolist (mode '(vimrc-mode))
       (add-to-list 'ac-modes mode))
     (after 'linum
       (ac-linum-workaround))
    (defadvice ac-expand (before advice-for-ac-expand activate)
        (when (yas-expand)
          (ac-stop)))
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
        (use-package ob
            :init
            (org-babel-do-load-languages
                'org-babel-load-languages
                '((plantuml . t)
                  )
                )
            )
        )
    )

(require-package 'smart-mode-line)
(use-package smart-mode-line
  :config
  (progn
    (setq sml/theme 'dark)
    (sml/setup)
    (setq)
  )
)
(require-package 'pretty-mode)
(use-package pretty-mode
  :config
  (progn
    (setq pretty-default-groups '(:function))
    (global-pretty-mode)
))

(require-package 'color-theme)
(use-package color-theme
  :config
  (progn (color-theme-sanityinc-tomorrow-night)
         )
  )


(require-package 'linum)
(use-package linum
  :config
  (progn
    (global-linum-mode 1)
    (unless window-system
      (add-hook 'linum-before-numbering-hook
                (lambda ()
                  (setq-local linum-format-fmt
                              (let ((w (length (number-to-string
                                                (count-lines (point-min) (point-max))))))
                                (concat "%" (number-to-string w) "d"))))))
    (defun linum-format-func (line)
      (concat
       (propertize (format linum-format-fmt line) 'face 'linum)
       (propertize " " 'face 'mode-line)))
    (unless window-system
      (setq linum-format 'linum-format-func))
    (global-hl-line-mode)
    (setq linum-format "%4d "
          linum-delay t)
    )
  )

; rainbow-mode is a minor mode for Emacs which displays strings
; representing colors with the color they represent as background.
(require-package 'rainbow-mode)
(use-package rainbow-mode)

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

(require-package 'autopair)
(use-package autopair
  :config
  (autopair-global-mode))

(require-package 'projectile)
(use-package projectile
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



(require-package 'helm)
(use-package helm
  :config
  (progn
    (setq helm-command-prefix-key "C-c h")
    (setq helm-quick-update t)
    (require-package 'helm-swoop)
    (after 'projectile
      (require-package 'helm-projectile))
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
    (setq ido-use-filename-at-point 'guess)
    (setq ido-save-directory-list-file (concat user-emacs-directory ".cache/ido.last"))
    (require-package 'ido-ubiquitous)
    (use-package ido-ubiquitous
      :config
      (progn
        (ido-ubiquitous-mode 1)
        )
      )
    (require-package 'flx-ido)
    (use-package flx-ido
      :defines (ido-cur-item ido-default-item ido-cur-list)
      :config
      (progn
        (flx-ido-mode 1)
        )
      )
    (require-package 'ido-vertical-mode)
    (use-package ido-vertical-mode
      :config
      (progn
        (ido-vertical-mode)
        )
      )
    )
  )

;; Smex is a M-x enhancement for Emacs. Built on top of IDO, it
;; provides a convenient interface to your recently and most
;; frequently used commands. And to all the other commands, too.
(require-package 'smex)
(use-package smex
  :config
  (progn
    (setq smex-save-file (concat user-emacs-directory ".cache/smex-items"))
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "C-x C-m") 'smex)
    (global-set-key (kbd "C-c C-m") 'smex)
    (smex-initialize)
))

(require-package 'magit)
(require-package 'gist)

(setq magit-diff-options '("--histogram"))
(after 'magit
  (global-set-key (kbd "C-x g") 'magit-status))

(global-git-gutter+-mode)

(require-package 'flycheck)
(use-package flycheck
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
    (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
    (global-flycheck-mode 1)
    )
  )


(require-package 'elisp-slime-nav)
(use-package elisp-slime-nav
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

(require-package 'haskell-mode)
(use-package haskell-mode
  :commands haskell-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.l?hs$" . haskell-mode))
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
    ;; <https://github.com/prooftechnique/.emacs.d/blob/
    ;; 6d08779adb8fe67acbe9ab82fe25e78a7fc40eb8/config/jhenahan-haskell.el>
    (add-hook 'haskell-mode-hook
              (lambda ()
                (turn-on-haskell-doc-mode)
                (setq evil-auto-indent nil)
                (turn-on-haskell-indentation)
                (ghc-init)))
    (add-hook 'haskell-interactive-mode
          (lambda ()
          (linum-mode 0)
          (evil-mode 0)))
    (use-package haskell-mode-autoloads)
    (use-package inf-haskell)
    (use-package haskell-cabal
      :init
      (progn
        (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))
    )
    (use-package hs-lint)))

(require-package 'evil)
(require-package 'evil-leader)
(require-package 'evil-visualstar)
(require-package 'evil-nerd-commenter)
(require-package 'evil-indent-textobject)
(require-package 'evil-matchit)
(require-package 'surround)

(setq evil-want-C-u-scroll t
    evil-want-C-w-in-emacs-state t
    evil-search-module 'evil-search
    evil-magic 'very-magic
    evil-emacs-state-cursor '("red" box)
    evil-normal-state-cursor '("green" box)
    evil-insert-state-cursor '("orange" bar)
    evilnc-hotkey-comment-operator "gc"
    )
(use-package evil
  :config
  (progn
    (use-package evil-leader
      :config
      (progn
        (evil-mode 1)
        (global-evil-leader-mode)
        (evil-leader/set-leader ",")
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
    (use-package evil-nerd-commenter)
    (require 'evil-indent-textobject)
    (require 'evil-visualstar)
    (require 'evil-matchit)
    (use-package surround
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

    (define-key evil-motion-state-map "gn" 'my-evil-next-match)
    (define-key evil-motion-state-map "gN" 'my-evil-previous-match)

    (defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (defadvice evil-ex-search-previous (after advice-for-evil-ex-search-previous activate)
      (evil-scroll-line-to-center (line-number-at-pos)))
    )
  )

(require-package 'ag)
(use-package ag
  :init
  (progn
    (setq ag-highlight-search t)
    (add-hook 'ag-mode-hook (lambda () (toggle-truncate-lines t)))
  )
)

(require-package 'project-explorer)
(use-package project-explorer
  :config
    (progn
      (setq pe/omit-regex (concat pe/omit-regex "\\|^node_modules$"))
    )
  )


(require-package 'ace-jump-mode)
(use-package ace-jump-mode)


(require-package 'expand-region)
(use-package expand-region)


(require-package 'editorconfig)
(use-package editorconfig)


(require-package 'etags-select)
(use-package etags-select
  :init
    (setq etags-select-go-if-unambiguous t)
  )

(require-package 'windsize)
(use-package windsize
  :init
  (progn
    (setq windsize-cols 16)
    (setq windsize-rows 8)
    (windsize-default-keybindings)
    )
  )

(require-package 'rainbow-delimiters)
(use-package rainbow-delimiters
  :init
  (progn
    (global-rainbow-delimiters-mode)
  )
  )

(require-package 'guide-key)
(use-package guide-key
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x" "C-c"))
    (setq guide-key/recursive-key-sequence-flag t)
    (guide-key-mode 1)
    )
)

(setq my-eshell-buffer-count 0)

(after 'evil
    (require-package 'key-chord)
    (key-chord-mode 1)
    (define-key evil-motion-state-map "gn" 'my-evil-next-match)
    (define-key evil-motion-state-map "gN" 'my-evil-previous-match)

    ;;; esc quits
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

    (after 'ace-jump
        (key-chord-define evil-normal-state-map "jw" 'ace-jump-word-mode)
        (key-chord-define evil-normal-state-map "jc" 'ace-jump-char-mode)
        (key-chord-define evil-normal-state-map "jl" 'ace-jump-line-mode))

  (after 'evil-matchit
    (define-key evil-normal-state-map "%" 'evilmi-jump-items))

  (after 'git-gutter+-autoloads
    (define-key evil-normal-state-map (kbd "[ h") 'git-gutter+-previous-hunk)
    (define-key evil-normal-state-map (kbd "] h") 'git-gutter+-next-hunk)
    (define-key evil-normal-state-map (kbd ", g a") 'git-gutter+-stage-hunks)
    (define-key evil-normal-state-map (kbd ", g r") 'git-gutter+-revert-hunks)
    (evil-ex-define-cmd "Gw" (bind (git-gutter+-stage-whole-buffer))))

  (after 'smex
    (define-key evil-visual-state-map (kbd "SPC SPC") 'smex)
    (define-key evil-normal-state-map (kbd "SPC SPC") 'smex))

  (define-key evil-normal-state-map (kbd "SPC o") 'imenu)
  (define-key evil-normal-state-map (kbd "SPC b") 'switch-to-buffer)
  (define-key evil-normal-state-map (kbd "SPC k") 'ido-kill-buffer)
  (define-key evil-normal-state-map (kbd "SPC f") 'ido-find-file)

  (after 'helm-autoloads
    (define-key evil-normal-state-map (kbd "SPC e") 'helm-recentf)
    (define-key evil-normal-state-map (kbd "SPC t") 'helm-etags-select)
    (define-key evil-normal-state-map (kbd "SPC l") 'helm-swoop)
    (define-key evil-normal-state-map (kbd "SPC y") 'helm-show-kill-ring)
    (define-key evil-normal-state-map [f5] 'helm-mini))

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

  (define-key evil-normal-state-map (kbd "Q") 'my-window-killer)
  (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

  (define-key evil-visual-state-map (kbd ", e") 'eval-region)

  ;; emacs lisp
  (after 'elisp-slime-nav-autoloads
    (evil-define-key 'normal emacs-lisp-mode-map (kbd "g d") 'elisp-slime-nav-find-elisp-thing-at-point)
    (evil-define-key 'normal emacs-lisp-mode-map (kbd "K") 'elisp-slime-nav-describe-elisp-thing-at-point))

  (after 'ag-autoloads
    (define-key evil-normal-state-map (kbd "SPC /") 'ag-regexp-project-at-point))

  (after 'multiple-cursors
    (define-key evil-visual-state-map (kbd "C->") 'mc/mark-all-like-this)
    (define-key evil-normal-state-map (kbd "C->") 'mc/mark-next-like-this)
    (define-key evil-normal-state-map (kbd "C-<") 'mc/mark-previous-like-this))

  (after 'ace-jump-mode-autoloads
    (define-key evil-normal-state-map (kbd "SPC j") 'ace-jump-char-mode)
    (define-key evil-motion-state-map (kbd "SPC") 'evil-ace-jump-char-mode)
    (define-key evil-motion-state-map (kbd "S-SPC") 'evil-ace-jump-line-mode))

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
  (evil-ex-define-cmd "QA" 'evil-quit-all))

;; escape minibuffer
(define-key minibuffer-local-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'my-minibuffer-keyboard-quit)

(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

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


(global-set-key [prior] 'previous-buffer)
(global-set-key [next] 'next-buffer)

(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x g") 'my-google)
(global-set-key (kbd "C-c e") 'my-eval-and-replace)

;; have no use for these default bindings
(global-unset-key (kbd "C-x m"))
