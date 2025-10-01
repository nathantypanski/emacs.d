;; my-languages.el -*- lexical-binding: t; -*-
;;
;; Integrations between emacs and programming languages.
;;

;;--------------------------------------------------------------------
;; LSP - language server protocol
;;
(use-package eglot
  :straight nil
  :ensure nil  ;; Eglot is built-in since Emacs 29
  :demand t
  :config
  ;; Debug eglot-ensure
  (defun my-debug-eglot-ensure ()
    "Debug version of eglot-ensure with logging."
    (message "my-debug-eglot-ensure called in %s" major-mode)
    (condition-case err
        (eglot-ensure)
      (error (message "eglot-ensure failed: %s" err))))

  ;; Comprehensive buffer filtering for LSP
  (defun my-should-start-lsp-p ()
    "Determine if LSP should start for the current buffer."
    (and buffer-file-name
         ;; Must be a real file on disk
         (file-exists-p buffer-file-name)
         ;; Not a temporary file
         (not (string-match-p "~.*~$" (buffer-name)))
         ;; Not a revision/backup file
         (not (string-match-p "\\.#" (buffer-name)))
         ;; Not in a temporary directory
         (not (string-match-p "/tmp/" buffer-file-name))
         ;; Not a TRAMP file (remote files can be slow)
         (not (file-remote-p buffer-file-name))
         ;; Not too large (LSP can struggle with huge files)
         (< (buffer-size) (* 5 1024 1024)) ; 5MB limit
         ;; Not a read-only file
         (not buffer-read-only)
         ;; In a project or reasonable directory
         (or (project-current)
             (string-match-p (regexp-quote (expand-file-name "~")) buffer-file-name))))

  ;; Check if LSP server is available for current mode
  (defun my-lsp-server-available-p ()
    "Check if an LSP server is configured and available for current major mode."
    (when-let ((server-config (assoc major-mode eglot-server-programs)))
      (let ((server-command (cdr server-config)))
        (cond
         ;; Function-based server command
         ((functionp server-command) t)
         ;; List-based command - check first executable
         ((listp server-command)
          (executable-find (car server-command)))
         ;; String command
         ((stringp server-command)
          (executable-find server-command))
         (t nil)))))

  ;; Enhanced logging for LSP issues
  (defun my-log-lsp-status (action &optional details)
    "Log LSP status with contextual information."
    (message "[LSP %s] %s in %s (mode: %s, project: %s)"
             action
             (or details "")
             (buffer-name)
             major-mode
             (if (project-current) "yes" "no")))

  ;; Safe eglot-ensure wrapper with comprehensive checks
  (defun my-safe-eglot-ensure ()
    "Start eglot only for appropriate buffers with proper error handling."
    (cond
     ((not (my-should-start-lsp-p))
      (my-log-lsp-status "SKIP" "buffer not suitable for LSP"))
     ((not (my-lsp-server-available-p))
      (my-log-lsp-status "SKIP" "no server available"))
     (t
      (condition-case err
          (progn
            (eglot-ensure)
            (my-log-lsp-status "START" "server started successfully"))
        (error
         (my-log-lsp-status "ERROR" (error-message-string err)))))))

  ;; Project-aware LSP management
  (defun my-cleanup-unused-lsp-servers ()
    "Clean up LSP servers for projects that are no longer open."
    (interactive)
    (dolist (server (eglot--all-major-modes))
      (let ((server-instance (eglot-current-server)))
        (when (and server-instance
                   (not (seq-some (lambda (buf)
                                    (with-current-buffer buf
                                      (and (eglot--managed-p)
                                           (eq (eglot-current-server) server-instance))))
                                  (buffer-list))))
          (eglot-shutdown server-instance)))))

  ;; Auto-cleanup timer (run every 10 minutes)
  (run-with-timer 600 600 'my-cleanup-unused-lsp-servers)

  ;; Enhanced mode hook with delay for better startup
  (defun my-delayed-eglot-ensure ()
    "Start eglot after a short delay to avoid startup conflicts."
    (run-with-idle-timer 0.5 nil 'my-safe-eglot-ensure))

  ;; Add hooks for both python-mode and python-ts-mode
  (add-hook 'python-mode-hook 'my-delayed-eglot-ensure)
  (add-hook 'python-ts-mode-hook 'my-delayed-eglot-ensure)
  (add-hook 'rust-mode-hook 'my-delayed-eglot-ensure)
  (add-hook 'go-mode-hook 'my-delayed-eglot-ensure)
  (add-hook 'nix-mode-hook 'my-delayed-eglot-ensure)
  (add-hook 'ruby-mode-hook 'my-delayed-eglot-ensure)
  (add-hook 'ruby-ts-mode-hook 'my-delayed-eglot-ensure)
  :custom
  ;; be more responsive
  (eglot-send-changes-idle-time 0.1)
  ;; shut down unused servers
  (eglot-autoshutdown t)
  (eglot-code-action-indicator "*")
  :config
  (setq-default eglot-workspace-configuration
                '(:pylsp
                  (:plugins
                   (:pydocstyle (:enabled t)
                                :rope_completion (:enabled t)
                                :jedi_completion (:include_params t :include_class_objects t)
                                :jedi_hover (:enabled t)
                                :jedi_signatures (:enabled t)
                                :jedi_definition (:enabled t)
                                :jedi_references (:enabled t)
                                :pylint (:enabled t)
                                :flake8 (:enabled t)))
                  :rust-analyzer
                  (:cargo
                   (:buildScripts (:enable t))
                   :procMacro (:enable t)
                   :diagnostics (:disabled
                                 ["unresolved-proc-macro"
                                  "unresolved-macro-call"]))))

  ;; Helper to find project python executable
  (defun my-python-find-executable ()
    "Find the appropriate Python executable for the current project."
    (interactive)
    (or
     ;; Check for venv in common locations relative to project root
     (when-let* ((project (project-current))
                 (root (project-root project)))
       (or (executable-find (expand-file-name "venv/bin/python" root))
           (executable-find (expand-file-name ".venv/bin/python" root))
           (executable-find (expand-file-name "env/bin/python" root))))
     ;; Fall back to system python
     (executable-find "python")))

  ;; Helper function to build pylsp command
  (defun my-pylsp-command (&rest _ignored)
    "Return pylsp command with appropriate Python executable."
    (interactive)
    (executable-find "pylsp")
    (let ((name "pylsp")) (start-process-shell-command name (get-buffer-create name) (list (my-python-find-executable) "-m" name)))

    )

  ;; Debug eglot startup
  (defun my-debug-eglot ()
    "Debug eglot startup issues."
    (interactive)
    (message "Python mode: %s, Eglot managed: %s, LSP server: %s"
             (derived-mode-p 'python-mode)
             (bound-and-true-p eglot--managed-mode)
             (when (bound-and-true-p eglot--managed-mode)
               (eglot-current-server))))

  ;; Explicitly set language servers with dynamic python path
  (add-to-list 'eglot-server-programs
               '(python-mode . my-pylsp-command))
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . my-pylsp-command))
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (add-to-list 'eglot-server-programs '(nix-mode . ("nixd")))
  ;; Helper function to find project root reliably
  (defun my-find-project-root (&optional dir)
    "Find project root by looking for .git directory or using project.el.
Starts from DIR or current directory if DIR is nil."
    (let ((start-dir (or dir default-directory)))
      (or
       ;; Try project.el first
       (when-let ((project (project-current nil start-dir)))
         (project-root project))
       ;; Fallback: look for .git directory
       (locate-dominating-file start-dir ".git")
       ;; Last resort: current directory
       start-dir)))

  ;; Helper function to check if Gemfile contains ruby-lsp
  (defun my-gemfile-contains-ruby-lsp-p (gemfile-path)
    "Check if GEMFILE-PATH contains ruby-lsp gem."
    (when (and gemfile-path (file-exists-p gemfile-path))
      (with-temp-buffer
        (insert-file-contents gemfile-path)
        (goto-char (point-min))
        (re-search-forward "gem ['\"]ruby-lsp['\"]" nil t))))

  ;; Smart Ruby LSP command that handles different gemfile locations
  (defun my-ruby-lsp-server-command (&rest _ignored)
    "Return Ruby LSP command with appropriate bundler setup.
Detects project gemfile configuration and uses the right bundler command."
    (interactive)
    (let* ((project-root (my-find-project-root))
           (ruby-lsp-gemfile (when project-root
                              (expand-file-name ".ruby-lsp/Gemfile" project-root)))
           (main-gemfile (when project-root
                          (expand-file-name "Gemfile" project-root))))
      (cond
       ;; Main Gemfile exists and contains ruby-lsp - use standard bundle exec
       ((and main-gemfile (my-gemfile-contains-ruby-lsp-p main-gemfile))
        (list "bundle" "exec" "ruby-lsp"))
       ;; .ruby-lsp/Gemfile exists - use it with explicit BUNDLE_GEMFILE
       ((and ruby-lsp-gemfile (file-exists-p ruby-lsp-gemfile))
        (list "env" (format "BUNDLE_GEMFILE=%s" ruby-lsp-gemfile)
              "bundle" "exec" "ruby-lsp"))
       ;; Main Gemfile exists but no ruby-lsp - try bundle exec anyway
       ((and main-gemfile (file-exists-p main-gemfile))
        (list "bundle" "exec" "ruby-lsp"))
       ;; No bundler setup - try direct ruby-lsp
       (t
        (if (executable-find "ruby-lsp")
            (list "ruby-lsp")
          ;; Last fallback - try with bundle anyway
          (list "bundle" "exec" "ruby-lsp"))))))

  ;; Ruby language server - use smart command for proper project context
  (add-to-list 'eglot-server-programs
               '(ruby-mode . my-ruby-lsp-server-command))
  (add-to-list 'eglot-server-programs
               '(ruby-ts-mode . my-ruby-lsp-server-command))

  ;; Sorbet LSP (alternative Ruby type checker LSP)
  ;; Use this for projects with Sorbet type annotations
  ;; To use: M-x my-ruby-use-sorbet-lsp in a Ruby buffer
  (defun my-ruby-use-sorbet-lsp ()
    "Switch to using Sorbet LSP for current Ruby project."
    (interactive)
    (setq-local eglot-server-programs
                (cons '(ruby-mode . ("bundle" "exec" "srb" "--lsp" "."))
                      (cons '(ruby-ts-mode . ("bundle" "exec" "srb" "--lsp" "."))
                            (seq-filter (lambda (entry)
                                          (not (memq (car entry) '(ruby-mode ruby-ts-mode))))
                                        eglot-server-programs))))
    (when (eglot-current-server)
      (eglot-shutdown (eglot-current-server)))
    (eglot-ensure)
    (message "Switched to Sorbet LSP")))


;;--------------------------------------------------------------------
;; eldoc.el --- Show fn arglist or variable docstring in echo area
;;
;; Used to provide documentation in programming language modes.
;;
(use-package eldoc
  :straight (:type built-in)
  :custom
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-echo-area-max-lines 8)
  (eldoc-documentation-strategy #'eldoc-documentation-default)
  :config
  (general-define-key
   :states     'normal
   :keymaps    'eglot-managed-mode-map
   "K"        #'eglot-help-at-point)
  ;; Disable automatic eldoc but keep it available for eglot
  (global-eldoc-mode 1)
  (defun my-configure-eldoc-mode ()
    "Run eldoc mode setup."
    (interactive)
    (eldoc-mode 1)

    (setq eldoc-display-functions
          '(eldoc-display-in-echo-area eldoc-display-in-buffer))
    (setq-local eldoc-idle-delay 0.5))

  ;; Enable eldoc only in eglot-managed buffers
  (add-hook 'eglot-managed-mode-hook
            'my-configure-eldoc-mode)

  ;; Documentation buffer names for different modes
  (defconst my-doc-buffer-names
    '((python-mode . "*Python Doc*")
      (python-ts-mode . "*Python Doc*")
      (go-mode . "*godoc*")
      (go-ts-mode . "*godoc*")
      (rust-mode . "*rust-doc*")
      (rust-ts-mode . "*rust-doc*")
      (ruby-mode . "*Ruby Doc*")
      (ruby-ts-mode . "*Ruby Doc*")
      (emacs-lisp-mode . "*Help*")
      (lisp-interaction-mode . "*Help*"))
    "Alist mapping major modes to their documentation buffer names.")

  (defconst my-language-doc-functions
    '((python-mode . my-python-pydoc-help)
      (python-ts-mode . my-python-pydoc-help)
      (go-mode . godoc-at-point)
      (go-ts-mode . godoc-at-point)
      (rust-mode . rust-doc)
      (rust-ts-mode . rust-doc)
      (ruby-mode . my-ruby-help)
      (ruby-ts-mode . my-ruby-help)
      (emacs-lisp-mode . my-doc-at-point)
      (lisp-interaction-mode . my-doc-at-point))
    "Alist mapping major modes to their documentation display functions.")


  (defun my-eldoc-doc-buffer-popup (&optional arg)
    "Show eldoc documentation in a popup buffer.
With prefix ARG, force refresh."
    (interactive "P")
    (eldoc)
    (my-popup-buffer (eldoc-doc-buffer)))

  (defun my-eldoc-enhanced-help (&optional arg)
    "Show enhanced documentation with language-specific behavior.
With prefix ARG, use mode-specific documentation if available."
    (interactive "P")
    (if-let ((doc-fn (and arg (alist-get major-mode my-language-doc-functions))))
        (cond
         ;; Special case: Elisp uses our custom doc function
         ((memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
          (my-doc-at-point))
         ;; Other languages: call their function and popup result
         ((fboundp doc-fn)
          (let ((display-buffer-alist '((".*" . (display-buffer-no-window)))))
            (funcall doc-fn))
          (when-let ((buffer-name (alist-get major-mode my-doc-buffer-names)))
            (my-popup-buffer buffer-name)))
         (t (message "Documentation function %s not available" doc-fn)))
      ;; Default: popup eldoc
      (my-eldoc-doc-buffer-popup)))

  (defun my-python-pydoc-help ()
    "Show detailed Python help using pydoc."
    (interactive)
    (let* ((symbol (thing-at-point 'symbol))
           (python-exe (my-python-find-executable))
           (cmd (format "%s -c \"import pydoc; help('%s')\"" python-exe symbol)))
      (with-output-to-temp-buffer "*Python Help*"
        (shell-command cmd "*Python Help*"))))

  (defun my-ruby-help ()
    "Show Ruby documentation for symbol at point using LSP server."
    (interactive)
    (if (and (fboundp 'eglot-current-server) (eglot-current-server))
        (condition-case err
            (eglot-help-at-point)
          (error
           ;; Fall back to eldoc if eglot-help-at-point fails
           (message "LSP help failed (%s), trying eldoc..." (error-message-string err))
           (eldoc)
           (eldoc-doc-buffer)))
      (message "No LSP server active")))

  ;; Eldoc buffer display functions
  (defun my-show-eldoc-buffer ()
    "Display *eldoc* buffer in a side window."
    (interactive)
    (eldoc)  ; Trigger eldoc first
    (when-let ((eldoc-buf (get-buffer "*eldoc*")))
      (display-buffer-in-side-window
       eldoc-buf
       '((side . bottom) (window-height . 0.25)))))

  (defun my-show-eldoc-transient ()
    "Show *eldoc* buffer temporarily."
    (interactive)
    (eldoc)  ; Trigger eldoc first
    (when-let ((eldoc-buf (get-buffer "*eldoc*")))
      (let ((win (display-buffer-in-side-window
                  eldoc-buf
                  '((side . bottom) (window-height . 0.4))))
            (map (make-sparse-keymap)))
        ;; Any key closes it
        (define-key map [t] (lambda () (interactive) (delete-window win)))
        (set-transient-map map t)))))


;; Shows eldoc popups in a child frame/box, makes multiline docstrings
;; readable.
(use-package eldoc-box
  :after (eldoc general)
  :custom
  (eldoc-box-use-child-frame (display-graphic-p))  ; Use child frames in GUI, overlays in terminal
      (set-face-attribute 'eldoc-box-body nil
                          :family "DepartureMono Nerd Font" :height 0.7)


  (eldoc-box-clear-with-C-g t)                     ; Allow C-g to clear popups
  (eldoc-box-max-pixel-width 800)                  ; Reasonable max width
  (eldoc-box-max-pixel-height 400)                 ; Reasonable max height
  :config
  (when (display-graphic-p)
    (message "configuring eldoc-box for graphical mode")
    (eldoc-box-hover-mode 1)
    (eldoc-box-hover-at-point-mode 1)))

(use-package treesit
  :straight (:type built-in)  ; treesit is built into Emacs 29+
  :when (treesit-available-p)
  :custom
  ;; don't autoinstall grammars. Use nix with home-manager to get them.
  (treesit-auto-install-grammar nil)
  :config
  (setq treesit-language-source-alist
        (cond
         ((eq system-type 'gnu/linux)
          '())
         ((eq system-type 'darwin)
          '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master"
                        "typescript/src")
            (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))))

  (defun my-treesit-install-all-languages ()
    "Install all configured tree-sitter languages."
    (interactive)
    (dolist (lang-config treesit-language-source-alist)
      (let ((lang (car lang-config)))
        (unless (treesit-language-available-p lang)
          (message "Installing tree-sitter grammar for %s" lang)
          (treesit-install-language-grammar lang)))))

  ;; Set up auto-mode-alist for tree-sitter modes
  (setq major-mode-remap-alist
        '((bash-mode . bash-ts-mode)
          (sh-mode . bash-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (python-mode . python-ts-mode)
          (rust-mode . rust-ts-mode)
          (go-mode . go-ts-mode)
          (js-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (ruby-mode . ruby-ts-mode)
          (bash-mode . bash-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (sh-mdoe . bash-ts-mode))))

;;--------------------------------------------------------------------
;; shell (bash, zsh, ...)
;;
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; -*- lexical-binding: t; -*-
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))

(defun my-set-evil-shift-width ()
  "Set Evil's shift width for C editing."
  (after 'evil
    (setq evil-shift-width 8)))

(add-hook 'c-initialization-hook 'my-set-evil-shift-width)

(setq c-hungry-delete-key t)

(defun my-c-mode-common-setup ()
  "Setup C/C++-mode common configurations."
  ;; (setq flycheck-clang-language-standard "c17")
  (c-set-offset 'case-label '+))

(defun my-c++-mode-setup ()
  "Setup C++-mode configurations."
  (interactive)
  (google-set-c-style)
  (after 'flycheck
    ;; (setq flycheck-clang-language-standard "c++11")
    (after 'projectile
      (if (projectile-project-root)
          (add-to-list 'flycheck-clang-include-path (concat (projectile-project-root) "src")))
      )))


(add-hook 'c-mode-common-hook 'my-c-mode-common-setup)
(add-hook 'c++-mode-hook 'my-c++-mode-setup)

(after 'evil
  (evil-define-key 'normal c-mode-map (kbd "K")   'my-woman-entry)
  (evil-define-key 'insert c-mode-map (kbd "<backspace>") 'backward-delete-char-untabify)
  (evil-define-key 'normal c++-mode-map (kbd "K")   'my-woman-entry)
  (evil-define-key 'insert c++-mode-map (kbd "<backspace>") 'backward-delete-char-untabify))

;;--------------------------------------------------------------------
;; clojure
;;
(use-package clojure-mode
  :ensure clojure-mode
  :defer t
  :commands clojure-mode clojure-mode-men)

(use-package cider
  :ensure cider
  :after clojure-mode
  :defer t
  :commands clojure-mode cider cider-run cider-doc
  :config
  (progn
    ;; always eldoc in cider mode
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

    (after 'evil
      (defun my-evil-cider-repl-insert ()
        "Enter insert mode at the prompt, if we're behind the prompt."
        (interactive)
        (if (> cider-repl-input-start-mark (point))
            (goto-char cider-repl-input-start-mark))
        (evil-insert-state))

      (defun my-evil-cider-repl-append ()
        "Enter insert mode at the prompt, if we're behind the prompt."
        (interactive)
        (if (> cider-repl-input-start-mark (point))
            (goto-char cider-repl-input-start-mark))
        (evil-append))

      (defun my-cider-on-prompt-line ()
        "Returns t if point is on the same line as the current prompt."
        (my-same-line (point) (cider-repl-input-start-mark)))

      (defun my-evil-cider-repl-bol ()
        "Perform cider-bol, but never go before the prompt."
        (interactive)
        (cider-bol)
        (if (> cider-repl-input-start-mark (point))
            (goto-char cider-repl-input-start-mark)))

      ;; http://blog.jenkster.com/2013/12/a-cider-excursion.html
      (defun cider-namespace-refresh ()
        "Reset the Clojure namespace."
        (interactive)
        (cider-interactive-eval
         "(require 'clojure.tools.namespace.repl)
        (clojure.tools.namespace.repl/refresh)"))


      (setq nrepl-hide-special-buffers t)
      (evil-define-key 'insert cider-repl-mode-map
        (kbd "<up>") 'cider-repl-previous-input
        (kbd "<down>") 'cider-repl-next-input
        (kbd "C-c C-d") 'cider-doc-map
        (kbd "M-.") 'cider-jump
        (kbd "M-,") 'cider-jump-back
        (kbd "C-c M-.") 'cider-jump-to-resource
        (kbd "RET") 'cider-repl-return
        (kbd "TAB") 'cider-repl-tab
        (kbd "C-<return>") 'cider-repl-closing-return
        (kbd "C-j") 'cider-repl-newline-and-indent
        (kbd "C-c C-o") 'cider-repl-clear-output
        (kbd "C-c M-o") 'cider-repl-clear-buffer
        (kbd "C-c M-n") 'cider-repl-set-ns
        (kbd "C-c C-u") 'cider-repl-kill-input
        (kbd "C-a") 'cider-repl-bol
        (kbd "C-S-a") 'cider-repl-bol-mark
        [home] 'cider-repl-bol
        [S-home] 'cider-repl-bol-mark
        (kbd "C-<up>") 'cider-repl-backward-input
        (kbd "C-<down>") 'cider-repl-forward-input
        (kbd "M-p") 'cider-repl-previous-input
        (kbd "M-n") 'cider-repl-next-input
        (kbd "M-r") 'cider-repl-previous-matching-input
        (kbd "M-s") 'cider-repl-next-matching-input
        (kbd "C-c C-n") 'cider-repl-next-prompt
        (kbd "C-c C-p") 'cider-repl-previous-prompt
        (kbd "C-c C-b") 'cider-interrupt
        (kbd "C-c C-c") 'cider-interrupt
        (kbd "C-c C-m") 'cider-macroexpand-1
        (kbd "C-c M-m") 'cider-macroexpand-all
        (kbd "C-c C-z") 'cider-switch-to-last-clojure-buffer
        (kbd "C-c M-s") 'cider-selector
        (kbd "C-c M-f") 'cider-load-fn-into-repl-buffer
        (kbd "C-c C-q") 'cider-quit
        (kbd "C-c M-i") 'cider-inspect
        (kbd "C-c M-t") 'cider-toggle-trace
        (kbd "C-c C-x") 'cider-refresh
        (kbd "C-x C-e") 'cider-eval-last-sexp
        (kbd "C-c C-r") 'cider-eval-region
        )
      (evil-define-key 'visual cider-repl-mode-map
        (kbd "0") 'cider-repl-bol
        )
      (evil-define-key 'normal cider-repl-mode-map
        (kbd "i") 'my-evil-cider-repl-insert
        (kbd "<up>") 'cider-repl-previous-input
        (kbd "<down>") 'cider-repl-next-input
        (kbd "C-c C-d") 'cider-doc-map
        (kbd "M-.") 'cider-jump
        (kbd "M-,") 'cider-jump-back
        (kbd "C-c M-.") 'cider-jump-to-resource
        (kbd "RET") 'cider-repl-return
        (kbd "TAB") 'cider-repl-tab
        (kbd "C-<return>") 'cider-repl-closing-return
        (kbd "C-j") 'cider-repl-newline-and-indent
        (kbd "C-c C-o") 'cider-repl-clear-output
        (kbd "C-c M-o") 'cider-repl-clear-buffer
        (kbd "C-c M-n") 'cider-repl-set-ns
        (kbd "C-c C-u") 'cider-repl-kill-input
        (kbd "C-a") 'cider-repl-bol
        (kbd "C-S-a") 'cider-repl-bol-mark
        (kbd "0") 'cider-repl-bol
        ;; [S-home] 'cider-repl-bol-mark
        (kbd "C-<up>") 'cider-repl-backward-input
        (kbd "C-<down>") 'cider-repl-forward-input
        (kbd "M-p") 'cider-repl-previous-input
        (kbd "M-n") 'cider-repl-next-input
        (kbd "M-r") 'cider-repl-previous-matching-input
        (kbd "M-s") 'cider-repl-next-matching-input
        (kbd "C-c C-n") 'cider-repl-next-prompt
        (kbd "C-c C-p") 'cider-repl-previous-prompt
        (kbd "C-c C-b") 'cider-interrupt
        (kbd "C-c C-c") 'cider-interrupt
        (kbd "C-c C-m") 'cider-macroexpand-1
        (kbd "C-c M-m") 'cider-macroexpand-all
        (kbd "C-c C-z") 'cider-switch-to-last-clojure-buffer
        (kbd "C-c M-s") 'cider-selector
        (kbd "C-c M-f") 'cider-load-fn-into-repl-buffer
        (kbd "C-c C-q") 'cider-quit
        (kbd "C-c M-i") 'cider-inspect
        (kbd "C-c M-t") 'cider-toggle-trace
        (kbd "C-c C-x") 'cider-refresh
        (kbd "C-x C-e") 'cider-eval-last-sexp
        (kbd "C-c C-r") 'cider-eval-region))))

;;--------------------------------------------------------------------
;; elisp
;;
;; Always eldoc in lispy modes.

(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(when (not (display-graphic-p))
  (setq eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p nil
        eldoc-echo-area-prefer-doc-buffer t))

(use-package slime
  :straight nil
  :ensure slime)

(use-package elisp-slime-nav
  :after slime
  :ensure elisp-slime-nav
  :commands my-jump-to-elisp-docs
  :diminish elisp-slime-nav-mode
  :init
  (defun my-lisp-hook ()
    (elisp-slime-nav-mode)
    (turn-on-eldoc-mode))
  (add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
  (add-hook 'lisp-interaction-mode-hook 'my-lisp-hook)
  (add-hook 'ielm-mode-hook 'my-lisp-hook)
  (defun my-jump-to-elisp-docs (sym-name)
    "Jump to a pane and do elisp-slime-nav-describe-elisp-thing-at-point"
    (interactive (list (elisp-slime-nav--read-symbol-at-point)))
    (help-xref-interned (intern sym-name))))

(defun my-electric-lisp-comment ()
  "Autocomment things for lisp."
  (interactive)
  ;; we can get away with autocommenting on empty lines.
  ;; not so much on regular ones - that's more likely to be a mistake.
  (if (my-is-this-line-empty)
      (insert ";; ")
    (insert ";")))


(use-package emacs
  :straight nil
  :after slime elisp-slime-nav
  :config

  (after 'evil
    (evil-define-key 'insert emacs-lisp-mode-map ";" 'my-electric-lisp-comment)
    (evil-define-key 'normal emacs-lisp-mode-map "\C-c\C-c" 'eval-defun))
  (add-hook 'emacs-lisp-mode-hook 'my-setup-elisp-mode)

  (defun my-setup-elisp-mode ()
    "Commands to be run at the start of Emacs Lisp mode."
    (eldoc-mode)
    (my-coding-mode-eyecandy))

  :hook (emacs-lisp-mode . my-setup-elisp-mode))

;; XXX when do we actually use this? For anything?
(use-package slime-company
  :straight t
  :after (slime company)
  :defer t
  :custom
  (slime-company-completion 'fuzzy)
  (slime-company-after-completion 'slime-company-just-one-space))

;; -------------------------------------------------------------------
;; go language
;;
;; Settings for the Go programming language.

(use-package go-mode
  :commands go-mode godoc
  :ensure go-mode
  :mode "\\.go\\'"
  :config
  (progn
    (defun my-godoc--buffer-sentinel (proc event)
      "Modified sentinel function run when godoc command completes.
Doesn't jump to buffer automatically. Enters help mode on buffer."
      (with-current-buffer (process-buffer proc)
        (cond ((string= event "finished\n")  ;; Successful exit.
               (goto-char (point-min))
               (view-mode 1)
               (help-mode)
               )
              ((/= (process-exit-status proc) 0)  ;; Error exit.
               (let ((output (buffer-string)))
                 (kill-buffer (current-buffer))
                 (message (concat "godoc: " output)))))))


    (defun my-jump-to-go-docs ()
      "Jump to a pane and do godoc"
      (interactive)
      (let ((query (thing-at-point 'word)))
        (if (set-process-sentinel
             (start-process-shell-command "godoc" (godoc--get-buffer query)
                                          (concat "godoc " query))
             'my-godoc--buffer-sentinel)
            nil)
        (let ((helpdoc (-first
                        (lambda (e) (string-match ".*godoc.*" (buffer-name e)))
                        (buffer-list))))
          (pop-to-buffer (buffer-name helpdoc)))))

    (defun my-setup-go-mode ()
      "Configure go-mode settings and keybindings."
      (setq evil-shift-width 8)
      (setq indent-tabs-mode t)
      (evil-define-key 'normal go-mode-map (kbd "K") 'my-jump-to-go-docs))

    (add-hook 'go-mode-hook 'my-setup-go-mode)))


(use-package elisp-autofmt
  :ensure t)

;;  -*- lexical-binding: t; -*-
;; (eval-after-load "tex"
;;   '(remove-from-list 'TeX-command-list
;; 		'("Latex Make" "latexmk %(-pdf) %t" TeX-run-TeX) t)
;;   )
;; (use-package auctex
;;   :ensure auctex
;;   :config
;;   (progn
;;     (setq-default TeX-master nil)
;;     ;; (setq-default TeX-command-default "Latex Make")
;;
;;     (setq TeX-view-program-list '(("zathura" "zathura -s -x \"emacsclient --no-wait +%%{line} %%{input}\" %o")))
;;     ;; Always use PDF mode
;;     (setq TeX-PDF-mode t)
;;
;;     ;; View my PDFs with Zathura
;;     (setq TeX-view-program-selection '((output-pdf "zathura")))
;;
;;
;;     ;; Maybe someday this will actually work and I can sync PDFs with Zathura
;;     (setq TeX-source-correlate-mode t)
;;     (setq TeX-source-correlate-method 'synctex)
;;     (setq TeX-source-correlate-start-server t)
;;     (setq TeX-auto-save t)
;;     (setq TeX-parse-self t)
;;     (setq-default TeX-master nil)
;;     (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;;     (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;;     (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
;;     (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
;;     (add-hook 'LaTeX-mode-hook 'orgtbl-mode)
;;
;;     ;; source:
;;     ;; http://tex.stackexchange.com/questions/185688/how-to-force-emacs-with-auctex-to-show-compilation-in-new-buffer
;;     (add-to-list 'TeX-command-list '("pdfLaTeX" "pdflatex -shell-escape %t" TeX-run-interactive nil t))
;;     (defcustom tex-my-viewer "zathura --fork -s -x \"emacsclient --eval '(progn (switch-to-buffer  (file-name-nondirectory \"'\"'\"%{input}\"'\"'\")) (goto-line %{line}))'\""
;;       "PDF Viewer for TeX documents. You may want to fork the viewer
;;     so that it detects when the same document is launched twice, and
;;     persists when Emacs gets closed.
;;
;;     Simple command:
;;
;;       zathura --fork
;;
;;     We can use
;;
;;       emacsclient --eval '(progn (switch-to-buffer  (file-name-nondirectory \"%{input}\")) (goto-line %{line}))'
;;
;;     to reverse-search a pdf using SyncTeX. Note that the quotes and double-quotes matter and must be escaped appropriately."
;;       :safe 'stringp)
;;
;;     ;; (use-package auctex-latexmk
;;     ;;   :ensure auctex-latexmk
;;     ;;   :init
;;     ;;   (progn
;;     ;;     (auctex-latexmk-setup)
;;     ;;     )
;;     ;; )
;;     ))

;;--------------------------------------------------------------------
;; Python language
;;



;; Better REPL integration
(defun my-python-send-line ()
  "Send current line to Python shell."
  (interactive)
  (python-shell-send-region (line-beginning-position) (line-end-position))
  (forward-line))

(defun my-python-send-paragraph ()
  "Send current paragraph to Python shell."
  (interactive)
  (save-excursion
    (mark-paragraph)
    (python-shell-send-region (region-beginning) (region-end))))

(defun my-python-common-setup ()
  "Common Python configuration."
  (setq-local python-indent-offset 4
              python-indent-guess-indent-offset-verbose nil
              indent-tabs-mode nil)
  ;; Disable evil auto-indent for python-ts-mode
  (my-disable-insert-indent)

  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "-i")
  (setq python-shell-prompt-detect-failure-warning nil)
  ;; Better indentation
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset-verbose nil))

;; Setup function for python-mode
(defun my-python-mode-setup ()
  "Configure python-mode settings."
  (interactive)
  (my-python-common-setup)
  )

(defun my-python-ts-mode-setup ()
  "Configure python-ts-mode settings."
  (my-python-common-setup)

  (setq-local python-indent-guess-indent-offset-verbose nil)
  (setq-local indent-tabs-mode nil))

;; Debug function to check if insert indent is disabled
(defun my-debug-python-indent ()
  "Debug python indentation settings."
  (interactive)
  (message "my-should-insert-indent: %s, major-mode: %s"
           my-should-insert-indent major-mode))

(use-package python-ts-mode
                                        ;mimi

  :straight nil
  :when (treesit-available-p)
  :ensure nil
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset t)
  :mode (("\\.py\\'" . python-ts-mode)
         ("\\.pyi\\'" . python-ts-mode))
  :hook ((python-ts-mode . my-disable-insert-indent)
         (python-ts-mode . my-python-ts-mode-setup)
         (python-ts-mode . my-disable-insert-indent))
  :config
  ;; Setup function for python-ts-mode
  (minibuffer-message "entering python-ts-mode!"))

;; Fallback Python mode.
;;
;; The package is python" but the mode is "python-mode".
(use-package python
  :when (not (bound-and-true-p treesit-available-p))
  :ensure nil  ; builtin package
  :mode ("\\.py\\'" . python-mode)

  :commands (python-shell-switch-to-shell
             python-shell-send-region
             python-shell-send-buffer
             run-python
             python-shell-interpreter)

  :hook ((python-mode . my-disable-insert-indent)
         (python-mode . my-python-mode-setup))
  :config

  ;; Key bindings for Python mode
  (after 'evil
    (evil-define-key 'normal python-mode-map
      (kbd ", r") 'run-python
      (kbd ", s") 'python-shell-switch-to-shell
      (kbd ", l") 'my-python-send-line
      (kbd ", p") 'my-python-send-paragraph
      (kbd ", b") 'python-shell-send-buffer
      (kbd ", f") 'python-shell-send-file)

    (evil-define-key 'visual python-mode-map
      (kbd ", r") 'python-shell-send-region)))

;; Python testing with pytest
(use-package python-pytest
  :ensure t
  :after python
  :config
  (after 'evil
    (evil-define-key 'normal python-mode-map
      (kbd ", t t") 'python-pytest
      (kbd ", t f") 'python-pytest-file
      (kbd ", t k") 'python-pytest-function
      (kbd ", t r") 'python-pytest-repeat
      (kbd ", t p") 'python-pytest-popup)))

;;--------------------------------------------------------------------
;; rust language
;;

(use-package rust-mode
  :ensure rust-mode
  :commands rust-mode
  :config
  (progn
    (defun my-rust-electric-rbrace (&optional _)
      "Insert a rbrace, and then indent the line properly"
      (interactive "*P")
      (insert "}")
      (rust-mode-indent-line)
      )
    (after 'evil
      (defvar rust-mode-map () "Keymap used in Rust mode.")
      (defun my-rust-setup ()
        "Make rust do things the way I like it."
        (interactive)
        (setq tab-width 4)
        (evil-define-key 'insert rust-mode-map "}" 'my-rust-electric-rbrace)
        (use-local-map rust-mode-map)
        (flycheck-mode 0))
      (add-hook 'rust-mode-hook 'my-rust-setup))))


;; -------------------------------------------------------------------
;; scss language
;;

(use-package css-mode
  :commands css-mode
  :ensure css-mode
  :mode "\\.css\\'"
  )

(use-package scss-mode
  :commands scss-mode
  :ensure scss-mode
  :mode "\\.scss\\'"
  :config
  (progn
    (setq scss-compile-at-save nil)
    )
  )

;; -------------------------------------------------------------------
;; haskell language
;;

;; haskell-mode is loaded by the use-package declaration below
(use-package haskell-mode
  :ensure haskell-mode
  :commands haskell-mode
  :config
  (progn
    (setq haskell-process-show-debug-tips nil)
    (setq haskell-process-type 'cabal-repl)
    (defun my-haskell-autoloads ()
      "Autoloads for entering Haskell-mode."
      (turn-on-haskell-doc-mode)
      (after 'evil (setq evil-auto-indent nil))

      (electric-indent-mode 0)
      (turn-on-haskell-indentation))

    (add-hook 'haskell-mode-hook 'my-haskell-autoloads)

    (defun my-haskell-interactive-evil-bol ()
      "If λ prompt is before point, go to bol. Otherwise, go to λ."
      (interactive)
      (if (> haskell-interactive-mode-prompt-start (point))
          (evil-beginning-of-line)
        (my-haskell-interactive-jump-to-prompt)))

    (defun my-haskell-interactive-jump-to-prompt ()
      "Go to the prompt."
      (goto-char haskell-interactive-mode-prompt-start))

    (defun my-haskell-interactive-maybe-jump-to-prompt ()
      "Go to the prompt if it's after POINT. Otherwise do nothing."
      (if (> haskell-interactive-mode-prompt-start (point))
          (my-haskell-interactive-jump-to-prompt)))

    (defun my-haskell-interactive-evil-insert ()
      "If the λ prompt is before point, enter insert state. Otherwise, insert after the prompt"
      (interactive)
      (my-haskell-interactive-maybe-jump-to-prompt)
      (evil-insert-state))

    (defun my-haskell-interactive-evil-append (count &optional vcount skip-empty-lines)
      "If the comint prompt is before point, just do evil-append. Otherwise, insert after the prompt"
      (interactive
       (list (prefix-numeric-value current-prefix-arg)
             (and (evil-visual-state-p)
                  (memq (evil-visual-type) '(line block))
                  (save-excursion
                    ;; go to upper-left corner temporarily so
                    ;; `count-lines' yields accurate results
                    (evil-visual-rotate 'upper-left)
                    (count-lines evil-visual-beginning evil-visual-end)))))
      (if (> haskell-interactive-mode-prompt-start (point))
          (my-haskell-interactive-jump-to-prompt))
      (evil-append count vcount skip-empty-lines))

    (defun my-haskell-interactive-evil-append-line (count &optional vcount)
      "Go to the end of the prompt if before it.

       Otherwise, behave like a normal Evil append line."
      (interactive "p")
      (if (> haskell-interactive-mode-prompt-start (point))
          (progn
            (goto-char (point-max))
            (evil-insert-state))
        (evil-append-line count vcount)))

    (defun my-haskell-interactive-history-previous (arg)
      "Go to the prompt if we're before it. Then cycle through previous history."
      (interactive "*p")
      (my-haskell-interactive-maybe-jump-to-prompt)
      (haskell-interactive-mode-history-previous arg))

    (defun my-haskell-interactive-history-next (arg)
      "Go to the prompt if we're before it. Then cycle through next history."
      (interactive "*p")
      (my-haskell-interactive-maybe-jump-to-prompt)
      (haskell-interactive-mode-history-next arg))

    (after 'evil
      (evil-set-initial-state 'haskell-error-mode 'emacs)
      (evil-define-key 'insert haskell-interactive-mode-map
        (kbd "<up>")          'my-haskell-interactive-history-previous
        (kbd "<down>")        'my-haskell-interactive-history-next
        (kbd "RET")           'haskell-interactive-mode-return)
      (evil-define-key 'normal haskell-interactive-mode-map
        (kbd "<up>")          'my-haskell-interactive-history-previous
        (kbd "<down>")        'my-haskell-interactive-history-next
        (kbd "0")             'my-haskell-interactive-evil-bol
        (kbd "A")             'my-haskell-interactive-evil-append-line
        (kbd "a")             'my-haskell-interactive-evil-append
        (kbd "i")             'my-haskell-interactive-evil-insert
        (kbd "RET")           'haskell-interactive-mode-return)
      (evil-define-key 'normal haskell-mode-map (kbd "C-x C-d") nil)
      (evil-define-key 'normal haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
      (evil-define-key 'normal haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
      (evil-define-key 'normal haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
      (evil-define-key 'normal haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
      (evil-define-key 'normal haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
      (evil-define-key 'normal haskell-mode-map (kbd "C-c M-.") nil)
      (evil-define-key 'normal haskell-mode-map (kbd "C-c C-d") nil))
    )
  )

;;--------------------------------------------------------------------
;; markdown language
;;

(use-package markdown-mode
  :commands markdown-mode
  :ensure markdown-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.page\\'" . markdown-mode))
  (add-hook 'markdown-mode-hook 'visual-line-mode))


;; -*- lexical-binding: t; -*-
(use-package js2-mode
  :commands js2-mode
  :ensure js2-mode
  :mode "\\.js\\'")


;;--------------------------------------------------------------------
;; jade language
;;
;; Jade (HTML templating) editing.

(use-package jade-mode
  :ensure jade-mode
  :commands jade-mode
  :mode "\\.jade\\'")


;;--------------------------------------------------------------------
;; yaml
;;

(use-package yaml-mode
  :ensure yaml-mode
  :commands yaml-mode
  :mode "\\.yml\\'")


;;--------------------------------------------------------------------
;; yasnippet - Emacs snippets for programming languages
;;

(use-package yasnippet
  :ensure yasnippet)

;; -*- lexical-binding: t; -*-
(use-package dockerfile-mode
  :commands dockerfile-mode
  :ensure dockerfile-mode
  :mode "\\Docker\\'")

(use-package docker
  :commands docker
  :ensure docker)


;; -------------------------------------------------------------------
;; Ruby language support
;;
;; Complete Ruby development environment with:
;; - Tree-sitter syntax highlighting (ruby-ts-mode preferred)
;; - LSP integration via ruby-lsp (bundle exec for project context)
;; - Sorbet type checker support with custom functions
;; - Evil-friendly delimiter pairing via electric-pair-mode
;; - Eldoc documentation integration
;;
;; Setup requirements:
;; - Ruby 3.x with ruby-lsp gem in project Gemfile
;; - Optional: sorbet gems for type checking
;; - Tree-sitter Ruby grammar (automatically installed)
;;
;; Tree-sitter Ruby mode (preferred)
(use-package ruby-ts-mode
  :straight (:type built-in)
  :when (treesit-available-p)
  :ensure nil
  :mode ("\\.rb\\'" "\\.rake\\'" "Rakefile\\'" "Gemfile\\'" "\\.gemspec\\'")
  :config
  ;; Ruby indentation and formatting
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil)

  ;; Key bindings for ruby-ts-mode
  (after 'evil
    (evil-define-key 'normal ruby-ts-mode-map
      (kbd "K") 'my-ruby-help
      (kbd ", t") 'my-ruby-sorbet-typecheck
      (kbd ", i") 'my-ruby-sorbet-init
      (kbd ", r") 'ruby-send-region
      (kbd ", b") 'ruby-send-buffer)))

;; Fallback Ruby mode (when tree-sitter not available)
(use-package ruby-mode
  :ensure ruby-mode
  :when (not (treesit-available-p))
  :commands ruby-mode
  :mode ("\\.rb\\'" "\\.rake\\'" "Rakefile\\'" "Gemfile\\'" "\\.gemspec\\'")
  :config
  (progn
    ;; Ruby indentation and formatting
    (setq ruby-indent-level 2
          ruby-indent-tabs-mode nil
          ruby-deep-indent-paren nil
          ruby-bounce-deep-indent nil)

    ;; Helper functions
    (defun my-ruby-find-executable ()
      "Find the appropriate Ruby executable for the current project."
      (or
       ;; Check for project-specific Ruby (rbenv, rvm, etc.)
       (when-let* ((project (project-current))
                   (root (project-root project)))
         (or (executable-find (expand-file-name "bin/ruby" root))))
       ;; Fall back to system ruby
       (executable-find "ruby")))

    ;; Sorbet type checker integration
    ;; Use M-x my-ruby-use-sorbet-lsp to switch from ruby-lsp to sorbet LSP
    (defun my-ruby-sorbet-typecheck ()
      "Run Sorbet type checker on current project using bundle exec."
      (interactive)
      (if-let ((project-root (project-root (project-current))))
          (let ((default-directory project-root))
            (compile "bundle exec srb tc"))
        (message "Not in a project directory")))

    (defun my-ruby-sorbet-init ()
      "Initialize Sorbet configuration in current project."
      (interactive)
      (if-let ((project-root (project-root (project-current))))
          (let ((default-directory project-root))
            (shell-command "bundle exec srb init"))
        (message "Not in a project directory")))

    ;; Key bindings
    (after 'evil
      (evil-define-key 'normal ruby-mode-map
        (kbd "K") 'my-ruby-help
        (kbd ", t") 'my-ruby-sorbet-typecheck
        (kbd ", i") 'my-ruby-sorbet-init
        (kbd ", r") 'ruby-send-region
        (kbd ", b") 'ruby-send-buffer))))

;; -------------------------------------------------------------------
;; Nix language
;;

(require 'man)
(when (eq Man-header-file-path t)
  (setq Man-header-file-path nil))

(use-package nix-haskell-mode
  :commands (nix-haskell-mode)
  :ensure nix-haskell-mode)

(use-package nix-mode
  :commands nix-mode
  :ensure nix-mode
  :mode "\\.nix\\'")

(use-package graphviz-dot
  :ensure graphviz-dot
  :mode "\\dot\\'"
  :init
  (add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode)))

;; disable the (buggy) ess-r-mode autodetection
(after 'ess-r-package
  (setq ess-r-package-auto-activate nil))

(provide 'my-all-languages)
