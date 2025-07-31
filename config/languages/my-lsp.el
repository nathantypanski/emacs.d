;; generic language server protocol -*- lexical-binding: t; -*-

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

  ;; Add hooks for both python-mode and python-ts-mode
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'python-ts-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
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
                    :jedi_signatures (:enabled t)))
                  :rust-analyzer
                  (:cargo
                   (:buildScripts (:enable t))
                   :procMacro (:enable t)
                   :diagnostics (:disabled ["unresolved-proc-macro" "unresolved-macro-call"]))))

  ;; Helper to find project python executable
  (defun my-python-find-executable ()
    "Find the appropriate Python executable for the current project."
    (or
     ;; Check for venv in common locations relative to project root
     (when-let* ((project (project-current))
                 (root (project-root project)))
       (or (executable-find (expand-file-name "venv/bin/python" root))
           (executable-find (expand-file-name ".venv/bin/python" root))
           (executable-find (expand-file-name "env/bin/python" root))))
     ;; Fall back to system python
     (executable-find "python3")
     (executable-find "python")))

  ;; Helper function to build pylsp command
  (defun my-pylsp-command (&rest _ignored)
    "Return pylsp command with appropriate Python executable."
    (list (my-python-find-executable) "-m" "pylsp"))

  ;; Explicitly set language servers with dynamic python path
  (add-to-list 'eglot-server-programs '(python-mode . my-pylsp-command))
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . my-pylsp-command))
  (add-to-list 'eglot-server-programs
               '(go-mode . ("gopls"))))

(use-package eldoc
  :straight (:type built-in)
  :custom
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-echo-area-max-lines 8)
  (eldoc-display-functions '(eldoc-display-in-echo-area
                             eldoc-display-in-buffer))
  (eldoc-documentation-strategy #'eldoc-documentation-compose)
  :config
  (general-define-key
   :states     'normal
   :keymaps    'eglot-managed-mode-map
   "K"        #'eglot-help-at-point)
  ;; Disable automatic eldoc but keep it available for eglot
  (global-eldoc-mode -1)
  ;; Enable eldoc only in eglot-managed buffers
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (eldoc-mode 1)
              (setq-local eldoc-idle-delay 1.0)))

  ;; Debug eglot startup
  (defun my-debug-eglot ()
    "Debug eglot startup issues."
    (interactive)
    (message "Python mode: %s, Eglot managed: %s, LSP server: %s"
             (derived-mode-p 'python-mode)
             (bound-and-true-p eglot--managed-mode)
             (when (bound-and-true-p eglot--managed-mode)
               (eglot-current-server)))))

;; Shows eldoc popups in a child frame/box, makes multiline docstrings
;; readable.
(use-package eldoc-box
    :after (eldoc general)
    :custom
    (eldoc-echo-area-use-multiline-p t)
    :config
    (general-define-key
     :keymaps 'prog-mode-map
     "C-c d" #'eldoc-doc-buffer))

(provide 'my-lsp)
