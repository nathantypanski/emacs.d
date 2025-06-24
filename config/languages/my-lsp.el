;; generic language server protocol -*- lexical-binding: t; -*-

(use-package eglot
  :straight nil
  :ensure nil  ;; Eglot is built-in since Emacs 29
  :hook (((python-mode rust-mode go-mode) . eglot-ensure))
  :custom
  ;; be more responsive
  (eglot-send-changes-idle-time 0.1)
  ;; shut down unused servers
  (eglot-autoshutdown t)
  (eglot-code-action-indicator "*")
  :config
  (setq-default eglot-workspace-configuration
                '(:rust-analyzer
                  (:cargo
                   (:buildScripts (:enable t))
                   :procMacro (:enable t)
                   :diagnostics (:disabled ["unresolved-proc-macro" "unresolved-macro-call"]))))

  ;; Explicitly set language servers
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs
               '(go-mode . ("gopls"))))

(use-package eldoc
  :straight (:type built-in)
  :custom
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-echo-area-max-lines 8)
  (eldoc-display-functions '(eldoc-display-in-echo-area
                             eldoc-display-in-buffer))
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  (general-define-key
   :states     'normal
   :keymaps    'eglot-managed-mode-map
   "K"        #'eglot-help-at-point)
  (global-eldoc-mode -1))

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
