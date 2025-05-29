;; generic language server protocol -*- lexical-binding: t; -*-

(use-package eglot
  :straight nil
  :ensure nil  ;; Eglot is built-in since Emacs 29
  :hook (((python-mode rust-mode) . eglot-ensure))
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

  ;; Explicitly set pylsp as the server (if necessary)
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pylsp"))))

(use-package eldoc
  :straight (:type built-in)
  :custom
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-echo-area-max-lines 8)
  :config)

(use-package eldoc-box
  :after (eldoc eglot)
  :demand t
  :ensure t
  :straight t
  :config
  (general-define-key
   :states 'normal
   :keymaps 'eglot-managed-mode-map
   "K"      #'eldoc-box-help-at-point))

(provide 'my-lsp)
