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
  ;; Explicitly set pylsp as the server (if necessary)
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pylsp"))))

(use-package eldoc
  :straight (:type built-in)
  :custom
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-echo-area-max-lines 8)
  :config
  (general-define-key
   :states 'normal
   :keymaps 'eglot-managed-mode-map
   "K"      #'eldoc-box-help-at-point))

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
