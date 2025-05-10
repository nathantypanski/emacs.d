;; generic language server protocol

(use-package exec-path-from-shell
  :ensure t
  :config
  ;; don’t invoke your login rc at all
  (setq exec-path-from-shell-arguments '())
  ;; just copy these two vars
  (exec-path-from-shell-copy-envs '("PATH" "GOPATH")))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  ((go-mode rust-mode) . lsp-deferred)
  :custom
  (ls-log-io nil)
  (read-process-output-max (* 1024 1024)) ;; 1mb
  (gc-cons-threshold (* 100 1024 1024))   ;; 100mb
  :config
  ;; tweak a couple of defaults
  (progn
    (setq
     ;; automatically configure before/later hooks
     lsp-auto-configure t
     lsp-log-io t
     ;; reduces flicker
     lsp-progress-spinner-type 'none
     ;; verbose logs can block/clutter
     ;; supposedly prevents point-jumps
     lsp-completion-enable-additional-text-edit nil
     lsp-enable-snippet nil
     lsp-prefer-flymake nil
     lsp-gopls-server-path "/home/ndt/.nix-profile/bin/gopls")
    ;;"/nix/store/pdhabp3icm7bd8ym9lb0labmw91qcfj5-gopls-0.18.1/bin/gopls")
    ))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-sideline-enable t) ; enable docs popup
  (lsp-ui-doc-enable t) ; enable docs popup
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-doc-position 'at-point)
  :config
  (progn
    (defun my-with-suppressed-capf (fn)
      "Suppress `completion-in-region-function` while calling FN."
      (let ((completion-in-region-function #'completion--in-region))
        (funcall fn)))
    (defun my-lsp-doc-no-completion ()
      "Show LSP docs without triggering completions."
      (interactive)
      (my-with-suppressed-capf #'lsp-ui-doc-glance))))

;; (use-package lsp-ui
;;   :straight
;;   (lsp-ui :type git :host github :repo "emacs-lsp/lsp-ui")
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :ensure t
;;   :config
;;   (progn (setq
;;    lsp-ui-sideline-enable t
;;    lsp-ui-doc-enable t
;; )))

;; simple version of lsp
;; (use-package eglot
;;   :ensure t
;;   :hook ((nix-mode go-mode rust-mode python-mode js-mode) . eglot-ensure)
;;   :config
;;   (setq eglot-extend-to-xref t)) ;; optional: better rename behavior

(provide 'my-lsp)
