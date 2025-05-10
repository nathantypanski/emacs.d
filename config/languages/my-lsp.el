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
  ((nix-mode go-mode rust-mode) . lsp-deferred)
  ;; when LSP’s completion system is initialized, switch to Consult/Vertico
  (lsp-completion-mode . (lambda ()
                           (setq-local completion-in-region-function
                                       #'consult-completion-in-region)))
  :custom
  (ls-log-io nil)
  (read-process-output-max (* 1024 1024)) ;; 1mb
  (gc-cons-threshold (* 100 1024 1024))   ;; 100mb
  (lsp-completion-provider :capf)
  (lsp-auto-configure t)
  ;; verbose logs can block/clutter
  (lsp-log-io t)
  ;; reduces flicker
  (lsp-progress-spinner-type 'none)
  :config
  ;; tweak a couple of defaults
  (progn
    (setq
     ;; supposedly prevents point-jumps
     lsp-completion-enable-additional-text-edit nil
     lsp-enable-snippet t
     lsp-completion-show-detail t
     lsp-prefer-flymake nil
     lsp-modeline-diagnostics-enable t
     lsp-gopls-server-path "/home/ndt/.nix-profile/bin/gopls")
    ;;"/nix/store/pdhabp3icm7bd8ym9lb0labmw91qcfj5-gopls-0.18.1/bin/gopls")
    (add-hook 'lsp-completion-mode-hook   #'my-vertico-toggle-capf)
    ))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)
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
      (my-with-suppressed-capf #'lsp-describe-thing-at-point))))

(provide 'my-lsp)
