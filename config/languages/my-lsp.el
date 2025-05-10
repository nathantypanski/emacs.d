;; generic language server protocol
;; (use-package exec-path-from-shell
;;   :ensure t
;;   :config
;;   ;; don’t invoke your login rc at all
;;   (setq exec-path-from-shell-arguments '())
;;   ;; just copy these two vars
;;   (exec-path-from-shell-copy-envs '("PATH" "GOPATH")))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  (go-mode . lsp-deferred)
  :init
  ;; automatically configure before/later hooks
  (setq lsp-auto-configure t
        lsp-log-io t
        ;; we use corfu/fido
        lsp-completion-provider :none
        ;; reduces flicker
        lsp-progress-spinner-type 'none
        ;;  remove clutter
        lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil
        ;; verbose logs can block/clutter
        lsp-log-io nil

        read-process-output-max (* 1024 1024) ;; 1mb
        gc-cons-threshold (* 100 1024 1024)   ;; 100mb

        lsp-keymap-prefix "C-c l")
  :config
  ;; tweak a couple of defaults
  (progn
  (setq lsp-enable-snippet nil
        lsp-prefer-flymake nil
         lsp-gopls-server-path "/home/ndt/.nix-profile/bin/gopls")
            ;;"/nix/store/pdhabp3icm7bd8ym9lb0labmw91qcfj5-gopls-0.18.1/bin/gopls")
))

(use-package lsp-ui
  :straight
  (lsp-ui :type git :host github :repo "emacs-lsp/lsp-ui")
  :hook (lsp-mode . lsp-ui-mode)
  :ensure t)

(provide 'my-lsp)
