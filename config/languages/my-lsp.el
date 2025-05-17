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
  ((sh-mode nix-mode go-mode rust-mode) . lsp-deferred)
  ;; when LSP’s completion system is initialized, switch to Consult/Vertico
  :custom
  (ls-log-io nil)
  (read-process-output-max (* 1024 1024)) ;; 1mb
  (gc-cons-threshold (* 100 1024 1024))   ;; 100mb
  (lsp-completion-provider :company-capf)
  (lsp-auto-configure t)
  ;; verbose logs can block/clutter
  (lsp-log-io t)
  ;; reduces flicker
  (lsp-progress-spinner-type 'none)
  (lsp-enable-folding nil)
  (lsp-lens-enable nil)
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

    (let* ((session (lsp-session))
           (blk     (lsp-session-folders-blocklist session))
           (folder  (my-home-path "src/github.com/nathantypanski/dotfiles/.emacs.d")))
      (unless (member folder blk)
        (setf (lsp-session-folders-blocklist session)
              (cons folder blk))))

    (advice-add
     'lsp-lens--make
     :filter-return
     (lambda (text)
       (replace-regexp-in-string "[^[:ascii:]]+" "" text)))))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-delay 2)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-side 'left))

(provide 'my-lsp)
