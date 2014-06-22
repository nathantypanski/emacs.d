;; my-interaction.el
;;
;; ido, helm, smart emacs navigation, etc.

(use-package projectile
  :ensure projectile
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

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (progn
    (after 'evil
      (define-key evil-insert-state-map (kbd "C-<tab>") 'yas-expand)
      )
    )
  )

(use-package helm
  :ensure helm
  :config
  (progn
    (require 'helm-files)
    (after 'projectile
      (use-package helm-projectile
        :ensure helm-projectile))
    (defun helm-jump ()
      "Find files with helm, but be smart about buffers and recent files."
      (interactive)
      (let ((helm-ff-transformer-show-only-basename nil))
        (helm-other-buffer '(helm-projectile-sources-list
                             helm-source-buffers-list
                             helm-source-recentf
                             helm-source-bookmarks
                             helm-source-file-cache
                             helm-source-files-in-current-dir
                             helm-source-locate
                             helm-source-buffer-not-found)
                           "*helm jump*")))

    (setq helm-command-prefix-key "C-c h")
    (setq helm-quick-update t)
    (use-package helm-swoop
      :ensure helm-swoop)
    (after 'helm-autoloads
      (global-set-key (kbd "C-x C-m") 'helm-M-x)
      (global-set-key (kbd "C-c C-m") 'helm-M-x)

      (after 'evil
        (define-key evil-visual-state-map (kbd "SPC SPC") 'helm-M-x)
        (define-key evil-normal-state-map (kbd "SPC SPC") 'helm-M-x)
        (define-key evil-normal-state-map (kbd "SPC o")   'helm-imenu)
        (define-key evil-normal-state-map (kbd "SPC e")   'helm-recentf)
        (define-key evil-normal-state-map (kbd "SPC t")   'helm-etags-select)
        (define-key evil-normal-state-map (kbd "SPC l")   'helm-swoop)
        (define-key evil-normal-state-map (kbd "SPC y")   'helm-show-kill-ring)
        (define-key evil-normal-state-map [f5] 'helm-mini)))
    (after 'flycheck
      (use-package helm-flycheck
        :ensure helm-flycheck))
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
    (setq ido-show-dot-for-dired t)
    (setq ido-confirm-unique-completion nil)
    (setq ido-enable-last-directory-history nil)
    (setq ido-use-filename-at-point 'guess)
    (setq ido-save-directory-list-file
          (concat user-emacs-directory ".cache/ido.last"))
    (use-package smex
      :ensure smex
      :config
      (progn
        (global-set-key (kbd "M-x") 'smex)
        (setq smex-save-file (concat user-emacs-directory ".cache/smex-items"))
        (smex-initialize)
        ;; the following is from
        ;; http://www.emacswiki.org/emacs/Smex

        ;; typing SPC inserts a hyphen
        (defadvice smex (around space-inserts-hyphen activate compile)
          (let ((ido-cannot-complete-command
                 `(lambda ()
                    (interactive)
                    (if (string= " " (this-command-keys))
                        (insert ?-)
                      (funcall ,ido-cannot-complete-command)))))
            ad-do-it))
        ;; update less often
        (defun smex-update-after-load (unused)
          (when (boundp 'smex-cache)
            (smex-update)))
        (add-hook 'after-load-functions 'smex-update-after-load)
        ))
    (add-hook
     'ido-setup-hook
     (lambda()
       ;; On ido-find-file, let `~` mean `~/` for fastness.
       (define-key ido-file-dir-completion-map "~"
         (lambda ()(interactive)
           (ido-set-current-directory "~/")
           (setq ido-exit 'refresh)
           (exit-minibuffer)))))
    ;;(use-package ido-ubiquitous
    ;;  :config
    ;;  (progn
    ;;    (ido-ubiquitous-mode 1)
    ;;    )
    ;;  )
    (use-package flx-ido
      :ensure flx-ido
      :defines (ido-cur-item ido-default-item ido-cur-list)
      :config
      (progn
        (flx-ido-mode 1)
        ;; disable ido faces to see flx highlights.
        (setq ido-use-faces nil)
        (setq flx-ido-use-faces t)
        )
      )
    (use-package ido-vertical-mode
      :ensure ido-vertical-mode
      :init (progn
              (ido-vertical-mode 1)
              )
      )
    (after 'evil
      (define-key evil-normal-state-map (kbd "SPC f") 'ido-find-file)
      (define-key evil-visual-state-map (kbd "SPC f") 'ido-find-file)
      (define-key evil-normal-state-map (kbd "SPC b") 'ido-switch-buffer)
      (define-key evil-visual-state-map (kbd "SPC b") 'ido-switch-buffer)
      (define-key evil-normal-state-map (kbd "SPC B") 'ibuffer)
      (define-key evil-visual-state-map (kbd "SPC B") 'ibuffer)
      (define-key evil-normal-state-map (kbd "SPC k") 'ido-kill-buffer)
      (define-key evil-visual-state-map (kbd "SPC k") 'ido-kill-buffer)
      )
    )
  )

(use-package ibuffer
  :commands ibuffer
  :ensure ibuffer
  :config
  (progn
    (use-package ibuffer-vc
      :ensure ibuffer-vc
      :config (progn
                (setq ibuffer-saved-filter-groups
                      (quote (("default"
                               ("dired" (mode . dired-mode))
                               ("haskell" (mode . haskell-mode))
                               ("python" (mode . python-mode))
                               ("notes" (or
                                         (name . "^\\*Calendar\\*$")
                                         (name . "^diary$")
                                         (mode . org-mode)))
                               ("*buffer*" (name . "\\*.*\\*"))
                               )))
                      ))
      ))
  )

(use-package ag
  :ensure ag
  :commands (ag ag-mode ag-files ag-regexp-at-point)
  :init
  (progn
    )
  :config
  (progn
    (setq ag-highlight-search t)
    (setq ag-reuse-buffers t)
    (add-hook 'ag-mode-hook (lambda () (toggle-truncate-lines t)))
    (add-hook 'ag-mode-hook (lambda () (linum-mode 0)))
    (add-hook 'ag-mode-hook (lambda () (switch-to-buffer-other-window "*ag search*")))
    )
  )

(use-package ace-jump-mode
  :ensure ace-jump-mode
  :commands (evil-ace-jump-char-mode
             evil-ace-jump-line-mode
             ace-jump-char-mode
             ace-jump-word-mode
             ace-jump-line-mode)
  :init
  (progn
    (after 'evil
      ;; Not sure if the `after` here is necessary, but anyway:
      (after 'ace-jump-mode-autoloads
        (define-key evil-normal-state-map (kbd "SPC j") 'ace-jump-char-mode)
        (define-key evil-motion-state-map (kbd "SPC") 'evil-ace-jump-char-mode)
        (define-key evil-motion-state-map (kbd "S-SPC") 'evil-ace-jump-line-mode)
        )
      ;; These will definitely work:
      (after 'key-chord-mode
        (key-chord-define evil-normal-state-map ";w" 'ace-jump-word-mode)
        (key-chord-define evil-normal-state-map ";c" 'ace-jump-char-mode)
        (key-chord-define evil-normal-state-map ";l" 'ace-jump-line-mode)
        )
      )
    )
  )
(use-package guide-key
  :ensure guide-key
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x" "C-c"))
    (setq guide-key/recursive-key-sequence-flag t)
    (guide-key-mode 1)
    (setq guide-key/idle-delay 2.5)
    )
  )
(use-package expand-region
  :ensure expand-region
  :init (progn
          (after 'evil
            (define-key evil-normal-state-map (kbd "C-e") 'er/expand-region)
            (define-key evil-visual-state-map (kbd "C-e") 'er/expand-region)
            )
          )
  :config (progn
            (after 'expand-region-autoloads
              (global-set-key (kbd "C-=") 'er/expand-region))
            )
  )

(provide 'my-interaction)
