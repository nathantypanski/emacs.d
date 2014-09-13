;; my-interaction.el
;;
;; ido, helm, smart emacs navigation, etc.

(use-package ctags-update
  :ensure ctags-update
  :init
  (progn
    (add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
    (add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode)
    (autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)
    ))

(use-package flycheck
  :ensure flycheck
  :init
  (progn
        (add-hook 'after-init-hook #'global-flycheck-mode))
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
    (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))

    (defun my-flycheck-list-errors ()
      "Jump to flycheck errors and switch to the errorlist buffer"
      (interactive)
      (flycheck-list-errors)
      (switch-to-buffer-other-window "*Flycheck errors*" t)
      )

    (after 'evil
      (evil-define-key 'normal flycheck-error-list-mode-map "k" #'flycheck-error-list-previous-error)
      (evil-define-key 'normal flycheck-error-list-mode-map "j" #'flycheck-error-list-next-error)
      (evil-define-key 'normal flycheck-error-list-mode-map "K" #'evil-previous-line)
      (evil-define-key 'normal flycheck-error-list-mode-map "J" #'evil-next-line)
      (evil-define-key 'normal flycheck-error-list-mode-map (kbd "RET") #'flycheck-error-list-goto-error)
      (evil-define-key 'normal flycheck-error-list-mode-map "q" 'quit-window)

      )
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

    (add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [tab] 'ido-complete)
            ))

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
      :init
      (progn
       (ido-vertical-mode 1)))
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
    (setq guide-key/idle-delay 1.5)
    (setq guide-key/popup-window-position 'top)
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
