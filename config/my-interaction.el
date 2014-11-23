;; my-interaction.el
;;
;; smart emacs navigation, etc.





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
      (switch-to-buffer-other-window "*Flycheck errors*" t))

    (after 'evil
      (evil-define-key 'normal flycheck-error-list-mode-map
        "q" 'quit-window
        "j" #'flycheck-error-list-next-error
        "k" #'flycheck-error-list-previous-error
        "K" #'evil-previous-line
        "J" #'evil-next-line
        (kbd "RET") #'flycheck-error-list-goto-error))))


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
        (setq ace-jump-mode-move-keys
              (nconc '(?j ?f ?k ?d ?l ?s ?a ?h ?g)
                     '(?y ?t ?u ?r ?i ?e ?o ?w ?p ?q)
                     '(?n ?v ?b ?m ?c ?x ?z))))))
  :config
  (progn
    (use-package ace-jump-buffer
      :commands
      (ace-jump-buffer
       ace-jump-buffer-in-one-window
       ace-jump-buffer-other-window
       )
      :ensure ace-jump-buffer)))


(use-package guide-key
  :ensure guide-key
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x" "C-c"))
    (setq guide-key/recursive-key-sequence-flag t)
    (guide-key-mode 1)
    (setq guide-key/idle-delay 1.5)
    (setq guide-key/popup-window-position 'top)))


(use-package expand-region
  :ensure expand-region
  :init
  (progn
    (after 'evil
      (define-key evil-normal-state-map (kbd "C-e") 'er/expand-region)
      (define-key evil-visual-state-map (kbd "C-e") 'er/expand-region)))
  :config
  (progn
    (after 'expand-region-autoloads
      (global-set-key (kbd "C-=") 'er/expand-region))))


(provide 'my-interaction)
