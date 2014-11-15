;; my-ido.el
;;
;; Settings for ido, a better way to select things in a minibuffer.


;; From <http://www.emacswiki.org/emacs/ImenuMode>
(defun ido-goto-symbol (&optional symbol-list)
      "Refresh imenu and jump to a place in the buffer using Ido."
      (interactive)
      (unless (featurep 'imenu)
        (require 'imenu nil t))
      (cond
       ((not symbol-list)
        (let ((ido-mode ido-mode)
              (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                   ido-enable-flex-matching t))
              name-and-pos symbol-names position)
          (unless ido-mode
            (ido-mode 1)
            (setq ido-enable-flex-matching t))
          (while (progn
                   (imenu--cleanup)
                   (setq imenu--index-alist nil)
                   (ido-goto-symbol (imenu--make-index-alist))
                   (setq selected-symbol
                         (ido-completing-read "λ sym " symbol-names))
                   (string= (car imenu--rescan-item) selected-symbol)))
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (setq position (cdr (assoc selected-symbol name-and-pos)))
          (cond
           ((overlayp position)
            (goto-char (overlay-start position)))
           (t
            (goto-char position)))))
       ((listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'symbol-names name)
              (add-to-list 'name-and-pos (cons name position))))))))


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

    ;; ido for M-x.
    (use-package smex
      :ensure smex
      :config
      (progn
        (global-set-key (kbd "M-x") 'smex)
        (setq smex-save-file (concat user-emacs-directory ".cache/smex-items"))
        (smex-initialize)

        ;; The following is from <http://www.emacswiki.org/emacs/Smex>.
        ;; Typing SPC inserts a hyphen:
        (defadvice smex (around space-inserts-hyphen activate compile)
          (let ((ido-cannot-complete-command
                 `(lambda ()
                    (interactive)
                    (if (string= " " (this-command-keys))
                        (insert ?-)
                      (funcall ,ido-cannot-complete-command)))))
            ad-do-it))

        ;; Update less often.
        (defun smex-update-after-load (unused)
          (when (boundp 'smex-cache)
            (smex-update)))

        (add-hook 'after-load-functions 'smex-update-after-load)))


    (defun my-ido-jump-to-home ()
      "Jump to the user's home directory in ido."
      (interactive)
      (ido-set-current-directory "~/")
      (setq ido-exit 'refresh)
      (exit-minibuffer))


    (defun my-setup-ido ()
      "Configure ido my way."
       ;; On ido-find-file, let `~` mean `~/` for fastness.
       (define-key ido-file-dir-completion-map "~" 'my-ido-jump-to-home)
       (define-key ido-file-dir-completion-map [tab] 'ido-complete)
       (define-key ido-file-dir-completion-map (kbd "RET") 'exit-minibuffer)
       (define-key ido-file-dir-completion-map (kbd "C-i") 'ido-select-text)
       (define-key ido-completion-map (kbd "RET") 'exit-minibuffer)
       (define-key ido-completion-map (kbd "TAB") 'ido-complete)
       (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
       (define-key ido-completion-map (kbd "C-i") 'ido-select-text)
       (define-key ido-completion-map (kbd "C-k") 'ido-prev-match)
       (define-key ido-completion-map (kbd "M-p") 'ido-toggle-prefix))

    (add-hook 'ido-setup-hook 'my-setup-ido)


    (use-package ido-ubiquitous
      :ensure ido-ubiquitous
      :config
      (progn
        (ido-ubiquitous-mode 1)))


    (use-package flx-ido
      :ensure flx-ido
      :defines (ido-cur-item ido-default-item ido-cur-list)
      :config
      (progn
        (flx-ido-mode 1)
        ;; disable ido faces to see flx highlights.
        (setq ido-use-faces nil)
        (setq flx-ido-use-faces t)))


    (use-package ido-vertical-mode
      :ensure ido-vertical-mode
      :init
      (progn
       (ido-vertical-mode 1)))))

(provide 'my-ido)
