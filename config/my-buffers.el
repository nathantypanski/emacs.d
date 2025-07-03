;; my-buffers.el   -*- lexical-binding:t; -*-
;;
;; From the docs:
;;
;; Narrowing means focusing in on some portion of the buffer, making
;; the rest temporarily inaccessible. The portion which you can still
;; get to is called the accessible portion. Canceling the narrowing,
;; which makes the entire buffer once again accessible, is called
;; widening. The bounds of narrowing in effect in a buffer are called
;; the buffer's restriction.
(put 'narrow-to-region 'disabled nil)

;; better buffer names for duplicates
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-ignore-buffers-re "^\\*" ; leave special buffers alone
      uniquify-after-kill-buffer-flag t)

(use-package ibuffer
  :commands ibuffer
  :custom
  (ibuffer-expert t)
  :config
  (defvar my-ibuffer-use-vc-groups t
    "Use filter groups detected from vc root when non-nil.
This will be done with `ibuffer-vc-set-filter-groups-by-vc-root'
If this is nil, then filter groups will be restored from `ibuffer-saved-filter-groups'.")

  (defun my-ibuffer-setup ()
    "Configure ibuffer the way I want it.
This sets `ibuffer-auto-mode' and restores the chosen filter group settings,
according to the values of `my-ibuffer-use-vc-groups' and
`ibuffer-saved-filter-groups'."
    (ibuffer-auto-mode 1)
    (if my-ibuffer-use-vc-groups
        (ibuffer-vc-set-filter-groups-by-vc-root)
      (ibuffer-switch-to-saved-filter-groups "default")))

  (add-hook 'ibuffer-mode-hook 'my-ibuffer-setup)
  (setq ibuffer-show-empty-filter-groups nil)

  (defun my-ibuffer-raise-other-window ()
    (interactive)
    (let* ((buf (ibuffer-list-buffers))
           (win (get-buffer-window buf 0)))
      (if win (select-window win) (ibuffer-other-window)))))

(setq display-buffer-alist
      '(("\\*Help\\*"
         (display-buffer-reuse-window display-buffer-pop-up-window)
         (inhibit-same-window . t)
         (side . right)
         (window-width . 0.4)
         (reusable-frames . visible))
        ("\\*Completions\\*"
         (display-buffer-at-bottom))
        ;; (add more rules as needed)
        ))

(use-package ibuffer-vc
  :ensure ibuffer-vc
  :config)

(defun my-kill-current-buffer-with-confirmation ()
  "Kill current buffer with confirmation prompt."
  (interactive)
  (when (yes-or-no-p (format "Kill buffer '%s'? " (buffer-name)))
    (kill-current-buffer)))

(provide 'my-buffers)
