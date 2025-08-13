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
(require 'ibuffer)

(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-ignore-buffers-re "^\\*" ; leave special buffers alone
)

(use-package ibuffer
  :commands ibuffer
  :custom
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil)
  :custom-face
  ;; Dim the dividers and group names to be less prominent
  (ibuffer-title-face ((t (:foreground "#5f5f5f"))))
  (ibuffer-filter-group-name-face ((t (:foreground "#7f7f7f"))))
  :init
 (progn
    (setq ibuffer-directory-abbrev-alist
          '(("^/home/ndt/" . "~/")
            ("^/home/ndt/src/" . "src/")
            ("^/home/ndt/src/github.com" . "gh/")
            ))
    (let ((div (propertize " │ " 'face '(:foreground "#5f5f5f"))))
      (setq ibuffer-formats
            (list (list 'mark 'modified 'read-only 'locked div
                        '(name 18 18 :left :elide) div
                        '(size 9 -1 :left) div
                        '(mode 16 16 :left :elide) div
                        'filename-and-process)
                  (list 'mark div
                        '(name 16 -1) div
                        'filename))))
    ;; for testing
    ;;    (with-current-buffer "*Ibuffer*"
    ;;      (ibuffer-redisplay))
    )

  (defun my-ibuffer-format-line-with-indent ()
  "Format ibuffer line with proper indentation for wrapped filenames."
  (let* ((buffer-name-width 30)  ; Adjust to match your format
         (indent-string (make-string (+ buffer-name-width 5) ?\s))) ; 5 for mark + spaces
    ;; Override the default line formatting
    (add-text-properties
     0 (length indent-string)
     '(wrap-prefix t line-prefix t)
     indent-string)
    ;; Apply to filename portion
    (put-text-property (+ (point-at-bol) buffer-name-width 5)
                       (point-at-eol)
                       'wrap-prefix indent-string)))

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
