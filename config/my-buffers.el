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
      uniquify-ignore-buffers-re "^\\*") ; leave special buffers alone

(defvar my-ibuffer-use-vc-groups t
  "Use filter groups detected from vc root when non-nil.
This will be done with `ibuffer-vc-set-filter-groups-by-vc-root'
If this is nil, then filter groups will be restored from `ibuffer-saved-filter-groups'.")

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
  ;; Order matters! More specific patterns first
  (setq ibuffer-directory-abbrev-alist
        '(("^/home/ndt/src/github\\.com/nathantypanski/dotfiles/emacs\\.d" . "~emacs")
          ("^/home/ndt/src/github\\.com/nathantypanski/dotfiles" . "~dots")
          ("^/home/ndt/src/github\\.com/nathantypanski" . "~ndt")
          ("^/home/ndt/src/github\\.com/" . "~gh/")
          ("^/home/ndt/src/" . "~src/")
          ("^~/src/github\.com/" . "~gh/")
          ("^/home/ndt/" . "~/")))

  (defun my-abbreviate-path (path)
    "Abbreviate PATH using ibuffer-directory-abbrev-alist rules."
    (dolist (abbrev ibuffer-directory-abbrev-alist path)
      (setq path (replace-regexp-in-string
                  (car abbrev)
                  (cdr abbrev)
                  path))))

  (setq ibuffer-formats
        '((mark modified read-only locked
                " " (name 35 35 :left :elide)
                " " (size 9 -1 :right)
                " " (mode 16 16 :left :elide))
          ;; Second format with filename for when you need it
          (mark " " (filename-and-process 60 60 :left :elide))))

  (defun my-abbreviate-filter-groups ()
    "Abbreviate current ibuffer filter groups."
    (when ibuffer-filter-groups
      (setq ibuffer-filter-groups
            (mapcar (lambda (group)
                      (cons (my-abbreviate-path (car group))
                            (cdr group)))
                    ibuffer-filter-groups))))

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

  (defun my-ibuffer-setup ()
    "Configure ibuffer without VC groups."
    (ibuffer-auto-mode 1)
    (unless (featurep 'ibuffer-vc)  ; Only run if ibuffer-vc isn't loaded
      (ibuffer-switch-to-saved-filter-groups "default")
      (my-abbreviate-filter-groups)))

  (add-hook 'ibuffer-mode-hook 'my-ibuffer-setup)

  (defun my-ibuffer-raise-other-window ()
    (interactive)
    (let* ((buf (ibuffer-list-buffers))
           (win (get-buffer-window buf 0)))
      (if win (select-window win) (ibuffer-other-window))))

  (setq display-buffer-alist
        '(("\\*Help\\*"
           (display-buffer-reuse-window display-buffer-pop-up-window)
           (inhibit-same-window . t)
           (side . right)
           (window-width . 0.4)
           (reusable-frames . visible))
          ("\\*Completions\\*"
           (display-buffer-at-bottom))))

  (defun my-kill-current-buffer-with-confirmation ()
    "Kill current buffer with confirmation prompt."
    (interactive)
    (when (yes-or-no-p (format "Kill buffer '%s'? " (buffer-name)))
      (kill-current-buffer)))

  ;; (define-ibuffer-filter unsaved-file-buffers
  ;;     "Toggle current view to buffers whose file is unsaved."
  ;;   (:description "file is unsaved")
  ;;   (ignore qualifier)
  ;;   (and (buffer-local-value 'buffer-file-name buf)
  ;;        (buffer-modified-p buf)))

  ;; (after 'general
  ;;   (general-define-key
  ;;    :maps ibuffer-mode-map
  ;;    :states '(emacs)
  ;;    "/u" #'ibuffer-filter-by-unsaved-file-buffers))
  )

(use-package ibuffer-vc
  :straight t
  :after ibuffer
  :config
  ;; Remove the basic setup hook since we'll use VC-aware one
  (remove-hook 'ibuffer-mode-hook 'my-ibuffer-setup)

  ;; VC-aware setup function
  (defun my-ibuffer-vc-setup ()
    "Configure ibuffer with VC groups and abbreviations."
    (ibuffer-auto-mode 1)
    (if my-ibuffer-use-vc-groups
        (ibuffer-vc-set-filter-groups-by-vc-root)
      (ibuffer-switch-to-saved-filter-groups "default"))
    ;; Don't call my-abbreviate-filter-groups here - let the advice handle it
    )

  ;; Add the VC setup hook
  (add-hook 'ibuffer-mode-hook 'my-ibuffer-vc-setup)

  ;; TODO: seems this is not very effective at abbreviation
  (defun my-abbreviate-vc-group-names (groups)
    "Simply abbreviate the group names in VC groups."
    (mapcar (lambda (group)
              (let ((name (car group))
                    (filters (cdr group)))
                ;; Apply abbreviations to the group name
                (dolist (abbrev ibuffer-directory-abbrev-alist)
                  (setq name (replace-regexp-in-string
                              (car abbrev)
                              (cdr abbrev)
                              name)))
                ;; Return the group with abbreviated name
                (cons name filters)))
            groups))

  (advice-add 'ibuffer-vc-generate-filter-groups-by-vc-root
              :filter-return 'my-abbreviate-vc-group-names))

(provide 'my-buffers)
