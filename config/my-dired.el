
;; ugly workaround because OSX ls doesn't support the -X switch
;;
;; ref:
;; http://stackoverflow.com/questions/4076360/error-in-dired-sorting-on-os-x
;;(when (eq system-type 'darwin)
;;  (require 'ls-lisp)
;;    (setq ls-lisp-use-insert-directory-program nil))

(put 'dired-find-alternate-file 'disabled nil)

;; on OSX, dired's ls binary doesn't support some of the required options.
;; Thankfully, I use Homebrew, so I can hack around this and tell it to use
;; the proper ls.

;; help me simplify this using the variable I defined
(setq my-default-dired-switches "--kibibytes --ignore-backups --human-readable -l")

(if (not (my-system-is-mac))
    (setq dired-listing-switches (concat my-default-dired-switches " --group-directories-first"))
  (setq dired-listing-switches my-default-dired-switches))

(use-package saveplace
  :config
  (progn
    (setq save-place-file (concat user-emacs-directory ".cache/places"))
    (setq-default save-place t)))

(use-package savehist
  :config
    (progn
        (setq savehist-file (concat user-emacs-directory ".cache/savehist")
            savehist-additional-variables '(search ring regexp-search-ring)
            savehist-autosave-interval 60)
        (savehist-mode t)))

(use-package recentf
  :config
  (progn
    (setq recentf-save-file (concat user-emacs-directory ".cache/recentf")
          recentf-max-saved-items 1000
          recentf-max-menu-items 500)
    (recentf-mode +1)))

(defun my-configure-dired ()
  "Setup dired and dired-x.

For use with dired-mode-hook."
  (dired-omit-mode 1))

(add-hook 'dired-mode-hook 'my-configure-dired)

(defun my-dired-up-directory ()
  "Take dired up one directory, but behave like dired-find-alternate-file"
  (interactive)
  (let ((old (current-buffer)))
    (dired-up-directory)
    (kill-buffer old)))

(defun my-dired-next-line (count)
  "Move to next line, always staying on the dired filename."
  (interactive "p")
  (dired-next-line count)
  (dired-move-to-filename))

(defun my-dired-previous-line (count)
  "Move to previous line, always staying on the dired filename."
  (interactive "p")
  (dired-previous-line count)
  (dired-move-to-filename))

(defun my-dired-interact-with-file ()
  "Interact with a dired file!"
  (interactive)
  (let ((this-file (dired-get-file-for-visit)))
    (if (file-directory-p this-file)
        (dired-maybe-insert-subdir this-file)
      (dired-find-file))))

(defun my-dired-at-title ()
  "Returns the current dir if point is at the title of a directory in dired.
Otherwise, returns nil."
  (interactive)
  (let* ((cur-dir (dired-current-directory))
	 (hidden-p (dired-subdir-hidden-p cur-dir))
	 (elt (assoc cur-dir dired-subdir-alist))
         (begin-pos (save-excursion
                      (goto-char (dired-get-subdir-min elt))
                      (point)))
         (end-pos (save-excursion
         (goto-char (dired-get-subdir-min elt))
         (skip-chars-forward "^\n\r")
         (point)))
         (at-title (and (> (+ 1 (point)) begin-pos)
                        (< (+ 1 (point)) end-pos)))
         buffer-read-only)
        (if at-title elt nil)))

(defun my-dired-remove-from-buffer ()
  (interactive)
  (if (my-dired-at-title)
      (dired-kill-subdir)))

(use-package diredfl
  :hook (dired-mode . diredfl-mode)
  :custom
  (custom-set-faces
   ;; Directories (blue)
   '(diredfl-dir-name ((t (:foreground "#8cd0d3" :weight bold))))
   '(diredfl-dir-heading ((t (:foreground "#efef8f" :weight bold))))
   '(diredfl-dir-priv ((t (:foreground "#8cd0d3" :background nil))))

   ;; Executables (green)
   '(diredfl-exec-priv ((t (:foreground "#7f9f7f"))))
   '(diredfl-executable-tag ((t (:foreground "#7f9f7f"))))

   ;; Symlinks (magenta)
   '(diredfl-symlink ((t (:foreground "#dc8cc3" :italic t))))

   ;; Compressed (orange)
   '(diredfl-compressed-file-name ((t (:foreground "#dfaf8f"))))

   ;; Media files (light gray)
   '(diredfl-media ((t (:foreground "#dcdccc"))))

   ;; Marked files (red)
   '(diredfl-flag-mark ((t (:foreground "#cc9393" :weight bold))))
   '(diredfl-flag-mark-line ((t (:background "#3f3f3f"))))

   ;; Other
   '(diredfl-date-time ((t (:foreground "#93e0e3"))))
   '(diredfl-number ((t (:foreground "#9fafaf"))))
   '(diredfl-permission ((t (:foreground "#9fafaf")))))
)

(add-hook 'dired-mode-hook 'hl-line-mode)
(setq dired-use-ls-dired t)

;; TODO: make this work
;; (after 'evil
;;  (evil-define-key 'normal evil-normal-state-map (kbd "R") 'revert-buffer))

(provide 'my-dired)
