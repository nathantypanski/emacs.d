;; my-dired.el   -*- lexical-binding:t; -*-

(put 'dired-find-alternate-file 'disabled nil)

(setq my-default-dired-switches "-akBhl")

(if (not (my-system-is-mac))
    (setq dired-listing-switches (concat my-default-dired-switches " --group-directories-first"))
  (setq dired-listing-switches my-default-dired-switches))

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
  :hook (dired-mode . diredfl-mode))

(add-hook 'dired-mode-hook 'hl-line-mode)

(add-hook 'dired-mode-hook
          (lambda ()
            (setq truncate-lines nil)))

(setq dired-use-ls-dired t)

;; TODO: make this work
;; (after 'evil
;;  (evil-define-key 'normal evil-normal-state-map (kbd "R") 'revert-buffer))

(provide 'my-dired)
