
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
(if (not (my-system-is-mac))
  (setq dired-listing-switches "-kABhl --group-directories-first")
  (setq dired-listing-switches "-kABhl")
  )

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

(use-package dired-x
  :init
  (progn
    (defun my-load-dired-x ()
      "Load dired-x.

For use on dired-load-hook"
        (load "dired-x"))
    (add-hook 'dired-load-hook 'my-load-dired-x)))

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

(after 'evil
  ;;
  ;; These are plain ol' dired bindings, kept here for reference and future
  ;; modification.
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (evil-define-key 'normal dired-mode-map "#" 'dired-flag-auto-save-files)
  (evil-define-key 'normal dired-mode-map "." 'dired-clean-directory)
  (evil-define-key 'normal dired-mode-map "~" 'dired-flag-backup-files)
  ;; Upper case keys (except !) for operating on the marked files
  (evil-define-key 'normal dired-mode-map "A" 'dired-do-search)
  (evil-define-key 'normal dired-mode-map "C" 'dired-do-copy)
  (evil-define-key 'normal dired-mode-map "B" 'dired-do-byte-compile)
  (evil-define-key 'normal dired-mode-map "D" 'dired-do-delete)
  (evil-define-key 'normal dired-mode-map "G" 'dired-do-chgrp)
  (evil-define-key 'normal dired-mode-map "H" 'dired-do-hardlink)
  (evil-define-key 'normal dired-mode-map "L" 'dired-do-load)
  (evil-define-key 'normal dired-mode-map "M" 'dired-do-chmod)
  (evil-define-key 'normal dired-mode-map "O" 'dired-do-chown)
  (evil-define-key 'normal dired-mode-map "P" 'dired-do-print)
  (evil-define-key 'normal dired-mode-map "Q" 'dired-do-query-replace-regexp)
  (evil-define-key 'normal dired-mode-map "R" 'dired-do-rename)
  (evil-define-key 'normal dired-mode-map "S" 'dired-do-symlink)
  (evil-define-key 'normal dired-mode-map "T" 'dired-do-touch)
  (evil-define-key 'normal dired-mode-map "X" 'dired-do-shell-command)
  (evil-define-key 'normal dired-mode-map "Z" 'dired-do-compress)
  (evil-define-key 'normal dired-mode-map "!" 'dired-do-shell-command)
  (evil-define-key 'normal dired-mode-map "&" 'dired-do-async-shell-command)
  ;; Comparison commands
  (evil-define-key 'normal dired-mode-map "=" 'dired-diff)
  ;; Tree Dired commands
  (evil-define-key 'normal dired-mode-map "\M-\C-?" 'dired-unmark-all-files)
  (evil-define-key 'normal dired-mode-map "\M-\C-d" 'dired-tree-down)
  (evil-define-key 'normal dired-mode-map "\M-\C-u" 'dired-tree-up)
  (evil-define-key 'normal dired-mode-map "\M-\C-n" 'dired-next-subdir)
  (evil-define-key 'normal dired-mode-map "\M-\C-p" 'dired-prev-subdir)
  ;; move to marked files
  (evil-define-key 'normal dired-mode-map "\M-{" 'dired-prev-marked-file)
  (evil-define-key 'normal dired-mode-map "\M-}" 'dired-next-marked-file)
  ;; Make all regexp commands share a `%' prefix:
  ;; We used to get to the submap via a symbol dired-regexp-prefix,
  ;; but that seems to serve little purpose, and copy-keymap
  ;; does a better job without it.
  (evil-define-key 'normal dired-mode-map "%" nil)
  (evil-define-key 'normal dired-mode-map "%u" 'dired-upcase)
  (evil-define-key 'normal dired-mode-map "%l" 'dired-downcase)
  (evil-define-key 'normal dired-mode-map "%d" 'dired-flag-files-regexp)
  (evil-define-key 'normal dired-mode-map "%g" 'dired-mark-files-containing-regexp)
  (evil-define-key 'normal dired-mode-map "%m" 'dired-mark-files-regexp)
  (evil-define-key 'normal dired-mode-map "%r" 'dired-do-rename-regexp)
  (evil-define-key 'normal dired-mode-map "%C" 'dired-do-copy-regexp)
  (evil-define-key 'normal dired-mode-map "%H" 'dired-do-hardlink-regexp)
  (evil-define-key 'normal dired-mode-map "%R" 'dired-do-rename-regexp)
  (evil-define-key 'normal dired-mode-map "%S" 'dired-do-symlink-regexp)
  (evil-define-key 'normal dired-mode-map "%&" 'dired-flag-garbage-files)
  ;; Commands for marking and unmarking.
  (evil-define-key 'normal dired-mode-map "*" nil)
  (evil-define-key 'normal dired-mode-map "**" 'dired-mark-executables)
  (evil-define-key 'normal dired-mode-map "*/" 'dired-mark-directories)
  (evil-define-key 'normal dired-mode-map "*@" 'dired-mark-symlinks)
  (evil-define-key 'normal dired-mode-map "*%" 'dired-mark-files-regexp)
  (evil-define-key 'normal dired-mode-map "*c" 'dired-change-marks)
  (evil-define-key 'normal dired-mode-map "*s" 'dired-mark-subdir-files)
  (evil-define-key 'normal dired-mode-map "*m" 'dired-mark)
  (evil-define-key 'normal dired-mode-map "*u" 'dired-unmark)
  (evil-define-key 'normal dired-mode-map "*?" 'dired-unmark-all-files)
  (evil-define-key 'normal dired-mode-map "*!" 'dired-unmark-all-marks)
  (evil-define-key 'normal dired-mode-map "U" 'dired-unmark-all-marks)
  (evil-define-key 'normal dired-mode-map "*\177" 'dired-unmark-backward)
  (evil-define-key 'normal dired-mode-map "*\C-n" 'dired-next-marked-file)
  (evil-define-key 'normal dired-mode-map "*\C-p" 'dired-prev-marked-file)
  (evil-define-key 'normal dired-mode-map "*t" 'dired-toggle-marks)
  ;; Lower keys for commands not operating on all the marked files
  (evil-define-key 'normal dired-mode-map "a" 'dired-find-alternate-file)
  (evil-define-key 'normal dired-mode-map "d" 'dired-flag-file-deletion)
  (evil-define-key 'normal dired-mode-map "e" 'dired-find-file)
  (evil-define-key 'normal dired-mode-map "f" 'dired-find-file)
  (evil-define-key 'normal dired-mode-map "\C-m" 'dired-find-file)
  (put 'dired-find-file :advertised-binding "\C-m")
  (evil-define-key 'normal dired-mode-map "g" 'revert-buffer)
  (evil-define-key 'normal dired-mode-map "i" 'dired-maybe-insert-subdir)
  (evil-define-key 'normal dired-mode-map "j" 'dired-goto-file)
  (evil-define-key 'normal dired-mode-map "k" 'dired-do-kill-lines)
  (evil-define-key 'normal dired-mode-map "l" 'dired-do-redisplay)
  (evil-define-key 'normal dired-mode-map "m" 'dired-mark)
  (evil-define-key 'normal dired-mode-map "n" 'dired-next-line)
  (evil-define-key 'normal dired-mode-map "o" 'dired-find-file-other-window)
  (evil-define-key 'normal dired-mode-map "\C-o" 'dired-display-file)
  (evil-define-key 'normal dired-mode-map "p" 'dired-previous-line)
  (evil-define-key 'normal dired-mode-map "s" 'dired-sort-toggle-or-edit)
  (evil-define-key 'normal dired-mode-map "t" 'dired-toggle-marks)
  (evil-define-key 'normal dired-mode-map "u" 'dired-unmark)
  (evil-define-key 'normal dired-mode-map "v" 'dired-view-file)
  (evil-define-key 'normal dired-mode-map "w" 'dired-copy-filename-as-kill)
  (evil-define-key 'normal dired-mode-map "x" 'dired-do-flagged-delete)
  (evil-define-key 'normal dired-mode-map "y" 'dired-show-file-type)
  (evil-define-key 'normal dired-mode-map "+" 'dired-create-directory)
  ;; moving
  (evil-define-key 'normal dired-mode-map "<" 'dired-prev-dirline)
  (evil-define-key 'normal dired-mode-map ">" 'dired-next-dirline)
  (evil-define-key 'normal dired-mode-map "^" 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map " "  'dired-next-line)
  (evil-define-key 'normal dired-mode-map [remap next-line] 'dired-next-line)
  (evil-define-key 'normal dired-mode-map [remap previous-line] 'dired-previous-line)
  ;; hiding
  (evil-define-key 'normal dired-mode-map "\M-$" 'dired-hide-all)
  (evil-define-key 'normal dired-mode-map "(" 'dired-hide-details-mode)
  ;; isearch
  (evil-define-key 'normal dired-mode-map (kbd "M-s a C-s")   'dired-do-isearch)
  (evil-define-key 'normal dired-mode-map (kbd "M-s a M-C-s") 'dired-do-isearch-regexp)
  (evil-define-key 'normal dired-mode-map (kbd "M-s f C-s")   'dired-isearch-filenames)
  (evil-define-key 'normal dired-mode-map (kbd "M-s f M-C-s") 'dired-isearch-filenames-regexp)
  ;; misc
  ;; `toggle-read-only' is an obsolete alias for `read-only-mode'
  (evil-define-key 'normal dired-mode-map "?" 'dired-summary)
  (evil-define-key 'normal dired-mode-map "\177" 'dired-unmark-backward)
  (evil-define-key 'normal dired-mode-map [remap undo] 'dired-undo)
  (evil-define-key 'normal dired-mode-map [remap advertised-undo] 'dired-undo)
  ;; thumbnail manipulation (image-dired)
  (evil-define-key 'normal dired-mode-map "\C-td" 'image-dired-display-thumbs)
  (evil-define-key 'normal dired-mode-map "\C-tt" 'image-dired-tag-files)
  (evil-define-key 'normal dired-mode-map "\C-tr" 'image-dired-delete-tag)
  (evil-define-key 'normal dired-mode-map "\C-tj" 'image-dired-jump-thumbnail-buffer)
  (evil-define-key 'normal dired-mode-map "\C-ti" 'image-dired-dired-display-image)
  (evil-define-key 'normal dired-mode-map "\C-tx" 'image-dired-dired-display-external)
  (evil-define-key 'normal dired-mode-map "\C-ta" 'image-dired-display-thumbs-append)
  (evil-define-key 'normal dired-mode-map "\C-t." 'image-dired-display-thumb)
  (evil-define-key 'normal dired-mode-map "\C-tc" 'image-dired-dired-comment-files)
  (evil-define-key 'normal dired-mode-map "\C-tf" 'image-dired-mark-tagged-files)
  (evil-define-key 'normal dired-mode-map "\C-t\C-t" 'image-dired-dired-toggle-marked-thumbs)
  (evil-define-key 'normal dired-mode-map "\C-te" 'image-dired-dired-edit-comment-and-tags)
  ;; encryption and decryption (epa-dired)

  ;;
  ;; My bindings.
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (evil-define-key 'normal dired-mode-map (kbd "C-j") 'dired-next-subdir)
  (evil-define-key 'normal dired-mode-map (kbd "C-k") 'dired-prev-subdir)
  (evil-define-key 'normal dired-mode-map "h" 'my-dired-up-directory)
  (evil-define-key 'normal dired-mode-map "L" 'my-dired-interact-with-file)
  (evil-define-key 'normal dired-mode-map "l" 'dired-find-alternate-file)
  (evil-define-key 'normal dired-mode-map "a" 'ag-dired)
  (evil-define-key 'normal dired-mode-map "o" 'dired-sort-toggle-or-edit)
  (evil-define-key 'normal dired-mode-map "v" 'dired-toggle-marks)
  (evil-define-key 'normal dired-mode-map "m" 'dired-mark)
  (evil-define-key 'normal dired-mode-map "u" 'dired-unmark)
  (evil-define-key 'normal dired-mode-map "U" 'dired-unmark-all-marks)
  (evil-define-key 'normal dired-mode-map "c" 'dired-create-directory)
  (evil-define-key 'normal dired-mode-map "q" 'kill-this-buffer)
  (evil-define-key 'normal dired-mode-map "/" 'evil-search-forward)
  (evil-define-key 'normal dired-mode-map "n" 'evil-search-next)
  (evil-define-key 'normal dired-mode-map "N" 'evil-search-previous)
  (evil-define-key 'normal dired-mode-map "j" 'my-dired-next-line)
  (evil-define-key 'normal dired-mode-map "k" 'my-dired-previous-line)
  (evil-define-key 'normal dired-mode-map (kbd "TAB") 'dired-hide-subdir)
  (evil-define-key 'normal dired-mode-map (kbd "<backspace>") 'my-dired-remove-from-buffer)
  )

(provide 'my-dired)
