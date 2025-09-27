;; my-copy.el   -*- lexical-binding:t; -*-

(defconst my-copy-program
  (cond ((eq system-type 'darwin) (or (executable-find "pbcopy") "pbcopy"))
        ((eq system-type 'gnu/linux) (or (executable-find "wl-copy") "wl-copy"))
        (t nil))
  "External program to copy to the OS clipboard.")

(defconst my-paste-program
  (cond ((eq system-type 'darwin) (or (executable-find "pbpaste") "pbpaste"))
        ((eq system-type 'gnu/linux) (or (executable-find "wl-paste") "wl-paste"))
        (t nil))
  "External program to paste from the OS clipboard.")


;; Non-nil means cutting and pasting uses the clipboard.
;; This can be in addition to, but in preference to, the primary selection,
;; if applicable (i.e. under X11).
;; ;
;;   This variable has an alias: ‘x-select-enable-clipboard’.
(setq select-enable-clipboard nil)

(defun my-wl-copy-region (start end &optional verify)
  "Copy region [START,END) to the OS clipboard via wl-copy/pbcopy.
With prefix arg VERIFY, round-trip check via wl-paste/pbpaste."
  (interactive (list (region-beginning) (region-end) current-prefix-arg))
  (unless my-copy-program
    (user-error "No system clipboard program found"))
  (let* ((coding-system-for-write 'utf-8-unix)
         (status (call-process-region start end my-copy-program)))
    (unless (and (integerp status) (zerop status))
      (user-error "Clipboard copy failed: %s exit %S" my-copy-program status)))
  (when verify
    (unless my-paste-program
      (user-error "No paste program found for verification"))
    (let ((coding-system-for-read 'utf-8-unix))
      (with-temp-buffer
        (let ((status (apply #'call-process my-paste-program nil t nil
                             (when (string-match-p "wl-paste\\'" my-paste-program) '("-n")))))
          (unless (and (integerp status) (zerop status))
            (user-error "Clipboard paste failed: %s exit %S" my-paste-program status))
          (let* ((orig (buffer-substring-no-properties (region-beginning) (region-end)))
                 (got  (buffer-string)))
            (unless (string= orig got)
              (user-error "Copy verification failed: %d sent vs %d received"
                          (length orig) (length got)))))))
  (message "Copied %d chars to clipboard" (- end start))))

(defun my-wl-paste-insert ()
  "Insert OS clipboard contents at point via wl-paste/pbpaste."
  (interactive)
  (unless my-paste-program
    (user-error "No paste program found"))
  (let ((coding-system-for-read 'utf-8-unix))
    (with-temp-buffer
      (let ((status (apply #'call-process my-paste-program nil t nil
                           (when (string-match-p "wl-paste\\'" my-paste-program) '("-n")))))
        (unless (and (integerp status) (zerop status))
          (user-error "Clipboard paste failed: %s exit %S" my-paste-program status))
        (insert (buffer-string))))))

(provide 'my-copy)
