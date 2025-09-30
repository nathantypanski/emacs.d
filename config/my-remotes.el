;; -*- lexical-binding: t; -*-

(after 'tramp
  (add-to-list 'tramp-methods
               '("tsssh"
                 (tramp-login-program "/usr/bin/tailscale")
                 (tramp-login-args (("ssh") ("%h")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c"))))



  (after 'recentf
    (add-to-list 'recentf-exclude
                 (if (boundp 'tramp-file-name-regexp) tramp-file-name-regexp
                   "^/[^:]+:")))

  (after 'vc
    ;; Or the sledgehammer (fastest, but disables VC everywhere):
    ;; (setq vc-handled-backends nil)
    (setq vc-ignore-dir-regexp
          (setq vc-ignore-dir-regexp
                (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp
                        (if (boundp 'tramp-file-name-regexp)
                            tramp-file-name-regexp
                          "^/[^:]+:")))))

  (defun my--has-remote-buffers-p ()
    (catch 'yes
      (dolist (b (buffer-list))
        (with-current-buffer b
          (let ((dir default-directory))
            (when (and dir (file-remote-p dir)) (throw 'yes t)))))
      nil))

  (defun my--tramp-handler-p (fn)
    (and (symbolp fn)
         (string-prefix-p "tramp" (symbol-name fn))))

  (defun my--without-tramp-handlers (fn &rest args)
    "Speed up FN by muting TRAMP handlers & VC if remote buffers exist."
    (if (not (my--has-remote-buffers-p))
        (apply fn args)
      (let* ((file-name-handler-alist
              (let (res)
                (dolist (cell file-name-handler-alist (nreverse res))
                  (unless (my--tramp-handler-p (cdr cell)) (push cell res)))))
             (vc-handled-backends nil)
             (default-directory (expand-file-name "~")))
        (apply fn args))))

  (advice-add 'list-buffers :around #'my--without-tramp-handlers)
  (with-eval-after-load 'ibuffer
    (advice-add 'ibuffer :around #'my--without-tramp-handlers)))

(provide 'my-remotes)
