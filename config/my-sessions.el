;; -*- lexical-binding: t; -*-
(use-package perspective
  :ensure perspective
  :config
  (progn
    ;; Configure perspective settings
    (setq persp-mode-prefix-key (kbd "C-x x"))
    (setq persp-initial-frame-name "main")
    (setq persp-sort 'created)

    ;; Enable perspective mode
    (persp-mode 1)

    ;; Custom functions for perspective management
    (defun my-perspective-switch ()
      "Switch to a perspective with completion"
      (interactive)
      (call-interactively 'persp-switch))

    (defun my-perspective-new ()
      "Create a new perspective"
      (interactive)
      (call-interactively 'persp-switch))

    (defun my-perspective-kill-with-confirmation ()
      "Kill current perspective with confirmation"
      (interactive)
      (when (y-or-n-p (format "Kill perspective '%s'? " (persp-current-name)))
        (persp-kill (persp-current-name))))

    ;; Automatic perspective persistence
    (setq persp-state-default-file (expand-file-name "perspectives" user-emacs-directory))))

(provide 'my-sessions)
