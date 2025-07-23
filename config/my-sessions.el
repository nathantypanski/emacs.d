;; -*- lexical-binding: t; -*-
(require 'desktop)

(use-package perspective
  :ensure perspective
  :custom
  ;; Configure perspective settings
  (persp-mode-prefix-key (kbd "C-x x"))
  (persp-initial-frame-name "main")
  (persp-sort 'created)

  ;; Automatic perspective persistence
  (persp-state-default-file (expand-file-name "perspectives" user-emacs-directory))
  :config
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

  ;; Exclude perspective variables from desktop save to prevent conflicts
  (add-to-list 'desktop-globals-to-save 'persp-mode nil)
  (add-to-list 'desktop-minor-mode-handlers '(persp-mode . ignore)))

(provide 'my-sessions)
