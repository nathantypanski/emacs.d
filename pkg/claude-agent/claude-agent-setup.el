;;; claude-agent-setup.el --- Setup for claude-agent -*- lexical-binding: t; -*-

;; Add this to your init.el or load it separately

;; Install and configure dependencies
(use-package request
  :ensure t)

;; Load claude-agent
(add-to-list 'load-path (expand-file-name "pkg/claude-agent" user-emacs-directory))
(require 'claude-agent)

;; Configuration
(setq claude-agent-api-key (getenv "ANTHROPIC_API_KEY"))  ; or set directly
(setq claude-agent-allowed-directories 
      '("~/src" 
        "~/dotfiles" 
        "~/.emacs.d"
        "~/src/github.com/nathantypanski/dotfiles"))

;; Require confirmation for potentially dangerous operations
(setq claude-agent-require-confirmation '(bash edit delete))

;; Keybindings (adjust to your preference)
(global-set-key (kbd "C-c C-a") 'claude-agent)
(global-set-key (kbd "C-c C-f") 'claude-agent-analyze-file)

;; Optional: function to analyze current buffer
(defun claude-agent-analyze-file ()
  "Analyze the current file with Claude agent."
  (interactive)
  (if (not buffer-file-name)
      (message "No file associated with current buffer")
    (claude-agent (format "Analyze this file: %s" buffer-file-name))))

;; Optional: project analysis
(defun claude-agent-analyze-project ()
  "Analyze the current project."
  (interactive)
  (let ((project-root (or (vc-root-dir) default-directory)))
    (claude-agent (format "Analyze the code structure in %s" project-root))))

(provide 'claude-agent-setup)