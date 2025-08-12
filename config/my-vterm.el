;; my-vterm.el   -*- lexical-binding:t; -*-
;;
;; Settings for vterm integration with evil-mode
;;
;;1. vterm with evil-collection: Added to evil-collection-mode-list for proper
;;  integration                                                         │
;;  2. Proper state management: vterm starts in emacs state, copy-mode uses normal
;;  state                                                               │
;;  3. Key bindings:                                                    │
;;    - C-c t - toggle vterm                                            │
;;    - C-c T - toggle vterm in current directory                       │
;;    - C-c C-t - enter copy mode (for navigation/copying)              │
;;    - C-c C-k - switch to normal mode in vterm                        │
;;                                                                      │
;;  Usage in -nw mode:                                                  │
;;                                                                      │
;;  - Terminal interaction: Use vterm normally in emacs state           │
;;  - Copy/navigation: C-c C-t enters copy-mode where you can use Evil motions (hjkl,
;;  / for search, etc.)                                                 │
;;  - Toggle between projects: C-c T opens vterm in current buffer's directory
;;  - Send control sequences: C-q then the key (e.g., C-q C-c to send C-c to terminal)
;;                                                                      │
;;  The configuration follows your existing patterns and should work seamlessly with
;;  your Evil-mode setup in terminal mode. The vterm buffer starts in emacs state
;;  (like your other terminals) but you can easily switch to normal mode for
;;  navigation when needed.                                             │

(use-package vterm
  :ensure t
  :demand t
  :config
  ;; Performance optimizations for terminal mode
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t)

  ;; Enable directory tracking
  (setq vterm-tramp-shells '(("docker" "/bin/bash")
                            ("ssh" "/bin/bash")))

  ;; Set vterm to start in emacs state (like other terminals)
  (after 'evil
    (evil-set-initial-state 'vterm-mode 'emacs))

  ;; Key bindings for vterm
  (define-key vterm-mode-map (kbd "C-q") 'vterm-send-next-key)
  (define-key vterm-mode-map (kbd "C-c C-t") 'vterm-copy-mode)

  ;; Evil-specific vterm configuration
  (with-eval-after-load 'evil
    ;; Allow C-u in vterm (for shell commands)
    (define-key vterm-mode-map (kbd "C-u") 'vterm-send-C-u)

    ;; Make sure we can switch to normal mode for navigation
    (evil-define-key 'emacs vterm-mode-map
      (kbd "C-c C-k") 'evil-normal-state))

  ;; Copy mode integration with evil
  (with-eval-after-load 'evil
    (evil-define-key 'normal vterm-copy-mode-map
      (kbd "q") 'vterm-copy-mode-done
      (kbd "RET") 'vterm-copy-mode-done
      (kbd "y") 'vterm-copy-mode-done)))

;; vterm-toggle for better workflow
(use-package vterm-toggle
  :ensure t
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)

  ;; Preserve window configurations - don't mess with existing layouts
  (setq vterm-toggle-use-dedicated-buffer t) ; Keep dedicated buffer behavior
  (setq vterm-toggle-hide-method 'bury-buffer) ; Don't delete windows
  (setq vterm-toggle-reset-window-configration-after-exit nil) ; Don't reset layouts

  ;; Configure window placement to be less disruptive
  (setq display-buffer-alist
        (append display-buffer-alist
                '(("\\*vterm\\*"
                   (display-buffer-reuse-window display-buffer-in-direction)
                   (direction . below)
                   (window-height . 0.3)
                   (reusable-frames . visible)))))

  ;; Custom split functions that preserve window layout
  (defun my-vterm-toggle-split-below ()
    "Open vterm in a split below current window."
    (interactive)
    (let ((current-window (selected-window)))
      (split-window-below)
      (other-window 1)
      (vterm-toggle-show)
      (select-window current-window)))

  (defun my-vterm-toggle-split-right ()
    "Open vterm in a split to the right of current window."
    (interactive)
    (let ((current-window (selected-window)))
      (split-window-right)
      (other-window 1)
      (vterm-toggle-show)
      (select-window current-window)))

  ;; Key bindings - use custom split functions that preserve window layout
  (global-set-key (kbd "C-c t") 'my-vterm-toggle-split-below) ; Split below
  (global-set-key (kbd "C-c T") 'my-vterm-toggle-split-right) ; Split right
  (global-set-key (kbd "C-c v") 'vterm)) ; Direct vterm fallback

(provide 'my-vterm)
