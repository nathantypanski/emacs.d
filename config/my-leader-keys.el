;; my-leader-keys.el
;;
;; <leader> keybindings for evil-leader

(evil-leader/set-key
  "qq"         'kill-this-buffer
  "qw"         'evil-window-delete
  "qo"         'my-kill-other-buffer
  "Q"          'kill-buffer-and-window
  "e"          'pp-eval-last-sexp
  "h"          'dired-jump
  "\\"         'split-window-horizontally
  "-"          'split-window-vertically
  "e"          'pp-eval-last-sexp
  "TAB"        'my-hop-around-buffers
  "RET"        'my-spawn-terminal-here
  ","          'other-window
  "."          'semantic-ia-fast-jump
  "/"          'evilnc-comment-or-uncomment-lines
  "jf"         'ffap
  "jb"         'ace-jump-buffer
  "cl"         'flycheck-list-errors
  "w"          'save-buffer
  "P"          'projectile-switch-project
  "`"          'visit-term-buffer
  "cc"         'projectile-compile-project
  "gs"         'magit-status
  "gl"         'magit-log
  "gd"         'magit-diff
  "l"          'ido-goto-symbol
  "f"          'ido-find-file
  "k"          'ido-kill-buffer
  "x"          'smex
  "ss"         'helm-swoop
  "sr"         'ag-regexp
  "b"          'switch-to-buffer
  "i"          'helm-imenu
  "n"          'neotree-toggle
  "zc"         'wg-create-workgroup
  "B"          'ibuffer
  "ps"         'persp-switch
  "pr"         'persp-rename
  "pK"         'persp-kill
  "pa"         'persp-add-buffer
  "pt"         'persp-temporarily-display-buffer
  "pi"         'persp-import-buffers
  "pk"         'persp-remove-buffer
)

;; toggle between a function declaration and its implementation
(evil-leader/set-key-for-mode 'c-mode   "d" 'semantic-analyze-proto-impl-toggle)
(evil-leader/set-key-for-mode 'c++-mode "d" 'semantic-analyze-proto-impl-toggle)
(evil-leader/set-key-for-mode 'git-commit-mode "qq" 'git-commit-abort)
(evil-leader/set-key-for-mode 'c-mode   "." 'semantic-ia-fast-jump)
(evil-leader/set-key-for-mode 'c++-mode "." 'semantic-ia-fast-jump)
(evil-leader/set-key-for-mode 'emacs-lisp-mode "." 'elisp-slime-nav-find-elisp-thing-at-point)

(provide 'my-leader-keys)
