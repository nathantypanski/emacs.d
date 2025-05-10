;; my-leader-keys.eljGk
;;
;; <leader> keybindings for evil-leader

(evil-leader/set-key
  "$"          'display-line-numbers-mode
  "qq"         'kill-current-buffer
  "qw"         'evil-window-delete
  "qo"         'my-kill-other-buffer
  "Q"          'kill-buffer-and-window
  "e"          'pp-eval-last-sexp
  "h"          'dired-jump
  ">"          'find-file-at-point
  "\\"         'split-window-horizontally
  "-"          'split-window-vertically
  "e"          'pp-eval-last-sexp
  "TAB"        'my-hop-around-buffers
  "RET"        'my-spawn-terminal-here
  "<SPC>RET"   'my-tmux-new-frame
  "b"          'consult-buffer
  "B"          'my-ibuffer-raise-other-window
  ","          'other-window
  "/"          'evilnc-comment-or-uncomment-lines
  "jf"         'ffap
  "jb"         'ace-jump-buffer
  "cl"         'flycheck-list-errors
  "w"          'save-buffer
  "p"          'projectile-find-file
  "P"          'projectile-switch-project
  "`"          'visit-term-buffer
  "cc"         'projectile-compile-project
  "gs"         'magit-status
  "gl"         'magit-log
  "gd"         'magit-diff
  "l"          'consult-imenu
  "f"          'find-file
  "k"          'kill-buffer
  "x"          'execute-extended-command
  "s r"        'ag-regexp
  "s s"        'consult-line
  "n"          'neotree-toggle
  ;; org bindings
  "o a"        'org-agenda
  "o T"        'org-todo-list
  "o p"        'org-priority
  "o c"        'org-capture
  "o l"        'org-store-link
  )

(evil-leader/set-key-for-mode 'git-commit-mode "qq" 'git-commit-abort)
(evil-leader/set-key-for-mode 'git-commit-mode "C" 'with-editor-finish)

(evil-leader/set-key-for-mode 'emacs-lisp-mode "." 'elisp-slime-nav-find-elisp-thing-at-point)

(provide 'my-leader-keys)
