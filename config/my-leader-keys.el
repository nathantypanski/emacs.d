;;; my-leader-keys.el    -*- lexical-binding:t; -*-
;;
;; <leader> keybindings for evil-leader

(evil-leader/set-key
  "$"          'display-line-numbers-mode
  "qq"         'kill-current-buffer
  "qw"         'evil-window-delete
  "qo"         'my-kill-other-buffer
  "Q"          'kill-buffer-and-window
  "h"          'dired-jump
  ">"          'find-file-at-point
  "\\"         'split-window-horizontally
  "-"          'split-window-vertically
  "e"          'pp-eval-last-sexp
  "E"          'eval-last-sexp
  "TAB"        'my-hop-around-buffers
  "RET"        'my-spawn-terminal-here
  "<SPC>RET"   'my-tmux-new-frame
  "B"          'my-ibuffer-raise-other-window
  ","          'other-window
  "/"          'evilnc-comment-or-uncomment-lines
  "jf"         'ffap
  "jb"         'find-buffer
  "cl"         'my-flycheck-list-errors
  "w"          'save-buffer
  "P"          'projectile-switch-project
  "`"          'visit-term-buffer
  "cc"         'projectile-compile-project
  "gs"         'magit-status
  "gl"         'magit-log
  "gd"         'magit-diff
  "f"          'find-file
  "k"          'kill-buffer
  "x"          'execute-extended-command
  "s r"        'ag-regexp
  "b"          'consult-buffer
  "l"          'consult-imenu
  "s s"        'consult-line
  "r"          'consult-register
  "n"          'neotree-toggle

  "pf"         'projectile-find-file
  "ps"         'projectile-ag
  "pp"         'projectile-commander
  "pv"         'projectile-vc
  "pb"         'projectile-switch-to-buffer
  "pT"         'projectile-find-test-file
  "pd"         'projectile-find-dir
  "pe"         'projectile-recentf
  "pj"         'projectile-find-tag
  "pr"         'projectile-replace
  "pR"         'projectile-regenerate-tags
  "pD"         'projectile-dired
  "pk"         'projectile-kill-buffers
  "pV"         'projectile-browse-dirty-projects

  "gz"         'eglot-inlay-hints-mode

  ;; gptel
  "aa"         'gptel-add
  "af"         'gptel-add-file
  "am"         'gptel-menu
  "as"         'gptel-send
  "aG"         'gptel

  ;; org bindings
  "o a"        'org-agenda
  "o T"        'org-todo-list
  "o p"        'org-priority
  "o c"        'org-capture
  "o l"        'org-store-link)


(evil-leader/set-key-for-mode 'git-commit-mode "qq" 'git-commit-abort)
(evil-leader/set-key-for-mode 'git-commit-mode "C" 'with-editor-finish)

(evil-leader/set-key-for-mode 'emacs-lisp-mode "." 'elisp-slime-nav-find-elisp-thing-at-point)

(provide 'my-leader-keys)
