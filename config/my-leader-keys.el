;;; my-leader-keys.el    -*- lexical-binding:t; -*-
;;
;; <leader> keybindings for evil-leader

(require 'evil-leader)
(evil-leader/set-key
  "$"          'display-line-numbers-mode
  "qq"         'kill-current-buffer
  "qw"         'evil-window-delete
  "qo"         'my-kill-other-buffer
  "Q"          'my-kill-current-buffer-with-confirmation
  "h"          'dired-jump
  ">"          'find-file-at-point
  "\\"         'split-window-horizontally
  "-"          'split-window-vertically
  "e"          'pp-eval-last-sexp
  "E"          'eval-last-sexp
  "TAB"        'my-hop-around-buffers
  "RET"        'my-spawn-terminal-here
  "<SPC>RET"   'my-tmux-new-frame
  "<SPC>-"     'my-tmux-new-pane
  "B"          'ibuffer
  ","          'other-window
  "/"          'evilnc-comment-or-uncomment-lines
  "jf"         'ffap
  "jb"         'find-buffer
  "cl"         'my-flycheck-list-errors
  "w"          'save-buffer
  "P"          'projectile-switch-project
  "`"          'visit-term-buffer
  "cc"         'projectile-compile-project
  "f"          'find-file
  "k"          'kill-buffer
  "x"          'execute-extended-command
  "X"          'consult-complex-command
  "s r"        'ag-regexp

  ;; consult
  "s R"        'consult-ripgrep
  "b"          'consult-buffer
  "l"          'consult-imenu
  "s i"        'consult-imenu
  "s l"        'consult-line
  "s m"        'consult-man
  "s f"        'consult-find

  "s e"        'consult-mark
  "s /"        'consult-isearch-history
  "r"          'consult-register

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

  ;; tabs
  "tt"         'tab-bar-switch-to-tab
  "t <TAB>"    'tab-bar-switch-to-last-tab
  "tl"         'tab-list
  "tc"         'tab-bar-new-tab
  "tq"         'tab-bar-close-tab
  "tn"         'tab-bar-switch-to-next-tab
  "tp"         'tab-bar-switch-to-prev-tab
  "tr"         'tab-bar-rename-tab

  ;; perspectives
  "pt"         'my-perspective-switch
  "pc"         'my-perspective-new
  "pq"         'my-perspective-kill-with-confirmation
  "pr"         'persp-rename
  "p <TAB>"    'persp-switch-last

  ;; magit
  "gs"         'magit-status
  "gt"         'magit-tag
  "gp"         'magit-push
  "gp"         'magit-push
  "gF"         'magit-pull
  "gd"         'magit-diff
  "gl"         'magit-log
  "gd"         'magit-diff

  ;; gptel
  "aa"         'gptel-add
  "af"         'gptel-add-file
  "am"         'gptel-menu
  "a RET"      'gptel-send
  "ag"         'gptel
  "ac"         'my-gptel-claude
  "a4"         'my-gptel-openai
  "ae"         'my-gptel-explain
  "ar"         'my-gptel-review
  "at"         'my-gptel-check-tokens

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
