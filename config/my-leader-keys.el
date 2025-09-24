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
  "e"          'elisp-eval-region-or-buffer
  "E"          'pp-eval-last-sexp
  "TAB"        'my-hop-around-buffers
  "RET"        'my-spawn-terminal-here
  "<SPC>RET"   'my-tmux-new-frame
  "<SPC>-"     'my-tmux-new-pane

  "B"          'ibuffer
  "/"          'evilnc-comment-or-uncomment-lines
  "jf"         'ffap
  "jb"         'switch-to-buffer
  "jp"         'project-switch-to-buffer
  "cl"         'my-flycheck-list-errors
  "w"          'save-buffer
  "P"          'projectile-switch-project
  "`"          'visit-term-buffer
  "cc"         'projectile-compile-project
  "f"          'find-file
  "k"          'my-eldoc-doc-buffer-popup
  "x"          'execute-extended-command
  "X"          'consult-complex-command
  "s r"        'ag-regexp

  ;;
  "u v"        'vundo

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
  "pc"         'projectile-invalidate-cache
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
  "T"          'tab-switch
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
  "ac"         'my-gptel-switch-model
  "ae"         'my-gptel-explain
  "ar"         'my-gptel-review
  "aT"         'my-gptel-check-tokens
  "ata"        'gptel-tool-confirm
  "ata"        'gptel--accept-tool-calls
  "atr"        'gptel--reject-tool-calls
  "ati"        'gptel--inspect-fsm

  ;; claude-code-ide
  "ai i"       'claude-code-ide-menu
  "ai t"       'claude-code-ide-toggle
  "ai w"       'claude-code-ide-toggle-window
  "ai d"       'claude-code-ide-debug-menu
  "ai q"       'claude-code-ide-quit

  ;; org bindings
  "o a"        'org-agenda
  "o T"        'org-todo-list
  "o p"        'org-priority
  "o c"        'org-capture
  "o l"        'org-store-link

  (kbd ".")    'embark-act
  "m"          'elisp-slime-nav-find-elisp-thing-at-point
  ","          'other-window)

(evil-leader/set-key-for-mode 'git-commit-mode "qq" 'git-commit-abort)
(evil-leader/set-key-for-mode 'git-commit-mode "C" 'with-editor-finish)

(provide 'my-leader-keys)
