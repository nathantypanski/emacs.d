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
  ","          'other-window
  "."          'semantic-ia-fast-jump
  "/"          'evilnc-comment-or-uncomment-lines
  "jf"         'ffap
  "cl"         'my-flycheck-list-errors
  "w"          'save-buffer
  "P"          'projectile-switch-project
  "`"          'visit-term-buffer
  "cc"         'projectile-compile-project
  "gs"         'magit-status
  "gl"         'magit-log
  "gd"         'magit-diff
  "l"          'helm-semantic-or-imenu
  "f"          'helm-find-files
  "x"          'helm-M-x
  "ss"         'helm-swoop
  "sr"         'ag-regexp
  "b"          'helm-mini
  "i"          'helm-imenu
  "zc"         'wg-create-workgroup
  "B"          'ibuffer
  "zk"         'wg-kill-workgroup
  "zv"         'my-wg-switch-to-workgroup
  "zj"         'wg-switch-to-workgroup-at-index
  "z0"         'wg-switch-to-workgroup-at-index-0
  "z1"         'wg-switch-to-workgroup-at-index-1
  "z2"         'wg-switch-to-workgroup-at-index-2
  "z3"         'wg-switch-to-workgroup-at-index-3
  "z4"         'wg-switch-to-workgroup-at-index-4
  "z5"         'wg-switch-to-workgroup-at-index-5
  "z6"         'wg-switch-to-workgroup-at-index-6
  "z7"         'wg-switch-to-workgroup-at-index-7
  "z8"         'wg-switch-to-workgroup-at-index-8
  "z9"         'wg-switch-to-workgroup-at-index-9
  "zs"         'my-wg-save-session
  "zp"         'wg-switch-to-workgroup-left
  "zn"         'wg-switch-to-workgroup-right
  "zwu"        'wg-undo-wconfig-change
  "zwr"        'wg-redo-wconfig-change
  "zws"        'wg-save-wconfig
)

;; toggle between a function declaration and its implementation
(evil-leader/set-key-for-mode 'c-mode   "d" 'semantic-analyze-proto-impl-toggle)
(evil-leader/set-key-for-mode 'c++-mode "d" 'semantic-analyze-proto-impl-toggle)
(evil-leader/set-key-for-mode 'git-commit-mode "qq" 'git-commit-abort)
(evil-leader/set-key-for-mode 'c-mode   "." 'semantic-ia-fast-jump)
(evil-leader/set-key-for-mode 'c++-mode "." 'semantic-ia-fast-jump)
(evil-leader/set-key-for-mode 'emacs-lisp-mode "." 'elisp-slime-nav-find-elisp-thing-at-point)

(provide 'my-leader-keys)
