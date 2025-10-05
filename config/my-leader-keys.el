;;; my-leader-keys.el    -*- lexical-binding:t; -*-
;;
;; <leader> keybindings for evil-leader

(require 'evil-leader)
(require 'project)

(defun my-make-frame (&optional name)
  "Make a new frame (for binding to keys) optionally with name `name'."
  (interactive "sName for new frame: ")
  (if name
      (make-frame `((name . ,name)))
    (make-frame)))

;; Core search function that works in any directory
(defun my-search-in-dir (dir)
  "Search in DIR using the best available tool (rg > ag > grep)."
  (let ((default-directory dir))
    (cond
     ((executable-find "rg") (consult-ripgrep))
     ((and (fboundp 'ag-regexp) (executable-find "ag"))
      (call-interactively 'ag-regexp))
     (t (consult-grep)))))

;; Interactive function for searching in current directory
(defun my-search-here ()
  "Search in current directory."
  (interactive)
  (my-search-in-dir default-directory))

;; Interactive function for searching in project
(defun my-search-project ()
  "Search in project root directory."
  (interactive)
  (if-let ((project (project-current)))
      (my-search-in-dir (project-root project))
    (user-error "Not in a project")))

;; Interactive function for searching in arbitrary directory
(defun my-search-directory (dir)
  "Search in specified directory."
  (interactive "DDirectory: ")
  (my-search-in-dir dir))

(defun my-eval-and-insert-result ()
  "Evaluate the preceding sexp and insert result on next line."
  (interactive)
  (let ((result (eval-last-sexp nil)))
    (end-of-line)
    (newline)
    (insert (format "%S" result))))

(defun my-eval-and-insert-as-comment ()
  "Evaluate the preceding sexp and insert result as a comment."
  (interactive)
  (let ((result (eval-last-sexp nil)))
    (end-of-line)
    (newline)
    (insert (format ";; => %S" result))))

(defun my-eval-print-last-sexp ()
  "Like eval-print-last-sexp but with cleaner formatting."
  (interactive)
  (let ((result (eval-last-sexp nil)))
    (end-of-line)
    (newline)
    (insert (format "%S" result))))

(defun my-eval-region-and-comment ()
  "Evaluate region and insert result as comment below."
  (interactive)
  (when (use-region-p)
    (let* ((code (buffer-substring-no-properties (region-beginning) (region-end)))
           (result (eval (read code))))
      (goto-char (region-end))
      (end-of-line)
      (newline)
      (insert (format ";; => %S" result)))))

(evil-leader/set-key
  "$"          'display-line-numbers-mode
  "qq"         'kill-current-buffer
  "qw"         'evil-window-delete
  "qo"         'my-kill-other-buffer
  "Q"          'my-kill-current-buffer-with-confirmation
  "d"          'dired-jump
  ">"          'find-file-at-point
  "\\"         'split-window-horizontally
  "-"          'split-window-vertically
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
  "F"          'my-make-frame
  "k"          'my-eldoc-doc-buffer-popup
  "x"          'execute-extended-command
  "X"          'consult-complex-command

  ;; help
  "hf"        'helpful-function
  "hk"        'helpful-key
  "hs"        'helpful-symbol
  "hm"        'helpful-macro
  "hv"        'helpful-variable
  "hB"        'helpful-kill-buffers

  ;;
  "u v"        'vundo

  ;; consult
  "s p"        'my-search-project
  "s R"        'my-search-here
  "s r"        'my-search-directory
  "b"          'consult-buffer
  "s i"        'consult-imenu
  "s u"        'apropos-function
  "s c"        'apropos-command
  "s I"        'consult-imenu-multi
  "s o"        'consult-outline
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
  "t <TAB>"    'tab-bar-switch-to-recent-tab
  "tt"         'tab-bar-switch-to-tab
  "tc"         'tab-bar-new-tab
  "tq"         'tab-bar-close-tab
  "tn"         'tab-bar-switch-to-next-tab
  "tp"         'tab-bar-switch-to-prev-tab
  "tr"         'tab-bar-rename-tab

  ;; eval
  "ee"         'elisp-eval-region-or-buffer
  "ei"         'my-eval-and-insert-result
  "ec"         'my-eval-and-insert-as-comment
  "ep"         'my-eval-print-last-sexp

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

  ;; modify
  "m f"        'text-scale-adjust ; modify font scale

  ;; gptel
  "aa"         'gptel-add
  "af"         'gptel-add-file
  "am"         'gptel-menu
  "a RET"      'gptel-send
  "ag"         'gptel
  "ac"         'my-gptel-switch-model
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
  ","          'other-window)

(evil-leader/set-key-for-mode 'git-commit-mode "qq" 'git-commit-abort)
(evil-leader/set-key-for-mode 'git-commit-mode "C" 'with-editor-finish)

(provide 'my-leader-keys)
