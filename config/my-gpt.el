;;; my-gpt.el --- Streamlined LLM config for gptel / Claude -*- lexical-binding:t; -*-

;;;; Deps
(use-package request :ensure t)
(require 'subr-x) ; string-empty-p / string-blank-p

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :custom
  (claude-code-ide-cli-extra-flags "")
  (claude-code-ide-cli-path "claude")
  (claude-code-ide-focus-claude-after-ediff t)
  (claude-code-ide-focus-on-open t)
  (claude-code-ide-show-claude-window-in-ediff nil)
  (claude-code-ide-system-prompt nil)
  (claude-code-ide-use-side-window nil)
  (claude-code-ide-window-side 'right)
  (claude-code-ide-window-width 81)    ; 1 char wider than previous
  :config
  (claude-code-ide-emacs-tools-setup)

  (defun my-claude-ediff-help-message ()
    (run-with-timer
     0.5 nil
     (lambda ()
       (message "Ediff: n/p next|prev  a accept orig  b accept Claude  q quit  ? help"))))
  (add-hook 'ediff-startup-hook #'my-claude-ediff-help-message)

  ;; Terminal-friendly keys (optional; only if `general` is present)
  (with-eval-after-load 'general
    (general-define-key
     :prefix "C-c"
     "C-j" #'claude-code-ide-insert-newline
     "C-e" #'claude-code-ide-send-escape
     "C-t" #'claude-code-ide-toggle
     "C-p" #'claude-code-ide-send-prompt))

  (defun my-claude-fix-windows-after-ediff ()
    "Repair side-window layout Claude uses after ediff."
    (when claude-code-ide-use-side-window
      (dolist (win (window-list))
        (when (window-parameter win 'window-side)
          (delete-window win)))
      (when-let* ((bufname (claude-code-ide--get-buffer-name))
                  (buf (get-buffer bufname)))
        (when-let ((w (get-buffer-window buf)))
          (delete-window w))
        (claude-code-ide--display-buffer-in-side-window buf))))
  (add-hook 'ediff-cleanup-hook #'my-claude-fix-windows-after-ediff))

(use-package mcp
  :after gptel
  :straight (:type git :host github :repo "lizqwerscott/mcp.el")
  :config
  (setq mcp-hub-servers '(("memory" :command "mcp-server-memory"))))

;;;; gptel core
(defgroup my-gptel nil
  "My gptel integration."
  :group 'applications)

(defcustom my-gptel-max-output-bytes 8192
  "Clamp tool output to this many bytes."
  :type 'integer :group 'my-gptel)

(defcustom my-gptel-max-lines 300
  "Clamp tool output to this many lines."
  :type 'integer :group 'my-gptel)

(defcustom my-gptel-tool-timeout-sec 5
  "Seconds before tool wrapper times out."
  :type 'number :group 'my-gptel)

(defgroup my-gptel-pager nil
  "Paged shell output and dynamic Org response prefix for gptel."
  :group 'applications)

(defcustom my-gptel-pager-default-lines 200
  "Default number of lines to return per page."
  :type 'integer :group 'my-gptel-pager)

(defcustom my-gptel-pager-max-lines 300
  "Hard cap on number of lines returned."
  :type 'integer :group 'my-gptel-pager)

(defcustom my-gptel-pager-max-bytes 8192
  "Hard cap on total bytes returned."
  :type 'integer :group 'my-gptel-pager)

(defcustom my-gptel-temp-buffer-prefix "*gptel-out*"
  "Prefix for temporary buffers that hold full, unclamped command outputs."
  :type 'string :group 'my-gptel)

(defcustom my-gptel-temp-buffer-keep 12
  "Best-effort limit on temp output buffers to retain."
  :type 'integer :group 'my-gptel)

;; Optional knobs for how Emacs prints evaluated objects (not line limits).
(defcustom my-gptel-eval-print-length 50
  "Max length for lists/vectors printed by `prin1` during eval."
  :type 'integer :group 'my-gptel)

(defcustom my-gptel-eval-print-level 5
  "Max nesting level printed by `prin1` during eval."
  :type 'integer :group 'my-gptel)

(defun my-gptel--list-temp-buffers ()
  (seq-filter (lambda (b) (string-prefix-p my-gptel-temp-buffer-prefix (buffer-name b)))
              (buffer-list)))

(defun my-gptel--prune-temp-buffers ()
  "Keep at most `my-gptel-temp-buffer-keep' temp output buffers."
  (let* ((bufs (my-gptel--list-temp-buffers))
         (extra (max 0 (- (length bufs) my-gptel-temp-buffer-keep))))
    (when (> extra 0)
      ;; Kill oldest first (end of (buffer-list) tends to be older)
      (mapc #'kill-buffer (seq-take (nreverse bufs) extra)))))

(defun my-gptel--stash-in-temp-buffer (content &optional label)
  "Create a temp buffer with CONTENT. Return its buffer name."
  (my-gptel--prune-temp-buffers)
  (let* ((base (if (and label (not (string-empty-p label)))
                   (format "%s:%s" my-gptel-temp-buffer-prefix label)
                 my-gptel-temp-buffer-prefix))
         (name (generate-new-buffer-name base))
         (buf  (get-buffer-create name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert content)
      (goto-char (point-min)))
    name))

(defun my-gptel--emit-paged (full &optional label)
  "Emit a clamped page of FULL. If truncated, stash FULL in a temp buffer and
append instructions to page the rest via `paged_read'."
  (let* ((lines (split-string (or full "") "\n"))
         (head-lines (seq-take lines my-gptel-max-lines))
         (head (string-join head-lines "\n"))
         (trim (if (> (string-bytes head) my-gptel-max-output-bytes)
                   (substring head 0 my-gptel-max-output-bytes)
                 head))
         (truncated (or (> (length lines) my-gptel-max-lines)
                        (> (string-bytes head) my-gptel-max-output-bytes))))
    (if (not truncated)
        trim
      (let ((bufname (my-gptel--stash-in-temp-buffer full label)))
        (format "%s\n[OUTPUT TRUNCATED → %s]\nUse tool `paged_read` with:\n  file_or_buffer: \"%s\"\n  start: %d\n  line_count: %d\n…to continue reading."
                trim bufname bufname
                (1+ (length head-lines))       ; next page start (1-based)
                (min 200 my-gptel-max-lines))))))

;; -------- Dynamic Org response prefix (one level deeper than current) --------

(defun my-gptel--nested-org-prefix ()
  "Return an Org heading prefix one level deeper than current point."
  (let ((level (if (and (derived-mode-p 'org-mode) (fboundp 'org-current-level))
                   (or (org-current-level) 0)
                 0)))
    (concat (make-string (1+ level) ?*) " Assistant\n")))

(defun my-gptel--with-nested-prefix (orig &rest args)
  "Around-advice for `gptel-send' to inject a computed Org response prefix."
  (let* ((prefix (and (derived-mode-p 'org-mode) (my-gptel--nested-org-prefix)))
         ;; Ensure gptel sees a STRING, not a function/symbol.
         (gptel-response-prefix-alist
          (if prefix `((org-mode . ,prefix)) gptel-response-prefix-alist)))
    (apply orig args)))

;;;###autoload
(defun my-gptel-enable-dynamic-org-prefix ()
  "Enable dynamic Org response prefix during `gptel-send'."
  (interactive)
  (advice-add 'gptel-send :around #'my-gptel--with-nested-prefix)
  (message "my-gptel: dynamic Org response prefix enabled."))

;;;###autoload
(defun my-gptel-disable-dynamic-org-prefix ()
  "Disable dynamic Org response prefix during `gptel-send'."
  (interactive)
  (advice-remove 'gptel-send #'my-gptel--with-nested-org-prefix)
  (message "my-gptel: dynamic Org response prefix disabled."))

(defun my-gptel-cmd-paged (command &optional start line-count)
  "Run shell COMMAND and return a paged, trimmed slice.
Also stash the full raw output in a temp buffer when larger than caps."
  (interactive "sCommand: ")
  (let* ((raw  (shell-command-to-string command))
         (out  (if (or (null raw) (string-empty-p raw))
                   "(Command completed with no output)" raw))
         (lines (split-string out "\n"))
         (s    (max 1 (or start 1)))
         (n    (min my-gptel-pager-max-lines
                    (max 1 (or line-count my-gptel-pager-default-lines)))))
      (my-gptel--emit-paged
       (string-join (seq-take (seq-drop lines (1- s)) n) "\n")
       (format "cmd:%s" command))))

(use-package gptel
  :straight (:repo "karthink/gptel" :branch "master" :files ("*.el"))
  :custom-face
  (gptel-user-header ((t (:foreground "#dca3a3" :weight bold))))
  (gptel-assistant-header ((t (:foreground "#7f9f7f" :weight bold))))
  (gptel-response ((t (:foreground "#9fc59f"))))
  :commands (gptel gptel-menu gptel-send gptel-request
                   my-gptel-review my-gptel-explain my-gptel-switch-model)
  :hook ((gptel-mode . my-gptel-setup-behavior))
  :custom
  (gptel-track-response t)
  (gptel-log-level 'info)
  (gptel-use-curl t)
  (gptel-stream t)
  (gptel-use-header-line t)
  (gptel-prompt-prefix-alist '((org-mode . "** Human\n")))
  (gptel-response-prefix-alist '((org-mode . "** Assistant\n")))
  (gptel-model 'claude-sonnet-4-20250514)
  (gptel-max-tokens 800)
  (gptel-use-tools t)
  (gptel-auto-repair-invalid-state t)
  (gptel-include-tool-results nil)
  (gptel-confirm-tool-calls 'auto)
  (gptel-default-mode 'org-mode)
  (gptel-directives my-gptel-directives)
  (gptel-org-branching-context nil)
  :init
  ;; Prompts / directives
  (defvar my-gptel-system-prompt
    "You are a LLM inside Emacs. Insert literal replies; use code comments for commentary (e.g., ;; for elisp). No Markdown/Org formatting.")
  (defvar my-gptel-elisp-prompt
    "You are a LLM inside Emacs. Help with Emacs Lisp config (Emacs 30.1.90, use-package + straight). Reply in org-mode.
Note you have a number of tools available to you. Use them liberally as needed, but take care to use the paging functions so the output is not too large. Grep files for the fn you want first, then output starting on that line, etc.")
  (defvar my-emacs-system-prompt
    "You can introspect Emacs (buffers, eval). Be concise; tool output is costly. Prefer minimal I/O.")
  (defvar my-gptel-directives
    `((Inline . ,my-gptel-system-prompt)
      (Elisp  . ,my-gptel-elisp-prompt)
      (Emacs  . ,my-emacs-system-prompt)))
  :config
  ;; API keys (optional envs)
  (when-let ((k (getenv "OPENAI_API_KEY"))) (setq gptel-api-key k))
  (setq gptel-expert-commands t)

  ;; Model switcher (small + clear)
  (defvar my-gptel-models
    '(("Claude Sonnet" . (claude-sonnet-4-20250514 anthropic))
      ("Claude Opus"   . (claude-opus-4-20250514   anthropic))
      ("GPT-4o"        . (gpt-4o                   openai))
      ("GPT-4o Mini"   . (gpt-4o-mini              openai))))

  (defun my-gptel-switch-model ()
    "Interactively switch gptel model + backend."
    (interactive)
    (let* ((choice (completing-read "Model: " (mapcar #'car my-gptel-models) nil t))
           (model-info (alist-get choice my-gptel-models nil nil #'string=))
           (model (car model-info))
           (provider (cadr model-info)))
      (pcase provider
        ('anthropic
         (require 'gptel-anthropic)
         (let ((key (or (getenv "ANTHROPIC_API_KEY") (read-passwd "Anthropic API Key: "))))
           (setq gptel-model   model
                 gptel-backend (gptel-make-anthropic "Claude" :key key :stream t)
                 gptel-api-key key)))
        ('openai
         (let* ((key (or (getenv "OPENAI_API_KEY") (read-passwd "OpenAI API Key: ")))
                (backend (cdr (assoc "ChatGPT" gptel--known-backends))))
           (setq gptel-model   model
                 gptel-backend backend
                 gptel-api-key key))))
      (message "Switched to %s" choice)))

  ;; Buffer behavior
  (defun my-gptel-setup-behavior ()
    (visual-line-mode 1)
    (setq-local auto-save-timeout 60
                auto-save-visited-mode nil))

  ;; Helpers
  (defun my-gptel--guess-mode (s)
    (cond
     ((string-match-p (rx bos (* space) "(") s) 'emacs-lisp-mode)
     ((string-match-p (rx "#!/bin/bash") s) 'sh-mode)
     ((string-match-p (rx (or " for " " while " " if " " exec ")) s) 'sh-mode)
     ((string-match-p (rx (or (seq bow "def" (+ space) (+ (any word "_")))
                              (seq bow "class" (+ space) (+ (any word "_"))))) s) 'python-mode)
     ((string-match-p (rx (or "function" "=>" (seq "import" (+ space) (+ word) (+ space) "from"))) s) 'js-mode)
     (t 'fundamental-mode)))

  (defun my-gptel-confirmation-buffer (tool-spec args)
    "Preview tool call ARGS; return non-nil to proceed."
    (let* ((name (plist-get tool-spec :name))
           (desc (or (plist-get tool-spec :description) ""))
           (code (or (plist-get args :code)
                     (plist-get args :command)
                     (plist-get args :diff_content)
                     (plist-get args :content)))
           (buf (get-buffer-create "*GPTEL Tool Preview*")))
      (with-current-buffer buf
        (read-only-mode -1)
        (erase-buffer)
        (insert (format "Tool: %s\n%s\n\nArgs:\n%s\n\n"
                        name desc (pp-to-string args)))
        (when code
          (insert "===== Proposed code =====\n")
          (let ((start (point)))
            (insert code)
            (delay-mode-hooks (funcall (my-gptel--guess-mode code)))
            (font-lock-ensure start (point)))))
      (setq buffer-read-only t)
      (display-buffer buf '((display-buffer-pop-up-window)))
      (yes-or-no-p (format "Run tool %s ?" name))))
  (setq gptel-confirm-tool-calls #'my-gptel-confirmation-buffer)

  ;; Review / Explain
  (defun my-gptel-explain ()
    (interactive)
    (if-let ((text (if (use-region-p)
                       (buffer-substring-no-properties (region-beginning) (region-end))
                     (thing-at-point 'defun t))))
        (gptel-request (format "Explain this code:\n\n%s" text))
      (message "No code to explain")))
  (defun my-gptel-review ()
    (interactive)
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (buffer-string))))
      (gptel-request (format "Review this code for bugs and improvements:\n\n%s" text))))

  (defun my-gptel--project-root ()
    (when (fboundp 'project-current)
      (when-let* ((proj (project-current nil)))
        (project-root proj))))

  (defun my-gptel--resolve (path-or-dir)
    "Resolve PATH-OR-DIR robustly.
Return the expanded absolute path (string)."
    (cond
     ;; absolute or ~
     ((or (file-name-absolute-p path-or-dir)
          (string-prefix-p "~" path-or-dir))
      (expand-file-name path-or-dir))
     ;; project root relative
     ((my-gptel--project-root)
      (expand-file-name path-or-dir (my-gptel--project-root)))
     ;; fallback to home
     (t (expand-file-name path-or-dir "~"))))

  (defun my-gptel-add-project-context ()
    (interactive)
    (if-let ((files (and (project-current) (project-files (project-current)))))
        (progn
          (dolist (file (completing-read-multiple "Add files: " files))
            (gptel-add-file file))
          (message "Added files to context"))
      (message "No project found")))

  ;;;; Safety gates
  (defconst my-gptel--sensitive-name-rx
    (rx (or "id_rsa" "id_ed25519" "private" ".pem" ".key" "wallet" ".gpg"))
    "Names that should never be touched by tools.")
  (defvar my-gptel-blocked-paths '("/etc/" "/usr/" "/bin/" "/sbin/" "/root/"))
  (defvar my-gptel-blocked-patterns
    '("\\.ssh/" "\\.gnupg/" "\\.aws/" "\\.kube/" "\\.docker/"
      "\\.password-store/" "\\.authinfo" "\\.netrc"))

  (defun my-gptel-path-allowed-p (path)
    (let ((p (expand-file-name path)))
      (not (or (string-match-p my-gptel--sensitive-name-rx p)
               (seq-some (lambda (dir) (string-prefix-p dir p)) my-gptel-blocked-paths)
               (seq-some (lambda (re)  (string-match-p re p)) my-gptel-blocked-patterns)))))

  ;;;; Small tools
  (defun my-gptel-tool-list-files (path &optional start limit)
    "List files at PATH (non-dot), paging from START to LIMIT."
    (let* ((dir (my-gptel--resolve (or path ".")))
           (s (max 0 (or start 0)))
           (l (min 500 (or limit 200))))
      (cond
       ((not (my-gptel-path-allowed-p dir))
        (format "Error: Path '%s' is outside allowed directories" path))
       ((not (file-directory-p dir))
        (format "Error: '%s' is not a directory" path))
       (t
        (let* ((all (directory-files dir nil directory-files-no-dot-files-regexp t))
               (slice (seq-take (seq-drop all s) l)))
          (string-join slice "\n"))))))

  (defun my-gptel-tool-bash (command)
    "Execute a (somewhat) safe shell COMMAND."
    (cond
     ((string-match-p (rx (or "sudo" "rm -rf" "dd if=" "mkfs")) command)
      (format "Error: Dangerous command blocked: %s" command))
     ((string-match-p (rx bow "rm" eow) command)
      (if (yes-or-no-p (format "Execute command containing 'rm'? %s" command))
          (shell-command-to-string command)
        "Command cancelled"))
     (t
      (let ((out (shell-command-to-string command)))
        (if (string-blank-p out)
            "(Command completed with no output)"
          (my-gptel--emit-paged out (format "sh:%s" command)))))))

  (defun my-gptel-get-buffer-info ()
    (list :buffer-name (buffer-name)
          :major-mode major-mode
          :point (point)
          :mark (when (mark t) (mark t))
          :buffer-size (buffer-size)
          :modified-p (buffer-modified-p)))

  (defun my-gptel-get-frame-info ()
    (list :frames (length (frame-list))
          :windows (length (window-list))
          :current-window-edges (when (selected-window)
                                  (window-edges (selected-window)))
          :current-window-buffer (when (selected-window)
                                   (buffer-name (window-buffer (selected-window))))
          :frame-width (frame-width)
          :frame-height (frame-height)))

  (defun my-gptel-eval-elisp-safely (code &optional start line-count)
    "Eval elisp CODE and return a paged slice of its printed value.

START is 1-based line offset (default 1).
LINE-COUNT defaults to `my-gptel-pager-default-lines` and is capped by
`my-gptel-pager-max-lines`.

If the *full* printed result exceeds caps, it is stashed into a temp buffer
so you (or the LLM) can continue via the `paged_read` tool."
    (let ((print-length my-gptel-eval-print-length)
          (print-level  my-gptel-eval-print-level))
      (condition-case err
          (let* ((val (eval (read code)))
                 (full (with-temp-buffer
                         (prin1 val (current-buffer))
                         (buffer-string))))
            (my-gptel--emit-paged full "elisp"))
        (error (format "Error: %s" (error-message-string err))))))

  (defun my-gptel-git-status (&optional path)
    (if (vc-git-root default-directory)
        (let ((cmd (if path
                       (format "git status --porcelain %s" (shell-quote-argument path))
                     "git status --porcelain")))
          (shell-command-to-string cmd))
      "Not in a git repository"))

  (defun my-gptel-git-diff (&optional file)
    (if (vc-git-root default-directory)
        (shell-command-to-string
         (if file (format "git diff %s" (shell-quote-argument file)) "git diff"))
      "Not in a git repository"))

  (defun my-gptel-switch-buffer (buffer-name)
    (if-let ((buf (get-buffer buffer-name)))
        (progn (switch-to-buffer buf) (format "Switched to buffer: %s" buffer-name))
      (format "Buffer not found: %s" buffer-name)))

  (defun my-gptel-list-buffers ()
    "List up to 20 non-internal buffers with mode."
    (let* ((bufs (seq-filter (lambda (b) (not (string-prefix-p " " (buffer-name b))))
                             (buffer-list)))
           (bufs (seq-take bufs 20)))
      (mapconcat
       (lambda (b)
         (with-current-buffer b (format "%s [%s]" (buffer-name) major-mode)))
       bufs "\n")))

  (defun my-gptel-project-files (&optional pattern)
    (if-let* ((proj (project-current))
              (files (project-files proj)))
        (string-join (let ((files (seq-take files 50)))
                       (if pattern
                           (seq-filter (lambda (f) (string-match-p pattern f)) files)
                         files)) "\n")
      '("Not in a project")))

  (defun my-gptel-grep-project (pattern &optional file-pattern)
    "Return ≤30 matches via grep."
    (if-let* ((proj (project-current))
              (default-directory (project-root proj)))
        (let* ((inc (if file-pattern (format "--include=%s" (shell-quote-argument file-pattern)) ""))
               (pat (shell-quote-argument pattern))
               (cmd (format "grep -R -n %s %s . | head -30" inc pat))
               (out (shell-command-to-string cmd)))
          (if (> (length out) 5000)
              (concat (substring out 0 5000) "\n... (output truncated)")
            out))
      "Not in a project"))

  ;; Diff application (kept functional, just minor style nits)
  (defun my-gptel-parse-diff-hunks (diff)
    (let ((hunks nil)
          (lines (split-string diff "\n"))
          (cur nil))
      (dolist (line lines)
        (cond
         ((string-match "^@@[ \t]+\\-\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?[ \t]+\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?" line)
          (when cur (push cur hunks))
          (setq cur (list :start-line (string-to-number (match-string 3 line))
                          :old-lines '() :new-lines '())))
         ((and cur (string-match "^\\-\\(.*\\)" line))
          (push (match-string 1 line) (plist-get cur :old-lines)))
         ((and cur (string-match "^\\+\\(.*\\)" line))
          (push (match-string 1 line) (plist-get cur :new-lines)))
         ((and cur (string-match "^ \\(.*\\)" line))
          nil)))
      (when cur (push cur hunks))
      (mapcar (lambda (h)
                (plist-put h :old-lines (nreverse (plist-get h :old-lines)))
                (plist-put h :new-lines (nreverse (plist-get h :new-lines)))
                h)
              (nreverse hunks))))

  (defun my-gptel-apply-diff-internal (buffer-name diff-content)
    "Apply unified diff directly to BUFFER-NAME."
    (unless (get-buffer buffer-name) (error "Buffer %s does not exist" buffer-name))
    (with-current-buffer buffer-name
      (let ((pt (point))
            (hunks (my-gptel-parse-diff-hunks diff-content)))
        (condition-case err
            (progn
              (setq hunks (sort hunks (lambda (a b) (> (plist-get a :start-line)
                                                       (plist-get b :start-line)))))
              (dolist (h hunks)
                (let* ((start (plist-get h :start-line))
                       (old (plist-get h :old-lines))
                       (new (plist-get h :new-lines)))
                  (goto-char (point-min))
                  (forward-line (1- start))
                  (let ((beg (point)))
                    (forward-line (length old))
                    (delete-region beg (point)))
                  (when new
                    (insert (string-join new "\n"))
                    (unless (bolp) (insert "\n")))))
              (goto-char pt)
              "Patch applied successfully")
          (error (format "Patch failed: %s" (error-message-string err)))))))

  ;;;; Output clamps / timeouts (simplified)
  (defun my-gptel--with-timeout (thunk)
    (with-timeout (my-gptel-tool-timeout-sec "[TIMEOUT]")
      (funcall thunk)))

  (defun my-gptel-wrap (fn &optional label)
    "Return a wrapper around FN that clamps output and stashes the full text in a temp buffer if needed.
LABEL tags the temp buffer name."
    (lambda (&rest args)
      (my-gptel--emit-paged
       (my-gptel--with-timeout (lambda () (apply fn args)))
       label)))

  ;; File helpers
  (defun my-gptel-find-file (path &optional dir)
    "Find or create PATH in DIR (or project/home)."
    (let* ((base (or (and dir (my-gptel--resolve dir))
                     (my-gptel--project-root)
                     "~"))
           (full (if (file-name-absolute-p path)
                     path
                   (expand-file-name (or path "") base))))
      (if (my-gptel-path-allowed-p full)
          (progn
            (make-directory (file-name-directory full) t)
            (find-file (file-truename full))
            (format "Opened file: %s" (file-truename full)))
        (format "Path not allowed: %s" full))))

  (defun my-gptel-tool-wc (path)
    (let* ((p (my-gptel--resolve path)))
      (cond
       ((not (my-gptel-path-allowed-p p))
        (format "Error: Path '%s' is outside allowed directories" path))
       ((not (file-exists-p p))
        (format "Error: File '%s' does not exist" path))
       ((file-directory-p p)
        (format "Error: '%s' is a directory" path))
       (t (shell-command-to-string (format "wc -l -w -c %s" (shell-quote-argument p)))))))

  (defun my-gptel-paged-read (file-or-buffer &optional start line-count)
    "Return ≤LINE-COUNT lines starting at START (1-based)."
    (let* ((content (cond
                     ((get-buffer file-or-buffer)
                      (with-current-buffer file-or-buffer (buffer-string)))
                     ((file-exists-p file-or-buffer)
                      (with-temp-buffer
                        (insert-file-contents file-or-buffer)
                        (buffer-string)))
                     (t (user-error "Not found: %s" file-or-buffer))))
           (lines (split-string content "\n"))
           (s (max 1 (or start 1)))
           (n (min 300 (max 1 (or line-count 200)))))
      (string-join (seq-take (seq-drop lines (1- s)) n) "\n")))

  (defun my-gptel-file-summary (path)
    (let* ((p (expand-file-name path)))
      (cond
       ((not (my-gptel-path-allowed-p p))
        (format "Error: Path '%s' is outside allowed directories" path))
       ((not (file-exists-p p))
        (format "Error: File '%s' does not exist" path))
       ((file-directory-p p)
        (format "Error: '%s' is a directory" path))
       (t
        (with-temp-buffer
          (insert-file-contents p)
          (let* ((txt (buffer-string))
                 (lines (split-string txt "\n"))
                 (n (length lines))
                 (size (string-bytes txt))
                 (first (seq-take lines 10))
                 (last5 (and (> n 15) (seq-take (last lines) 5))))
            (format "File: %s\nSize: %d bytes, %d lines\n\nFirst 10 lines:\n%s%s"
                    path size n
                    (string-join first "\n")
                    (if last5 (format "\n...\n\nLast 5 lines:\n%s"
                                      (string-join last5 "\n"))
                      ""))))))))

  ;;;; Tool registration
  (defun my-gptel-setup-tools ()
    "Register tools with gptel."
    (interactive)
    (setq gptel-tools
          (list
           (gptel-make-tool
            :function #'my-gptel-tool-list-files
            :name "list_files"
            :description "List files in a directory (paged)"
            :args (list '(:name "path"  :type "string" :description "Directory path")
                        '(:name "start" :type "number" :optional t :description "paging start")
                        '(:name "limit" :type "number" :optional t :description "limit (default 200)"))
            :category "file")

           (gptel-make-tool :function #'my-gptel-get-buffer-info :name "get_buffer_info"
                            :description "Current buffer info" :args nil :category "emacs")
           (gptel-make-tool :function #'my-gptel-get-frame-info  :name "get_frame_info"
                            :description "Frames/windows info" :args nil :category "emacs")
           (gptel-make-tool :function #'my-gptel-eval-elisp-safely :name "eval_elisp"
                            :description "Safely eval elisp"
                            :args (list '(:name "code" :type "string" :description "Elisp code"))
                            :category "emacs" :confirm t)

           ;; Git
           (gptel-make-tool :function #'my-gptel-git-status :name "git_status"
                            :description "Porcelain status"
                            :args (list '(:name "repo" :type "string" :optional t
                                                :description "Path to repo or file"))
                            :category "git")
           (gptel-make-tool :function #'my-gptel-git-diff :name "git_diff"
                            :description "Show git diff (optionally for FILE)"
                            :args (list '(:name "file" :type "string" :optional t
                                                :description "File to diff"))
                            :category "git")

           ;; Buffers / project / files
           (gptel-make-tool :function #'my-gptel-switch-buffer :name "switch_buffer"
                            :description "Switch to buffer by name"
                            :args (list '(:name "buffer_name" :type "string" :description "Buffer name"))
                            :category "emacs")
           (gptel-make-tool :function #'my-gptel-list-buffers  :name "list_buffers"
                            :description "List open buffers (≤20)" :args nil :category "emacs")
           (gptel-make-tool :function #'my-gptel-project-files :name "project_files"
                            :description "List project files (≤50), optional filter"
                            :args (list '(:name "pattern" :type "string" :optional t
                                                :description "Regex to filter"))
                            :category "project")
           (gptel-make-tool :function #'my-gptel-find-file :name "find_file"
                            :description "Open/create file"
                            :args (list '(:name "path"
                                                :type "string"
                                                :description "Relative path")
                                        '(:name "dir"
                                                :type "string"
                                                :optional t
                                                :description "Parent directory"))
                            :category "file")
           (gptel-make-tool :function #'my-gptel-tool-wc :name "wc"
                            :description "wc -l -w -c for a file"
                            :args (list '(:name "path" :type "string" :description "File path"))
                            :category "file")

           ;; Search / read / patch
           (gptel-make-tool :function #'my-gptel-grep-project :name "grep_project"
                            :description "grep project (≤30 matches)"
                            :args (list '(:name "pattern" :type "string" :description "Search pattern")
                                        '(:name "file_pattern" :type "string" :optional t
                                                :description "grep --include pattern"))
                            :category "search")
           (gptel-make-tool :function #'my-gptel-paged-read :name "paged_read"
                            :description "Read N lines from file/buffer"
                            :args (list '(:name "file_or_buffer" :type "string")
                                        '(:name "start" :type "number" :optional t)
                                        '(:name "line_count" :type "number" :optional t))
                            :category "emacs")
           (gptel-make-tool :function #'my-gptel-apply-diff-internal :name "patch_buffer"
                            :description "Apply unified diff to a buffer"
                            :args (list '(:name "buffer_name" :type "string")
                                        '(:name "diff_content" :type "string"))
                            :category "emacs" :confirm t)

           ;; Simple file creation / mkdir (kept; confirm writes)
           (gptel-make-tool
            :name "make_directory" :category "file" :confirm t
            :description "Create directory NAME under PARENT"
            :function (lambda (parent name)
                        (condition-case err
                            (progn
                              (make-directory (expand-file-name name parent) t)
                              (format "Directory %s created/verified in %s" name parent))
                          (error (format "Error creating directory %s in %s: %s"
                                         name parent (error-message-string err)))))
            :args (list '(:name "parent" :type "string" :description "Parent directory")
                        '(:name "name"   :type "string" :description "New directory name")))
           (gptel-make-tool
            :name "create_file" :category "file" :confirm t
            :description "Create file with content"
            :function (lambda (path filename content)
                        (let ((full (expand-file-name filename path)))
                          (with-temp-buffer
                            (insert content)
                            (write-file full))
                          (format "Created file %s in %s" filename path)))
            :args (list '(:name "path"     :type "string" :description "Directory")
                        '(:name "filename" :type "string" :description "File name")
                        '(:name "content"  :type "string" :description "Content")))
           )))

  ;; Fix for gptel transient crash (if present)
  (ignore-errors (require 'gptel-menu-fix))

  ;; Auto-enable gptel-mode in org files with GPTEL props
  (defun my-auto-enable-gptel-mode ()
    (when (eq major-mode 'org-mode)
      (save-excursion
        (goto-char (point-min))
        (when (and (re-search-forward "^:PROPERTIES:" nil t)
                   (let ((end (save-excursion (re-search-forward "^:END:" nil t))))
                     (and end (re-search-forward "^:GPTEL_" end t))))
          (gptel-mode 1)))))

  (defun my-gptel-clean-completion ()
    "Tone down completion noise in gptel buffers."
    (interactive)
    (when (bound-and-true-p corfu-mode) (corfu-mode -1))
    (setq-local completion-at-point-functions
                (cl-remove-if
                 (lambda (fn)
                   (memq fn '(ispell-completion-at-point
                              org-completion-at-point
                              pcomplete-completions-at-point
                              comint-completion-at-point)))
                 completion-at-point-functions)))

  (add-hook 'find-file-hook #'my-auto-enable-gptel-mode)
  (add-hook 'gptel-mode-hook #'my-gptel-clean-completion)

  ;; Optional generic output limiter if you want a single call site
  (defun my-gptel-limit-output (output max-size)
    (if (> (length output) max-size)
        (concat (substring output 0 max-size) "\n[OUTPUT TRUNCATED]")
      output)))

(provide 'my-gpt)
;;; my-gpt.el ends here
