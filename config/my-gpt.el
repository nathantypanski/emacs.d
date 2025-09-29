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

(defcustom my-gptel-max-lines 500
  "Clamp tool output to this many lines."
  :type 'integer :group 'my-gptel)

(defcustom my-gptel-tool-timeout-sec 5
  "Seconds before tool wrapper times out."
  :type 'number :group 'my-gptel)

(defgroup my-gptel-pager nil
  "Paged shell output and dynamic Org response prefix for gptel."
  :group 'applications)

(defcustom my-gptel-pager-default-lines 300
  "Default number of lines to return per page."
  :type 'integer :group 'my-gptel-pager)

(defcustom my-gptel-pager-max-lines 500
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

(defun my-gptel--nested-org-prefix (&optional label)
  "Return an Org heading prefix that continues the conversation thread.
LABEL defaults to 'Assistant' but can be 'Human' or other text.
This ensures proper nesting like a chat conversation."
  (let* ((label (or label "Assistant"))
         (level (if (derived-mode-p 'org-mode)
                    (or (my-gptel--find-conversation-context)
                        ;; Fallback: nest under current heading
                        (save-excursion
                          (beginning-of-line)
                          (or (org-current-level)
                              (progn
                                (ignore-errors (org-back-to-heading t))
                                (org-current-level))
                              0)))
                  0)))
    (concat (make-string level ?*) " " label "\n")))

(defun my-gptel--with-nested-prefix (orig &rest args)
  "Around-advice for `gptel-send' to inject computed Org prompt/response prefixes.
Ensures conversation threads nest properly like a chat interface."
  (let* ((response-prefix (and (derived-mode-p 'org-mode)
                               (my-gptel--nested-org-prefix "Assistant")))
         (prompt-prefix (and (derived-mode-p 'org-mode)
                             (my-gptel--nested-org-prefix "Human")))
         ;; Ensure gptel sees a STRING, not a function/symbol.
         (gptel-prompt-prefix-alist
          (if prompt-prefix
              `((org-mode . ,prompt-prefix))
            gptel-prompt-prefix-alist))
         (gptel-response-prefix-alist
          (if response-prefix
              `((org-mode . ,response-prefix))
            gptel-response-prefix-alist)))
    ;; Debug info (remove in production)
    (when (and (derived-mode-p 'org-mode) response-prefix)
      (message "GPT conversation: Using level %d for Human/Assistant headings"
               (length (car (split-string response-prefix " ")))))
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


(use-package gptel
  :straight (:repo "karthink/gptel" :branch "master" :files ("*.el"))
  :custom-face
  (gptel-user-header ((t (:foreground "#dca3a3" :weight bold))))
  (gptel-assistant-header ((t (:foreground "#7f9f7f" :weight bold))))
  (gptel-response ((t (:foreground "#9fc59f"))))
  :commands (gptel gptel-menu gptel-send gptel-request
                   my-gptel-switch-model)
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
  (gptel-max-tokens 1000)
  (gptel-use-tools t)
  (gptel-auto-repair-invalid-state t)
  (gptel-include-tool-results t)
  (gptel-confirm-tool-calls t)
  (gptel-default-mode 'org-mode)
  (gptel-directives my-gptel-directives)
  (gptel-org-branching-context t)
  :init
  ;; Prompts / directives
  (defvar my-gptel-tone-instruction
    "<tone>
 1. Be terse and to the point.  Speak directly.
 2. Explain your reasoning.
 3. Do NOT hedge or qualify.
 4. If you don't know, say you don't know.
 5. Do not offer unprompted advice or clarifications.
 6. Never apologize.
 7. Do NOT summarize your answers.
</tone>")

  (defvar my-gptel-system-prompt
    (format "%s

You are a LLM inside Emacs. Insert literal replies; use code comments for commentary (e.g., ;; for elisp). No Markdown/Org formatting." my-gptel-tone-instruction))

  (defvar my-gptel-elisp-prompt
    (format "%s

You are a LLM inside Emacs. Help with Emacs Lisp config (Emacs
30.1.90, use-package + straight). Write in an *idiomatic* and *modern*
emacs style, avoiding clunkiness like =cl-lib= and instead use the
recent builtin functions.

Note you have a number of tools available to you. Use them as needed, but take care to use the size-limiting functionality in calls so the output size is not too large." my-gptel-tone-instruction))

  (defvar my-emacs-system-prompt
    (format "%s

You can introspect Emacs (buffers, eval). Be concise; tool output is costly. Prefer minimal I/O." my-gptel-tone-instruction))

  (defvar my-gptel-directives
    `((Inline . ,my-gptel-system-prompt)
      (Elisp  . ,my-gptel-elisp-prompt)
      (Emacs  . ,my-emacs-system-prompt)))

  ;;;; Output clamps / timeouts (simplified)
  (defun my-gptel--with-timeout (thunk)
    (with-timeout (my-gptel-tool-timeout-sec "[TIMEOUT]")
      (funcall thunk)))

  (defmacro my-gptel-with-safe-execution (label &rest body)
    "Execute BODY with timeout, error handling, and paging.
LABEL is used for the temp buffer name if output is truncated."
    `(condition-case err
         (my-gptel--with-timeout
          (lambda ()
            (let ((result (progn ,@body)))
              (if (and result (not (string-blank-p (format "%s" result))))
                  (my-gptel--emit-paged (format "%s" result) ,label)
                result))))
       (error (format "Error: %s" (error-message-string err)))))

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

  (defun my-gptel--get-project (&optional project-dir)
    "Get project info for PROJECT-DIR (default: current directory).
Returns (project-root . project-instance) or nil if no project."
    (when (fboundp 'project-current)
      (let* ((dir (or project-dir default-directory))
             (default-directory dir)
             (proj (project-current nil)))
        (when proj
          (cons (project-root proj) proj)))))

  (defun my-gptel--project-root (&optional project-dir)
    "Get project root for PROJECT-DIR (default: current directory)."
    (car (my-gptel--get-project project-dir)))

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

  (defvar my-gptel-allowed-paths
    (list (expand-file-name "~/src")
          (expand-file-name "~/dotfiles")
          (expand-file-name "~/.emacs.d/straight/repos"))
    "List of allowed base paths for file operations.")

  (defun my-gptel-path-allowed-p (path)
    "Check if PATH is allowed for file operations with robust security checks."
    (let* ((p (expand-file-name path))
           ;; Resolve symlinks to get the real path
           (resolved (file-truename p)))
      (and
       ;; Must be under one of the allowed base paths (check both original and resolved)
       (or (seq-some (lambda (allowed) (string-prefix-p allowed p)) my-gptel-allowed-paths)
           (seq-some (lambda (allowed) (string-prefix-p allowed resolved)) my-gptel-allowed-paths))
       ;; Must not match sensitive patterns
       (not (string-match-p my-gptel--sensitive-name-rx resolved))
       ;; Must not be in blocked system paths
       (not (seq-some (lambda (dir) (string-prefix-p dir resolved)) my-gptel-blocked-paths))
       ;; Must not match blocked patterns
       (not (seq-some (lambda (re) (string-match-p re resolved)) my-gptel-blocked-patterns)))))

  ;;;; Small tools
  (defun my-gptel-tool-list-files (path &optional start limit)
    "List files at PATH (non-dot), paging from START to LIMIT."
    (let* ((dir (my-gptel--resolve (or path ".")))
           (validation-error (my-gptel--validate-file-path dir :allow-directory t)))
      (if validation-error
          validation-error
        (let* ((all (directory-files dir nil directory-files-no-dot-files-regexp t))
               (slice (my-gptel--paginate-list all start (min 500 (or limit 200)))))
          (string-join slice "\n")))))

  (defvar my-gptel-dangerous-commands
    (rx (or "sudo" "rm -rf" "dd if=" "mkfs" "format" "fdisk" "parted"
            "poweroff" "shutdown" "reboot" "halt" "init 0" "init 6"))
    "Regex pattern for dangerous commands to block.")

  (defun my-gptel-tool-bash (command)
    "Execute a shell COMMAND with safety checks and error handling."
    (cond
     ;; Block dangerous commands
     ((string-match-p my-gptel-dangerous-commands command)
      (format "Error: Dangerous command blocked: %s" command))

     ;; Special handling for rm commands (require confirmation)
     ((string-match-p (rx bow "rm" eow) command)
      (if (yes-or-no-p (format "Execute command containing 'rm'? %s" command))
          (my-gptel-with-safe-execution (format "sh:%s" command)
            (shell-command-to-string command))
        "Command cancelled"))

     ;; Execute other commands with safety wrapper
     (t
      (my-gptel-with-safe-execution (format "sh:%s" command)
        (shell-command-to-string command)))))

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

  ;;;; Elisp introspection tools (using helpful package)
  (defun my-gptel-symbol-exists (name)
    "Check if symbol NAME exists in obarray."
    (if (intern-soft name) "exists" "nil"))

  (defun my-gptel-function-completions (prefix &optional limit)
    "Return functions matching PREFIX. LIMIT defaults to 50."
    (let ((limit (or limit 50))
          (matches '()))
      (mapatoms (lambda (sym)
                  (when (and (fboundp sym)
                             (string-match-p (regexp-quote prefix) (symbol-name sym))
                             (< (length matches) limit))
                    (push (symbol-name sym) matches))))
      (string-join (sort matches #'string<) "\n")))

  (defun my-gptel-variable-completions (prefix &optional limit)
    "Return variables matching PREFIX. LIMIT defaults to 50."
    (let ((limit (or limit 50))
          (matches '()))
      (mapatoms (lambda (sym)
                  (when (and (boundp sym)
                             (string-match-p (regexp-quote prefix) (symbol-name sym))
                             (< (length matches) limit))
                    (push (symbol-name sym) matches))))
      (string-join (sort matches #'string<) "\n")))

  (defun my-gptel-helpful-function (symbol)
    "Get detailed function help using helpful package."
    (when-let ((sym (intern-soft symbol)))
      (if (fboundp sym)
          (progn
            (helpful-function sym)
            (with-current-buffer (helpful--buffer sym t)
              (buffer-string)))
        (format "Symbol '%s' is not a function" symbol))))

  (defun my-gptel-helpful-variable (symbol)
    "Get detailed variable help using helpful package."
    (when-let ((sym (intern-soft symbol)))
      (if (boundp sym)
          (progn
            (helpful-variable sym)
            (with-current-buffer (helpful--buffer sym nil)
              (buffer-string)))
        (format "Symbol '%s' is not a variable" symbol))))

  (defun my-gptel-helpful-symbol (symbol)
    "Get comprehensive help for SYMBOL using helpful package."
    (when-let ((sym (intern-soft symbol)))
      (helpful-symbol sym)
      (with-current-buffer (helpful--buffer sym nil)
        (buffer-string))))

  (defun my-gptel-features ()
    "List loaded Emacs features."
    (string-join (mapcar #'symbol-name (sort features #'string<)) "\n"))

  (defun my-gptel-featurep (feature)
    "Check if FEATURE is loaded."
    (if (featurep (intern feature))
        "loaded"
      "not-loaded"))

  (defun my-gptel-info-manuals ()
    "List available Info manuals."
    (require 'info)
    (let ((manuals (info--manual-names nil)))
      (string-join (sort (info--filter-manual-names manuals) #'string<) "\n")))

  (defun my-gptel-info-read (manual node)
    "Read contents of NODE in MANUAL using Info system."
    (require 'info)
    (condition-case err
        (save-window-excursion
          (info (format "(%s)%s" manual node))
          (with-current-buffer "*info*"
            (buffer-substring-no-properties (point-min) (point-max))))
      (error (format "Error reading Info node: %s" (error-message-string err)))))

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
    (let* ((dir (my-gptel--path-to-dir path))
           (default-directory dir))
      (if (vc-git-root dir)
          (let* ((cmd (if path
                          (format "git status --porcelain -- %s" (shell-quote-argument path))
                        "git status --porcelain"))
                 (output (shell-command-to-string cmd)))
            (my-gptel--emit-paged output "git-status"))
        "Not in a git repository")))

  (defun my-gptel-git-diff (&optional file)
    (let* ((dir (my-gptel--path-to-dir file))
           (default-directory dir))
      (if (vc-git-root dir)
          (let* ((cmd (if file
                          (format "git diff -- %s" (shell-quote-argument file))
                        "git diff"))
                 (output (shell-command-to-string cmd)))
            (my-gptel--emit-paged output "git-diff"))
        "Not in a git repository")))
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

  (defun my-gptel-project-files (&optional pattern project-dir)
    "List project files, optionally filtered by PATTERN.
PROJECT-DIR specifies which project (default: current directory)."
    (if-let* ((project-info (my-gptel--get-project project-dir))
              (proj (cdr project-info))
              (files (project-files proj)))
        (string-join (let ((files (seq-take files 50)))
                       (if pattern
                           (seq-filter (lambda (f) (string-match-p pattern f)) files)
                         files)) "\n")
      (format "Not in a project (checked: %s)" (or project-dir default-directory))))

  (defun my-gptel-grep-project (pattern &optional file-pattern project-dir)
    "Return ≤30 matches via grep in project.
PROJECT-DIR specifies which project (default: current directory)."
    (if-let* ((project-root (my-gptel--project-root project-dir))
              (default-directory project-root))
        (let* ((inc (if file-pattern (format "--include=%s" (shell-quote-argument file-pattern)) ""))
               (pat (shell-quote-argument pattern))
               (cmd (format "grep -R -n %s %s . | head -30" inc pat))
               (out (shell-command-to-string cmd)))
          (if (> (length out) 5000)
              (concat (substring out 0 5000) "\n... (output truncated)")
            out))
      (format "Not in a project (checked: %s)" (or project-dir default-directory))))

  ;; File helpers
  (defun my-gptel--with-buffer-or-file (name type buffer-fn file-fn)
    "Execute BUFFER-FN or FILE-FN based on NAME and TYPE.
TYPE can be 'buffer', 'file', or nil (auto-detect).
BUFFER-FN receives (buffer-name).
FILE-FN receives (resolved-path).
Returns result or error message."
    (let ((use-buffer (cond
                       ((string= type "buffer") t)
                       ((string= type "file") nil)
                       (t (get-buffer name))))) ; auto-detect if no type specified
      (if use-buffer
          ;; Handle as buffer
          (if (get-buffer name)
              (funcall buffer-fn name)
            (format "Error: Buffer '%s' does not exist" name))
        ;; Handle as file
        (let* ((p (my-gptel--resolve name))
               (validation-error (my-gptel--validate-file-path p :allow-directory nil :allow-missing nil)))
          (if validation-error
              validation-error
            (funcall file-fn p))))))

  (defun my-gptel--buffer-dir ()
    (or (and buffer-file-name (file-name-directory buffer-file-name))
        default-directory))

  (defun my-gptel--path-to-dir (path)
    "Convert PATH to a directory path. If PATH is a file, return its directory."
    (if path
        (if (file-directory-p path)
            path
          (file-name-directory (expand-file-name path)))
      default-directory))

  (defun my-gptel--validate-file-path (path &key allow-directory allow-missing)
    "Validate PATH for file operations. Return error string or nil if valid.
ALLOW-DIRECTORY: if t, don't error on directories
ALLOW-MISSING: if t, don't error on non-existent files"
    (let ((p (expand-file-name path)))
      (cond
       ((not (my-gptel-path-allowed-p p))
        (format "Error: Path '%s' is outside allowed directories" path))
       ((and (not allow-missing) (not (file-exists-p p)))
        (format "Error: File '%s' does not exist" path))
       ((and (not allow-directory) (file-directory-p p))
        (format "Error: '%s' is a directory" path))
       (t nil))))

  (defun my-gptel--safe-file-create (path content)
    "Safely create file at PATH with CONTENT, creating directories as needed."
    (make-directory (file-name-directory path) t)
    (with-temp-buffer
      (insert content)
      (write-file path)))

  (defun my-gptel--paginate-lines (content &optional start line-count)
    "Return a paginated slice of CONTENT lines.
START is 1-based (default 1), LINE-COUNT defaults to 200."
    (let* ((lines (split-string (or content "") "\n"))
           (s (max 1 (or start 1)))
           (n (min 300 (max 1 (or line-count 200)))))
      (string-join (seq-take (seq-drop lines (1- s)) n) "\n")))

  (defun my-gptel--paginate-list (items &optional start limit)
    "Return a paginated slice of ITEMS list.
START is 0-based (default 0), LIMIT defaults to 200."
    (let* ((s (max 0 (or start 0)))
           (l (max 1 (or limit 200))))
      (seq-take (seq-drop items s) l)))

  (defun my-gptel--project-candidates (basename)
    "Return absolute paths in current project whose basename equals BASENAME."
    (when-let* ((proj (project-current))
                (root (project-root proj))
                (files (project-files proj)))
      (cl-loop for f in files
               for abs = (expand-file-name f root)
               when (string= (file-name-nondirectory abs) basename)
               collect abs)))

  (defun my-gptel-open-file (path)
    "Open existing file by absolute or relative path."
    (let ((full-path (expand-file-name path)))
      (if (file-exists-p full-path)
          (let ((buffer (find-file-noselect full-path)))
            (format "Opened file: %s\nBuffer name: %s" full-path (buffer-name buffer)))
        (format "File does not exist: %s" full-path))))

  (defun my-gptel-create-file-at-path (path content)
    "Create new file at specific path with content."
    (let ((full-path (expand-file-name path)))
      (make-directory (file-name-directory full-path) t)
      (my-gptel--safe-file-create full-path content)
      (let ((buffer (find-file-noselect full-path)))
        (format "Created file: %s\nBuffer name: %s" full-path (buffer-name buffer)))))

  (defun my-gptel-make-directory (parent name)
    "Create directory NAME under PARENT."
    (condition-case err
        (progn
          (make-directory (expand-file-name name parent) t)
          (format "Directory %s created/verified in %s" name parent))
      (error (format "Error creating directory %s in %s: %s"
                     name parent (error-message-string err)))))

  (defun my-gptel-create-file (path filename content)
    "Create file FILENAME in PATH with CONTENT."
    (let ((full (expand-file-name filename path)))
      (my-gptel--safe-file-create full content)
      (format "Created file %s in %s" filename path)))

  (defun my-gptel-tool-wc (name &optional type)
    "Get word count for NAME. TYPE can be 'buffer' or 'file' (default: auto-detect)."
    (my-gptel--with-buffer-or-file
     name type
     ;; Buffer handler
     (lambda (buffer-name)
       (with-current-buffer buffer-name
         (let ((lines (count-lines (point-min) (point-max)))
               (words (count-words (point-min) (point-max)))
               (chars (- (point-max) (point-min))))
           (format "%d %d %d %s" lines words chars buffer-name))))
     ;; File handler
     (lambda (file-path)
       (shell-command-to-string (format "wc -l -w -c %s" (shell-quote-argument file-path))))))

  (defun my-gptel-paged-read (file-or-buffer &optional start line-count)
    "Return ≤LINE-COUNT lines starting at START (1-based)."
    (let* ((content (cond
                     ((get-buffer file-or-buffer)
                      (with-current-buffer file-or-buffer (buffer-string)))
                     ((file-exists-p file-or-buffer)
                      (with-temp-buffer
                        (insert-file-contents file-or-buffer)
                        (buffer-string)))
                     (t (user-error "Not found: %s" file-or-buffer)))))
      (my-gptel--paginate-lines content start line-count)))

  (defun my-gptel-replace-lines (buffer-name start-line end-line new-content)
    "Replace lines START-LINE to END-LINE with NEW-CONTENT."
    (unless (get-buffer buffer-name)
      (error "Buffer %s does not exist" buffer-name))
    (with-current-buffer buffer-name
      (let ((line-count (line-number-at-pos (point-max))))
        (when (or (< start-line 1) (> start-line line-count)
                  (< end-line start-line) (> end-line line-count))
          (error "Invalid line range: %d-%d (buffer has %d lines)"
                 start-line end-line line-count)))
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- start-line))
        (let ((start (point))
              (old-content))
          (forward-line (1+ (- end-line start-line)))
          (setq old-content (buffer-substring start (point)))
          (delete-region start (point))
          (insert new-content)
          (unless (string-suffix-p "\n" new-content)
            (insert "\n"))
          (format "Replaced lines %d-%d:\nOLD:\n%s\nNEW:\n%s"
                  start-line end-line old-content new-content)))))

  (defun my-gptel-insert-at-line (buffer-name line-number content)
    "Insert CONTENT at LINE-NUMBER in BUFFER-NAME."
    (unless (get-buffer buffer-name)
      (error "Buffer %s does not exist" buffer-name))
    (with-current-buffer buffer-name
      (let ((line-count (line-number-at-pos (point-max))))
        (when (or (< line-number 1) (> line-number (1+ line-count)))
          (error "Invalid line number: %d (buffer has %d lines)"
                 line-number line-count)))
      (save-excursion
        (goto-char (point-min))
        (if (= line-number 1)
            (goto-char (point-min))
          (forward-line (1- line-number)))
        (beginning-of-line)
        (insert content)
        (unless (string-suffix-p "\n" content)
          (insert "\n"))
        (format "Inserted at line %d:\n%s" line-number content))))

  (defun my-gptel-search-replace (buffer-name old-text new-text &optional literal)
    "Replace OLD-TEXT with NEW-TEXT in BUFFER-NAME."
    (unless (get-buffer buffer-name)
      (error "Buffer %s does not exist" buffer-name))
    (with-current-buffer buffer-name
      (save-excursion
        (goto-char (point-min))
        (let ((count 0)
              (replacements '()))
          (if literal
              (while (search-forward old-text nil t)
                (let ((line (line-number-at-pos)))
                  (replace-match new-text nil t)
                  (push (format "Line %d" line) replacements)
                  (setq count (1+ count))))
            (while (re-search-forward old-text nil t)
              (let ((line (line-number-at-pos)))
                (replace-match new-text)
                (push (format "Line %d" line) replacements)
                (setq count (1+ count)))))
          (if (> count 0)
              (format "Replaced %d occurrences at: %s"
                      count (string-join (nreverse replacements) ", "))
            "No matches found")))))

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

           ;; Elisp introspection tools
           (gptel-make-tool :function #'my-gptel-symbol-exists :name "symbol_exists"
                            :description "Check if a symbol exists in Emacs"
                            :args (list '(:name "name" :type "string" :description "Symbol name to check"))
                            :category "emacs")
           (gptel-make-tool :function #'my-gptel-function-completions :name "function_completions"
                            :description "Find functions matching a prefix pattern"
                            :args (list '(:name "prefix" :type "string" :description "Function name prefix to match")
                                        '(:name "limit" :type "number" :optional t :description "Max results (default 50)"))
                            :category "emacs")
           (gptel-make-tool :function #'my-gptel-variable-completions :name "variable_completions"
                            :description "Find variables matching a prefix pattern"
                            :args (list '(:name "prefix" :type "string" :description "Variable name prefix to match")
                                        '(:name "limit" :type "number" :optional t :description "Max results (default 50)"))
                            :category "emacs")
           (gptel-make-tool :function #'my-gptel-helpful-function :name "help_function"
                            :description "Get detailed help for a function using helpful package (includes examples, source, references)"
                            :args (list '(:name "symbol" :type "string" :description "Function name"))
                            :category "emacs")
           (gptel-make-tool :function #'my-gptel-helpful-variable :name "help_variable"
                            :description "Get detailed help for a variable using helpful package (includes current value, references)"
                            :args (list '(:name "symbol" :type "string" :description "Variable name"))
                            :category "emacs")
           (gptel-make-tool :function #'my-gptel-helpful-symbol :name "help_symbol"
                            :description "Get comprehensive help for any symbol (function, variable, face, etc) using helpful package"
                            :args (list '(:name "symbol" :type "string" :description "Symbol name"))
                            :category "emacs")
           (gptel-make-tool :function #'my-gptel-features :name "features"
                            :description "List all loaded Emacs features/libraries"
                            :args nil
                            :category "emacs")
           (gptel-make-tool :function #'my-gptel-featurep :name "feature_loaded"
                            :description "Check if a specific feature/library is loaded"
                            :args (list '(:name "feature" :type "string" :description "Feature name to check"))
                            :category "emacs")
           (gptel-make-tool :function #'my-gptel-info-manuals :name "info_manuals"
                            :description "List available Info documentation manuals (Emacs manual, Elisp manual, etc)"
                            :args nil
                            :category "emacs")
           (gptel-make-tool :function #'my-gptel-info-read :name "info_read"
                            :description "Read specific node from Info documentation system"
                            :args (list '(:name "manual" :type "string" :description "Manual name (e.g., 'emacs', 'elisp')")
                                        '(:name "node" :type "string" :description "Node name (e.g., 'Top', 'Variables')"))
                            :category "emacs")

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
                                                :description "Regex to filter")
                                        '(:name "project_dir" :type "string" :optional t
                                                :description "Project directory (default: current)"))
                            :category "project")
           (gptel-make-tool :function #'my-gptel-open-file :name "open_file"
                            :description "Open existing file by absolute or relative path"
                            :args (list '(:name "path" :type "string" :description "File path (absolute or relative to current directory)"))
                            :category "file")

           (gptel-make-tool :function #'my-gptel-create-file-at-path :name "create_file_at_path"
                            :description "Create new file at specific path with content"
                            :args (list '(:name "path" :type "string" :description "File path to create")
                                        '(:name "content" :type "string" :description "File content"))
                            :category "file" :confirm t)
           (gptel-make-tool :function #'my-gptel-tool-wc :name "wc"
                            :description "Word count for buffer or file"
                            :args (list '(:name "name" :type "string" :description "Buffer name or file path")
                                        '(:name "type" :type "string" :optional t :description "Specify 'buffer' or 'file' (default: auto-detect)"))
                            :category "file")

           ;; Search / read / patch
           (gptel-make-tool :function #'my-gptel-grep-project :name "grep_project"
                            :description "grep project (≤30 matches)"
                            :args (list '(:name "pattern" :type "string" :description "Search pattern")
                                        '(:name "file_pattern" :type "string" :optional t
                                                :description "grep --include pattern")
                                        '(:name "project_dir" :type "string" :optional t
                                                :description "Project directory (default: current)"))
                            :category "search")
           (gptel-make-tool :function #'my-gptel-paged-read :name "paged_read"
                            :description "Read N lines from buffer or file (buffer-first auto-detect)"
                            :args (list '(:name "file_or_buffer" :type "string" :description "Buffer name or file path")
                                        '(:name "start" :type "number" :optional t :description "1-based start line (default: 1)")
                                        '(:name "line_count" :type "number" :optional t :description "Lines to read (default: 200)"))
                            :category "emacs")
           ;; Simple file creation / mkdir (kept; confirm writes)
           (gptel-make-tool :function #'my-gptel-make-directory :name "make_directory"
                            :description "Create directory NAME under PARENT"
                            :args (list '(:name "parent" :type "string" :description "Parent directory")
                                        '(:name "name"   :type "string" :description "New directory name"))
                            :category "file" :confirm t)

           (gptel-make-tool :function #'my-gptel-create-file :name "create_file"
                            :description "Create file with content"
                            :args (list '(:name "path"     :type "string" :description "Directory")
                                        '(:name "filename" :type "string" :description "File name")
                                        '(:name "content"  :type "string" :description "Content"))
                            :category "file" :confirm t)
           (gptel-make-tool
            :name "replace_lines" :category "edit" :confirm t
            :description "Replace lines START..END (inclusive) in BUFFER-NAME."
            :function #'my-gptel-replace-lines
            :args (list '(:name "buffer_name" :type "string" :description "Target buffer")
                        '(:name "start_line"  :type "number" :description "1-based")
                        '(:name "end_line"    :type "number" :description "1-based, ≥ start_line")
                        '(:name "new_content" :type "string" :description "Replacement text")))

           (gptel-make-tool
            :name "search_replace" :category "edit" :confirm t
            :description "Replace OLD-TEXT with NEW-TEXT; literal or regex."
            :function #'my-gptel-search-replace
            :args (list '(:name "buffer_name" :type "string" :description "Target buffer")
                        '(:name "old_text"    :type "string" :description "String or regex")
                        '(:name "new_text"    :type "string" :description "Replacement")
                        '(:name "literal"     :type "boolean" :optional t :description "Treat OLD-TEXT literally? (default: regex)")))

           (gptel-make-tool
            :name "insert_at_line" :category "edit" :confirm t
            :description "Insert CONTENT at LINE-NUMBER (before existing line)."
            :function #'my-gptel-insert-at-line
            :args (list '(:name "buffer_name" :type "string" :description "Target buffer")
                        '(:name "line_number" :type "number" :description "1-based line")
                        '(:name "content"     :type "string" :description "Text to insert"))))))


  ;; Fix for gptel transient crash (if present)
  (ignore-errors (require 'gptel-menu-fix))

  ;; Initialize tools
  (my-gptel-setup-tools)

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
  (add-hook 'gptel-mode-hook #'my-gptel-clean-completion))


(provide 'my-gpt)
;;; my-gpt.el ends here
