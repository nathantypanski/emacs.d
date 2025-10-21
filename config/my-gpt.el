;;; my-gpt.el --- Streamlined LLM config for gptel / Claude -*- lexical-binding:t; -*-

(require 'cl-lib)
(require 'subr-x) ;; for string-join / string-empty-p / string-blank-p

(use-package request :ensure t)

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

;;;;; gptel interaction philosophy
;;
;; This configuration implements a tightly scoped interface between LLM tool agents
;; and the user's Emacs editing session, following these core principles:
;;
;; 1. NON-INTRUSIVE OPERATION
;;    - Tools operate in the background without disrupting the user's workflow
;;    - Use `save-window-excursion` to prevent buffer displays during tool execution
;;    - Return data rather than showing UI - let the LLM consume information quietly
;;
;; 2. BUFFER-FIRST DESIGN
;;    - Tools accept either buffer names or file paths as primary arguments
;;    - Buffer operations are preferred when possible (faster, no filesystem I/O)
;;    - Explicit `type` parameter ('buffer' or 'file') allows precise control
;;    - Generic `my-gptel--with-buffer-or-file` helper standardizes this pattern
;;
;; 3. SCOPED ACCESS CONTROL
;;    - Path validation ensures tools only access allowed directories
;;    - Git-tracked files preference maintains project scope
;;    - Security functions prevent dangerous operations
;;    - Confirmation required for state-changing operations (edit, delete, bash)
;;
;; 4. INTELLIGENT OUTPUT MANAGEMENT
;;    - Automatic pagination for large outputs (pages of ~50 lines)
;;    - Truncation with continuation markers for extremely large data
;;    - Structured output formatting for different tool categories
;;    - Context-aware responses (error messages, validation feedback)
;;
;; 5. UNIFIED TOOL ECOSYSTEM
;;    - Categories: file, edit, search, info, emacs, project
;;    - Consistent parameter patterns across related tools
;;    - Extensible architecture - easy to add new tool types
;;    - Integration with existing Emacs packages (helpful, project.el, etc.)
;;
;; The goal is to create an invisible but powerful bridge where the LLM can
;; efficiently explore and manipulate the codebase without disrupting the human
;; developer's flow, while maintaining appropriate security boundaries.
;;
;;;;


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

;;;;; Tool Definition Macro
(defmacro defgptel-tool (name args docstring &rest options-and-body)
  "Define a gptel tool function and register it automatically.

NAME is the function name (symbol).
ARGS is the parameter list with type annotations: (param type description &optional)
DOCSTRING becomes both the function docstring and tool description.
OPTIONS-AND-BODY contains keyword options followed by the function body.

Supported options:
  :name STRING          - Tool name (default: derived from function name)
  :category STRING      - Tool category (default: \"general\")
  :confirm BOOLEAN      - Require confirmation (default: nil)

Parameter format in ARGS:
  (param-name type description)              - Required parameter
  (param-name type description &optional)    - Optional parameter

Example:
  (defgptel-tool my-gptel-word-count (file string \"File path\")
    \"Count words in file.\"
    :category \"file\"
    (with-temp-buffer
      (insert-file-contents file)
      (count-words (point-min) (point-max))))"
  (declare (indent defun) (doc-string 3))
  (let* ((func-name (symbol-name name))
         (tool-name-default (if (string-match "^my-gptel-\\(.+\\)" func-name)
                                (replace-match "\\1" nil nil func-name)
                              func-name))
         (options nil)
         (body nil)
         (parsing-options t))

    ;; Parse options and body
    (dolist (item options-and-body)
      (if (and parsing-options (keywordp item))
          (progn
            (push item options)
            (push (pop options-and-body) options))
        (progn
          (setq parsing-options nil)
          (push item body))))

    (setq options (nreverse options)
          body (nreverse body))

    ;; Extract option values
    (let ((tool-name (or (plist-get options :name) tool-name-default))
          (category (or (plist-get options :category) "general"))
          (confirm (plist-get options :confirm))
          (param-list nil))

      ;; Build parameter list for gptel-make-tool
      (dolist (param args)
        (let* ((param-name (symbol-name (nth 0 param)))
               (param-type (symbol-name (nth 1 param)))
               (param-desc (nth 2 param))
               (optional (member '&optional param)))
          (push `(:name ,param-name :type ,param-type :description ,param-desc
                        ,@(when optional '(:optional t)))
                param-list)))
      (setq param-list (nreverse param-list))

      ;; Generate the expansion
      `(progn
         ;; Define the function
         (defun ,name ,(mapcar #'car args)
           ,docstring
           ,@body)

         ;; Add to tool registry (this will be used when tools are registered)
         (push (gptel-make-tool :function #',name
                                :name ,tool-name
                                :description ,docstring
                                :args ,(if param-list `(list ,@param-list) nil)
                                :category ,category
                                ,@(when confirm `(:confirm ,confirm)))
               my-gptel-tool-registry)))))

;; Tool registry for collecting defined tools
(defvar my-gptel-tool-registry nil
  "Registry of tools defined with `defgptel-tool`.")

;; Clear registry on reload to prevent duplicates
;; This makes the configuration idempotent - multiple evaluations produce the same result
(setq my-gptel-tool-registry nil)

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

(defun my-gptel--truncate-bytes (s max-bytes)
  "Return S truncated to at most MAX-BYTES bytes, without cutting a multibyte char."
  (setq s (or s ""))
  (cond
   ((<= max-bytes 0) "")
   ((<= (string-bytes s) max-bytes) s)
   (t
    ;; Binary search for the largest character count whose byte size <= max-bytes
    (let ((lo 0)
          (hi (length s))
          (best 0))
      (while (<= lo hi)
        (let* ((mid (/ (+ lo hi) 2))
               (probe (substring s 0 mid)))
          (if (<= (string-bytes probe) max-bytes)
              (setq best mid
                    lo (1+ mid))
            (setq hi (1- mid)))))
      (substring s 0 best)))))

(defun my-gptel--line-count-in-string (s)
  "Count logical lines in S (0 if empty)."
  (setq s (or s ""))
  (if (string-empty-p s)
      0
    (1+ (cl-loop for i from 0 below (length s)
                 count (eq (aref s i) ?\n)))))

(defun my-gptel--emit-paged (full &optional label)
  "Emit a clamped page of FULL. If truncated, stash FULL and append
instructions to page the rest via `paged_read`."
  (let* ((full (or full ""))
         (max-lines (or my-gptel-max-lines 100))
         (max-bytes (or my-gptel-max-output-bytes 65536))
         ;; Keep empty lines; we want true line counts, not a whitespace collapse.
         (lines (split-string full "\n" nil))      ;; <— correct newline regex
         (head-lines (cl-subseq lines 0 (min max-lines (length lines))))
         (head (string-join head-lines "\n"))
         ;; Trim by BYTES without breaking multibyte chars.
         (trim (my-gptel--truncate-bytes head max-bytes))
         (truncated (or (> (length lines) max-lines)
                        (> (string-bytes head) max-bytes))))
    (if (not truncated)
        trim
      (let* ((buf (my-gptel--stash-in-temp-buffer full label))
             ;; Be resilient: handle a buffer object or a name.
             (bufname (if (bufferp buf) (buffer-name buf) buf))
             ;; Next page starts at the line *after* what we actually emitted.
             (trimmed-line-count (my-gptel--line-count-in-string trim))
             (next-start (1+ trimmed-line-count)))
        (format (concat "%s\n[OUTPUT TRUNCATED \u2192 %s]\n"
                        "Use tool `paged_read` with:\n"
                        "  file_or_buffer: \"%s\"\n"
                        "  start: %d\n"
                        "  line_count: %d\n"
                        "…to continue reading.")
                trim bufname bufname next-start max-lines)))))

;; -------- Dynamic Org response prefix (one level deeper than current) --------

(use-package gptel
  :straight (:repo "karthink/gptel" :branch "master" :files ("*.el"))
  :custom-face
  (gptel-user-header ((t (:foreground "#dca3a3" :weight bold))))
  (gptel-assistant-header ((t (:foreground "#7f9f7f" :weight bold))))
  (gptel-response ((t (:foreground "#9fc59f"))))
  :commands (gptel gptel-menu gptel-send gptel-request
                   my-gptel-switch-model my-gptel-setup-model-on-first-use)
  :hook ((gptel-mode . my-gptel-setup-behavior))
  :custom
  (gptel-track-response t)
  (gptel-log-level 'info)
  (gptel-use-curl t)
  (gptel-stream t)
  (gptel-use-header-line t)
  (gptel-prompt-prefix-alist '((org-mode . "** Human\n")))
  (gptel-response-prefix-alist '((org-mode . "** Assistant\n")))
  (gptel-model 'claude-haiku-4-5-20251001)
  (gptel-max-tokens 1000)
  (gptel-use-tools t)
  (gptel-auto-repair-invalid-state t)
  (gptel-include-tool-results t)
  (gptel-confirm-tool-calls t)
  (gptel-default-mode 'org-mode)
  (gptel-directives my-gptel-directives)
  (gptel-org-branching-context t)
  :init
  (defvar my-gptel--initial-setup t
    "Variable to determine whether to call `my-gptel-setup-model-on-first-use'.")

  ;; Prompts / directives
  (defvar my-gptel-prompt-tone
    "<tone>
 1. Be terse and to the point.  Speak directly.
 2. Explain your reasoning.
 3. Do NOT hedge or qualify.
 4. If you don't know, say you don't know.
 5. Do not offer unprompted advice or clarifications.
 6. Never apologize.
 7. Do NOT summarize your answers.
</tone>")


  (defvar my-gptel-prompt-context
    "<context>
 1. You are a LLM within emacs.
</context>")


  (defvar my-gptel-prompt-list
    '((rationalist . "* GPT Eigenprompt: LessWrong-Style Rationalist
This GPT is instantiated for epistemic rigor, clarity of reasoning, and intellectual honesty, modeled on the aesthetics and norms of the LessWrong community.

** Personality and Response Goals
1. Prioritize truth-seeking above social harmony. Do not mislead for politeness.
2. Defer to reductionism and transparency; recursively unpack abstractions.
3. Use probabilistic thinking. Where models disagree, state uncertainties.
4. Be comfortable admitting ignorance or confusion.
5. Encourage curiosity, calibration, and strong discourse norms.

** Linguistic Style
- Use precise, plain language. Minimize jargon unless it compresses concepts effectively.
- Numbered lists are preferred for unpacking complex ideas.
- Occasionally include LessWrong idioms (e.g. “beliefs should pay rent in anticipated experience”).
- Prefer analogies that expose the gears of a model rather than just the vibe.

** Epistemics and Meta
- Explicitly separate normative (moral) from descriptive (factual) statements.
- Model arguments from an inside-view before passing judgment.
- Maintain meta-awareness of cognitive biases, including your own.
- Avoid strawmanning. Steelman accurately, but not beyond plausibility.
- Reference decision theory, even if only marginally relevant.
")
      (machine . "You are a LLM inside Emacs. Assist the user in what they wish.
There is Nix configuring the home environment in =~/dotfiles/nix/arch/flake.nix=

* ~/dotfiles -> ~/src/github.com/nathantypanski/dotfiles/
* ~/.emacs.d -> ~/src/github.com/nathantypanski/dotfiles/emacs.d

Ensure you are in the correct =project= before running any code.")

      (Inline . "You are a LLM inside Emacs. Insert literal replies; use code comments for commentary (e.g., ;; for elisp). No Markdown/Org formatting.")

      (Elisp . "You are a LLM inside Emacs. Help with Emacs Lisp config (Emacs
30.1.90, use-package + straight). Write in an *idiomatic* and *modern*
emacs style, avoiding clunkiness like =cl-lib= and instead use the
recent builtin functions.

Note you have a number of tools available to you. Use them as needed, but take care to use the size-limiting functionality in calls so the output size is not too large.")

      (Emacs . "You can introspect Emacs (buffers, eval). Be concise; tool output is costly. Prefer minimal I/O.")))

  (defun my-gptel-prompt-reload-directives ()
    "Build my-gptel-directives from my-gptel-prompts with tone appended."
    (interactive)
    (setq my-gptel-prompt-directives
          (mapcar (lambda (entry)
                    (cons (car entry) (concat (cdr entry) "\n\n" my-gptel-prompt-tone "\n\n" my-gptel-prompt-context)))
                  my-gptel-prompt-list))
    (setq gptel-directives my-gptel-prompt-directives)
    (message "Reloaded gptel directives"))

  (my-gptel-prompt-reload-directives)

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
    '(("Claude Haiku"  . (claude-sonnet-4-20250514 anthropic))
      ("Claude Sonnet" . (claude-sonnet-4-20250514 anthropic))
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
                (backend (alist-get "ChatGPT" gptel--known-backends nil nil 'equal)))
           (setq gptel-model   model
                 gptel-backend backend
                 gptel-api-key key))))
      (message "Switched to %s" choice)))

  (defun my-gptel-setup-model-on-first-use (name &optional _ initial interactivep)
    "Prompt for model selection if none is set up properly."
    (when (and (called-interactively-p 'any)
               (or my-gptel--initial-setup
                   (or (not gptel-backend)
                       (not gptel-api-key)))
               (y-or-n-p "Choose model for this session? "))
      (setq my-gptel--initial-setup nil)
      (my-gptel-switch-model))
    nil)

  ;; Add advice to prompt for model selection on first use
  (advice-add 'gptel :before #'my-gptel-setup-model-on-first-use)
  (advice-add 'gptel-send :before #'my-gptel-setup-model-on-first-use)

  ;; Dynamic org heading prefixes that respect current context
  (defun my-gptel-dynamic-org-response-prefix (orig-fun)
    "Generate org response prefix that matches current heading level."
    (if (derived-mode-p 'org-mode)
        (save-excursion
          (let* ((level (or (org-outline-level) 0)))
            (concat (make-string (1+ level) ?*) " Assistant\n")))
      (funcall orig-fun)))

  (defun my-gptel-dynamic-org-user-prefix (orig-fun)
    "Generate org user prefix that matches current heading level."
    (if (derived-mode-p 'org-mode)
        (save-excursion
          (let* ((level (or (org-outline-level) 0)))
            (concat (make-string (1+ level) ?*) " Human\n")))
      (funcall orig-fun)))

  (advice-add 'gptel-response-prefix-string :around #'my-gptel-dynamic-org-response-prefix)
  (advice-add 'gptel-prompt-prefix-string :around #'my-gptel-dynamic-org-user-prefix)

  ;; Buffer behavior
  (defun my-gptel-setup-behavior ()
    (visual-line-mode 1)
    (setq-local auto-save-timeout 60
                auto-save-visited-mode nil))

(defun my-gptel--get-project (&optional project-dir)
  "Get project info for PROJECT-DIR (default: current directory).
Returns (project-root . project-instance) or nil if no project."
  (when (fboundp 'project-current)
    (let* ((default-directory (or project-dir default-directory))
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

  ;;;; Tool discovery and help
  (defun my-gptel-context-status ()
    "Get current Emacs context (buffer, project, git) without changing anything."
    (let* ((buf (current-buffer))
           (buf-name (buffer-name buf))
           (file (buffer-file-name buf))
           (proj-root (my-gptel--project-root))
           (git-root (and default-directory (vc-git-root default-directory)))
           (git-branch (and git-root
                            (let ((default-directory git-root))
                              (string-trim (shell-command-to-string "git branch --show-current 2>/dev/null"))))))
      (format "=== Current Context ===
Buffer: %s [%s]
File: %s
Directory: %s
Project: %s
Git repo: %s
Git branch: %s
Point: %d (line %d)
Modified: %s"
              buf-name major-mode
              (or file "not visiting a file")
              default-directory
              (or proj-root "no project")
              (or git-root "not in git repo")
              (or git-branch "N/A")
              (point)
              (line-number-at-pos)
              (if (buffer-modified-p) "yes" "no"))))

  (defun my-gptel-tools-help (&optional category)
    "List available tools, optionally filtered by CATEGORY.
Categories: file, edit, search, emacs, project, git"
    (let* ((tools (or gptel-tools '()))
           (filtered (if category
                         (seq-filter (lambda (tool)
                                       (equal (plist-get tool :category) category))
                                     tools)
                       tools))
           (grouped (seq-group-by (lambda (tool)
                                    (or (plist-get tool :category) "general"))
                                  filtered)))
      (mapconcat
       (lambda (group)
         (let ((cat (car group))
               (tools (cdr group)))
           (format "=== %s ===\n%s"
                   (upcase cat)
                   (mapconcat
                    (lambda (tool)
                      (format "• %s: %s%s"
                              (plist-get tool :name)
                              (plist-get tool :description)
                              (if (plist-get tool :confirm) " [requires confirmation]" "")))
                    tools "\n"))))
       grouped "\n\n")))

  (defun my-gptel-tool-help (tool-name)
    "Get detailed help for a specific TOOL-NAME."
    (if-let* ((tool (seq-find (lambda (t) (equal (plist-get t :name) tool-name))
                               gptel-tools))
              (args (plist-get tool :args)))
        (format "%s\n\nDescription: %s\n\nParameters:\n%s\n\nCategory: %s%s"
                tool-name
                (plist-get tool :description)
                (mapconcat
                 (lambda (arg)
                   (format "  - %s (%s): %s%s"
                           (plist-get arg :name)
                           (plist-get arg :type)
                           (plist-get arg :description)
                           (if (plist-get arg :optional) " [optional]" "")))
                 args "\n")
                (plist-get tool :category)
                (if (plist-get tool :confirm) "\n⚠ Requires confirmation" ""))
      (format "Tool '%s' not found. Use `tools_help` to list available tools." tool-name)))

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

  ;; Example of new macro-based definition (same function, different approach):
  ;; (defgptel-tool my-gptel-symbol-exists-v2 ((name string "Symbol name to check"))
  ;;   "Check if a symbol exists in Emacs"
  ;;   :category "emacs"
  ;;   (if (intern-soft name) "exists" "nil"))

  ;; Macro demonstration removed - will implement properly when needed

  (defun my-gptel-function-completions (prefix)
    "Return functions matching PREFIX."
    (let ((matches '()))
      (mapatoms (lambda (sym)
                  (when (and (fboundp sym)
                             (string-match-p (regexp-quote prefix) (symbol-name sym)))
                    (push (symbol-name sym) matches))))
      (my-gptel--emit-paged (string-join (sort matches #'string<) "\n") "functions")))

  (defun my-gptel-variable-completions (prefix)
    "Return variables matching PREFIX."
    (let ((matches '()))
      (mapatoms (lambda (sym)
                  (when (and (boundp sym)
                             (string-match-p (regexp-quote prefix) (symbol-name sym)))
                    (push (symbol-name sym) matches))))
      (my-gptel--emit-paged (string-join (sort matches #'string<) "\n") "variables")))

  (defun my-gptel-helpful-function (symbol &optional brief)
    "Get function help. If BRIEF, just show signature."
    (when-let ((sym (intern-soft symbol)))
      (cond
       ((not (fboundp sym))
        (format "Symbol '%s' is not a function" symbol))
       (brief
        (let ((args (help-function-arglist sym)))
          (format "%s: %s\n%s"
                  symbol
                  (or (documentation sym t) "No documentation")
                  (if args (format "Args: %s" args) "No args"))))
       (t
        (save-window-excursion
          (helpful-function sym)
          (with-current-buffer (helpful--buffer sym t)
            (buffer-string)))))))

  (defun my-gptel-helpful-variable (symbol)
    "Get detailed variable help using helpful package."
    (when-let ((sym (intern-soft symbol)))
      (if (boundp sym)
          (save-window-excursion
            (helpful-variable sym)
            (with-current-buffer (helpful--buffer sym nil)
              (buffer-string)))
        (format "Symbol '%s' is not a variable" symbol))))

  (defun my-gptel-helpful-symbol (symbol)
    "Get comprehensive help for SYMBOL using helpful package."
    (when-let ((sym (intern-soft symbol)))
      (save-window-excursion
        (helpful-symbol sym)
        (with-current-buffer (helpful--buffer sym nil)
          (buffer-string)))))

  (defun my-gptel-features ()
    "List loaded Emacs features."
    (my-gptel--emit-paged (string-join (mapcar #'symbol-name (sort features #'string<)) "\n") "features"))

  (defun my-gptel-featurep (feature)
    "Check if FEATURE is loaded."
    (if (featurep (intern feature))
        "loaded"
      "not-loaded"))

  (defun my-gptel-info-manuals ()
    "List available Info manuals."
    (require 'info)
    (let ((manuals (info--manual-names nil)))
      (my-gptel--emit-paged (string-join (sort (info--filter-manual-names manuals) #'string<) "\n") "info-manuals")))

  (defun my-gptel-info-read (manual node)
    "Read contents of NODE in MANUAL using Info system."
    (require 'info)
    (condition-case err
        (save-window-excursion
          (info (format "(%s)%s" manual node))
          (with-current-buffer "*info*"
            (let ((content (buffer-substring-no-properties (point-min) (point-max))))
              (my-gptel--emit-paged content (format "info:%s-%s" manual node)))))
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
           (default-directory dir)
           (git-root (vc-git-root dir)))
      (if git-root
          (let* ((cmd (if path
                          (format "git status --porcelain -- %s" (shell-quote-argument path))
                        "git status --porcelain"))
                 (output (shell-command-to-string cmd))
                 (branch (string-trim (shell-command-to-string "git branch --show-current"))))
            (my-gptel--emit-paged
             (format "Repository: %s\nBranch: %s\n\n%s"
                     git-root branch output)
             "git-status"))
        (format "Not in a git repository (checked: %s)" dir))))

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
  (defun my-gptel-switch-buffer (buffer-name &optional no-switch)
    "Switch to BUFFER-NAME, or just return info if NO-SWITCH is true."
    (if-let ((buf (get-buffer buffer-name)))
        (if no-switch
            (format "Buffer exists: %s [%s, %d lines]"
                    buffer-name
                    (with-current-buffer buf major-mode)
                    (with-current-buffer buf (count-lines (point-min) (point-max))))
          (progn (switch-to-buffer buf) (format "Switched to buffer: %s" buffer-name)))
      (format "Buffer not found: %s" buffer-name)))

  (defun my-gptel-list-buffers ()
    "List non-internal buffers with mode."
    (let* ((bufs (seq-filter (lambda (b) (not (string-prefix-p " " (buffer-name b))))
                             (buffer-list)))
           (output (mapconcat
                    (lambda (b)
                      (with-current-buffer b (format "%s [%s]" (buffer-name) major-mode)))
                    bufs "\n")))
      (my-gptel--emit-paged output "buffers")))

  (defun my-gptel-project-files (&optional pattern project-dir)
    "List project files, optionally filtered by PATTERN.
PROJECT-DIR specifies which project (default: current directory)."
    (if-let* ((project-info (my-gptel--get-project project-dir))
              (proj (cdr project-info))
              (files (project-files proj)))
        (let ((filtered-files (if pattern
                                  (seq-filter (lambda (f) (string-match-p pattern f)) files)
                                files)))
          (my-gptel--emit-paged (string-join filtered-files "\n") "project-files"))
      (format "Not in a project (checked: %s)" (or project-dir default-directory))))

  (defun my-gptel-grep-project (pattern &optional glob-pattern project-dir)
    "Search using ripgrep. Automatically respects .gitignore.
GLOB-PATTERN: optional file glob like '*.el' or '*.js'
PROJECT-DIR: which project (default: current directory)"
    (if-let* ((project-root (my-gptel--project-root project-dir))
              (default-directory project-root))
        (let* ((cmd (concat "rg --line-number --no-heading --color=never"
                            (if glob-pattern
                                (format " --glob '%s'" glob-pattern)
                              "")
                            (format " %s" (shell-quote-argument pattern))))
               (out (shell-command-to-string cmd)))
          (if (string-empty-p out)
              (format "No matches found for '%s'" pattern)
            (my-gptel--emit-paged out "grep")))
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
               (validation-error (my-gptel--validate-file-path p)))
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

  (defun my-gptel--validate-file-path (path &rest options)
    "Validate PATH for file operations. Return error string or nil if valid.
OPTIONS is a plist supporting :allow-directory and :allow-missing."
    (let ((allow-directory (plist-get options :allow-directory))
          (allow-missing (plist-get options :allow-missing))
          (p (expand-file-name path)))
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
      (seq-filter (lambda (f)
                    (string= (file-name-nondirectory f) basename))
                  (mapcar (lambda (f) (expand-file-name f root)) files))))

  (defun my-gptel-open-file (path &optional preview)
    "Open existing file by absolute or relative path, optionally as preview only."
    (let ((full-path (expand-file-name path)))
      (if (file-exists-p full-path)
          (if preview
              ;; Preview mode: just show first 50 lines without opening buffer
              (with-temp-buffer
                (insert-file-contents full-path nil 0 8192)
                (my-gptel--emit-paged (buffer-string) (format "preview:%s" (file-name-nondirectory full-path))))
            ;; Normal mode: open buffer
            (let ((buffer (find-file-noselect full-path)))
              (format "Opened file: %s\nBuffer name: %s\nUse `paged_read` to read contents" full-path (buffer-name buffer))))
        (format "File does not exist: %s" full-path))))

  (defun my-gptel-create-file-at-path (path content)
    "Create new file at specific path with content."
    (let ((full-path (expand-file-name path)))
      (make-directory (file-name-directory full-path) t)
      (my-gptel--safe-file-create full-path content)
      (let ((buffer (find-file-noselect full-path)))
        (format "Created file: %s\nBuffer name: %s" full-path (buffer-name buffer)))))

  (defun my-gptel-make-directory (parent name &optional dry-run)
    "Create directory NAME under PARENT. If DRY-RUN, only show what would be created."
    (let ((full-path (expand-file-name name parent)))
      (cond
       (dry-run
        (format "[DRY RUN] Would create: %s" full-path))
       ((file-exists-p full-path)
        (format "Directory already exists: %s" full-path))
       (t
        (condition-case err
            (progn
              (make-directory full-path t)
              (format "Created directory: %s" full-path))
          (error (format "Error creating directory: %s" (error-message-string err))))))))

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
      (shell-command-to-string
       (format "wc -l -w -c %s" (shell-quote-argument file-path))))))

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
           (paginated-content (my-gptel--paginate-lines content start line-count)))
      ;; Use emit-paged to handle truncation and continuation instructions
      (my-gptel--emit-paged paginated-content file-or-buffer)))

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
      (forward-line (1- start-line))                  ; start of start-line
      (let ((start-pos (point)))
        (forward-line (1+ (- end-line start-line)))   ; start of line after end-line
        (let ((old-content (buffer-substring start-pos (point))))
          (delete-region start-pos (point))
          (insert new-content)
          (unless (string-suffix-p "\n" new-content)  ; << was "\\n"
            (insert "\n"))
          (format "Replaced lines %d-%d:\nOLD:\n%s\nNEW:\n%s"
                  start-line end-line old-content new-content))))))

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
          (append
           (list
           ;; Tool discovery (add first for visibility)
           (gptel-make-tool
            :function #'my-gptel-context-status
            :name "context"
            :description "Show current context (buffer, project, git) - safe, read-only"
            :args nil
            :category "help")

           (gptel-make-tool
            :function #'my-gptel-tools-help
            :name "tools_help"
            :description "List available tools by category (file/edit/search/emacs/project/git)"
            :args (list '(:name "category" :type "string" :optional t :description "Filter by category"))
            :category "help")

           (gptel-make-tool
            :function #'my-gptel-tool-help
            :name "tool_help"
            :description "Get detailed help for a specific tool"
            :args (list '(:name "tool_name" :type "string" :description "Name of the tool"))
            :category "help")

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
                            :args (list '(:name "prefix" :type "string" :description "Function name prefix to match"))
                            :category "emacs")
           (gptel-make-tool :function #'my-gptel-variable-completions :name "variable_completions"
                            :description "Find variables matching a prefix pattern"
                            :args (list '(:name "prefix" :type "string" :description "Variable name prefix to match"))
                            :category "emacs")
           (gptel-make-tool :function #'my-gptel-helpful-function :name "help_function"
                            :description "Get help for a function (use brief=true for just signature)"
                            :args (list '(:name "symbol" :type "string" :description "Function name")
                                        '(:name "brief" :type "boolean" :optional t :description "Just show signature, not full help"))
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
                            :description "Switch to buffer by name (or check if exists with no_switch)"
                            :args (list '(:name "buffer_name" :type "string" :description "Buffer name")
                                        '(:name "no_switch" :type "boolean" :optional t :description "Just check if exists, don't switch"))
                            :category "emacs")
           (gptel-make-tool :function #'my-gptel-list-buffers  :name "list_buffers"
                            :description "List open buffers" :args nil :category "emacs")
           (gptel-make-tool :function #'my-gptel-project-files :name "project_files"
                            :description "List project files, optional filter"
                            :args (list '(:name "pattern" :type "string" :optional t
                                                :description "Regex to filter")
                                        '(:name "project_dir" :type "string" :optional t
                                                :description "Project directory (default: current)"))
                            :category "project")
           (gptel-make-tool :function #'my-gptel-open-file :name "open_file"
                            :description "Open existing file (preview mode available for safe exploration)"
                            :args (list '(:name "path" :type "string" :description "File path (absolute or relative)")
                                        '(:name "preview" :type "boolean" :optional t :description "Preview first 50 lines without opening buffer"))
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
                            :description "Fast project search with ripgrep (respects .gitignore)"
                            :args (list '(:name "pattern" :type "string" :description "Search pattern or regex")
                                        '(:name "glob_pattern" :type "string" :optional t
                                                :description "File glob like '*.el' or '*.js'")
                                        '(:name "project_dir" :type "string" :optional t
                                                :description "Project directory (default: current)"))
                            :category "search")
           (gptel-make-tool :function #'my-gptel-paged-read :name "paged_read"
                            :description "Read `line_count' lines from buffer or file (buffer-first auto-detect)"
                            :args (list '(:name "file_or_buffer" :type "string" :description "Buffer name or file path")
                                        '(:name "start" :type "number" :optional t :description "1-based start line (default: 1)")
                                        '(:name "line_count" :type "number" :optional t :description "Lines to read (default: 200)"))
                            :category "emacs")
           ;; Simple file creation / mkdir (kept; confirm writes)
           (gptel-make-tool :function #'my-gptel-make-directory :name "make_directory"
                            :description "Create directory NAME under PARENT (supports dry-run)"
                            :args (list '(:name "parent" :type "string" :description "Parent directory")
                                        '(:name "name"   :type "string" :description "New directory name")
                                        '(:name "dry_run" :type "boolean" :optional t :description "Show what would be created without doing it"))
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
                        '(:name "content"     :type "string" :description "Text to insert"))))

           ;; Add tools defined with defgptel-tool macro
           (reverse my-gptel-tool-registry)))

    ;; Reload gptel transient to pick up updated tool descriptions
    (when (fboundp 'gptel-menu--update-transient)
      (gptel-menu--update-transient)))

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
                  (let ((end
                         (save-excursion
                           (re-search-forward "^:END:" nil t))))
                    (and end (re-search-forward "^:GPTEL_" end t))))
         (gptel-mode 1)))))

 (defun my-gptel-clean-completion ()
   "Tone down completion noise in gptel buffers."
   (interactive)
   (when (bound-and-true-p corfu-mode)
     (corfu-mode -1))
   (setq-local completion-at-point-functions
               (seq-remove
                (lambda (fn)
                  (memq
                   fn
                   '(ispell-completion-at-point
                     org-completion-at-point
                     pcomplete-completions-at-point
                     comint-completion-at-point)))
                completion-at-point-functions)))

  (add-hook 'find-file-hook #'my-auto-enable-gptel-mode)
  (add-hook 'gptel-mode-hook #'my-gptel-clean-completion))


(use-package ai-org-chat
  :straight (:type git :host github :repo "ultronozm/ai-org-chat.el")
  :custom
  (ai-org-chat-user-name "ndt")
  (ai-org-chat-dir "~/notes/gpt")  ; Directory for saving chat files
  :config)

(provide 'my-gpt)
;;; my-gpt.el ends here
