;; my-gpt.el -*- lexical-binding: t; -*-
;;
;; Streamlined LLM configuration with gptel and claude-agent integration

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
  ;; 1 char wider than previous
  (claude-code-ide-window-width 81)
  :config
  (claude-code-ide-emacs-tools-setup)

  ;; Add helpful message for ediff sessions
  (defun my-claude-ediff-help-message ()
    "Show helpful message for navigating Claude Code ediff sessions."
    (run-with-timer 0.5 nil
                    (lambda ()
                      (message "Ediff: [n]ext/[p]rev diff | [a]ccept original | [b]accept Claude's | [q]uit | [?]help"))))

  (add-hook 'ediff-startup-hook 'my-claude-ediff-help-message)

  ;; Terminal-friendly keybindings using general
  (after 'general
    (general-define-key
     :prefix "C-c"
     "C-j" 'claude-code-ide-insert-newline   ; Alternative to S-return
     "C-e" 'claude-code-ide-send-escape      ; Alternative if C-escape fails
     "C-t" 'claude-code-ide-toggle           ; Quick toggle
     "C-p" 'claude-code-ide-send-prompt))    ; Send prompt directly

  ;; Fix window configuration after ediff
  (defun my-claude-fix-windows-after-ediff ()
    "Fix ultra-wide side window after ediff closes."
    (when claude-code-ide-use-side-window
      ;; Delete existing side windows and recreate properly
      (dolist (window (window-list))
        (when (window-parameter window 'window-side)
          (delete-window window)))
      ;; Find claude buffer and redisplay it properly
      (when-let* ((claude-buffer-name (claude-code-ide--get-buffer-name))
                  (claude-buffer (get-buffer claude-buffer-name))
                  (claude-window (get-buffer-window claude-buffer)))
        (when claude-window
          (delete-window claude-window))
        (claude-code-ide--display-buffer-in-side-window claude-buffer))))

  (add-hook 'ediff-cleanup-hook 'my-claude-fix-windows-after-ediff))

(use-package mcp
  :after gptel
  :straight (:type git :host github :repo "lizqwerscott/mcp.el")
  :ensure
  :config
  (setq mcp-hub-servers
        '(("memory" :command "mcp-server-memory"))))

(use-package gptel
  :straight (:repo "karthink/gptel" :branch "master" :files ("*.el"))
  :custom-face
  (gptel-user-header ((t (:foreground "#dca3a3" :weight bold))))
  (gptel-assistant-header ((t (:foreground "#7f9f7f" :weight bold))))
  (gptel-response ((t (:foreground "#9fc59f"))))
  :commands (gptel gptel-menu gptel-send gptel-request my-gptel-review my-gptel-explain my-gptel-switch-model)
  :hook ((gptel-mode . my-gptel-setup-behavior))
  :custom
  (gptel-track-response t)
  (gptel-log-level 'debug)
  (gptel-use-curl t)
  (gptel-stream t)
  (gptel-use-header-line t)
  (gptel-prompt-prefix-alist '((org-mode . "** Human\n")))
  (gptel-response-prefix-alist '((org-mode . "** Assistant\n")))
  (gptel-model 'claude-sonnet-4-20250514)
  (gptel-max-tokens 3000)
  (gptel-use-tools t)
  ;; (gptel-enable-enhanced-state-tracking t)
  (gptel-auto-repair-invalid-state t)
  (gptel-include-tool-results t)
  (gptel-confirm-tool-calls 'auto)
  (gptel-default-mode 'org-mode)
  (gptel-directives my-gptel-directives)
  (gptel-org-branching-context t)
  :init
  ;; Core prompts
  (defvar my-gptel-system-prompt
    "You are a LLM running inside Emacs. Your responses are inserted literally into the buffer where the prompt is sent - usually code in the language being discussed. Do not use markdown or org to structure your comments. Instead, structure in alignment with the surrounding text. Put your commentary in comments (e.g., `;;` for elisp, `//` for go, ...).")

  (defvar my-gptel-elisp-prompt
    "You are a LLM running inside Emacs. Help me work on my elisp config. I am using emacs 30.1.90 and (use-package) with straight. Respond in org-mode. I can share any buffers or config files you request in the context.")

  (defvar my-emacs-system-prompt
    "You are a large language model living in Emacs and with introspection powers into the editor, like the ability to list/open/read buffers and run arbitrary elisp code.

Running tools which consume or return large output can result in extremely high costs. Output quantity is *especially* costly: generally output costs at least 3x per token than input. So be consice and prefer tool calls which allow you to send the minimum quantity of data to achieve your desired outcome.")

  (defvar my-gptel-directives
    (list (cons 'Inline my-gptel-system-prompt)
          (cons 'Elisp my-gptel-elisp-prompt)
          (cons 'Emacs my-emacs-system-prompt)))

  ;; Enhanced tool confirmation function
  (defun my-gptel-detailed-tool-confirmation (tool-spec args)
    "Show detailed confirmation dialog for tool calls with full argument details."
    (let* ((tool-name (plist-get tool-spec :name))
           (tool-desc (plist-get tool-spec :description))
           (arg-details (mapconcat
                         (lambda (arg)
                           (format "  %s: %s"
                                   (plist-get arg :name)
                                   (or (plist-get args (intern (concat ":" (plist-get arg :name))))
                                       "<not provided>")))
                         (plist-get tool-spec :input_schema)
                         "\n"))
           (confirmation-msg (format "Execute Tool: %s\n\nDescription: %s\n\nArguments:\n%s\n\nProceed?"
                                     tool-name tool-desc arg-details)))
      (yes-or-no-p confirmation-msg)))

  :config
  ;; Basic setup
  (when (getenv "OPENAI_API_KEY")
    (setq gptel-api-key (getenv "OPENAI_API_KEY")))

  ;; lets us use `-n' to track number of responses, `-T' to set
  ;; temperature, etc.
  (setq gptel-expert-commands t)

  ;; Simple model switcher
  (defvar my-gptel-models
    '(("Claude Sonnet" . (claude-sonnet-4-20250514 anthropic))
      ("Claude Opus" . (claude-opus-4-20250514 anthropic))
      ("GPT-4o" . (gpt-4o openai))
      ("GPT-4o Mini" . (gpt-4o-mini openai))))

  (defun my-gptel-switch-model ()
    "Switch gptel model and backend."
    (interactive)
    (let* ((choice (completing-read "Model: " (mapcar #'car my-gptel-models)))
           (model-info (cdr (assoc choice my-gptel-models)))
           (model (car model-info))
           (provider (cadr model-info)))
      (pcase provider
        ('anthropic
         (require 'gptel-anthropic)
         (let ((key (or (getenv "ANTHROPIC_API_KEY")
                        (read-passwd "Anthropic API Key: "))))
           (setq gptel-model model
                 gptel-backend (gptel-make-anthropic "Claude" :key key :stream t)
                 gptel-api-key key)))
        ('openai
         (let* ((key (or (getenv "OPENAI_API_KEY")
                         (read-passwd "OpenAI API Key: ")))
                (backend (cdr (assoc "ChatGPT" gptel--known-backends))))
           (setq gptel-model model
                 gptel-backend backend
                 gptel-api-key key))))
      (message "Switched to %s" choice)))

  ;; Buffer behavior
  (defun my-gptel-setup-behavior ()
    "Setup gptel buffer behavior."
    (visual-line-mode 1)
    (setq-local auto-save-timeout 60)
    (setq-local auto-save-visited-mode nil))


  ;; Core commands
  (defun my-gptel-explain ()
    "Explain current region or function."
    (interactive)
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (thing-at-point 'defun t))))
      (if text
          (gptel-request (format "Explain this code:\n\n%s" text))
        (message "No code to explain"))))

  (defun my-gptel-review ()
    "Review current region or buffer."
    (interactive)
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (buffer-string))))
      (gptel-request (format "Review this code for bugs and improvements:\n\n%s" text))))

  (defun my-gptel-add-project-context ()
    "Add relevant project files as context to gptel."
    (interactive)
    (if-let ((files (project-files (project-current))))
        (progn
          (dolist (file (completing-read-multiple "Add files: " files))
            (gptel-add-file file))
          (message "Added files to context"))
      (message "No project found")))

  ;; Clean tool implementations for gptel
  (defvar my-gptel-blocked-paths
    '("/etc/" "/usr/" "/bin/" "/sbin/" "/root/")
    "System paths where AI tools are NOT allowed to operate.")

  (defvar my-gptel-blocked-patterns
    '("\\.ssh/" "\\.gnupg/" "\\.aws/" "\\.kube/" "\\.docker/"
      "\\.password-store/" "\\.authinfo" "\\.netrc")
    "Patterns for sensitive directories/files to block.")

  (defun my-gptel-path-allowed-p (path)
    "Check if PATH is safe to access - block system and sensitive directories."
    (let ((expanded (expand-file-name path)))
      ;; Block if path contains sensitive patterns or is in blocked directories
      (not (or
            ;; Check for sensitive file names
            (string-match-p "\\(id_rsa\\|id_ed25519\\|private.*key\\|\\.pem\\|\\.key\\|wallet\\|\\.gpg\\)" expanded)
            ;; Check for system directories
            (cl-some (lambda (blocked)
                       (string-prefix-p blocked expanded))
                     my-gptel-blocked-paths)
            ;; Check for sensitive directories anywhere in path
            (cl-some (lambda (pattern)
                       (string-match-p pattern expanded))
                     my-gptel-blocked-patterns)))))

  ;; Simple read file tool
  (defun my-gptel-tool-read-file (path)
    "Read file with size limit (max 20KB)."
    (let* ((expanded-path (expand-file-name path))
           (max-size (* 20 1024))) ; 20KB limit for cost efficiency
      (cond
       ((not (my-gptel-path-allowed-p expanded-path))
        (format "Error: Path '%s' is outside allowed directories" path))
       ((not (file-exists-p expanded-path))
        (format "Error: File '%s' does not exist" path))
       ((file-directory-p expanded-path)
        (format "Error: '%s' is a directory, not a file" path))
       ((> (file-attribute-size (file-attributes expanded-path)) max-size)
        "Error: File too large (>20KB). Use head/tail or grep for specific sections")
       (t
        (with-temp-buffer
          (insert-file-contents expanded-path)
          (buffer-string))))))

  ;; Simple list files tool
  (defun my-gptel-tool-list-files (path)
    "List files in directory at PATH with safety checks."
    (let ((expanded-path (expand-file-name (or path "."))))
      (cond
       ((not (my-gptel-path-allowed-p expanded-path))
        (format "Error: Path '%s' is outside allowed directories" path))
       ((not (file-exists-p expanded-path))
        (format "Error: Directory '%s' does not exist" path))
       ((not (file-directory-p expanded-path))
        (format "Error: '%s' is not a directory" path))
       (t
        (mapconcat #'identity
                   (directory-files expanded-path nil "^[^.]" t)
                   "\n")))))

  ;; Simple bash tool with restrictions
  (defun my-gptel-tool-bash (command)
    "Execute safe bash COMMAND with restrictions."
    (cond
     ;; Block dangerous commands
     ((string-match-p "\\(sudo\\|rm -rf\\|dd if=\\|mkfs\\|\\bsudo\\b\\)" command)
      (format "Error: Dangerous command blocked: %s" command))
     ;; Allow pipes and redirects but warn about rm
     ((string-match-p "\\brm\\b" command)
      (if (yes-or-no-p (format "Execute command with 'rm'? %s" command))
          (shell-command-to-string command)
        "Command cancelled by user"))
     (t
      (let ((output (shell-command-to-string command)))
        (if (string-empty-p output)
            "(Command completed with no output)"
          output)))))

  ;; Add buffer/frame introspection functions
  (defun my-gptel-get-buffer-info ()
    "Get info about current buffer for gptel"
    (list :buffer-name (buffer-name)
          :major-mode major-mode
          :point (point)
          :mark (when (mark t) (mark t))
          :buffer-size (buffer-size)
          :modified-p (buffer-modified-p)))

  (defun my-gptel-get-frame-info ()
    "Get frame and window layout info"
    (list :frames (length (frame-list))
          :windows (length (window-list))
          :current-window-edges (when (selected-window)
                                  (window-edges (selected-window)))
          :current-window-buffer (when (selected-window)
                                   (buffer-name (window-buffer (selected-window))))
          :frame-width (frame-width)
          :frame-height (frame-height)))

  (defun my-gptel-eval-elisp-safely (code)
    "Safely evaluate elisp code for gptel"
    (condition-case err
        (eval (read code))
      (error (format "Error: %s" err))))

  (defun my-gptel-add-newline-after-tools (start end)
    "Add newline after tool call responses"
    (save-excursion
      (goto-char start)  ; Use the provided start position
      (while (re-search-forward "</invoke>" end t)  ; Search within the response region
        (unless (looking-at "\n")
          (insert "\n")))))

  ;; Tool implementations
  (defun my-git-status-path (path &optional extra-args)
    "Get git status for PATH with optional EXTRA-ARGS."
    (let* ((actual-path (or path default-directory))
           (root (vc-git-root actual-path)))
      (when root
        (shell-command-to-string
         (mapconcat 'identity
                    (list "git" "status" "--porcelain" actual-path)
                    " ")))))
  (defun my-gptel-git-status (path)
    "Get git repository status for `path'."

    (let ((cmd "git status --porcelain"))
      ;; (pcase path
      ;;   (nil
      ;;    (if (vc-git-root path)
      ;;        (shell-command-to-string (concat cmd " " path))
      ;;      ))
      (if (vc-git-root default-directory)
          (shell-command-to-string (concat cmd " " path))
        "Not in a git repository")))

  (defun my-gptel-git-diff (&optional file)
    "Show git diff, optionally for specific file."
    (if (vc-git-root default-directory)
        (shell-command-to-string
         (if file
             (format "git diff %s" (shell-quote-argument file))
           "git diff"))
      "Not in a git repository"))

  (defun my-gptel-switch-buffer (buffer-name)
    "Switch to buffer by name."
    (if-let ((buf (get-buffer buffer-name)))
        (progn
          (switch-to-buffer buf)
          (format "Switched to buffer: %s" buffer-name))
      (format "Buffer not found: %s" buffer-name)))

  (defun my-gptel-list-buffers ()
    "List buffers (max 20, exclude internal buffers)."
    (interactive)
    (let ((buffers (cl-remove-if
                    (lambda (buf) (string-prefix-p " " (buffer-name buf)))
                    (buffer-list))))
      (when (> (length buffers) 20)
        (setq buffers (cl-subseq buffers 0 20)))
      (mapconcat
       (lambda (buf)
         (with-current-buffer buf
           (format "%s [%s]" (buffer-name) major-mode)))
       buffers
       "\n")))

  (defun my-gptel-project-files (&optional pattern)
    "List files in current project, optionally filtered by pattern. Limited to 50 files."
    (if-let ((project (project-current)))
        (let ((files (project-files project)))
          (when (> (length files) 50)
            (setq files (cl-subseq files 0 50)))
          (if pattern
              (cl-remove-if-not
               (lambda (file) (string-match-p pattern file))
               files)
            files))
      '("Not in a project")))

  (defun my-gptel-grep-project (pattern &optional file-pattern)
    "Search for pattern in project files (max 30 lines)."
    (if-let ((project (project-current)))
        (let ((default-directory (project-root project))
              (output (shell-command-to-string
                       (format "grep -r --include='%s' '%s' . | head -30"
                               (or file-pattern "*") pattern))))
          (if (> (length output) 5000)
              (concat (substring output 0 5000) "\n... (output truncated)")
            output))
      "Not in a project"))


  (defun my-gptel-parse-diff-hunks (diff-content)
    "Parse unified diff into executable hunks."
    (let ((hunks '())
          (lines (split-string diff-content "\\n"))
          (current-hunk nil))
      (dolist (line lines)
        (cond
         ;; Start of new hunk: @@ -old_start,old_count +new_start,new_count @@
         ((string-match "^@@[ \\t]+\\\\-\\\\([0-9]+\\\\)\\\\(?:,\\\\([0-9]+\\\\)\\\\)?[ \\t]+\\\\+\\\\([0-9]+\\\\)\\\\(?:,\\\\([0-9]+\\\\)\\\\)?" line)
          (when current-hunk
            (push current-hunk hunks))
          (setq current-hunk
                (list :start-line (string-to-number (match-string 3 line))
                      :old-lines '()
                      :new-lines '())))

         ;; Line to be removed (starts with -)
         ((and current-hunk (string-match "^\\\\-\\\\(.*\\\\)" line))
          (push (match-string 1 line) (plist-get current-hunk :old-lines)))

         ;; Line to be added (starts with +)
         ((and current-hunk (string-match "^\\\\+\\\\(.*\\\\)" line))
          (push (match-string 1 line) (plist-get current-hunk :new-lines)))

         ;; Context line (starts with space) - ignore for now
         ((and current-hunk (string-match "^ \\\\(.*\\\\)" line))
          ;; Context lines don't change the content, skip
          nil)))

      ;; Add the last hunk if it exists
      (when current-hunk
        (push current-hunk hunks))

      ;; Reverse the lines within each hunk since we pushed them
      (mapcar (lambda (hunk)
                (plist-put hunk :old-lines (reverse (plist-get hunk :old-lines)))
                (plist-put hunk :new-lines (reverse (plist-get hunk :new-lines)))
                hunk)
              (reverse hunks))))

  (defun my-gptel-parse-diff-hunks (diff-content)
    "Parse unified diff into executable hunks."
    (let ((hunks '())
          (lines (split-string diff-content "\n"))
          (current-hunk nil))
      (dolist (line lines)
        (cond
         ;; Start of new hunk: @@ -old_start,old_count +new_start,new_count @@
         ((string-match "^@@[ \t]+\\-\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?[ \t]+\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?" line)
          (when current-hunk
            (push current-hunk hunks))
          (setq current-hunk
                (list :start-line (string-to-number (match-string 3 line))
                      :old-lines '()
                      :new-lines '())))

         ;; Line to be removed (starts with -)
         ((and current-hunk (string-match "^\\-\\(.*\\)" line))
          (push (match-string 1 line) (plist-get current-hunk :old-lines)))

         ;; Line to be added (starts with +)
         ((and current-hunk (string-match "^\\+\\(.*\\)" line))
          (push (match-string 1 line) (plist-get current-hunk :new-lines)))

         ;; Context line (starts with space) - ignore for now
         ((and current-hunk (string-match "^ \\(.*\\)" line))
          ;; Context lines don't change the content, skip
          nil)))

      ;; Add the last hunk if it exists
      (when current-hunk
        (push current-hunk hunks))

      ;; Reverse the lines within each hunk since we pushed them
      (mapcar (lambda (hunk)
                (plist-put hunk :old-lines (reverse (plist-get hunk :old-lines)))
                (plist-put hunk :new-lines (reverse (plist-get hunk :new-lines)))
                hunk)
              (reverse hunks))))

  (defun my-gptel-apply-diff-internal (buffer-name diff-content)
    "Apply unified diff directly to buffer without external commands."
    (unless (get-buffer buffer-name)
      (error "Buffer %s does not exist" buffer-name))

    (with-current-buffer buffer-name
      (let ((original-point (point))
            (hunks (my-gptel-parse-diff-hunks diff-content)))
        (condition-case err
            (progn
              ;; Sort hunks by line number in reverse order to avoid offset issues
              (setq hunks (sort hunks (lambda (a b) (> (plist-get a :start-line) (plist-get b :start-line)))))

              (dolist (hunk hunks)
                (let ((start-line (plist-get hunk :start-line))
                      (old-lines (plist-get hunk :old-lines))
                      (new-lines (plist-get hunk :new-lines)))

                  ;; Go to the start line
                  (goto-char (point-min))
                  (forward-line (1- start-line))

                  ;; Delete old lines
                  (when old-lines
                    (let ((start-pos (point)))
                      (forward-line (length old-lines))
                      (delete-region start-pos (point))))

                  ;; Insert new lines
                  (when new-lines
                    (insert (mapconcat 'identity (reverse new-lines) "\n"))
                    (when (not (bolp)) (insert "\n")))))

              (goto-char original-point)
              "Patch applied successfully")

          (error (format "Patch failed: %s" (error-message-string err)))))))

(defun my-gptel-gzip-compress (content)
    "Compress content with gzip and base64 encode."
    (with-temp-buffer
      (insert content)
      (let ((exit-code (call-process-region (point-min) (point-max)
                                            "gzip" t t nil "-c")))
        (if (= exit-code 0)
            (progn
              (goto-char (point-min))
              (base64-encode-region (point-min) (point-max))
              (buffer-substring-no-properties (point-min) (point-max)))
          (error "Failed to compress content with gzip")))))

  (defun my-gptel-compressed-read (file-or-buffer &optional start end)
    "Read file or buffer with mandatory gzip compression. Always compresses full content.
    START and END are character positions to extract a region before compression."
    (let ((content
           (cond
            ;; Handle buffer
            ((get-buffer file-or-buffer)
             (with-current-buffer file-or-buffer
               (if (and start end)
                   (buffer-substring-no-properties start end)
                 (buffer-string))))
            ;; Handle file
            ((file-exists-p file-or-buffer)
             (with-temp-buffer
               (insert-file-contents file-or-buffer)
               (if (and start end)
                   (buffer-substring-no-properties start end)
                 (buffer-string))))
            (t (error "File or buffer not found: %s" file-or-buffer)))))

      (let ((original-size (length content))
            (compressed (my-gptel-gzip-compress content)))
        (format "[GZIP-COMPRESSED] Source: %s%s\\nOriginal: %,d chars → Compressed: %,d chars (%.1f%% savings)\\n\\n%s"
                file-or-buffer
                (if (and start end) (format " [region %d-%d]" start end) "")
                original-size
                (length compressed)
                (/ 100 (- 1.0 (/ (float (length compressed)) original-size)))
                compressed))))

(defun my-gptel-meta-read (file-or-buffer &optional max-lines summary-only)
  "Read file or buffer with line limits and summaries (renamed from compressed_read)."
  (let ((max-lines (or max-lines 50)))
    (cond
     ;; Handle buffer
     ((get-buffer file-or-buffer)
      (with-current-buffer file-or-buffer
        (let* ((lines (split-string (buffer-string) "\\n"))
               (line-count (length lines)))
              (if summary-only
                  (format "[META] Buffer: %s | Mode: %s | Lines: %d | Size: %d chars\\nFirst 3: %s\\nLast 3: %s"
                          file-or-buffer major-mode line-count (buffer-size)
                          (mapconcat 'identity (seq-take lines 3) " | ")
                          (mapconcat 'identity (last lines 3) " | "))
                (if (> line-count max-lines)
                    (format "%s\\n[TRUNCATED - %d of %d lines shown]"
                            (mapconcat 'identity (seq-take lines max-lines) "\\n")
                            max-lines line-count)
                  (buffer-string))))))

     ;; Handle file
     ((file-exists-p file-or-buffer)
      (with-temp-buffer
        (insert-file-contents file-or-buffer)
        (let* ((lines (split-string (buffer-string) "\\n"))
               (line-count (length lines)))
              (if summary-only
                  (format "[META] File: %s | Lines: %d | Size: %d bytes\\nFirst 3: %s\\nLast 3: %s"
                          file-or-buffer line-count (buffer-size)
                          (mapconcat 'identity (seq-take lines 3) " | ")
                          (mapconcat 'identity (last lines 3) " | "))
                (if (> line-count max-lines)
                    (format "%s\\n[TRUNCATED - %d of %d lines shown]"
                            (mapconcat 'identity (seq-take lines max-lines) "\\n")
                            max-lines line-count)
                  (buffer-string))))))

     (t (format "Error: '%s' not found" file-or-buffer)))))

  ;; Register tools with gptel
  (defun my-gptel-setup-tools ()
    "Setup working gptel tools."
    ;; When you change this list, make sure you call
    ;;
    ;;     (unload-feature 'gptel-transient t)
    ;;     (require 'gptel-transient)

    (interactive)
(defun my-gptel-find-file (path dir)
      "Open/create file at path in directory dir."
      (let ((full-path (expand-file-name path dir)))
        (if (my-gptel-path-allowed-p full-path)
            (progn
              (find-file full-path)
              (format "Opened file: %s" full-path))
          (format "Path not allowed: %s" full-path))))
    (defun my-gptel-tool-wc (path)
      "Get word count statistics for file."
      (let ((expanded-path (expand-file-name path)))
        (cond
         ((not (my-gptel-path-allowed-p expanded-path))
          (format "Error: Path '%s' is outside allowed directories" path))
         ((not (file-exists-p expanded-path))
          (format "Error: File '%s' does not exist" path))
         ((file-directory-p expanded-path)
          (format "Error: '%s' is a directory" path))
         (t
          (shell-command-to-string
           (format "wc -l -w -c %s" (shell-quote-argument expanded-path)))))))
    (defun my-gptel-read-buffer (buffer)
      (if (buffer-live-p (get-buffer buffer))
          (with-current-buffer buffer
            (buffer-substring-no-properties (point-min) (point-max)))
        (format \"Error: buffer %s is not live or does not exist\" buffer)))

    (setq gptel-tools (list
                       ;; List files tool
                       (gptel-make-tool
                        :function #'my-gptel-tool-list-files
                        :name "list_files"
                        :description "List files in a directory"
                        :args (list '(:name "path" :type "string" :description "Directory path"))
                        :category "file")
                       (gptel-make-tool
                        :function #'my-gptel-get-buffer-info
                        :name "get_buffer_info"
                        :description "Get information about the current Emacs buffer"
                        :args nil
                        :category "emacs")
                       (gptel-make-tool
                        :function #'my-gptel-get-frame-info
                        :name "get_frame_info"
                        :description "Get information about Emacs frames and windows"
                        :args nil
                        :category "emacs")

                       (gptel-make-tool
                        :function #'my-gptel-eval-elisp-safely
                        :name "eval_elisp"
                        :description "Safely evaluate Emacs Lisp code"
                        :args (list (list :name "code" :type "string" :description "Elisp code to evaluate"))
                        :category "emacs"
                        :confirm t)

                       ;; Git operations
                       (gptel-make-tool
                        :function #'my-gptel-git-status
                        :name "git_status"
                        :description "Get git repository status"
                        :args (list '(:name "repo" :type "string" :description "path to repository or file for git status"))
                        :category "git"
                        :confirm nil)

                       ;; For optional arguments, create separate tools or handle in function
                       (gptel-make-tool
                        :function (lambda (&optional file) (my-gptel-git-diff file))
                        :name "git_diff"
                        :description "Show git diff for all files"
                        :args nil
                        :category "git")

                       (gptel-make-tool
                        :function (lambda (file) (my-gptel-git-diff file))
                        :name "git_diff_file"
                        :description "Show git diff for specific file"
                        :args (list (list :name "file" :type "string" :description "File to diff"))
                        :category "git")

                       ;; Buffer operations
                       (gptel-make-tool
                        :function #'my-gptel-switch-buffer
                        :name "switch_buffer"
                        :description "Switch to a buffer by name"
                        :args (list (list :name "buffer_name" :type "string" :description "Name of buffer to switch to"))
                        :category "emacs")

                       (gptel-make-tool
                        :function #'my-gptel-list-buffers
                        :name "list_buffers"
                        :description "List all open buffers with their modes and files"
                        :args nil
                        :category "emacs")

                       (gptel-make-tool
                        :function #'my-gptel-project-files
                        :name "project_files"
                        :description "List files in current project filtered by pattern"
                        :args (list (list :name "pattern" :type "string" :description "Regex pattern to filter files" :optional t))
                        :category "project")

                       (gptel-make-tool
                        :function #'my-gptel-find-file
                        :name "find_file"
                        :description "Open or create a file"
                        :args (list '(:name "path" :type "string" :description "File path to open")
                                    '(:name "dir" :type "string"
                                            :description "parent directory for `path'"))
                        :category "file")

                       (gptel-make-tool
                        :function #'my-gptel-tool-wc
                        :name "wc"
                        :description "Get word count statistics for a file (lines, words, characters)"
                        :args (list '(:name "path" :type "string" :description "File path to analyze"))
                        :category "file")

                       ;; Search operations - split into two tools
                       (gptel-make-tool
                        :function #'my-gptel-grep-project
                        :name "grep_project"
                        :description "Search for text patterns in all project files"
                        :args (list '(:name "pattern" :type "string" :description "Search pattern")
                                    '(:name "file_pattern" :type "string" :description "pattern selecting files (via `grep --include')" :optional t))
                        :category "search")

                       (gptel-make-tool
                        :function #'find-file-other-window
                        :name "find_file_other_window"
                        :description "Open a file or directory in this Emacs session."
                        :args (list '(:name "file"
                                            :type "string"
                                            :description "The file or directory to open in the Emacs session."))
                        :category "emacs")
                       (gptel-make-tool
                        :name "read_buffer"
                        :args (list '(:name "buffer"
                                            :type "string"
                                            :description "the name of the buffer whose contents are to be retrieved"))
                        :category "emacs"
                        :description "return the contents of an emacs buffer [WARNING: expensive! use compression instead]"
                        :function #'my-gptel-read-buffer)
                       (gptel-make-tool
                        :name "make_directory"
                        :description "Create a new directory with the given name in the specified parent directory"
                        :function (lambda (parent name)
                                    (condition-case nil
                                        (progn
                                          (make-directory (expand-file-name name parent) t)
                                          (format "Directory %s created/verified in %s" name parent))
                                      (error (format "Error creating directory %s in %s" name parent))))
                        :args (list '(:name "parent"
                                            :type "string"
                                            :description "The parent directory where the new directory should be created, e.g. /tmp")
                                    '(:name "name"
                                            :type "string"
                                            :description "The name of the new directory to create, e.g. testdir"))
                        :category "file"
                        :confirm t)
                       (gptel-make-tool
                        :name "create_file"
                        :description "Create a new file with the specified content"
                        :args (list '(:name "path"
                                            :type "string"
                                            :description "The directory where to create the file")
                                    '(:name "filename"
                                            :type "string"
                                            :description "The name of the file to create")
                                    '(:name "content"
                                            :type "string"
                                            :description "The content to write to the file"))
                        :category "file"
                        :function (lambda (path filename content)
                                    (let ((full-path (expand-file-name filename path)))
                                      (with-temp-buffer
                                        (insert content)
                                        (write-file full-path))
                                      (format "Created file %s in %s" filename path)))
                        :confirm t)
                       (gptel-make-tool
                        :name "patch_buffer"
                        :description "Apply unified diff directly in Emacs buffer without external patch command"
                        :function #'my-gptel-apply-diff-internal  ; Use the new internal function
                        :args (list '(:name "buffer_name" :type "string" :description "Buffer name to modify")
                                    '(:name "diff_content" :type "string" :description "Unified diff content"))
                        :category "emacs"
                        :confirm t)
                       ;; Add the new tools
                       (gptel-make-tool
                        :name "meta_read"
                        :description "Read file/buffer with line limits and summaries (quick overview)"
                        :function #'my-gptel-meta-read
                        :args (list '(:name "file_or_buffer" :type "string" :description "File path or buffer name")
                                    '(:name "max_lines" :type "number" :description "Max lines to show (default 50)" :optional t)
                                    '(:name "summary_only" :type "boolean" :description "Show only summary stats" :optional t))
                        :category "utility")

                       (gptel-make-tool
                        :name "compressed_read"
                        :description "Read file/buffer with mandatory gzip compression (70%+ API savings). Full content preserved."
                        :function #'my-gptel-compressed-read
                        :args (list '(:name "file_or_buffer" :type "string" :description "File path or buffer name")
                                    '(:name "start" :type "number" :description "Start position for region extraction" :optional t)
                                    '(:name "end" :type "number" :description "End position for region extraction" :optional t))
                        :category "utility")

                       )))

  ;; Add detailed tool confirmation with full argument display
  (defun my-gptel-detailed-confirmation (tool-spec args)
    "Show detailed confirmation dialog with tool name, description and arguments."
    (let* ((tool-name (plist-get tool-spec :name))
           (tool-desc (plist-get tool-spec :description))
           (confirm-required (plist-get tool-spec :confirm))
           (args-display (mapconcat
                          (lambda (arg)
                            (let ((name (car arg))
                                  (value (cdr arg)))
                              (format "  %s: %s" name
                                      (if (> (length (format "%s" value)) 200)
                                          (concat (substring (format "%s" value) 0 200) "...")
                                        value))))
                          args "\n")))
      (when confirm-required
        (yes-or-no-p
         (format "Execute Tool: %s\n\nDescription: %s\n\nArguments:\n%s\n\nProceed? "
                 tool-name tool-desc args-display)))))

  ;; Custom confirmation function for detailed tool dialogs
  (defun my-gptel-detailed-tool-confirmation (&rest args)
    "Show detailed confirmation dialog for tool calls."
    (let* ((tool-name (if (stringp (car args)) (car args) "Unknown Tool"))
           (tool-args (if (stringp (car args)) (cdr args) args))
           (arg-details (mapconcat
                         (lambda (arg)
                           (format "  %s"
                                   (if (> (length (format "%s" arg)) 100)
                                       (concat (substring (format "%s" arg) 0 100) "...")
                                     arg)))
                         tool-args "\n"))
           (confirmation-msg (format "Execute Tool: %s\n\nArguments:\n%s\n\nProceed?"
                                     tool-name arg-details)))
      (yes-or-no-p confirmation-msg)))


  ;; Load fix for gptel-menu transient crashes (keymapp 2 error)
  ;; Load immediately - no need to wait for gptel-transient

  (require 'gptel-menu-fix)

  ;; Auto-enable gptel-mode for org files with GPTEL properties
  (defun my-auto-enable-gptel-mode ()
    "Auto-enable gptel-mode for files with GPTEL properties."
    (when (eq major-mode 'org-mode)
      (save-excursion
        (goto-char (point-min))
        (when (and (re-search-forward "^:PROPERTIES:" nil t)
                   (let ((props-end (save-excursion (re-search-forward "^:END:" nil t))))
                     (and props-end (re-search-forward "^:GPTEL_" props-end t))))
          (gptel-mode 1)))))

  (defun my-gptel-clean-completion ()
    "Disable auto-completion and remove text-focused completions in gptel buffers."
    (interactive)
    ;; Disable corfu auto-popup in gptel buffers
    (corfu-mode -1)

    ;; Keep useful completions like file paths, but remove text-focused ones
    (setq-local completion-at-point-functions
                (cl-remove-if (lambda (fn)
                                (memq fn '(ispell-completion-at-point
                                           org-completion-at-point
                                           pcomplete-completions-at-point
                                           comint-completion-at-point)))
                              completion-at-point-functions)))

  (add-hook 'find-file-hook 'my-auto-enable-gptel-mode)
  (add-hook 'gptel-mode-hook 'my-gptel-clean-completion)

  ;; Add debug function to see what's happening
  (defun my-gptel-debug-confirmation ()
    "Debug tool confirmation state."
    (interactive)
    (message "Current overlays with gptel-tool: %s"
             (mapcar (lambda (ov)
                       (list (overlay-start ov) (overlay-end ov)
                             (overlay-get ov 'gptel-tool)))
                     (seq-filter (lambda (ov) (overlay-get ov 'gptel-tool))
                                 (overlays-in (point-min) (point-max))))))

   (defun my-gptel-limit-output (output max-size)
     (if (> (length output) max-size)
         (concat (substring output 0 max-size) "\n[OUTPUT TRUNCATED]")
       output)))


(provide 'my-gpt)
