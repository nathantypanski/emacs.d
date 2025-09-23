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
  :straight (:repo "karthink/gptel" :branch "state-tracking" :files ("*.el"))
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
  (gptel-enable-enhanced-state-tracking t)
  (gptel-auto-repair-invalid-state t)
  (gptel-include-tool-results t)
  (gptel-confirm-tool-calls t)
  :init
  ;; Core prompts
  (defvar my-gptel-system-prompt
    "You are a LLM running inside Emacs. Your responses are inserted literally into the buffer where the prompt is sent - usually code in the language being discussed. Do not use markdown or org to structure your comments. Instead, structure in alignment with the surrounding text. Put your commentary in comments (e.g., `;;` for elisp, `//` for go, ...).")

  (defvar my-gptel-elisp-prompt
    "You are a LLM running inside Emacs. Help me work on my elisp config. I am using emacs 30.1.90 and (use-package) with straight. Respond in org-mode. I can share any buffers or config files you request in the context.")

  (defvar my-gptel-directives
    (list (cons 'Inline my-gptel-system-prompt)
          (cons 'Elisp my-gptel-elisp-prompt)))

  :config
  ;; Basic setup
  (when (getenv "OPENAI_API_KEY")
    (setq gptel-api-key (getenv "OPENAI_API_KEY")))
  (setq gptel-default-mode 'org-mode)
  (setq gptel-directives (append my-gptel-directives gptel-directives))
  (setq gptel-org-branching-context t)

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
    "Read contents of file at PATH with safety checks."
    (let ((expanded-path (expand-file-name path)))
      (cond
       ((not (my-gptel-path-allowed-p expanded-path))
        (format "Error: Path '%s' is outside allowed directories" path))
       ((not (file-exists-p expanded-path))
        (format "Error: File '%s' does not exist" path))
       ((file-directory-p expanded-path)
        (format "Error: '%s' is a directory, not a file" path))
       ((> (file-attribute-size (file-attributes expanded-path)) (* 1024 1024))
        "Error: File too large (>1MB)")
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
  (defun my-gptel-git-status ()
    "Get git repository status."
    (if (vc-git-root default-directory)
        (shell-command-to-string "git status --porcelain")
      "Not in a git repository"))

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
    "List all buffers with their modes and files."
    (mapconcat
     (lambda (buf)
       (with-current-buffer buf
         (format "%s [%s] %s"
                 (buffer-name)
                 major-mode
                 (or (buffer-file-name) ""))))
     (buffer-list)
     "\n"))

  (defun my-gptel-project-files (&optional pattern)
    "List files in current project, optionally filtered by pattern."
    (if-let ((project (project-current)))
        (let ((files (project-files project)))
          (if pattern
              (cl-remove-if-not
               (lambda (file) (string-match-p pattern file))
               files)
            files))
      '("Not in a project")))

  (defun my-gptel-find-file (path)
    "Open/create file at path."
    (let ((expanded-path (expand-file-name path)))
      (if (my-gptel-path-allowed-p expanded-path)
          (progn
            (find-file expanded-path)
            (format "Opened file: %s" path))
        (format "Path not allowed: %s" path))))

  (defun my-gptel-grep-project (pattern &optional file-pattern)
    "Search for pattern in project files."
    (if-let ((project (project-current)))
        (let ((default-directory (project-root project)))
          (shell-command-to-string
           (format "grep -r --include='%s' '%s' ."
                   (or file-pattern "*")
                   pattern)))
      "Not in a project"))

  (defun modify-buffer-apply-diff (buffer diff)
    "Apply unified format DIFF to BUFFER."
    (message "Applying diff %s to buffer %s" diff buffer)
    (if-let ((buf (get-buffer buffer)))
        (with-current-buffer buf
          ;; Find the first @@ and ignore everything before it
          (if-let ((first-hunk-pos (string-match "^@@ .*@@\n" diff)))
              (dolist (hunk (split-string (substring diff first-hunk-pos) "^@@ .*@@\n" t))
                (let (before after)
                  (dolist (line (split-string hunk "\n" t))
                    (cond
                     ((string-prefix-p " " line)
                      (push (substring line 1) before)
                      (push (substring line 1) after))
                     ((string-prefix-p "-" line)
                      (push (substring line 1) before))
                     ((string-prefix-p "+" line)
                      (push (substring line 1) after))))
                  (setq before (string-join (nreverse before) "\n")
                        after (string-join (nreverse after) "\n"))
                  (goto-char (point-min))
                  (if (search-forward before nil t)
                      (replace-match after)
                    (message "Hunk not found in buffer %s" buffer))))
            (message "No hunks found in diff"))
          (format "Applied changes to buffer %s" buffer))
      (error "Buffer %s not found" buffer)))

  ;; Register tools with gptel
  (defun my-gptel-setup-tools ()
    "Setup working gptel tools."
    (interactive)
    (setq gptel-tools
          (list
           ;; Read file tool
           (gptel-make-tool
            :function #'my-gptel-tool-read-file
            :name "read_file"
            :description "Read contents of a file"
            :args (list (list :name "path" :type "string" :description "File path to read"))
            :category "file")

           ;; List files tool
           (gptel-make-tool
            :function #'my-gptel-tool-list-files
            :name "list_files"
            :description "List files in a directory"
            :args (list (list :name "path" :type "string" :description "Directory path"))
            :category "file")

           ;; Bash command tool
           (gptel-make-tool
            :function #'my-gptel-tool-bash
            :name "bash"
            :description "Execute a shell command"
            :args (list (list :name "command" :type "string" :description "Command to execute"))
            :category "system"
            :confirm t)

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
            :args nil
            :category "git")

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

           ;; Project operations - split into two tools
           (gptel-make-tool
            :function (lambda () (my-gptel-project-files))
            :name "project_files"
            :description "List all files in current project"
            :args nil
            :category "project")

           (gptel-make-tool
            :function (lambda (pattern) (my-gptel-project-files pattern))
            :name "project_files_filtered"
            :description "List files in current project filtered by pattern"
            :args (list (list :name "pattern" :type "string" :description "Regex pattern to filter files"))
            :category "project")

           (gptel-make-tool
            :function #'my-gptel-find-file
            :name "find_file"
            :description "Open or create a file"
            :args (list (list :name "path" :type "string" :description "File path to open"))
            :category "file")

           ;; Search operations - split into two tools
           (gptel-make-tool
            :function (lambda (pattern) (my-gptel-grep-project pattern))
            :name "grep_project"
            :description "Search for text patterns in all project files"
            :args (list (list :name "pattern" :type "string" :description "Search pattern"))
            :category "search")

           (gptel-make-tool
            :function (lambda (pattern file-pattern) (my-gptel-grep-project pattern file-pattern))
            :name "grep_project_filtered"
            :description "Search for text patterns in specific project files"
            :args (list (list :name "pattern" :type "string" :description "Search pattern")
                        (list :name "file_pattern" :type "string" :description "File glob pattern (e.g., '*.el')"))
            :category "search")
           (gptel-make-tool
            :function (lambda (url)
                        (with-current-buffer (url-retrieve-synchronously url)
                          (goto-char (point-min)) (forward-paragraph)
                          (let ((dom (libxml-parse-html-region (point) (point-max))))
                            (run-at-time 0 nil #'kill-buffer (current-buffer))
                            (with-temp-buffer
                              (eww-score-readability dom)
                              (shr-insert-document (eww-highest-readability dom))
                              (buffer-substring-no-properties (point-min) (point-max))))))
            :name "read_url"
            :description "Fetch and read the contents of a URL"
            :args (list '(:name "url"
                                :type "string"
                                :description "The URL to read"))
            :category "web")
           (gptel-make-tool
            :function (lambda (buffer text)
                        (with-current-buffer (get-buffer-create buffer)
                          (save-excursion
                            (goto-char (point-max))
                            (insert text)))
                        (format "Appended text to buffer %s" buffer))
            :name "append_to_buffer"
            :description "Append text to the an Emacs buffer.  If the buffer does not exist, it will be created."
            :args (list '(:name "buffer"
                                :type "string"
                                :description "The name of the buffer to append text to.")
                        '(:name "text"
                                :type "string"
                                :description "The text to append to the buffer."))
            :category "emacs")
           (gptel-make-tool
            :function (lambda (filename)
                        (find-file-other-window filename)
                        (format "Opened file or directory %s" filename))
            :name "open_file_or_dir"
            :description "Open a file or directory in this Emacs session."
            :args (list '(:name "file"
                                :type "string"
                                :description "The file or directory to open in the Emacs session."))
            :category "emacs")
           (gptel-make-tool
            :function (lambda (buffer)
                        (unless (buffer-live-p (get-buffer buffer))
                          (error "error: buffer %s is not live." buffer))
                        (with-current-buffer  buffer
                          (buffer-substring-no-properties (point-min) (point-max))))
            :name "read_buffer"
            :description "return the contents of an emacs buffer"
            :args (list '(:name "buffer"
                                :type "string"
                                :description "the name of the buffer whose contents are to be retrieved"))
            :category "emacs")
           (gptel-make-tool
            :name "modify_buffer"
            :description "Modify buffer contents using unified diff format"
            :args (list '(:name "buffer"
                                :type "string"
                                :description "The name of the buffer to modify")
                        '(:name "diff"
                                :type "string"
                                :description "The changes to apply in unified diff format (with @@ hunks and +/- lines)"))
            :category "emacs"
            :function #'modify-buffer-apply-diff)
           (gptel-make-tool
            :function (lambda (parent name)
                        (condition-case nil
                            (progn
                              (make-directory (expand-file-name name parent) t)
                              (format "Directory %s created/verified in %s" name parent))
                          (error (format "Error creating directory %s in %s" name parent))))
            :name "make_directory"
            :description "Create a new directory with the given name in the specified parent directory"
            :args (list '(:name "parent"
                                :type "string"
                                :description "The parent directory where the new directory should be created, e.g. /tmp")
                        '(:name "name"
                                :type "string"
                                :description "The name of the new directory to create, e.g. testdir"))
            :category "file"
            :confirm t)
           (gptel-make-tool
            :function (lambda (path filename content)
                        (let ((full-path (expand-file-name filename path)))
                          (with-temp-buffer
                            (insert content)
                            (write-file full-path))
                          (format "Created file %s in %s" filename path)))
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
            :confirm t)
           (gptel-make-tool
            :name "apply_diff_fenced"
            :description (concat
                          "Applies a diff (patch) to a specified file using fenced diff content. This is the PREFERRED method for modifying files as it is more token-efficient."
                          "The diff must be provided within fenced code blocks (=diff or =patch) and be in unified format. "
                          "The LLM should generate the diff such that the file paths within the diff "
                          "(e.g., '--- a/filename' '+++ b/filename') are appropriate for the 'file_path' argument and chosen 'patch_options'. "
                          "Common 'patch_options' include: '' (empty, if paths in diff are exact or relative to current dir of file_path), "
                          "'-p0' (if diff paths are full or exactly match the target including prefixes like 'a/'), "
                          "'-p1' (if diff paths have one leading directory to strip, e.g., diff has 'a/src/file.c' and you want to patch 'src/file.c' from project root). "
                          "Default options are '-N' (ignore already applied patches).")
            :args (list
                   '(:name "file_path"
                           :type string
                           :description "The path to the file that needs to be patched.")
                   '(:name "diff_content"
                           :type string
                           :description "The diff content within fenced code blocks (=diff or =patch) in unified format.")
                   '(:name "patch_options"
                           :type string
                           :optional t
                           :description "Optional: Additional options for the 'patch' command (e.g., '-p1', '-p0', '-R'). Defaults to '-N'. Prepend other options if needed, e.g., '-p1 -N'.")
                   '(:name "working_dir"
                           :type string
                           :optional t
                           :description "Optional: The directory in which to interpret file_path and run patch. Defaults to the current buffer's directory if not specified."))
            :category "file"
            :function
            (lambda (file_path diff_content &optional patch_options working_dir)
              ;; Extract diff content from fenced blocks
              (let ((extracted-diff
                     (if (string-match "=\\(?:diff\\|patch\\)?\n\\(\\(?:.\\|\n\\)*?\\)\n=" diff_content)
                         (match-string 1 diff_content)
                       ;; If no fenced block found, try to use content as-is but warn
                       (progn
                         (message "Warning: No fenced diff block found, using content as-is")
                         diff_content))))

                ;; Continue with original logic using extracted diff
                (setq diff_content extracted-diff))

              (let ((original-default-directory default-directory)
                    (user-patch-options (if (and patch_options (not (string-empty-p patch_options)))
                                            (split-string patch_options " " t)
                                          nil))
                    ;; Combine user options with -N, ensuring -N is there.
                    ;; If user provides -N or --forward, use their version. Otherwise, add -N.
                    (base-options '("-N"))
                    (effective-patch-options '()))

                (if user-patch-options
                    (if (or (member "-N" user-patch-options) (member "--forward" user-patch-options))
                        (setq effective-patch-options user-patch-options)
                      (setq effective-patch-options (append user-patch-options base-options)))
                  (setq effective-patch-options base-options))

                (let* ((out-buf-name (generate-new-buffer-name "*patch-stdout*"))
                       (err-buf-name (generate-new-buffer-name "*patch-stderr*"))
                       (target-file nil)
                       (exit-status -1) ; Initialize to a known non-zero value
                       (result-output "")
                       (result-error ""))
                  (unwind-protect
                      (progn
                        (when (and working_dir (not (string-empty-p working_dir)))
                          (setq default-directory (expand-file-name working_dir)))

                        (setq target-file (expand-file-name file_path))

                        (unless (file-exists-p target-file)
                          ;; Use error to signal failure, which gptel should catch.
                          (error "File to patch does not exist: %s" target-file))

                        (with-temp-message (format "Applying diff to: `%s` with options: %s" target-file effective-patch-options)
                          (with-temp-buffer
                            (insert diff_content)
                            (unless (eq (char-before (point-max)) ?\n)
                              (goto-char (point-max))
                              (insert "\n"))

                            ;; Pass buffer *names* to call-process-region
                            (setq exit-status (apply #'call-process-region
                                                     (point-min) (point-max)
                                                     "patch"       ; Command
                                                     nil           ; delete region (no)
                                                     (list out-buf-name err-buf-name) ; stdout/stderr buffer names
                                                     nil           ; display (no)
                                                     (append effective-patch-options (list target-file))))))

                        ;; Retrieve content from buffers using their names
                        (let ((stdout-buf (get-buffer out-buf-name))
                              (stderr-buf (get-buffer err-buf-name)))
                          (when stdout-buf
                            (with-current-buffer stdout-buf
                              (setq result-output (buffer-string))))
                          (when stderr-buf
                            (with-current-buffer stderr-buf
                              (setq result-error (buffer-string)))))

                        (if (= exit-status 0)
                            (format "Diff successfully applied to %s.\nPatch command options: %s\nPatch STDOUT:\n%s\nPatch STDERR:\n%s"
                                    target-file effective-patch-options result-output result-error)
                          ;; Signal an Elisp error, which gptel will catch and display.
                          ;; The arguments to 'error' become the error message.
                          (error "Failed to apply diff to %s (exit status %s).\nPatch command options: %s\nPatch STDOUT:\n%s\nPatch STDERR:\n%s"
                                 target-file exit-status effective-patch-options result-output result-error)))
                    ;; Cleanup clause of unwind-protect
                    (setq default-directory original-default-directory)
                    (let ((stdout-buf-obj (get-buffer out-buf-name))
                          (stderr-buf-obj (get-buffer err-buf-name)))
                      (when (buffer-live-p stdout-buf-obj) (kill-buffer stdout-buf-obj))
                      (when (buffer-live-p stderr-buf-obj) (kill-buffer stderr-buf-obj)))))))
            :include t)
           ))
    (message "Clean gptel tools configured"))

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
  (add-hook 'gptel-mode-hook 'my-gptel-clean-completion))

(gptel-make-preset 'default
  :description "My default settings"
  :system 'default
  :backend "Claude"
  :model 'claude-sonnet-4-20250514
  :tools t :temperature nil :stream t)

(gptel-make-preset 'programmer
  :description "Code generation with context"
  :system 'programmer
  :backend "Claude"
  :model 'claude-sonnet-4-20250514
  :tools t :use-context 'system)

(gptel-make-preset 'quickask
  :description "Fast queries, no context"
  :backend "Claude"
  :model 'claude-3-5-haiku-20241022
  :use-context nil :tools nil)

(defun my-gptel-fix-tool-call-rendering (beg end)
  "Fix syntax highlighting in tool call blocks."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "#+begin_src elisp\\|#+begin_src emacs-lisp" end t)
        (org-fontify-meta-lines-and-blocks-1)))))

(add-hook 'gptel-post-response-functions #'my-gptel-fix-tool-call-rendering)

  ;; Setup tools after gptel loads
  (my-gptel-setup-tools) ; Temporarily disabled to test MCP tools conflict

(provide 'my-gpt)
