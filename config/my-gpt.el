;; my-gpt.el -*- lexical-binding: t; -*-
;;
;; configure llm interactions
(require 'cl-lib)

(use-package request
  :ensure t)
;; (straight-pull-recipe-repositories)

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

(use-package gptel
  :straight (:repo "karthink/gptel" :branch "state-tracking" :files ("*.el") :no-byte-compile t)
  :custom-face
  (gptel-user-header ((t (:foreground "#dca3a3" :weight bold))))
  (gptel-assistant-header ((t (:foreground "#7f9f7f" :weight bold))))
  (gptel-response ((t (:foreground "#9fc59f"))))
  :commands
  (gptel gptel-menu gptel-send gptel-request
         my-gptel-review my-gptel-explain my-gptel-review
         mgptel-add-file gptel-add)

  :hook ((gptel-mode . my-gptel-setup-behavior)
         (gptel-mode . my-gptel-setup-keybindings))
  :custom
  (gptel-track-response t)
  (gptel-log-level 'error)
  ;; WARNING: insecure
  (gptel-use-curl t)
  (gptel-stream t)
  (gptel-use-header-line t)
  (gptel-prompt-prefix-alist '((org-mode . "** Human\n")))
  (gptel-response-prefix-alist '((org-mode . "** Assistant\n")))
  ;; should always be a symbol - see docs
  (gptel-model 'claude-sonnet-4-20250514)
  ;; response length in tokens
  (gptel-max-tokens 3000)
  ;; disable tools by default
  (gptel-use-tools nil)
  ;; enable expert commands for enhanced functionality
  (gptel-expert-commands t)

  ;; enable visual debugging of conversation state
  (gptel-enable-visual-debugging nil)
  ;; enable enhanced state tracking with markers and overlays
  (gptel-enable-enhanced-state-tracking t)
  (gptel-enable-strict-validation nil)
  (gptel-auto-repair-invalid-state t)
  (gptel-validation-log-level 'info)

  :init
  (defvar my-gptel-system-prompt
    "You are a LLM running inside Emacs. Your responses are inserted literally into the buffer where the prompt is sent - usually code in the language being discussed. Do not use markdown or org to structure your comments. Instead, structure in alignment with the surrounding text. Put your commentary in comments (e.g., `;;` for elisp, `//` for go, ...)."
    "my preferred system prompt for gptel")

  (defvar my-gptel-elisp-prompt
    "You are a LLM running inside Emacs. Help me work on my elisp
config. I am using emacs 30.1.90 and (use-package) with =straight=.
Respond in org-mode. I can share any buffers or config files you
request in the context."
    "my elisp authorship system prompt for gptel")

  (defvar my-gptel-nix-prompt
    "you are a nix and nixos expert. help with concise, idiomatic nix expressions and troubleshooting. explain key concepts clearly when asked."
    "prompt for nix/nixos configuration and code.")

  (defvar my-gptel-explain-prompt
    "you are a friendly technical explainer. given any code or configuration, break it down into clear, step-by-step explanations that could help a developer new to that language or tool."
    "prompt to ask for code explanations.")

  (defvar my-gptel-debug-prompt
    "you are a debugging assistant. help the user systematically diagnose and solve bugs in code or config, suggesting possible causes and useful troubleshooting steps, highlighting common mistakes relevant to the language in question."
    "prompt for troubleshooting and debugging.")

  (defvar my-gptel-edit-prompt
    "you are a code review and refactoring assistant. suggest edits for clarity, idiomatic style, or robustness, while keeping the original structure where possible. explain your suggestions."
    "prompt for code review or edit requests.")

  ;; Session management
  (defun my-gptel-setup-behavior ()
    "Setup gptel buffer behavior."
    (interactive)
    ;; Enable word wrap for better AI text display
    (visual-line-mode 1)
    ;; Auto-save gptel buffers less frequently to avoid interrupting streams
    (setq-local auto-save-timeout 60)
    ;; Disable aggressive auto-save during streaming
    (setq-local auto-save-visited-mode nil))

  (defvar my-gptel-directives
    (list
     (cons 'Nix     my-gptel-nix-prompt)
     (cons 'Explain my-gptel-explain-prompt)
     (cons 'Debug   my-gptel-debug-prompt)
     (cons 'Edit    my-gptel-edit-prompt)
     (cons 'Inline my-gptel-system-prompt)
     (cons 'Elisp  my-gptel-elisp-prompt))
    "My preferred gptel directives for multi-language workflows.")

  :config
  ;; Initialize the hash table
  (defvar my-gptel-api-keys (make-hash-table :test 'equal))

  (if (getenv "OPENAI_API_KEY")
      (setq gptel-api-key (getenv "OPENAI_API_KEY")))

  ;; Set default mode for gptel conversation
  (setq gptel-default-mode 'org-mode)

  (setq gptel-directives
        (let* ((my-keywords (mapcar #'car my-gptel-directives))
               (filtered (seq-remove (lambda (item)
                                       (memq (car item) my-keywords))
                                     gptel-directives)))
          (append my-gptel-directives filtered)))

  ;; Only require if it exists
  (when (featurep 'gptel-integrations)
    (require 'gptel-integrations))

  (defun my-gptel-get-key (provider)
    "Get API key for PROVIDER, cached or prompt for input."
    ;; Ensure hash table exists
    (unless (and (boundp 'my-gptel-api-keys)
                 (hash-table-p my-gptel-api-keys))
      (setq my-gptel-api-keys (make-hash-table :test 'equal)))

    (or (gethash provider my-gptel-api-keys)
        (let ((key (pcase provider
                     ("openai"
                      (or (getenv "OPENAI_API_KEY")
                          (condition-case nil
                              (auth-source-pick-first-password
                               :host "api.openai.com" :user "apikey")
                            (error nil))
                          (read-passwd "OpenAI API Key: ")))
                     ("anthropic"
                      (or (getenv "ANTHROPIC_API_KEY")
                          (condition-case nil
                              (auth-source-pick-first-password
                               :host "api.anthropic.com" :user "apikey")
                            (error nil))
                          (read-passwd "Anthropic API Key: ")))
                     (_ (error "Unknown provider: %s" provider)))))
          (when (and key (stringp key) (not (string-empty-p key)))
            (puthash provider key my-gptel-api-keys)
            key))))

  (defun my-gptel-list-models ()
    "List all models available in gptel."
    (interactive)
    (with-current-buffer (get-buffer-create "*GPTel Models*")
      (erase-buffer)
      (insert "== OpenAI Models ==\n\n")
      (dolist (model-pair gptel--openai-models)
        (insert (format "• %s\n" (car model-pair))))

      (when (featurep 'gptel-anthropic)
        (insert "\n\n== Anthropic Models ==\n\n")
        (dolist (model-pair gptel--anthropic-models)
          (insert (format "• %s\n" (car model-pair)))))

      (pop-to-buffer (current-buffer))))

  ;; Provider switching with model selection
  (defun my-gptel-openai (model)
    "Switch to OpenAI backend with MODEL selection."
    (interactive
     (list (let* ((backend (cdr (assoc "ChatGPT" gptel--known-backends)))
                  (models (gptel-backend-models backend)))
             (unless models (error "No OpenAI backend found"))
             (completing-read "OpenAI Model: " (mapcar #'gptel--model-name models)
                              nil t nil nil "gpt-4o-mini"))))
    (if-let* ((key (my-gptel-get-key "openai"))
              (backend (cdr (assoc "ChatGPT" gptel--known-backends)))
              (model-obj (cl-find model (gptel-backend-models backend)
                                  :test (lambda (name model)
                                          (string= name (gptel--model-name model))))))
        (progn
          (setq gptel-model model-obj
                gptel-backend backend
                gptel-api-key key)
          (message "Switched to OpenAI %s" model))
      (user-error "OpenAI backend or model not found")))

  (defun my-gptel-claude (model)
    "Switch to Claude backend with MODEL selection."
    (interactive
     (list (completing-read
            "Claude Model: "
            ;; Use the actual models from gptel--anthropic-models
            (progn
              (require 'gptel-anthropic)  ; Ensure anthropic backend is loaded
              (mapcar #'symbol-name (mapcar #'car gptel--anthropic-models)))
            nil t nil nil "claude-3-5-sonnet-20241022")))
    (if-let ((key (my-gptel-get-key "anthropic")))
        (progn
          (require 'gptel-anthropic)  ; Ensure it's loaded before making backend
          (setq gptel-model (intern model)  ; Convert back to symbol
                gptel-backend (gptel-make-anthropic "Claude" :key key :stream t)
                gptel-api-key key)
          (message "Switched to Claude %s" model))
      (user-error "No Anthropic key found")))

  (defun my-gptel-explain ()
    "Explain current region or function."
    (interactive)
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (thing-at-point 'defun t))))
      (if text
          (gptel-request (format "Explain this code:\n\n=\n%s\n=" text))
        (message "No code to explain"))))

  (defun my-gptel-review ()
    "Review current region or buffer."
    (interactive)
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (buffer-string))))
      (gptel-request (format "Review this code for bugs and improvements:\n\n=\n%s\n=" text))))

  (defun my-gptel-add-project-context ()
    "Add relevant project files as context to gptel."
    (interactive)
    (if-let ((files (project-files (project-current))))
        (progn
          (dolist (file (completing-read-multiple "Add files: " files))
            (gptel-add-file file))
          (message "Added %d files to context" (length files)))
      (message "No project found")))

  (defun my-gptel-clear-context ()
    "Clear all context from the current gptel session."
    (interactive)
    (setq-local gptel-context nil)
    (message "Context cleared"))

  ;; Documentation generation
  (defun my-gptel-document-function ()
    "Generate documentation for function at point."
    (interactive)
    (when-let ((fn (thing-at-point 'defun t)))
      (gptel-request (format "Write documentation for this function:\n\n%s" fn))))

  (defun my-gptel-setup-keybindings ()
    (interactive)
    "Ensure keybindings work in gptel buffers."
    (when (derived-mode-p 'org-mode)
      (my-org-setup-keybindings)))

  ;; Only call if the function exists
  (when (fboundp 'evil-collection-gptel-setup)
    (evil-collection-gptel-setup))

  ;; Commands to inspect and clean up gptel backends

  ;; 1. Check what backends you currently have
  (defun my-gptel-list-backends ()
    "List all registered gptel backends."
    (interactive)
    (with-current-buffer (get-buffer-create "*gptel-backends*")
      (erase-buffer)
      (insert "=== Current gptel backends ===\n\n")
      (insert (format "Default backend: %s\n\n" (gptel-backend-name gptel-backend)))
      (insert "All known backends:\n")
      (dolist (backend-pair gptel--known-backends)
        (let* ((name (car backend-pair))
               (backend (cdr backend-pair))
               (type (type-of backend))
               (models (length (gptel-backend-models backend))))
          (insert (format "- %s (%s, %d models)\n" name type models))))
      (display-buffer (current-buffer))))

  ;; 2. Reset gptel backends to default state
  (defun my-gptel-reset-backends ()
    "Reset gptel backends to clean state."
    (interactive)
    (when (yes-or-no-p "Reset all gptel backends to defaults? ")
      ;; Clear known backends except the default OpenAI one
      (setq gptel--known-backends
            (list (cons "ChatGPT" gptel--openai)))
      ;; Reset to default backend
      (setq gptel-backend gptel--openai)
      (setq gptel-model 'gpt-4.1)
      ;; Clear any cached keys
      (when (boundp 'my-gptel-api-keys)
        (clrhash my-gptel-api-keys))
      (message "gptel backends reset to defaults")))

  ;; 3. Remove a specific backend
  (defun my-gptel-remove-backend (name)
    "Remove a specific backend by NAME."
    (interactive
     (list (completing-read "Remove backend: "
                            gptel--known-backends nil t)))
    (setq gptel--known-backends
          (assoc-delete-all name gptel--known-backends))
    (when (equal (gptel-backend-name gptel-backend) name)
      (setq gptel-backend gptel--openai))
    (message "Removed backend: %s" name))

  ;; 4. Check if your current backend is working
  (defun my-gptel-test-backend ()
    "Test if current backend is properly configured."
    (interactive)
    (condition-case err
        (let* ((backend gptel-backend)
               (name (gptel-backend-name backend))
               (models (gptel-backend-models backend))
               (key-fn (gptel-backend-key backend)))
          (message "Backend: %s, Models: %d, Key function: %s"
                   name (length models) key-fn)
          (when key-fn
            (condition-case key-err
                (let ((key (gptel--get-api-key)))
                  (if (and key (not (string-empty-p key)))
                      (message "[success] Backend %s appears properly configured" name)
                    (message "[err] Backend %s: no valid API key" name)))
              (error (message "[err] Backend %s: key error: %s" name key-err)))))
      (error (message "[err] Backend error: %s" err))))

  ;; Integration with claude-agent tools
  ;;
  ;; Tool setup architecture:
  ;;   1. my-gptel-create-tool-handlers - Creates tool handler functions
  ;;   2. my-gptel-register-tools       - Registers handlers with gptel
  ;;   3. my-gptel-enable-tools         - Main entry point (calls 1 & 2)
  ;;
  ;; Usage: my-gptel-enable-tools is called automatically on load

  (defvar my-gptel-tools nil
    "AI agent tools available to gptel.")

  (defun my-gptel-create-tool-handlers ()
    "Create AI agent tool handler functions.
This is layer 1: creates the tool handler functions."
    ;; Add pkg/claude-agent directly to load path
    (add-to-list 'load-path (expand-file-name "pkg/claude-agent" user-emacs-directory))
    (when (locate-library "claude-agent")
      ;; Try to install request if not available
      (unless (locate-library "request")
        (when (fboundp 'straight-use-package)
          (condition-case nil
              (straight-use-package 'request)
            (error (message "Warning: Failed to install request package")))))
      (require 'claude-agent)
      (setq my-gptel-tools
            `((read_file . ,(lambda (args)
                              (claude-agent--tool-read-file
                               (if (stringp args) `((path . ,args)) args))))
              (list_files . ,(lambda (args)
                               (claude-agent--tool-list-files
                                (if (stringp args) `((path . ,args)) args))))
              (bash . ,(lambda (args)
                         (claude-agent--tool-bash
                          (if (stringp args) `((command . ,args)) args))))
              (edit_file . ,(lambda (args)
                              (if (and (listp args) (= (length args) 2))
                                  (claude-agent--tool-edit-file
                                   `((path . ,(car args))
                                     (content . ,(cadr args))))
                                (claude-agent--tool-edit-file args))))
              (grep . ,(lambda (args)
                         (if (and (listp args) (= (length args) 2))
                             (claude-agent--tool-grep
                              `((pattern . ,(car args))
                                (path . ,(cadr args))))
                           (claude-agent--tool-grep args))))))))

  (defun my-gptel-execute-tool (tool-name &rest args)
    "Execute an AI agent tool via gptel."
    (unless my-gptel-tools
      (my-gptel-create-tool-handlers))
    (if-let* ((handler (alist-get tool-name my-gptel-tools)))
        (funcall handler (car args))  ; Pass the first argument directly
      (format "Error: Unknown tool %s" tool-name)))


  ;; Register claude-agent tools with gptel's tool system
  (defun my-gptel-register-tools ()
    "Register AI agent tools with gptel.
This is layer 2: registers tool handlers with gptel's tool system."
    (my-gptel-create-tool-handlers)

    ;; Store tools in gptel-tools variable
    (setq gptel-tools
          (list
           ;; read_file tool
           (gptel-make-tool
            :function (lambda (path) (my-gptel-execute-tool 'read_file path))
            :name "read_file"
            :description "Read contents of a file with security checks"
            :args (list (list :name "path" :type "string" :description "Path to the file to read"))
            :category "filesystem")

           ;; list_files tool
           (gptel-make-tool
            :function (lambda (path) (my-gptel-execute-tool 'list_files path))
            :name "list_files"
            :description "List files and directories in a given path"
            :args (list (list :name "path" :type "string" :description "Directory path to list"))
            :category "filesystem")

           ;; bash tool
           (gptel-make-tool
            :function (lambda (command) (my-gptel-execute-tool 'bash command))
            :name "bash"
            :description "Execute shell commands safely with security restrictions"
            :args (list (list :name "command" :type "string" :description "Shell command to execute"))
            :category "system"
            :confirm t)

           ;; edit_file tool
           (gptel-make-tool
            :function (lambda (path content) (my-gptel-execute-tool 'edit_file (list path content)))
            :name "edit_file"
            :description "Write content to a file with security checks"
            :args (list (list :name "path" :type "string" :description "Path to the file to write")
                        (list :name "content" :type "string" :description "Content to write to the file"))
            :category "filesystem"
            :confirm t)

           ;; grep tool
           (gptel-make-tool
            :function (lambda (pattern path) (my-gptel-execute-tool 'grep (list pattern path)))
            :name "grep"
            :description "Search for patterns in files within allowed directories"
            :args (list (list :name "pattern" :type "string" :description "Pattern to search for")
                        (list :name "path" :type "string" :description "Path to search in"))
            :category "search")))

    (message "Registered %d tools with gptel" (length gptel-tools)))

  ;; Setup function to be called on gptel initialization
  (defun my-gptel-enable-tools ()
    "Enable AI agent tools for gptel.
This is layer 3: main entry point that coordinates the full setup."
    (interactive)
    (my-gptel-register-tools)
    (message "Enhanced gptel tools with AI agent backend enabled"))

  ;; Auto-setup when gptel loads
  ;; This is the main initialization - calls layer 3 which calls layers 2 & 1
  (my-gptel-enable-tools)

  ;; Auto-enable gptel-mode for org files with GPTEL properties
  (defun my-auto-enable-gptel-mode ()
    "Auto-enable gptel-mode for files with GPTEL properties in the top properties block."
    (when (eq major-mode 'org-mode)
      (save-excursion
        (goto-char (point-min))
        ;; Look for :PROPERTIES: block at the start of the file
        (when (re-search-forward "^:PROPERTIES:" nil t)
          (let ((props-start (point))
                (props-end (when (re-search-forward "^:END:" nil t) (point))))
            (when (and props-end
                       (goto-char props-start)
                       (re-search-forward "^:GPTEL_" props-end t))
              (gptel-mode 1)))))))

  (add-hook 'find-file-hook 'my-auto-enable-gptel-mode)

  ) ;; Close the main use-package gptel block

(use-package gptel-plus
  :straight
  (:host github
   :repo "benthamite/gptel-plus"
   :files ("*.el"))
  :after 'gptel)

(use-package mcp
  :straight (:repo "lizqwerscott/mcp.el" :files ("*.el"))
  ;; Auto-start all MCP servers after initialization
  ;; :hook (after-init . mcp-hub-start-all-server))
  :after gptel
  :config ; configure here
  )

(provide 'my-gpt)
