;; my-gpt.el -*- lexical-binding: t; -*-
;;
;; configure llm interactions

(use-package gptel
  :straight (gptel
             :type git
             :host github
             :repo "karthink/gptel"
             :branch "master")

  :commands
  (gptel gptel-menu gptel-send gptel-request
         my-gptel-review my-gptel-explain my-gptel-review
         my-gptel-openai my-gptel-claude
         gptel-add-file gptel-add)

  :hook ((gptel-mode . my-gptel-setup-behavior)
         (gptel-mode . my-gptel-setup-keybindings))
  :custom
  (gptel-track-response t)
  (gptel-log-level 'error)
  ;; WARNING: insecure
  (gptel-use-curl t)
  (gptel-stream t)
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

  ;; Fix model name - should be string
  (setq gptel-model "gpt-4.1")

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
    (unless (hash-table-p my-gptel-api-keys)
      (setq my-gptel-api-keys (make-hash-table :test 'equal)))
    (or (gethash provider my-gptel-api-keys)
        (let ((key (pcase provider
                     ("openai" (or (getenv "OPENAI_API_KEY")
                                   (auth-source-pick-first-password
                                    :host "api.openai.com" :user "apikey")
                                   (read-string "OpenAI API Key: ")))
                     ("anthropic" (or (getenv "ANTHROPIC_API_KEY")
                                      (auth-source-pick-first-password
                                       :host "api.anthropic.com" :user "apikey")
                                      (read-string "Anthropic API Key: "))))))
          (when key
            (puthash provider key my-gptel-api-keys))
          key)))

  ;; Provider switching
  (defun my-gptel-openai ()
    "Switch to OpenAI."
    (interactive)
    (if-let ((key (my-gptel-get-key "openai")))
        (progn
          (setq gptel-model 'gpt-4.1
                gptel-backend (gptel-make-openai "OpenAI" :key key))
          (message "Switched to OpenAI GPT-4.1"))
      (user-error "No OpenAI key found")))

  (defun my-gptel-claude ()
    "Switch to Claude."
    (interactive)
    (if-let ((key (my-gptel-get-key "anthropic")))
        (progn
          (setq gptel-model 'claude-sonnet-4-20250514
                gptel-backend
                (gptel-make-anthropic "Claude"
                  :key key
                  :stream t))
          (message "Switched to Claude Sonnet 4"))
      (user-error "No Anthropic key found")))

  ;; Two simple functions to start
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
    (evil-collection-gptel-setup)))

(use-package mcp
  :straight (:type git :host github :repo "lizqwerscott/mcp.el" :files ("*.el"))
  ;; Auto-start all MCP servers after initialization
  ;; :hook (after-init . mcp-hub-start-all-server))
  :after gptel
  :config
  ;; Configure MCP servers
  )

(provide 'my-gpt)
