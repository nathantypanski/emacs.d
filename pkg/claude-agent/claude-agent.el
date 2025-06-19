;;; claude-agent.el --- Agentic Claude integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (request "0.3.0"))
;; Keywords: ai, claude, tools

;;; Commentary:
;; Claude agent with tool capabilities, complementing gptel for agentic workflows.
;; Provides file operations, bash execution, and other Claude Code-like features.

;;; Code:

(require 'json)
(require 'request)

;;; Customization

(defgroup claude-agent nil
  "Agentic Claude integration."
  :group 'external)

(defcustom claude-agent-api-key nil
  "Anthropic API key for Claude agent."
  :type '(choice (const nil) string)
  :group 'claude-agent)

(defcustom claude-agent-model "claude-3-5-sonnet-20241022"
  "Claude model to use."
  :type 'string
  :group 'claude-agent)

(defcustom claude-agent-allowed-directories
  '("~/src" "~/dotfiles" "~/.emacs.d")
  "Directories Claude can access."
  :type '(repeat directory)
  :group 'claude-agent)

(defcustom claude-agent-require-confirmation
  '(bash edit delete)
  "Tool types requiring user confirmation."
  :type '(repeat symbol)
  :group 'claude-agent)

(defcustom claude-agent-max-file-size (* 1024 1024)  ; 1MB
  "Maximum file size Claude can read."
  :type 'integer
  :group 'claude-agent)

;;; Security

(defun claude-agent--path-allowed-p (path)
  "Check if PATH is in allowed directories."
  (let ((expanded-path (expand-file-name path))
        (allowed nil))
    (dolist (dir claude-agent-allowed-directories allowed)
      (let ((expanded-dir (expand-file-name dir)))
        (when (string-prefix-p expanded-dir expanded-path)
          (setq allowed t))))))

(defun claude-agent--command-safe-p (command)
  "Basic safety check for COMMAND."
  (not (string-match-p "\\(rm -rf\\|sudo\\|su \\|>.*passwd\\)" command)))

(defun claude-agent--require-confirmation-p (tool-type)
  "Check if TOOL-TYPE requires confirmation."
  (memq tool-type claude-agent-require-confirmation))

;;; Tools

(defun claude-agent--tool-read-file (args)
  "Read file specified in ARGS."
  (let ((path (alist-get 'path args)))
    (if (not (claude-agent--path-allowed-p path))
        (format "Error: Access denied to %s" path)
      (if (not (file-exists-p path))
          (format "Error: File %s does not exist" path)
        (if (> (nth 7 (file-attributes path)) claude-agent-max-file-size)
            (format "Error: File %s too large" path)
          (with-temp-buffer
            (insert-file-contents path)
            (buffer-string)))))))

(defun claude-agent--tool-list-files (args)
  "List files in directory specified in ARGS."
  (let ((path (or (alist-get 'path args) ".")))
    (if (not (claude-agent--path-allowed-p path))
        (format "Error: Access denied to %s" path)
      (if (not (file-directory-p path))
          (format "Error: %s is not a directory" path)
        (mapconcat 'identity
                   (directory-files path nil "^[^.]")
                   "\n")))))

(defun claude-agent--tool-bash (args)
  "Execute bash command specified in ARGS."
  (let ((command (alist-get 'command args)))
    (if (not (claude-agent--command-safe-p command))
        "Error: Command blocked for security"
      (if (claude-agent--require-confirmation-p 'bash)
          (if (y-or-n-p (format "Execute: %s" command))
              (shell-command-to-string command)
            "Command cancelled by user")
        (shell-command-to-string command)))))

(defun claude-agent--tool-edit-file (args)
  "Edit file specified in ARGS."
  (let ((path (alist-get 'path args))
        (content (alist-get 'content args)))
    (if (not (claude-agent--path-allowed-p path))
        (format "Error: Access denied to %s" path)
      (if (claude-agent--require-confirmation-p 'edit)
          (if (y-or-n-p (format "Edit file %s?" path))
              (progn
                (with-temp-file path
                  (insert content))
                (format "Successfully wrote to %s" path))
            "Edit cancelled by user")
        (progn
          (with-temp-file path
            (insert content))
          (format "Successfully wrote to %s" path))))))

(defun claude-agent--tool-grep (args)
  "Search for pattern in files."
  (let ((pattern (alist-get 'pattern args))
        (path (or (alist-get 'path args) ".")))
    (if (not (claude-agent--path-allowed-p path))
        (format "Error: Access denied to %s" path)
      (shell-command-to-string 
       (format "grep -r %s %s" 
               (shell-quote-argument pattern)
               (shell-quote-argument path))))))

;;; Tool dispatch

(defvar claude-agent--available-tools
  '((read_file . claude-agent--tool-read-file)
    (list_files . claude-agent--tool-list-files)
    (bash . claude-agent--tool-bash)
    (edit_file . claude-agent--tool-edit-file)
    (grep . claude-agent--tool-grep))
  "Available tools and their handlers.")

(defun claude-agent--execute-tool (tool-name args)
  "Execute TOOL-NAME with ARGS."
  (let ((handler (alist-get tool-name claude-agent--available-tools)))
    (if handler
        (funcall handler args)
      (format "Error: Unknown tool %s" tool-name))))

;;; API Integration

(defun claude-agent--get-api-key ()
  "Get API key from customization or environment."
  (or claude-agent-api-key
      (getenv "ANTHROPIC_API_KEY")))

(defun claude-agent--build-tool-definitions ()
  "Build tool definitions for API."
  `[((name . "read_file")
     (description . "Read contents of a file")
     (input_schema . ((type . "object")
                      (properties . ((path . ((type . "string")
                                               (description . "Path to file")))))
                      (required . ["path"]))))
    ((name . "list_files")
     (description . "List files in directory")
     (input_schema . ((type . "object")
                      (properties . ((path . ((type . "string")
                                               (description . "Directory path")))))
                      (required . ["path"]))))
    ((name . "bash")
     (description . "Execute bash command")
     (input_schema . ((type . "object")
                      (properties . ((command . ((type . "string")
                                                  (description . "Command to execute")))))
                      (required . ["command"]))))
    ((name . "edit_file")
     (description . "Write content to file")
     (input_schema . ((type . "object")
                      (properties . ((path . ((type . "string")
                                               (description . "File path")))
                                     (content . ((type . "string")
                                                 (description . "File content")))))
                      (required . ["path" "content"]))))
    ((name . "grep")
     (description . "Search for pattern in files")
     (input_schema . ((type . "object")
                      (properties . ((pattern . ((type . "string")
                                                  (description . "Search pattern")))
                                     (path . ((type . "string")
                                              (description . "Search path")))))
                      (required . ["pattern"]))))])

;;; Main interface

(defvar claude-agent--conversation-history '()
  "Current conversation history.")

(defun claude-agent--call-api (messages)
  "Call Claude API with MESSAGES."
  (let ((api-key (claude-agent--get-api-key)))
    (if (not api-key)
        (error "No API key found. Set claude-agent-api-key or ANTHROPIC_API_KEY")
      (let ((response
             (request
              "https://api.anthropic.com/v1/messages"
              :type "POST"
              :headers `(("Content-Type" . "application/json")
                         ("x-api-key" . ,api-key)
                         ("anthropic-version" . "2023-06-01"))
              :data (json-encode
                     `((model . ,claude-agent-model)
                       (max_tokens . 4000)
                       (tools . ,(claude-agent--build-tool-definitions))
                       (messages . ,messages)))
              :parser 'json-read
              :sync t)))
        (if (request-response-error-thrown response)
            (error "API call failed: %s" (request-response-error-thrown response))
          (request-response-data response))))))

;;;###autoload
(defun claude-agent (prompt)
  "Start agentic conversation with Claude using PROMPT."
  (interactive "sPrompt: ")
  (claude-agent--display-user-message prompt)
  (setq claude-agent--conversation-history
        `[((role . "user") (content . ,prompt))])
  (claude-agent--process-conversation))

(defun claude-agent--process-conversation ()
  "Process the current conversation with tool calling support."
  (let ((response (claude-agent--call-api claude-agent--conversation-history)))
    (if (not response)
        (message "No response from Claude")
      (let ((content (alist-get 'content response))
            (stop-reason (alist-get 'stop_reason response)))
        (cond
         ((string= stop-reason "tool_use")
          ;; Handle tool calls
          (claude-agent--handle-tool-calls content))
         (t
          ;; Regular response
          (claude-agent--display-response content)))))))

(defun claude-agent--handle-tool-calls (content)
  "Handle tool calls in CONTENT."
  (let ((tool-results '()))
    (dolist (item content)
      (when (string= (alist-get 'type item) "tool_use")
        (let* ((tool-name (intern (alist-get 'name item)))
               (tool-args (alist-get 'input item))
               (tool-id (alist-get 'id item))
               (result (claude-agent--execute-tool tool-name tool-args)))
          (push `((type . "tool_result")
                  (tool_use_id . ,tool-id)
                  (content . ,result))
                tool-results))))

    ;; Add tool results to conversation and continue
    (setq claude-agent--conversation-history
          (vconcat claude-agent--conversation-history
                   `[((role . "assistant") (content . ,content))
                     ((role . "user") (content . ,(nreverse tool-results)))]))
    (claude-agent--process-conversation)))

(defun claude-agent--display-user-message (prompt)
  "Display user's PROMPT in conversation buffer."
  (with-current-buffer (get-buffer-create "*Claude Agent*")
    (unless (= (point-max) 1)
      (goto-char (point-max))
      (insert "\n\n"))
    (insert "## You\n\n" prompt "\n\n")
    (switch-to-buffer-other-window (current-buffer))))

(defun claude-agent--display-response (content)
  "Display Claude's CONTENT response."
  (with-current-buffer (get-buffer-create "*Claude Agent*")
    (goto-char (point-max))
    (insert "## Claude\n\n")
    (if (vectorp content)
        (dotimes (i (length content))
          (let ((item (aref content i)))
            (when (string= (alist-get 'type item) "text")
              (insert (alist-get 'text item)))))
      (dolist (item content)
        (when (string= (alist-get 'type item) "text")
          (insert (alist-get 'text item)))))
    (insert "\n")
    (switch-to-buffer-other-window (current-buffer))))

(provide 'claude-agent)
;;; claude-agent.el ends here
