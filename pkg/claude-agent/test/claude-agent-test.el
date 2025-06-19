;;; claude-agent-test.el --- Tests for claude-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:
;; Unit tests for claude-agent functionality

;;; Code:

(require 'ert)
(require 'claude-agent)

;;; Security Tests

(ert-deftest claude-agent-test-path-allowed-p ()
  "Test path access control."
  (let ((claude-agent-allowed-directories '("~/test" "/tmp")))
    (should (claude-agent--path-allowed-p "~/test/file.txt"))
    (should (claude-agent--path-allowed-p "/tmp/file.txt"))
    (should-not (claude-agent--path-allowed-p "/etc/passwd"))
    (should-not (claude-agent--path-allowed-p "/root/file.txt"))))

(ert-deftest claude-agent-test-command-safe-p ()
  "Test command safety checks."
  (should (claude-agent--command-safe-p "echo hello"))
  (should (claude-agent--command-safe-p "ls -la"))
  (should (claude-agent--command-safe-p "pwd"))
  (should-not (claude-agent--command-safe-p "rm -rf /"))
  (should-not (claude-agent--command-safe-p "sudo rm file"))
  (should-not (claude-agent--command-safe-p "su root"))
  (should-not (claude-agent--command-safe-p "echo password > /etc/passwd")))

(ert-deftest claude-agent-test-require-confirmation-p ()
  "Test confirmation requirement logic."
  (let ((claude-agent-require-confirmation '(bash edit)))
    (should (claude-agent--require-confirmation-p 'bash))
    (should (claude-agent--require-confirmation-p 'edit))
    (should-not (claude-agent--require-confirmation-p 'read_file))
    (should-not (claude-agent--require-confirmation-p 'list_files))))

;;; Tool Tests

(ert-deftest claude-agent-test-tool-list-files ()
  "Test list_files tool."
  (let ((claude-agent-allowed-directories '("/tmp")))
    ;; Test with allowed directory
    (let ((result (claude-agent--tool-list-files '((path . "/tmp")))))
      (should (stringp result))
      (should-not (string-prefix-p "Error:" result)))
    
    ;; Test with disallowed directory
    (let ((result (claude-agent--tool-list-files '((path . "/etc")))))
      (should (string-prefix-p "Error: Access denied" result)))
    
    ;; Test with non-existent directory
    (let ((result (claude-agent--tool-list-files '((path . "/nonexistent")))))
      (should (string-prefix-p "Error:" result)))))

(ert-deftest claude-agent-test-tool-read-file ()
  "Test read_file tool."
  (let ((claude-agent-allowed-directories '("/tmp"))
        (test-file "/tmp/claude-agent-test.txt"))
    
    ;; Create test file
    (with-temp-file test-file
      (insert "test content"))
    
    ;; Test reading allowed file
    (let ((result (claude-agent--tool-read-file `((path . ,test-file)))))
      (should (string= result "test content")))
    
    ;; Test reading disallowed file
    (let ((result (claude-agent--tool-read-file '((path . "/etc/passwd")))))
      (should (string-prefix-p "Error: Access denied" result)))
    
    ;; Test reading non-existent file
    (let ((result (claude-agent--tool-read-file '((path . "/tmp/nonexistent.txt")))))
      (should (string-prefix-p "Error: File" result)))
    
    ;; Clean up
    (delete-file test-file)))

(ert-deftest claude-agent-test-tool-bash-safe ()
  "Test bash tool with safe commands."
  (let ((claude-agent-require-confirmation nil))
    ;; Test safe command
    (let ((result (claude-agent--tool-bash '((command . "echo hello")))))
      (should (string= (string-trim result) "hello")))
    
    ;; Test unsafe command
    (let ((result (claude-agent--tool-bash '((command . "rm -rf /")))))
      (should (string= result "Error: Command blocked for security")))))

(ert-deftest claude-agent-test-tool-edit-file ()
  "Test edit_file tool."
  (let ((claude-agent-allowed-directories '("/tmp"))
        (claude-agent-require-confirmation nil)
        (test-file "/tmp/claude-agent-edit-test.txt"))
    
    ;; Test editing allowed file
    (let ((result (claude-agent--tool-edit-file `((path . ,test-file)
                                                  (content . "new content")))))
      (should (string-prefix-p "Successfully wrote" result))
      (should (file-exists-p test-file))
      (with-temp-buffer
        (insert-file-contents test-file)
        (should (string= (buffer-string) "new content"))))
    
    ;; Test editing disallowed file
    (let ((result (claude-agent--tool-edit-file '((path . "/etc/test.txt")
                                                  (content . "bad content")))))
      (should (string-prefix-p "Error: Access denied" result)))
    
    ;; Clean up
    (when (file-exists-p test-file)
      (delete-file test-file))))

(ert-deftest claude-agent-test-tool-grep ()
  "Test grep tool."
  (let ((claude-agent-allowed-directories '("/tmp"))
        (test-file "/tmp/claude-agent-grep-test.txt"))
    
    ;; Create test file
    (with-temp-file test-file
      (insert "hello world\ntest line\nhello again"))
    
    ;; Test grep in allowed directory
    (let ((result (claude-agent--tool-grep `((pattern . "hello")
                                             (path . "/tmp")))))
      (should (stringp result))
      (should (string-match-p "hello" result)))
    
    ;; Test grep in disallowed directory
    (let ((result (claude-agent--tool-grep '((pattern . "test")
                                             (path . "/etc")))))
      (should (string-prefix-p "Error: Access denied" result)))
    
    ;; Clean up
    (delete-file test-file)))

;;; Tool Dispatch Tests

(ert-deftest claude-agent-test-execute-tool ()
  "Test tool execution dispatch."
  (let ((claude-agent-allowed-directories '("/tmp"))
        (claude-agent-require-confirmation nil))
    
    ;; Test valid tool
    (let ((result (claude-agent--execute-tool 'bash '((command . "echo test")))))
      (should (string= (string-trim result) "test")))
    
    ;; Test invalid tool
    (let ((result (claude-agent--execute-tool 'nonexistent '((arg . "value")))))
      (should (string-prefix-p "Error: Unknown tool" result)))))

;;; Utility Tests

(ert-deftest claude-agent-test-get-api-key ()
  "Test API key retrieval."
  (let ((claude-agent-api-key "test-key")
        (process-environment (cons "ANTHROPIC_API_KEY=env-key" process-environment)))
    
    ;; Should prefer customization over environment
    (should (string= (claude-agent--get-api-key) "test-key")))
  
  (let ((claude-agent-api-key nil)
        (process-environment (cons "ANTHROPIC_API_KEY=env-key" process-environment)))
    
    ;; Should use environment when customization is nil
    (should (string= (claude-agent--get-api-key) "env-key"))))

(provide 'claude-agent-test)
;;; claude-agent-test.el ends here