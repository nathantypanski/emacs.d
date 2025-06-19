;;; claude-agent-test-standalone.el --- Standalone tests for claude-agent core functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:
;; Standalone unit tests for claude-agent core functionality (no API calls)

;;; Code:

(require 'ert)

;; Load only the core functions we want to test
(defvar claude-agent-allowed-directories '("~/test" "/tmp")
  "Test directories for testing.")

(defvar claude-agent-require-confirmation '(bash edit delete)
  "Test confirmation settings.")

(defvar claude-agent-max-file-size (* 1024 1024)
  "Test file size limit.")

;; Core functions under test (copied without dependencies)
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
          "Would require confirmation in interactive mode"
        (shell-command-to-string command)))))

(defun claude-agent--tool-edit-file (args)
  "Edit file specified in ARGS."
  (let ((path (alist-get 'path args))
        (content (alist-get 'content args)))
    (if (not (claude-agent--path-allowed-p path))
        (format "Error: Access denied to %s" path)
      (if (claude-agent--require-confirmation-p 'edit)
          "Would require confirmation in interactive mode"
        (progn
          (with-temp-file path
            (insert content))
          (format "Successfully wrote to %s" path))))))

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
      (should (string-prefix-p "Error: Access denied" result)))))

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

;; Test runner
(defun claude-agent-run-standalone-tests ()
  "Run standalone claude-agent tests."
  (interactive)
  (ert-run-tests-batch-and-exit "^claude-agent-test-"))

;; When run as script
(when noninteractive
  (claude-agent-run-standalone-tests))

(provide 'claude-agent-test-standalone)
;;; claude-agent-test-standalone.el ends here