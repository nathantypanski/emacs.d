;;; run-tests.el --- Test runner for claude-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:
;; Simple test runner for claude-agent tests

;;; Code:

;; Add claude-agent to load path
(add-to-list 'load-path (expand-file-name "../" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "." (file-name-directory load-file-name)))

;; Load dependencies
(require 'ert)
(require 'claude-agent)
(require 'claude-agent-test)

(defun claude-agent-run-tests ()
  "Run all claude-agent tests."
  (interactive)
  (ert-run-tests-batch-and-exit "^claude-agent-test-"))

;; When run as script
(when noninteractive
  (claude-agent-run-tests))

(provide 'run-tests)
;;; run-tests.el ends here