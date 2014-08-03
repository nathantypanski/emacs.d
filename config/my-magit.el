(use-package magit
  :ensure magit
  :config
  (progn
    (after 'evil
      (define-key magit-status-mode-map (kbd "C-n") 'magit-goto-next-sibling-section)
      (define-key magit-status-mode-map (kbd "C-p") 'magit-goto-previous-sibling-section)
      (evil-add-hjkl-bindings magit-status-mode-map 'emacs
        "K" 'magit-discard-item
        "l" 'magit-key-mode-popup-logging
        "h" 'magit-toggle-diff-refine-hunk)
      (evil-set-initial-state 'magit-mode 'normal)
      (evil-set-initial-state 'magit-status-mode 'normal)
      (evil-set-initial-state 'magit-diff-mode 'normal)
      (evil-define-key 'normal magit-mode-map
        (kbd "TAB") 'magit-toggle-section
        (kbd "RET") 'magit-visit-item
        (kbd "C-n") 'magit-goto-next-sibling-section
        (kbd "C-p") 'magit-goto-previous-sibling-section
        (kbd "C-w") 'magit-copy-item-as-kill
        (kbd "SPC") 'magit-show-item-or-scroll-up
        "!" 'magit-key-mode-popup-running
        "$" 'magit-process
        "+" 'magit-diff-larger-hunks
        "-" 'magit-diff-smaller-hunks
        "." 'magit-mark-item
        "0" 'magit-diff-default-hunks
        "1" 'magit-show-level-1
        "2" 'magit-show-level-2
        "3" 'magit-show-level-3
        "4" 'magit-show-level-4
        ":" 'magit-git-command
        "<" 'beginning-of-buffer
        "=" 'magit-diff-with-mark
        ">" 'end-of-buffer
        "?" 'magit-key-mode-popup-dispatch
        "A" 'magit-cherry-pick-item
        "B" 'magit-key-mode-popup-bisecting
        "C" 'magit-commit-add-log
        "D" 'magit-diff
        "E" 'magit-interactive-rebase
        "F" 'magit-key-mode-popup-pulling
        "G" 'magit-refresh-all
        "H" 'magit-diff-toggle-refine-hunk
        "I" 'magit-ignore-item-locally
        ;; "J" 'magit-key-mode-popup-apply-mailbox
        "L" 'magit-add-change-log-entry
        "M" 'magit-key-mode-popup-remoting
        "P" 'magit-key-mode-popup-pushing
        "R" 'magit-rebase-step
        "S" 'magit-stage-all
        "U" 'magit-unstage-all
        "X" 'magit-reset-working-tree
        "^" 'magit-goto-parent-section
        "a" 'magit-apply-item
        "b" 'magit-key-mode-popup-branching
        "c" 'magit-key-mode-popup-committing
        "d" 'magit-diff-working-tree
        "e" 'magit-ediff
        "f" 'magit-key-mode-popup-fetching
        "g" 'magit-refresh
        "h" 'magit-key-mode-popup-diff-options
        "i" 'magit-ignore-item
        ";" 'magit-section-jump-map
        "j" 'magit-goto-next-section
        "k" 'magit-goto-previous-section
        "K" 'magit-discard-item
        "m" 'magit-key-mode-popup-merging
        "n" 'magit-goto-next-section
        "o" 'magit-key-mode-popup-submodule
        "p" 'magit-goto-previous-section
        "q" 'magit-mode-quit-window
        "r" 'magit-key-mode-popup-rewriting
        "s" 'magit-stage-item
        "t" 'magit-key-mode-popup-tagging
        "u" 'magit-unstage-item
        "v" 'magit-revert-item
        "w" 'magit-wazzup
        "x" 'magit-reset-head
        "y" 'magit-cherry
        "z" 'magit-key-mode-popup-stashing
        (kbd "DEL") 'magit-show-item-or-scroll-down
        (kbd "S-SPC") 'scroll-down-command
        (kbd "<C-return>") 'magit-dired-jump
        (kbd "<backtab>") 'magit-expand-collapse-section
        )
      (evil-define-key
        'normal magit-diff-mode-map
        (kbd "TAB") 'magit-toggle-section
        (kbd "RET") 'magit-visit-item
        (kbd "SPC") 'magit-visit-item
        "!" 'magit-key-mode-popup-running
        "$" 'magit-process
        "+" 'magit-diff-larger-hunks
        "-" 'magit-diff-smaller-hunks
        "0" 'magit-diff-default-hunks
        "1" 'magit-show-level-1
        "2" 'magit-show-level-2
        "3" 'magit-show-level-3
        "4" 'magit-show-level-4
        ":" 'magit-git-command
        "<" 'beginning-of-buffer
        ">" 'end-of-buffer
        "?" 'magit-key-mode-popup-dispatch
        "A" 'magit-cherry-pick-item
        "B" 'magit-key-mode-popup-bisecting
        "D" 'magit-diff
        "E" 'magit-interactive-rebase
        "F" 'magit-key-mode-popup-pulling
        "G" 'magit-refresh-all
        "H" 'magit-diff-toggle-refine-hunk
        "J" 'magit-key-mode-popup-apply-mailbox
        "L" 'magit-add-change-log-entry
        "M" 'magit-key-mode-popup-remoting
        "P" 'magit-key-mode-popup-pushing
        "R" 'magit-rebase-step
        "S" 'magit-stage-all
        "U" 'magit-unstage-all
        "X" 'magit-reset-working-tree
        "^" 'magit-goto-parent-section
        "a" 'magit-apply-item
        "b" 'magit-key-mode-popup-branching
        "c" 'magit-key-mode-popup-committing
        "d" 'magit-diff-working-tree
        "e" 'magit-ediff
        "f" 'magit-key-mode-popup-fetching
        "g" 'magit-refresh
        "h" 'magit-key-mode-popup-diff-options
        "l" 'magit-key-mode-popup-logging
        "m" 'magit-key-mode-popup-merging
        "j" 'magit-goto-next-section
        "o" 'magit-key-mode-popup-submodule
        "k" 'magit-goto-previous-section
        "q" 'magit-mode-quit-window
        "r" 'magit-key-mode-popup-rewriting
        "t" 'magit-key-mode-popup-tagging
        "v" 'magit-revert-item
        "w" 'magit-wazzup
        "x" 'magit-reset-head
        "y" 'magit-cherry
        "z" 'magit-key-mode-popup-stashing
        (kbd "DEL") 'scroll-down
        (kbd "S-SPC") 'scroll-down-command
        )
      )


      
    )
  )
  
(provide 'my-magit)
