(use-package workgroups2
  :disabled t
  :ensure workgroups2
  :init
  (progn
    (setq wg-use-default-session-file nil)
    (setq wg-mode-line-display-on t)          ; Default: (not (featurep 'powerline))
    (setq wg-flag-modified t)                 ; Display modified flags as well
    (setq wg-mode-line-decor-left-brace "["
          wg-mode-line-decor-right-brace "]"  ; how to surround it
          wg-mode-line-decor-divider ":")
    (defun my-wg-switch-to-workgroup (wg)
      "Switch to a workgroup using wokgroups2"
      (interactive (list
                    (progn
                      (wg-find-session-file wg-default-session-file)
                      (wg-read-workgroup-name))))
      (wg-switch-to-workgroup wg)
      )
    (defun my-wg-save-session ()
      (interactive)
      (wg-save-session)
      )
    (workgroups-mode 1))
)

(provide 'my-sessions)
