;; my-modeline.el
;;
;; Theme for my modeline.
;;
;; This file is required by my-eyecandy.el

(use-package powerline
  :ensure powerline
  :init
  (progn
    (use-package powerline-evil
      :ensure powerline-evil
      :init
      (progn
        (setq powerline-arrow-shape 'arrow14)
        (set-face-foreground 'mode-line "#fff")
        (set-face-background 'mode-line "#000")
        (setq-default mode-line-format
                      '("%e"
                        (:eval
                         (let*
                             ((active
                               (powerline-selected-window-active))
                              (mode-line
                               (if active 'mode-line 'mode-line-inactive))
                              (face1
                               (if active 'powerline-active1 'powerline-inactive1))
                              (face2
                               (if active 'powerline-active2 'powerline-inactive2))
                              (separator-left
                               (intern
                                (format "powerline-%s-%s" powerline-default-separator
                                        (car powerline-default-separator-dir))))
                              (separator-right
                               (intern
                                (format "powerline-%s-%s" powerline-default-separator
                                        (cdr powerline-default-separator-dir))))
                              (lhs
                               (list
                                (let
                                    ((evil-face
                                      (powerline-evil-face)))
                                  (if (and evil-mode
                                           (eq (current-buffer) (car (buffer-list))))
                                      (powerline-raw
                                       (powerline-evil-tag)
                                       evil-face)))
                                (powerline-buffer-id
                                 `(mode-line-buffer-id ,mode-line)
                                 'l)
                                (powerline-raw "[" mode-line 'l)
                                (powerline-major-mode mode-line)
                                (powerline-process mode-line)
                                (powerline-raw "]" mode-line)
                                (when
                                    (buffer-modified-p)
                                  (powerline-raw "[+]" mode-line))
                                (when buffer-read-only
                                  (powerline-raw "[RO]" mode-line))
                                (powerline-raw "[%z]" mode-line)
                                (when
                                    (and
                                     (boundp 'which-func-mode)
                                     which-func-mode)
                                  (powerline-raw which-func-format nil 'l))
                                (when
                                    (boundp 'erc-modified-channels-object)
                                  (powerline-raw erc-modified-channels-object face1 'l))
                                (when
                                    (and vc-mode buffer-file-name)
                                  (let
                                      ((backend
                                        (vc-backend buffer-file-name)))
                                    (when backend
                                      (concat
                                       (powerline-raw "[" mode-line 'l)
                                       (powerline-raw
                                        (format "%s / %s" backend
                                                (vc-working-revision buffer-file-name backend)))
                                       (powerline-raw "]" mode-line)))))))
                              (rhs
                               (list
                                (powerline-raw
                                 '(10 "%i"))
                                (powerline-raw global-mode-string mode-line 'r)
                                (powerline-raw "%l," mode-line 'l)
                                (powerline-raw
                                 (format-mode-line
                                  '(10 "%c")))
                                (powerline-raw
                                 (replace-regexp-in-string "%" "%%"
                                                           (format-mode-line
                                                            '(-3 "%p")))
                                 mode-line 'r))))
                           (concat
                            (powerline-render lhs)
                            (powerline-fill mode-line
                                            (powerline-width rhs))
                            (powerline-render rhs))))))
        )
      )
    )
  )

(provide 'my-modeline)
