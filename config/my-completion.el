;; my-ido.el
;;
;; Settings for ido, a better way to select things in a minibuffer.


(use-package ivy
  :ensure ivy
  :config (progn
            (after 'projectile
              (progn
                (setq projectile-completion-system 'ivy)))
            (ivy-mode)))

(provide 'my-completion)
