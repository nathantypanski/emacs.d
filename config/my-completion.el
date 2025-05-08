;; my-ido.el
;;
;; Settings for ido, a better way to select things in a minibuffer.


(use-package ivy
  :ensure ivy
  :diminish
  :bind (("C-s" . swiper)        ;; in-buffer search
         ("C-c C-r" . ivy-resume) ;; resume last Ivy completion
         ("M-x" . counsel-M-x)   ;; smarter M-x
         ("C-x C-f" . counsel-find-file) ;; fuzzy find files
         ("C-x b" . ivy-switch-buffer)) ;; switch buffers
  :config (progn
            (after 'projectile
              (progn
                (setq projectile-completion-system 'ivy)))
            (ivy-mode))
  :custom
  (ivy-use-virtual-buffers t)   ;; show recent files and bookmarks
  (enable-recursive-minibuffers t)
  (ivy-count-format "(%d/%d) ") ;; show candidate count
  (ivy-wrap nil))

;; Counsel: Ivy-enhanced versions of built-ins
(use-package counsel
  :after ivy
  :config (counsel-mode 1))

;; Swiper: in-buffer search with Ivy interface
(use-package swiper
  :after ivy)

(provide 'my-completion)
