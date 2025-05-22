;;  -*- lexical-binding: t; -*-
;; (eval-after-load "tex"
;;   '(remove-from-list 'TeX-command-list
;; 		'("Latex Make" "latexmk %(-pdf) %t" TeX-run-TeX) t)
;;   )
;; (use-package auctex
;;   :ensure auctex
;;   :config
;;   (progn
;;     (setq-default TeX-master nil)
;;     ;; (setq-default TeX-command-default "Latex Make")
;; 
;;     (setq TeX-view-program-list '(("zathura" "zathura -s -x \"emacsclient --no-wait +%%{line} %%{input}\" %o")))
;;     ;; Always use PDF mode
;;     (setq TeX-PDF-mode t)
;; 
;;     ;; View my PDFs with Zathura
;;     (setq TeX-view-program-selection '((output-pdf "zathura")))
;; 
;; 
;;     ;; Maybe someday this will actually work and I can sync PDFs with Zathura
;;     (setq TeX-source-correlate-mode t)
;;     (setq TeX-source-correlate-method 'synctex)
;;     (setq TeX-source-correlate-start-server t)
;;     (setq TeX-auto-save t)
;;     (setq TeX-parse-self t)
;;     (setq-default TeX-master nil)
;;     (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;;     (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;;     (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
;;     (add-hook 'LaTeX-mode-hook 'auto-fill-mode) 
;;     (add-hook 'LaTeX-mode-hook 'orgtbl-mode)
;; 
;;     ;; source:
;;     ;; http://tex.stackexchange.com/questions/185688/how-to-force-emacs-with-auctex-to-show-compilation-in-new-buffer
;;     (add-to-list 'TeX-command-list '("pdfLaTeX" "pdflatex -shell-escape %t" TeX-run-interactive nil t))
;;     (defcustom tex-my-viewer "zathura --fork -s -x \"emacsclient --eval '(progn (switch-to-buffer  (file-name-nondirectory \"'\"'\"%{input}\"'\"'\")) (goto-line %{line}))'\"" 
;;       "PDF Viewer for TeX documents. You may want to fork the viewer
;;     so that it detects when the same document is launched twice, and
;;     persists when Emacs gets closed.
;; 
;;     Simple command:
;; 
;;       zathura --fork
;; 
;;     We can use
;; 
;;       emacsclient --eval '(progn (switch-to-buffer  (file-name-nondirectory \"%{input}\")) (goto-line %{line}))'
;; 
;;     to reverse-search a pdf using SyncTeX. Note that the quotes and double-quotes matter and must be escaped appropriately."
;;       :safe 'stringp)
;; 
;;     ;; (use-package auctex-latexmk
;;     ;;   :ensure auctex-latexmk
;;     ;;   :init
;;     ;;   (progn
;;     ;;     (auctex-latexmk-setup)
;;     ;;     )
;;     ;; )
;;     ))
(provide 'my-latex)
