(setq TeX-PDF-mode t)
;; (eval-after-load "tex"
;;   '(remove-from-list 'TeX-command-list
;; 		'("Latex Make" "latexmk %(-pdf) %t" TeX-run-TeX) t)
;;   )

(setq-default TeX-master nil)
;; (setq-default TeX-command-default "Latex Make")

(setq TeX-view-program-list '(("zathura" "zathura -s -x \"emacsclient -n +%%{line} %%{input}\" %o")))

(setq TeX-view-program-selection '((output-pdf "zathura")))


(setq TeX-source-correlate-mode t)

(setq TeX-source-correlate-method 'synctex)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

(add-hook 'LaTeX-mode-hook 'auto-fill-mode)

(add-hook 'LaTeX-mode-hook 'orgtbl-mode)

(provide 'my-latex)
