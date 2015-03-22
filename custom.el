(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-output-view-style
   (quote
    (("^dvi$"
      ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$")
      "%(o?)dvips -t landscape %d -o && gv %f")
     ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f")
     ("^dvi$"
      ("^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "^landscape$")
      "%(o?)xdvi %dS -paper a4r -s 0 %d")
     ("^dvi$" "^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "%(o?)xdvi %dS -paper a4 %d")
     ("^dvi$"
      ("^\\(?:a5\\(?:comb\\|paper\\)\\)$" "^landscape$")
      "%(o?)xdvi %dS -paper a5r -s 0 %d")
     ("^dvi$" "^\\(?:a5\\(?:comb\\|paper\\)\\)$" "%(o?)xdvi %dS -paper a5 %d")
     ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d")
     ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d")
     ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d")
     ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d")
     ("^dvi$" "." "%(o?)xdvi %dS %d")
     ("^pdf$" "." "zathura %o --page %(outpage)")
     ("^html?$" "." "netscape %o"))))
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#c5c8c6" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#1d1f21"))
 '(ansi-term-color-vector
   [unspecified "#14191f" "#d15120" "#81af34" "#deae3e" "#7e9fc9" "#a878b5" "#7e9fc9" "#dcdddd"] t)
 '(company-auto-complete t)
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "b1471d88b39cad028bd621ae7ae1e8e3e3fca2c973f0dfe3fd6658c194a542ff" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "9370aeac615012366188359cb05011aea721c73e1cb194798bc18576025cabeb" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "57072d797dc09fcf563051a85a29d6a51d6f2b1a602e029c35b05c30df319b2a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "8eef22cd6c122530722104b7c82bc8cdbb690a4ccdd95c5ceec4f3efa5d654f5" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(ess-swv-pdflatex-commands (quote ("texi2pdf" "pdflatex" "make" "xelatex")))
 '(fci-rule-character-color "#192028")
 '(fci-rule-color "#282a2e")
 '(fill-column 80)
 '(flycheck-clang-language-standard "c++11")
 '(flycheck-haskell-ghc-executable "/usr/bin/ghc")
 '(flycheck-haskell-hlint-executable "/usr/bin/cabal exec hlint")
 '(gdb-many-windows t)
 '(global-semantic-decoration-mode t)
 '(global-semantic-highlight-func-mode t)
 '(global-semantic-idle-summary-mode t)
 '(ibuffer-saved-filter-groups
   (quote
    (("groups"
      ("nasa-filer"
       (filename . "/home/nathan/devel/nasa/filer/")))
     ("default"
      ("dired"
       (mode . dired-mode))
      ("haskell"
       (mode . haskell-mode))
      ("python"
       (mode . python-mode))
      ("notes"
       (or
        (name . "^\\*Calendar\\*$")
        (name . "^diary$")
        (mode . org-mode)))
      ("*buffer*"
       (name . "\\*.*\\*"))))))
 '(ibuffer-saved-filters
   (quote
    (("builtin-buffers"
      ((name . "^\\*.*\\*$")))
     ("nasa-filer"
      ((filename . "/home/nathan/devel/nasa/filer/")))
     ("gnus"
      ((or
        (mode . message-mode)
        (mode . mail-mode)
        (mode . gnus-group-mode)
        (mode . gnus-summary-mode)
        (mode . gnus-article-mode))))
     ("programming"
      ((or
        (mode . emacs-lisp-mode)
        (mode . cperl-mode)
        (mode . c-mode)
        (mode . java-mode)
        (mode . idl-mode)
        (mode . lisp-mode)))))))
 '(ido-everywhere t)
 '(latex-run-command "pdflatex")
 '(org-agenda-files (quote ("~/org/agenda.org")))
 '(org-columns-ellipses "…")
 '(org-ellipsis "…")
 '(org-footnote-auto-adjust t)
 '(org-footnote-auto-label nil)
 '(org-startup-folded (quote content))
 '(org-startup-truncated nil)
 '(org-use-sub-superscripts (quote {}))
 '(preview-TeX-style-dir "/home/nathan/.emacs.d/elpa/auctex-11.87.2/latex" t)
 '(python-indent-guess-indent-offset nil)
 '(python-indent-offset 0)
 '(semantic-mode t)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(use-file-dialog nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil)
 '(verilog-auto-delete-trailing-whitespace t)
 '(verilog-auto-lineup (quote all))
 '(verilog-auto-newline nil)
 '(verilog-case-indent 4)
 '(verilog-cexp-indent 4)
 '(verilog-indent-begin-after-if nil)
 '(verilog-indent-level 4)
 '(verilog-indent-level-behavioral 4)
 '(verilog-indent-level-declaration 4)
 '(verilog-indent-level-module 4)
 '(whitespace-line-column 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-directory ((t (:inherit font-lock-function-name-face :weight bold))))
 '(flycheck-info ((t (:underline nil))))
 '(flycheck-warning ((t (:underline nil))))
 '(font-lock-string-face ((t (:foreground "#CC9393"))))
 '(helm-selection ((t (:background "#3e4446" :underline nil))))
 '(helm-source-header ((t nil)))
 '(helm-swoop-target-line-block-face ((t (:inherit helm-swoop-target-line-face))))
 '(helm-swoop-target-line-face ((t (:background "#2B2B2B" :foreground "#F0DFAF"))))
 '(helm-swoop-target-word-face ((t (:background "#2B2B2B" :foreground "#F0DFAF"))))
 '(highlight-indentation-face ((t (:background "#3E3E3E"))))
 '(info-title-1 ((t (:inherit info-title-2 :height 1.1))))
 '(info-title-2 ((t (:inherit info-title-3 :height 0.9))))
 '(linum ((t (:background "#4f4f4f" :foreground "#656555"))))
 '(mode-line-highlight ((t (:box nil))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#404045" :foreground "gray60" :inverse-video nil :box nil :weight light))))
 '(org-block-background ((t (:background "gray23"))))
 '(org-block-begin-line ((t (:inherit org-meta-line :background "gray21"))))
 '(org-block-end-line ((t (:inherit org-meta-line :background "gray21"))))
 '(show-paren-match ((t (:background "#4f4f4f" :weight normal))))
 '(show-paren-mismatch ((t (:background "#dca3a3" :foreground "#6f6f6f" :weight bold))))
 '(tooltip ((t (:inherit default :background "lightyellow" :foreground "black"))))
 '(trailing-whitespace ((t nil)))
 '(variable-pitch ((t (:height 1.0 :family "Courier"))))
 '(whitespace-line ((t nil))))
