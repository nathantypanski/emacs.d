;;; my-proverif.el --- ProVerif support via tree-sitter -*- lexical-binding: t; -*-

;;; Commentary:

;; Font-lock only.  There is no ProVerif major mode in the wild, so
;; `proverif-ts-mode' is defined here on top of the tree-sitter grammar
;; from <https://codeberg.org/maribu/tree-sitter-proverif>, which nix
;; builds for us (see nix/pkgs/tree-sitter-proverif.nix).  The queries
;; below are translated from that grammar's queries/highlights.scm.
;;
;; No indentation or imenu support: indentation stays whatever prog-mode
;; gives you.

;;; Code:

(require 'treesit)

(defvar my-proverif-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; ProVerif comments are (* nestable *).
    (modify-syntax-entry ?\( "()1n" table)
    (modify-syntax-entry ?\) ")(4n" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?' "_" table)
    (dolist (char '(?= ?< ?> ?+ ?- ?& ?| ?!))
      (modify-syntax-entry char "." table))
    table)
  "Syntax table for `proverif-ts-mode'.")

(defvar my-proverif-ts-font-lock-rules
  '(:language proverif
    :feature comment
    ((comment) @font-lock-comment-face
     ;; CryptoVerif-only opaque blocks: not our dialect, dim them out.
     [(proof) (def)] @font-lock-comment-face)

    :language proverif
    :feature string
    ((string) @font-lock-string-face)

    :language proverif
    :feature keyword
    ([ "among" "axiom" "choice" "clauses" "const" "def" "elimtrue" "else"
       "equation" "equivalence" "event" "expand" "forall" "foreach" "free"
       "fun" "get" "if" "inj-event" "insert" "lemma" "let" "letfun"
       "letproba" "new" "noninterf" "noselect" "not" "nounif" "otherwise"
       "param" "phase" "pred" "proba" "process" "proof" "public_vars"
       "putbegin" "query" "reduc" "restriction" "secret" "select" "set"
       "suchthat" "sync" "table" "then" "type" "weaksecret" "yield" ]
     @font-lock-keyword-face
     ;; "channel" doubles as a type name, so only flag the declaration form.
     (decl_channel "channel" @font-lock-keyword-face)
     ;; "in" is a keyword in "let ... in" but the input builtin otherwise.
     (process_let "in" @font-lock-keyword-face))

    :language proverif
    :feature constant
    ([ "true" "false" ] @font-lock-constant-face
     "fail" @font-lock-constant-face
     (process_final_item "0" @font-lock-constant-face))

    :language proverif
    :feature number
    ([ (nat) (int) ] @font-lock-number-face)

    :language proverif
    :feature type
    ((type_def name: (ident) @font-lock-type-face)
     (decl_table name: (ident) @font-lock-type-face)
     (typeid) @font-lock-type-face
     (typeid (ident) @font-lock-type-face))

    :language proverif
    :feature definition
    ((function_def name: (ident) @font-lock-function-name-face)
     (function_macro_def name: (ident) @font-lock-function-name-face)
     (decl_pred name: (ident) @font-lock-function-name-face)
     (decl_event name: (ident) @font-lock-function-name-face)
     (decl_process_macro name: (ident) @font-lock-function-name-face))

    :language proverif
    :feature function
    ((term_function_call name: (ident) @font-lock-function-call-face)
     (pterm_function_call name: (ident) @font-lock-function-call-face)
     (gterm_function_call name: (ident) @font-lock-function-call-face)
     (gformat_function_call name: (ident) @font-lock-function-call-face)
     (process_function_call name: (ident) @font-lock-function-call-face)
     (process_input "in" @font-lock-builtin-face)
     (process_output "out" @font-lock-builtin-face))

    :language proverif
    :feature property
    ((option) @font-lock-preprocessor-face
     (nounifoption) @font-lock-preprocessor-face)

    :language proverif
    :feature operator
    ([ "=" "<>" "<=" ">=" "<" ">" "+" "-" "==>" "<-" "<-R" "->" "<->"
       "<=>" "||" "&&" "|" "!" ]
     @font-lock-operator-face)

    :language proverif
    :feature delimiter
    ([ "." "," ";" ":" ] @font-lock-delimiter-face
     [ "(" ")" "[" "]" "{" "}" ] @font-lock-bracket-face)

    :language proverif
    :feature variable
    ((decl_process_macro (typedecl (ident) @font-lock-variable-name-face))
     (term_function_call (term (ident) @font-lock-variable-use-face))
     (pterm_function_call (pterm (ident) @font-lock-variable-use-face))
     (gterm_function_call (gterm (ident) @font-lock-variable-use-face))
     (gformat_function_call (gformat (ident) @font-lock-variable-use-face))
     (process_function_call (pterm (ident) @font-lock-variable-use-face))
     (process_input (pterm (ident) @font-lock-variable-use-face))
     (process_input (pattern (ident) @font-lock-variable-name-face))
     (process_input (pattern (pattern (ident) @font-lock-variable-name-face)))
     (process_output (pterm (ident) @font-lock-variable-use-face))
     (process_output (pterm (pterm (ident) @font-lock-variable-use-face))))

    ;; Fallback, deliberately last so the rules above win.
    :language proverif
    :feature variable-fallback
    ((ident) @font-lock-variable-use-face))
  "Tree-sitter font-lock settings for `proverif-ts-mode'.")

;;;###autoload
(define-derived-mode proverif-ts-mode prog-mode "ProVerif"
  "Major mode for ProVerif's typed pi calculus, powered by tree-sitter."
  :syntax-table my-proverif-ts-mode-syntax-table
  (unless (treesit-ready-p 'proverif)
    (error "Tree-sitter grammar for ProVerif is not available"))

  (setq-local comment-start "(* ")
  (setq-local comment-end " *)")
  (setq-local comment-start-skip "(\\*+[ \t]*")

  (treesit-parser-create 'proverif)
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules my-proverif-ts-font-lock-rules))
  (setq-local treesit-font-lock-feature-list
              '((comment string)
                (keyword constant type definition)
                (function number property)
                (operator delimiter variable variable-fallback)))
  (treesit-major-mode-setup))

;; The mode lives in this file, so there is nothing for use-package to load;
;; just claim the extensions when nix actually gave us the grammar.
(when (and (treesit-available-p) (treesit-language-available-p 'proverif))
  (add-to-list 'auto-mode-alist '("\\.pv\\'" . proverif-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.pvl\\'" . proverif-ts-mode)))

(provide 'my-proverif)
;;; my-proverif.el ends here
