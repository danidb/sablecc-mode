;; Copyright (C) 2016 by Daniel Del Balso
;;
;; An emacs major mode for editing SableCC grammars.
;;
;;  (SableCC was developed by Etienne M. Gagnon : http://www.sablecc.org/)
;;
;; Thanks/Resources:
;;  - Xah Lee for putting so many great emacs tutorials online
;;  - The author(s) of https://www.emacswiki.org/emacs/ModeTutorial
;;    and the article it was derived from
;;  - http://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Flags.html
;;  - https://www.emacswiki.org/emacs-test/SampleMode
;;
;; A SableCC grammar for SableCC 2 is defined (in SableCC Syntax) here:
;;    http://sablecc.sourceforge.net/grammars/sablecc-2x.sablecc.html
;; and all the expressions etc. below are derived from this. The contents
;; of this mode definition may need to be modified to reflect different
;; versions of SableCC. Test grammars are also available in the SableCC
;; repo https://github.com/SableCC/sablec
;;
;; This software should conform to the Major Mode Conventions described
;; in the emacs manual. Suggestions, comments, ideas, contribution
;; are always welcome.
;;
;; The sablecc- prefix is used whenever lexical scoping does not apply.

(defvar sablecc-mode-hook nil)

;; definitions/settings
;; ----------------------------------------------------------------------------


;; syntax (highlighting/font-lock, keyword definitions etc.)
;; -----------------------------------------------------------------------------

;; define the syntax table
(defvar sablecc-syntax-table
      (let ((syntax-table-sub (make-syntax-table)))
	;; SableCC uses java-style comments.
	;; Follows the C++ example in the major mode tutorial
	;; //. /*...*/
        (modify-syntax-entry ?/ ". 124b" syntax-table-sub)
        (modify-syntax-entry ?* ". 23" syntax-table-sub)
	(modify-syntax-entry ?\n "> b" syntax-table-sub)
	;; add underscores as word constituents, prevents odd highlighting
	(modify-syntax-entry ?_ "w" syntax-table-sub)
	;; only ' should be used as a string delimiter.
	(modify-syntax-entry ?\' "\"" syntax-table-sub)
	(modify-syntax-entry ?\" "w" syntax-table-sub)
	;; there is no magic escape character
	(modify-syntax-entry ?\\ "w" syntax-table-sub)
        syntax-table-sub)
      "SableCC syntax table")

;; keywords/syntax definitions
;;   literals
(defvar sablecc--syntax-hex "0[xX][[:xdigit:]]+")
;;  constructs/keywords
;;    sections
(defvar sablecc--syntax-sections-keywords
      '("Package"
	"States"
	"Helpers"
	"Tokens"
	"Ignored Tokens"
	"Productions"))
(defvar sablecc--syntax-sections (regexp-opt sablecc--syntax-sections-keywords 'words))
;;  specifiers (T. and P.)
(defvar sablecc--syntax-specifiers "\\(T\\.\\)\\|\\(P\\.\\)")
;;  identifiers, names
(defvar sablecc--syntax-id "\\([a-z]+[a-z0-9_]*[a-z0-9]\\)")
(defvar sablecc--syntax-name
      (concat "{[[:space:]]*"
	      (concat sablecc--syntax-id
		      (concat "\\([,\\(->\\)][[:space:]]*"
			      (concat sablecc--syntax-id "\\)*}")))))
(defvar sablecc--syntax-idfirstuse
      (concat "[[:space:]]*" (concat sablecc--syntax-id "[[:space:]]*=")))
;;  package ids (split to make it a bit more readable, mimic sablecc- rule)
(defvar sablecc--syntax-packageid "[[:alpha:]][[:alnum:]]+")
(defvar sablecc--syntax-package
      (concat "Package[[:space:]]+\\("
	      (concat sablecc--syntax-packageid
		      (concat "\\(\\."
			      (concat sablecc--syntax-packageid "\\)*\\)")))))
;; define font-lock
(defvar sablecc--font-lock
  `(( ,sablecc--syntax-package 1 font-lock-builtin-face)
    ( ,sablecc--syntax-idfirstuse 1 font-lock-variable-name-face)
    ( ,sablecc--syntax-name . font-lock-type-face)
    ( ,sablecc--syntax-specifiers . font-lock-function-name-face)
    ( ,sablecc--syntax-sections . font-lock-builtin-face)
    ( ,sablecc--syntax-hex . font-lock-constant-face)))



;; indentation
;; -----------------------------------------------------------------------------


;; helpers
;;   current line is a comment ?
(defun sablecc--line-is-comment ()
  "Check if a line is inside a comment (single line or block)."
  (nth 4 (syntax-ppss)))

;;   move point to the last non-whitespace character,
(defun sablecc--point-to-last-non-whitespace ()
  "Move (point) to last non whitespace/newline character"
  (re-search-backward "[^[:space:]\n]"))

;;   the last character before (point) that is not whitespace or newline.
(defun sablecc--prev-non-whitespace ()
  "Return the last character before (point) that is not whitespace/newline."
  (progn
    (save-excursion
      (sablecc--point-to-last-non-whitespace)
      (string (char-after (point))))))

;;   count unmatched brackets/parens/braces before (point) up to a ;
;(defun sablecc--prev-unmatched-paren ()
;  "Count previous open parens."
;  (progn
;    (save-excursion
;      (

;;   last character that ends a line above (point): not whitespace, newline, or in comment.
(defun sablecc--prev-non-whitespace-line-end ()
  "Return the last character on a prev. line before (point) that is not whitespace/newline."
  (progn
    (save-excursion
      (beginning-of-line)
      (previous-line)
      (while (sablecc--line-is-comment) (previous-line))
      (sablecc--prev-non-whitespace))))

;; indentation case tests
;;   beginning of the buffer
(defun sablecc--indent-case-begin ()
  "indent-case : the beginning of the buffer"
  (bobp))

;;   section name
(defun sablecc--indent-case-section ()
  "indent-case : line contains a section name"
  (looking-at
   (concat "^[ \t]*\\(" (concat (mapconcat 'identity sablecc--syntax-sections "\\|") "\\)"))))

;;   previous line ends in a semicolon
(defun sablecc--indent-case-prev-line-semicolon ()
  "indent-case : does the last nonempty line end in a semicolon"
  (string= (sablecc--prev-non-whitespace-line-end) ";"))

;;   previous nonempty line is a section name
(defun sablecc--indent-case-prev-line-section ()
  "indent-case : previous non-empty line is a section name"
  (progn
    (save-excursion
      (beginning-of-line)
      (sablecc-point-to-last-non-whitespace)
      (sablecc-indent-case-section))))



;;   indent-line function
;(defun salbecc-indent-line ()
;  "Indent the current line of a SableCC specification."
;  (interactive)
;  (beginning-of-line)
;  (let (
;  (if (or (sablecc-indent-case-begin) (sablecc-indent-case-section))
;      (indent-line-to 0)
;    (if (or (sablecc-prev-line-semicolon) (sablecc-prev-line-section))
;	(indent-line-to 2)
;      ((let
;







;; keybindings
(defvar sablecc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c \C-c" 'sablecc-compile)
    map)
  "Keymap for SableCC major mode")

;; sablecc-mode commands
;; -----------------------------------------------------------------------------



;; sablecc-mode final definition
;; -----------------------------------------------------------------------------
;; define mode
(defun sablecc-mode ()
  "A major mode for editing SableCC grammar specifications."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table sablecc-syntax-table)
  (use-local-map sablecc-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(sablecc-font-lock))
  (setq major-mode 'sablecc-mode)
  (setq mode-name "SableCC")
  (run-hooks 'sablecc-mode-hook))
;; autoload for .sablecc or .sableccN where N is some digit
;; (at least it will load for whatever version the user is using)
(add-to-list 'auto-mode-alist '("\\.sablecc[[:digit:]]*\\'" . sablecc-mode))
(provide 'sablecc-mode)
