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
;;  - c-mode.el : a lot of hints http://opensource.apple.com/source/emacs/emacs-56/emacs/lisp/obsolete/c-mode.el
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
	;; Only ' should be used as a string delimiter.
	;; This may require further change since ''' is allowed in
	;; sablecc, analogous to what would usually be  '\'' in other languages.
	;; Currently, this will break syntax highlighting etc., so use an alternative to '''
	;; (let's face it, ''' is a bit odd)
	(modify-syntax-entry ?\' "\"" syntax-table-sub)
	(modify-syntax-entry ?\' "\"'" syntax-table-sub)
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
;; todo make sablecc--syntax-name more specific so only valid names are highlighted
(defvar sablecc--syntax-name "{[^{}]*}")
(defvar sablecc--syntax-idfirstuse
  (concat "[[:space:]\n]*"
	   sablecc--syntax-id
	   "[[:space:]\n]*\\("
	   sablecc--syntax-name
	   "\\)?[[:space:]\n]*="))
;;  package ids (split to make it a bit more readable, mimic sablecc- rule)
(defvar sablecc--syntax-packageid "[[:alpha:]][[:alnum:]]+")
(defvar sablecc--syntax-package
  (concat "Package[[:space:]]+\\("
	  sablecc--syntax-packageid
	  "\\(\\."
	  sablecc--syntax-packageid "\\)*\\)"))
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

;; constants
;;   paren indent
(defcustom sablecc-paren-indent-amount 1 "Amount by which to within parens")
;;   other indentation
(defcustom sablecc-indent-amount 2 "General indentation amount")

;; helpers/specific case tests
;;   current line is a comment ?
(defun sablecc--line-is-comment ()
  "Check if a line is inside a comment (single line or block)."
  (nth 4 (syntax-ppss)))

;;   move point to the last non-whitespace character,
(defun sablecc--point-to-last-non-whitespace ()
  "Move (point) to last non whitespace/newline character"
  (forward-char)
  (re-search-backward "[^[:space:]\n]"))

;;   the last character before (point) that is not whitespace or newline.
(defun sablecc--prev-non-whitespace ()
  "Return the last character before (point) that is not whitespace/newline."
  (progn
    (save-excursion
      (sablecc--point-to-last-non-whitespace)
      (string (char-after (point))))))

;;   point to the end of the line above (point) that is not whitespace, newline, or in a commnet
(defun sablecc--point-to-prev-non-whitespace-line-end ()
  "Move (point) to the last line end that is not (whitespace/newline) or in a comment."
  (progn
    (previous-line)
    (end-of-line)
    (sablecc--point-to-last-non-whitespace)
    (while (sablecc--line-is-comment)
      (previous-line)
      (end-of-line)
      (sablecc--point-to-last-non-whitespace))))

;;   last character that ends a line above (point): not whitespace, newline, or in comment.
(defun sablecc--prev-non-whitespace-line-end ()
  "Return the last character on a prev. line before (point) that is not whitespace/newline or a comment."
  (progn
    (save-excursion
      (sablecc--point-to-prev-non-whitespace-line-end)
      (sablecc--prev-non-whitespace))))

;;   compute paren indentation (backward scan)
(defun sablecc--prev-line-unmatched-paren-column ()
  "Get column of the last unmatched paren above the current line, exclude comments, strings."
  (progn
    (save-excursion
      (previous-line)
      (end-of-line)
      (let ((parser-state (nth 1 (parse-partial-sexp (+ (point) 1) 1))))
	(if parser-state
	    (progn
	      (goto-char parser-state)
	      (+ (current-column) sablecc-paren-indent-amount))
	  nil)))))

;;  compute indentation when we're dealing with lists
(defun sablecc--prev-line-comma-column ()
  "Get the column of the first word on the pevious line (of code) if it ends in a comma."
  (progn
    (save-excursion
      (sablecc--point-to-prev-non-whitespace-line-end)
      (forward-char)
      (if (string= (string (char-before)) ",")
	  (progn
	    (back-to-indentation)
	    (current-column))
	nil))))

;;   last nonempty/noncomment line contains only a name
(defun sablecc--prev-line-name-column ()
  "If the last nonempty/noncomment line has a name, indent to its level."
  (progn
    (save-excursion
      (previous-line)
      (while (sablecc--line-is-comment)
	(previous-line))
      (back-to-indentation)
      (if (looking-at sablecc--syntax-name)
	  (current-column)
	nil))))



;; simple cases
;;   beginning of the buffer
(defun sablecc--indent-case-begin ()
  "indent-case : the beginning of the buffer"
  (bobp))

;;   section name
(defun sablecc--indent-case-section ()
  "indent-case : line contains a section name"
  (progn
    (save-excursion
      (beginning-of-line)
      (looking-at
       (concat sablecc--syntax-sections)))))

;;   previous line ends in a semicolon
(defun sablecc--indent-case-prev-line-semicolon ()
  "indent-case : does the last nonempty line end in a semicolon"
  (string= (sablecc--prev-non-whitespace-line-end) ";"))

;;   previous nonempty line is a section name
(defun sablecc--indent-case-prev-line-section ()
  "indent-case : previous non-empty line is a section name"
  (progn
    (save-excursion
      (sablecc--point-to-prev-non-whitespace-line-end)
      (sablecc--indent-case-section))))


;;   indent-line function
(defun sablecc--indent-line ()
  "Indent the current line of a SableCC specification."
  ;; section names and the beginning of the buffer are always in column 0
  (if (or (sablecc--indent-case-begin) (sablecc--indent-case-section))
      (indent-line-to 0)
    ;; if the last line of code(!) ended in a semicolon or was a section
    ;; name, current line goes to column 2.
    (if (or (sablecc--indent-case-prev-line-semicolon) (sablecc--indent-case-prev-line-section))
	(indent-line-to 2)
      ;; if we have an unbalanced paren above this line, we indent to its level + 1
      (let ((paren-indent (sablecc--prev-line-unmatched-paren-column)))
	(if paren-indent
	    (indent-line-to paren-indent)
	  ;; if the last line ended in a comma, indent to the level of the first
	  ;; word on that line
	  (let ((comma-column (sablecc--prev-line-comma-column)))
	    (if comma-column
		(indent-line-to comma-column)
	      ;; if previous line ia a name, indent to same level
	      (let ((name-column (sablecc--prev-line-name-column)))
		(if name-column
		    (indent-line-to name-column)
		  ;; for any other case, indent line to column (+ 2 sablecc-indent-amount)
		  (indent-line-to (+ 2 sablecc-indent-amount)))))))))))

;; sablecc-mode commands
;; -----------------------------------------------------------------------------

;; vars
;;   command prefix - user settable
(defcustom sablecc-compile-commmand-prefix
  "sablecc"
  "The command prefix to use when running sablecc. If you want the jar, java -jar /path/to/sablecc.jar")
;;   default args - user settable
(defcustom sablecc-command-line-args-default
  " "
  "Default command line arguments to sablecc.")

;;   previous args
(defvar sablecc--previous-args " ")


;; functions
;;   (internal) compile a file with sablecc
(defun sablecc--compile (file-path args-string)
  "Compile  file with sablecc. Not interactive."
  ;; check if --pretty-print is in args-string, if yes provide appropriate buffer
  (let ((pretty-printing (string-match "^.*--pretty-print.*$" args-string)))
    (if pretty-printing
	(let ((pretty-buffer-name-pref (concat (file-name-sans-extension (buffer-name))
					       "_pretty."
					       (file-name-extension (buffer-name)))))
	  ;; We need to catch the output here, to enable creation of enumerated
	  ;; buffers with the same name.
	  (let ((pretty-buffer-name (buffer-name (generate-new-buffer pretty-buffer-name-pref))))
	    (with-output-to-temp-buffer "*sablecc-info*"
	      (shell-command (concat sablecc-compile-commmand-prefix
				     " "
				     args-string
				     " "
				     file-path)
			     "*sablecc-info*"
			     pretty-buffer-name))
	    (pop-to-buffer pretty-buffer-name)
	    (beginning-of-buffer)
	    (sablecc-mode)))
      (with-output-to-temp-buffer "*sablecc-info*"
	(shell-command (concat sablecc-compile-commmand-prefix
			       " "
			       args-string
			       " "
			       file-path)
		       "*sablecc-info*"
		       "*Messages*")
	(pop-to-buffer "*sablecc-info*")))))

;;   get command line arguments from the user
(defun sablecc--get-command-line-args ()
  "Get command line arguments from the user when sablecc is run. Remember them for the next run."
  (read-string "SableCC arguments : "))

;;   compile a user-specified file
(defun sablecc-compile-file ()
  "Compile a file with sablecc."
  (interactive)
  (let ((file-path (expand-file-name
		    (read-file-name "SableCC file: "))))
    (sablecc--compile file-path (sablecc--get-command-line-args))))

;; compile current buffer
(defun sablecc-compile-buffer ()
  "Compile the current buffer with sablecc."
  (interactive)
  (let ((file-path (expand-file-name (buffer-file-name))))
    (if (buffer-modified-p)
	(if ((y-or-n-p (format "Save file %s?" file-path)))
	    (save-buffer)))
    (sablecc--compile file-path (sablecc--get-command-line-args))))



;; keybindings
(defvar sablecc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c \C-c" 'sablecc-compile-buffer)
    (define-key map "\C-c \C-f" 'sablecc-compile-file)
    map)
  "Keymap for SableCC major mode")


;; sablecc-mode final definition
;; -----------------------------------------------------------------------------
;; define mode
(defun sablecc-mode ()
  "A major mode for editing SableCC grammar specifications."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table sablecc-syntax-table)
  (use-local-map sablecc-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(sablecc--font-lock))
  (set (make-local-variable 'indent-line-function) 'sablecc--indent-line)
  (setq major-mode 'sablecc-mode)
  (setq mode-name "SableCC")
  (run-hooks 'sablecc-mode-hook))
;; autoload for .sablecc or .sableccN where N is some digit
;; (at least it will load for whatever version the user is using)
(add-to-list 'auto-mode-alist '("\\.sablecc[[:digit:]]*\\'" . sablecc-mode))
(provide 'sablecc-mode)
