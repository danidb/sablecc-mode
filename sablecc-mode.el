;; Copyright (C) 2016 by Daniel Del Balso
;;
;; An emacs major mode for editing sablecc grammars.
;; SableCC (by Etienne Gagnon : http://www.sablecc.org/)
;;
;; Thanks/Resources:
;;  - Xah Lee for putting so many great emacs tutorials online
;;  - The author(s) of https://www.emacswiki.org/emacs/ModeTutorial
;;    and the article it was derived from
;;  - http://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Flags.html
;;  - https://www.emacswiki.org/emacs-test/SampleMode
;;
;; A SableCC grammar is defined (in SableCC Syntax) here:
;;    http://sablecc.sourceforge.net/grammars/sablecc-2x.sablecc.html
;; and all the expression etc. below are derived from this. The contents
;; of this mode definition may need to be modified to reflect different
;; versions of SableCC.

(defvar sablecc-mode-hook nil)

;; syntax-highlighting
(defvar sablecc-syntax-table nil "SableCC syntax table.")
;;   comments
(setq sablecc-syntax-table
      (let ((syntax-table-sub (make-syntax-table)))
	;; SableCC uses java-style comments.
	;; //
        (modify-syntax-entry ?/ ". 124b" syntax-table-sub)
        (modify-syntax-entry ?* ". 23" syntax-table-sub)
	(modify-syntax-entry ?\n "> b" syntax-table-sub)
	;; /* .... */
        syntax-table-sub))
;; add underscores as word constituents, prevents odd highlighting
(modify-syntax-entry ?_ "w" pipelang-syntax-table)

;; keywords/syntax definitions
;;   literals
(defconst sablecc-syntax-hex (regexp-opt '("0[xX][[:xdigit:]]")))
(defconst sablecc-syntax-string (regexp-opt '("[\\(\".*\"\\)\\('.*'\\)]")))
;;  constructs/keywords
;;    sections
(defconst sablecc-syntax-sections
  '(concat "\\<"
	   (concat (regexp-opt
		    '("Package"
		      "States"
		      "Helpers"
		      "Tokens"
		      "Ignored Tokens"
		      "Productions")
		    t)
		   "\\>")))
;;  specifiers (T. and P.)
(defconst sablecc-syntax-specifiers
  (regexp-opt '("\\(T\\.\\)\\|\\(P\\.\\)")))
;;  identifiers, names
(defconst sablecc-syntax-id
  (regexp-opt '("\\([a-z]+[a-z0-9_]*[a-z0-9]\\)")))
(defconst sablecc-syntax-name
  (concat "{" (concat sablecc-syntax-id "}")))
(defconst sablecc-syntax-idfirstuse
  (concat"[[:space:]]*" (concat sablecc-syntax-id "[[:space:]]*=")))
;;  package ids (split to make it a bit more readable, mimic sablecc rule)
(defconst sablecc-syntax-packageid (regexp-opt '("[[:alpha:]][[:alnum:]]+")))
(defconst sablecc-syntax-package
  (concat "Package[[:space:]]+\\("
	  (concat sablecc-syntax-packageid
		  (concat "\\(\\."
			  (concat sablecc-syntax-packageid "\\)*\\)")))))
;; add to font-lock

;; TODO first group match not always appropriate
(defvar sablecc-font-lock-keywords
  '((sablecc-syntax-packageid (1 font-lock-function-name-face)),
    (sablecc-syntax-idfirstuse (. font-lock-variable-name-face)),
    (sablecc-syntax-name (. font-lock-preprocessor-face)),
    (sablecc-syntax-specifiers (1 font-lock-builtin-face))
    (sablecc-syntax-sections (. font-lock-builtin-face)),
    (sablecc-syntax-hex (. font-lock-constant-face)))
    (sabeecc-syntax-string (. font-lock-string-face))))


;; keybindings
(defvar sablecc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c \C-c" 'sablecc-compile)
    map)
  "Keymap for SableCC major mode")

;; sablecc-commands

;; define mode
(define-derived-mode sablecc-mode fundamental-mode
  "SableCC"
  "A major mode for editing SableCC grammar specifications."
  (setq font-lock-defaults '((sablecc-font-lock-keywords)))
  (set-syntax-table sablecc-syntax-table)
  (setq comment-start "// ")
  (setq comment-end   ""))
;; autoload
(add-to-list 'auto-mode-alist '("\\.sablecc\\'" . sablecc-mode))
(add-to-list 'auto-mode-alist '("\\.grammar\\'" . sablecc-mode))
(provide 'sablecc-mode)
