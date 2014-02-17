;;Let's define some utility functions
(defun mappend (fun list)
  "classic flat map"
  (apply #'append (mapcar fun list)))

(defun string->list (obj)
  "coerce if string, wrap to list otherwise"
  (typecase obj (string (coerce obj 'list)) (t (list obj))))

(defun list-split (expr sep &optional (acc1 nil) (acc2 nil))
  "receives list with separator and return list of lists"
  (if (null expr)
      (reverse (cons (reverse acc1) acc2))
      (if (equal (first expr) sep)
	  (list-split (rest expr) sep nil (cons (reverse acc1) acc2))
	  (list-split (rest expr) sep (cons (first expr) acc1) acc2))))

(defun flat->list (flat &optional (acc nil))
  "receives list with separators { } and return tree with the same structure"
  (cond ((eq (car flat) '{ )
	 (let ((res (flat->list (cdr flat))))
	   (flat->list (car res) (cons (cdr res) acc))))
	((eq (car flat) '} ) (cons (cdr flat) (reverse acc)))
	(flat (flat->list (cdr flat) (cons (car flat) acc)))
	(t (reverse acc))))

;;Structure and functions to define grammar
(defvar *bnf-grammar* '() "grammar is stored as associative list")

(defun bnf-expr (expr)
  (remove-if #'null (list-split (mappend #'string->list expr) '/ )))

(defun bnf-term-add (name expr)
  (push (cons name (bnf-expr expr)) *bnf-grammar*))

(defun bnf-term-lookup (term)
  (cdr (assoc term *bnf-grammar*)))

(defmacro ::= (name &rest body)
  "Macro to define grammar in plain BNF notation"
  `(bnf-term-add (quote ,name) (quote ,body)))

;; Grammar Matcher / Parser
(defun bnf-match-any (opts str &optional (acc nil))
  (mappend (lambda (opt) (bnf-match-exact opt str acc)) opts))

(defun bnf-match-exact (opts str &optional (acc nil))
  (if (eq (car opts) '} )
      (bnf-match-exact (cdr opts) str (cons '} acc))
      (if (null opts)
	  (unless str (list (reverse acc)))
	  (when str
	    (let ((term (first opts)))
	      (typecase term
		(character
		 (when (char-equal term (car str))
		   (bnf-match-exact (cdr opts) (cdr str) acc)))
		(symbol
		 (bnf-match-any (mapcar (lambda (x) (append x '( } ) (cdr opts))) (bnf-term-lookup term)) str (cons '{ (cons term acc))))))))))

(defun match (term str)
  (mapcar #'flat->list (bnf-match-exact (list term) (coerce str 'list))))

;;Let's define simplified JSON grammar

(setq *bnf-grammar* nil)
(::= SIGN "-")
(::= COMMA ",")
(::= COLON ":")
(::= DOUBLE-QUOTE "\"" )
(::= WHITESPACE " " / " " WHITESPACE)
(::= ZERO "0")
(::= POSITIVE-DIGIT "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9")
(::= DIGIT ZERO / POSITIVE-DIGIT)
(::= DIGIT-SEQUENCE DIGIT / DIGIT DIGIT-SEQUENCE)
(::= LETTER "a" / "b" / "c" / "d" / "e" / "f" / "g" / "h" / "i" / "j" /
     "k" / "l" / "m" / "n" / "o" / "p" / "q" / "r" / "s" / "t" / "u" /
     "v" / "w" / "x" / "y" / "z")
(::= CHARACTER LETTER / DIGIT / COMMA / COLON)
(::= CHARACTER-SEQUENCE CHARACTER / CHARACTER CHARACTER-SEQUENCE)
(::= POSITIVE-NUMBER POSITIVE-DIGIT / POSITIVE-DIGIT DIGIT-SEQUENCE)
(::= NUMBER ZERO / POSITIVE-NUMBER)
(::= INTEGER NUMBER / SIGN POSITIVE-NUMBER / SIGN WHITESPACE POSITIVE-NUMBER)
(::= BOOLEAN "true" / "false")
(::= STRING DOUBLE-QUOTE CHARACTER-SEQUENCE DOUBLE-QUOTE)
(::= JSON-OBJECT INTEGER / BOOLEAN / STRING / LIST / OBJECT)
(::= EMPTY-LIST "[]" / "[" WHITESPACE "]")
(::= EMPTY-OBJECT "{}" / "{" WHITESPACE "}")
(::= LIST EMPTY-LIST / "[" COMMA-SEPARATED-ANY-SEQUENCE "]")
(::= OBJECT EMPTY-OBJECT / "{" NAME-VALUE-SEQUENCE "}")
(::= NAME-VALUE-PAIR STRING COLON ANY / STRING WHITESPACE COLON ANY / WHITESPACE STRING COLON ANY / WHITESPACE STRING WHITESPACE COLON ANY)
(::= PADDED-JSON-OBJECT JSON-OBJECT / JSON-OBJECT WHITESPACE / WHITESPACE JSON-OBJECT / WHITESPACE JSON-OBJECT WHITESPACE)
(::= COMMA-SEPARATED-ANY-SEQUENCE ANY / ANY COMMA COMMA-SEPARATED-ANY-SEQUENCE)
(::= NAME-VALUE-SEQUENCE NAME-VALUE-PAIR / NAME-VALUE-PAIR COMMA NAME-VALUE-SEQUENCE)
(::= ANY PADDED-JSON-OBJECT)

;; Test
(match 'ANY "  { \"x\" : [1,-2,true,{},3]}")