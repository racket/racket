; Module header is generated automatically
#cs(module xpath-parser mzscheme
(require (lib "string.ss" "srfi/13"))
(require (lib "ssax.ss" "web-server/tmp/ssax"))
(require "sxpathlib.ss")
(require "sxml-tools.ss")

;; XPath/XPointer grammar parser.
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

;=========================================================================
; Parser parameterization
; For building a specific XPath/XPointer implementation, grammar parser is to
; be parameterized
;  txp-params ::= (listof  txp-param )
;  txp-param ::= (list  param-name  param-value  [parameterized-func] )
;  parameterized-func is optional
; Each parser-param generally specifies the parser result for the single
; XPath/XPointer grammar rule

; Given param-name, returns the corresponding lambda
(define (txp:param-value param-name txp-params)
  (cond
    ((assq param-name txp-params)
     => cadr)
    (else
     (display "Parameter unspecified: ")
     (display param-name)
     0  ; this would cause program termination
     )))


;=========================================================================
; Errors handling
; There are 2 kinds of errors: syntactic errors and semantic errors
; - Syntactic error is raised when the location path (fragment identifier)
;   doesn't satisfy XPath (XPointer) grammar. Syntactic error is discovered
;   and raised by the parser.
; - Semantic error can be raised by the specific parser parametrization

; Whether a parser returns an error
(define (txp:error? obj)
  (or (eq? obj 'txp:parser-error)
      (eq? obj 'txp:semantic-error)))

;-------------------------------------------------
; Syntactic error (also called a parser error)

(define (sxml:xpointer-parse-error . text)
  (apply cerr
         (append (list "XPath/XPointer parser error: ") text (list nl)))
  #f)

; A warning message for grammar features which are not supported by this
; implementation
(define (sxml:xpointer-parse-warning . text)
  (apply cerr (append (list "XPointer parser warning: ") text (list nl))))


;-------------------------------------------------
; Semantic error
; To signal the parser about the semantic error, the specific parametrization
; is to return the symbol  'txp:semantic-error

(define (txp:semantic-errs-detected? . res-list)
  (not (null?
        (filter
         (lambda (res) (eq? res 'txp:semantic-error))
         res-list))))

; Constructed specific parsers may wish to use this function
(define (txp:signal-semantic-error . text)
  (apply cerr
         (append (list "XPath/XPointer semantic error: ") text (list nl)))
  'txp:semantic-error)


;=========================================================================
; Low level parsing functions
; XPath location path (XPointer fragment identifier) is represented as a list
; of chars

; A list of whitespace characters
(define sxml:whitespace '(#\space #\return #\newline #\tab))

; A sxml:whitespace or () <> [] : / + * , = | ! " ' @ $
(define sxml:delimiter (append sxml:whitespace
                              '(#\( #\) #\< #\> #\[ #\] #\: #\/ #\+ 
                                #\* #\, #\= #\| #\! #\" #\' #\@ #\$)))

; A list of characters a NCName cannot start with
(define (sxml:non-first? ch)
  (or (char-numeric? ch)
      (memv ch sxml:delimiter) 
      (memv ch '(#\. #\-))))

; The function reads a whitespace , production [3] (S) in XML Rec.
;  path - xpointer path string as a list of chars 
; It returns a new path
(define (sxml:skip-ws path)
  (if (or (null? path)
	  (not (memv (car path) sxml:whitespace)))
    path
    (sxml:skip-ws (cdr path))))

; Asserts that the path is over, possibly with trailing whitespace symbols at
; the end. Returns the boolean value - whether assertion passes. If assertion
; fails, signals an error message
(define (sxml:assert-end-of-path path)
  (let ((path (sxml:skip-ws path)))
    (or 
     (null? path)
     (begin
       (sxml:xpointer-parse-error "unexpected - \"" (list->string path) "\"")
       #f))))


;------------------------------------------------
; These two functions read expected information from the path

; Whether the path begins with a 'str' (starting whitespaces are ignored)
;  str - a string to match
;  path - an xpointer path represented as a list of chars
;  char-list - an optional argument. If this argument is supplied, a 'str'
; pattern must be followed by a character from a 'char-list'
; If 'str' is really in the beginning of path, a new path is returned
; Otherwise, function returns #f (path remains unchanged)
(define (sxml:parse-check str path . char-list)
  (let loop ((lst (string->list str)) 
             (p (sxml:skip-ws path)))
    (cond
      ((null? lst)
       (if
        (or (null? p) (null? char-list) (memv (car p) (car char-list)))
        p
        #f))
      ((null? p) #f)
      ((char=? (car lst) (car p))
       (loop (cdr lst) (cdr p)))
      (else #f))))

; Checks whether the PATH starts with a sequence of strings (possibly
; separated by a whitespace) from STR-SEQ
; Returns a new PATH (match successful) or #f (otherwise)
(define (sxml:parse-check-sequence str-seq path . char-list)
  (let ((char-list (if (null? char-list) #f (car char-list))))
    (let loop ((str-seq str-seq)
               (path path))
      (cond
        ((null? str-seq) path)  ; successful match
        ((if char-list
             (sxml:parse-check (car str-seq) path char-list)
             (sxml:parse-check (car str-seq) path))
         => (lambda (new-path)
              (loop (cdr str-seq) new-path)))
        (else #f)))))  ; unsuccessful match

; Similar to the 'parse-check' function. But this function also has a side
; effect. It displays an error message if the 'str' doesn't match the beginning
; of 'path'.
(define (sxml:parse-assert str path)
  (let loop ((lst (string->list str)) 
	     (p (sxml:skip-ws path)))
    (cond
      ((null? lst) p)
      ((null? p) 
       (sxml:xpointer-parse-error 
        "unexpected end of XPointer path. "
        "Expected - \"" str "\", given - \"" (list->string path) "\""))
      ((char=? (car lst) (car p)) (loop (cdr lst) (cdr p)))
      (else
       (sxml:xpointer-parse-error
        "expected - \"" str "\", given - \"" (list->string path) "\"")))))

             
;------------------------------------------------
; NCName readers

; Reads a NCName, taking into account that whitespaces and characters:
; ( ) < > [ ] : / + * , = | ! " ' @ $
; may not be used in it.
; Moreover, its first character can't be: . - or a digit
; The result:  (list  ncname  new-path)
;          or  #f
;  ncname - NCName represented as a string
; If there is no NCName in the current position of the path, then an error 
; message is displayed and #f is returned
(define (sxml:parse-ncname path)
  (let((path (sxml:skip-ws path)))
    (cond
      ((null? path) 
       (sxml:xpointer-parse-error
        "unexpected end of XPointer path. Expected - NCName"))
      ((sxml:non-first? (car path))
       (sxml:xpointer-parse-error
        "expected - NCName instead of " (car path)))
      (else
       (let loop ((ncname (list (car path)))
                  (path (cdr path)))
         (cond
           ((null? path) (list (list->string (reverse ncname)) path))
           ((memv (car path) sxml:delimiter)           
            (list (list->string (reverse ncname)) path))
           (else (loop (cons (car path) ncname) (cdr path)))))))))

; Reads a Name production. It is similar to a 'parse-ncname' function.
; The only difference is that #\: is allowed within a Name
(define (sxml:parse-name path)
  (let ((path (sxml:skip-ws path)))
    (cond
      ((null? path)
       (sxml:xpointer-parse-error
	 "unexpected end of XPointer path. Expected - Name"))
      ((and (sxml:non-first? (car path))
	    (not (char=? (car path) #\:)))
       (sxml:xpointer-parse-error "expected - Name instead of " (car path)))
      (else (let loop ((ncname (list (car path)))
		       (path (cdr path)))
	      (cond
		((null? path) 
		 (list (list->string (reverse ncname)) path))
		((and (memv (car path) sxml:delimiter)
		      (not (char=? (car path) #\:)))
		 (list (list->string (reverse ncname)) path))
		(else (loop (cons (car path) ncname) (cdr path)))))))))

; The function reads a qualified name (QName)
; Returns: ( (prefix . local-part) new-path )
;      or  ( local-part new-path )    if there is no prefix
;       if there is not QName in the beginning of the 'path' it calls 
;          sxml:xpointer-parse-error
;  prefix, local-part - strings
;  new-path - a list of characters
(define (sxml:parse-qname path)
  (and-let* ((r1 (sxml:parse-ncname path)))
	    (let ((first (car r1))
		  (path2 (cadr r1)))
	      (cond
		((null? path2) (list first path2))
		((not (char=? (car path2) #\:)) (list first path2))
		((null? (cdr path2))
		 (sxml:xpointer-parse-error "no local part of a qualified name"))
		((char=? (cadr path2) #\:) (list first path2))
		(else (and-let* ((r2 (sxml:parse-ncname (cdr path2))))
				(list (cons first (car r2)) (cadr r2)))
		      )))))
                   
;------------------------------------------------
; Parsers for data of basic types

; Reads a natural number:
; [1-9] [0-9]*
; The result:  (list  number  new-path)  or  #f
(define (sxml:parse-natural path)
  (let ((path (sxml:skip-ws path)))
    (cond
      ((null? path)
       (sxml:xpointer-parse-error
        "unexpected end of XPointer path. Expected - number"))
      ((or (char<? (car path) #\1) (char>? (car path) #\9))
       (sxml:xpointer-parse-error "expected - number instead of " (car path)))
      (else (let loop ((res (- (char->integer (car path))
			  48)) ; (char->integer #\0)
                  (path (cdr path)))
         (cond
           ((null? path) (list res path))
           ((char-numeric? (car path))
            (loop (+ (* res 10) (- (char->integer (car path)) 
				   48)) ; (char->integer #\0)
                  (cdr path)))
           (else (list res path))))))))

; Reads a Literal ([29] in XPath specification)
; [29]    Literal    ::=    '"' [^"]* '"'  
;                           | "'" [^']* "'"
; The result:  (string new-path)  or  #f
(define (sxml:parse-literal path)
  (let ((ch (if (sxml:parse-check "\"" path) #\" #\')))
    (let loop ((res '())
	       (path (sxml:parse-assert (if (char=? ch #\") "\"" "'") 
				       path)))
      (cond
	((not path) #f)
	((null? path)
	 (sxml:parse-assert (if (char=? ch #\") "\"" "'") 
			   path)
	 #f)
	((char=? (car path) ch)
	 (list (list->string (reverse res))
	       (cdr path)))
	(else (loop (cons (car path) res) (cdr path)))))))

; Reads a Number ([30]-[31] in XPath specification)
; [30]    Number    ::=    Digits ('.' Digits?)?  
;                          | '.' Digits  
; [31]    Digits    ::=    [0-9]+
; The result:  (number new-path)  or  #f
(define (sxml:parse-number path) 
  (define (digits path)
    (let loop ((n-lst '())
               (path path))
      (cond
        ((and (null? path) (null? n-lst))
         (sxml:xpointer-parse-error 
          "unexpected end of XPointer path. Expected - number"))
        ((null? path) (list n-lst path))
        ((and (or (char<? (car path) #\0) (char>? (car path) #\9))
              (null? n-lst))       
         (sxml:xpointer-parse-error "expected - number instead of " (car path)))
        ((or (char<? (car path) #\0) (char>? (car path) #\9))
         (list n-lst path))
        (else
         (loop (cons (- (char->integer (car path)) (char->integer #\0)) n-lst)
               (cdr path))))))
    
  (let ((path (sxml:skip-ws path)))
    (cond
      ((null? path)
       (sxml:xpointer-parse-error 
        "unexpected end of XPointer path. Expected - number"))
      ((char=? (car path) #\.)
       (and-let* ((lst (digits (cdr path))))
            (let rpt ((res 0)
                      (n-lst (car lst))
                      (path (cadr lst)))
              (if(null? n-lst)
                 (list (/ res 10) path)
                 (rpt (+ (/ res 10) (car n-lst))
                      (cdr n-lst) 
                      path)))))
      (else (and-let* ((lst (digits path)))
		      (let loop ((num1 0)
				 (n-lst (reverse (car lst)))
				 (path (cadr lst)))
			(if (null? n-lst)
			  (cond
			    ((null? path) (list num1 path))
			    ((not (char=? (car path) #\.)) (list num1 path))
			    (else
			      (and-let* ((lst2 (digits (cdr path))))
					(let rpt ((num2 0)
						  (n-lst (car lst2))
						  (path (cadr lst2)))
					  (if (null? n-lst)
					    (list (+ num1 (/ num2 10)) path)
					    (rpt (+ (/ num2 10) (car n-lst))
						 (cdr n-lst) 
						 path))))))
			  (loop (+ (* num1 10) (car n-lst))
				(cdr n-lst) 
				path))))))))


;=========================================================================
; XPath/XPointer grammar parsing

; Produces a parameterized parser
; txp-params - a long associative list of parameters which specify handlers
;  for different grammar rules. Precise content for 'txp-params' is discussed
;  iteratively in comments within function's body. However, 'txp-params' are
;  currently intended for TXPath developers only and are thus documented very
;  briefly
;
; The function returns an associative list:
; (list  (list  'xpath     xpath-implementation-res)
;        (list  'xpointer  xpointer-implementation-res)
;        (list  'expr      xpath-expression-implementation-res))
; xpath-implementation-res - XPath implementation produced, as was conducted
;  by 'txp-params'
; xpointer-implementation-res - XPointer implementation produced (for XPointer
;  grammar from W3C Candidate Recommendation 11 September 2001), as was
;  conducted by 'txp-params'
; xpath-expression-implementation-res - implementation for XPath Expr grammar
;  production
;
; NOTE: Future versions of this function may include additional members to the
; associative list which is returned as the result
(define (txp:parameterize-parser txp-params)
  (letrec
      (
       ; All these functions have similar arguments:
       ;  path - an xpath location path represented as a list of chars
       ;  ns-binding - declared namespace prefixes (not for all functions)
       ; ns-binding = (listof (prefix . uri))
       ; prefix - symbol, uri - string
       
       ;-------------------------------------------------
       ; Functions which parse XPath grammar
       
       ; Parses an AxisSpecifier production ([5],[6],[13] in XPath specification)
       ; [5]    AxisSpecifier    ::=    AxisName '::'  
       ;                                | AbbreviatedAxisSpecifier
       ; [6]    AxisName    ::=    'ancestor'  
       ;                           | 'ancestor-or-self'  
       ;                           | 'attribute'  
       ;                           | 'child'  
       ;                           | 'descendant'  
       ;                           | 'descendant-or-self'  
       ;                           | 'following'  
       ;                           | 'following-sibling'  
       ;                           | 'namespace'  
       ;                           | 'parent'  
       ;                           | 'preceding'  
       ;                           | 'preceding-sibling'  
       ;                           | 'self' 
       ; [13]    AbbreviatedAxisSpecifier    ::=    '@'? 
       ;
       ; txp-params are to include the following parameter:
       ;  param-name = 'axis
       ;  param-value =
       ;   (list (list  'ancestor  (lambda (add-on) ...) )
       ;         (list  'ancestor-or-self  (lambda (add-on) ...) )
       ;         (list  'attribute  (lambda (add-on) ...) )
       ;         ...)  ; the remaining axes in the same manner
       (txp:parse-axis-specifier
        (let* ((axis-param-value (txp:param-value 'axis txp-params))
               (child-impl (txp:param-value 'child axis-param-value))
               (parser-pairs
                (cons
                 `(("@") ,(txp:param-value 'attribute axis-param-value))
                 (map
                  (lambda (single-pair)
                    (list
                     (list (symbol->string (car single-pair)) "::")
                     (cadr single-pair)))
                  axis-param-value))))
          (lambda (path ns-binding add-on)   ; ns-binding is dummy here
            (let loop ((pairs parser-pairs))
              (cond
                ((null? pairs)  ; a default (child) axis
                 (list (child-impl add-on) path))
                ((sxml:parse-check-sequence (caar pairs) path)
                 => (lambda (path)
                      (list ((cadar pairs) add-on) path)))
                (else  ; continue loop
                 (loop (cdr pairs))))))))
       
       ; Parses a NodeTest production 
       ; ([7],[37] in XPath specification, [11] in XPointer specification)
       ; [7]    NodeTest    ::=    NameTest  
       ;                           | NodeType '(' ')'  
       ;                           | 'processing-instruction' '(' Literal ')' 
       ; [37]    NameTest    ::=    '*'  
       ;                            | NCName ':' '*'  
       ;                            | QName  
       ; [11]   NodeType   ::=   'comment'  
       ;                         | 'text'  
       ;                         | 'processing-instruction'  
       ;                         | 'node'
       ;                         | 'point'
       ;                         | 'range'
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'node-test
       ;  param-value ::=
       ;   (list (list  'star  (lambda (add-on) ...) )
       ;         (list  'uri+star  (lambda (uri add-on) ...) )
       ;         (list  'qname  (lambda (uri local-name add-on) ...) )
       ;         (list  'comment  (lambda (add-on) ...) )
       ;         (list  'text  (lambda (add-on) ...) )
       ;         (list  'processing-instruction
       ;                (lambda (literal-string add-on) ...) )
       ;         (list  'node  (lambda (add-on) ...) )
       ;         (list  'point  (lambda (add-on) ...) )
       ;         (list  'range  (lambda (add-on) ...) ))
       ; uri - a string or #f (the latter is possible for 'qname only)
       ; local-name - a string
       ; literal - a string
       (txp:parse-node-test
        (let* ((ntest-param-value (txp:param-value 'node-test txp-params))
               (star-impl (txp:param-value 'star ntest-param-value))
               (uri+star-impl (txp:param-value 'uri+star ntest-param-value))
               (qname-impl (txp:param-value 'qname ntest-param-value))
               (comment-impl (txp:param-value 'comment ntest-param-value))
               (text-impl (txp:param-value 'text ntest-param-value))
               (pi-impl
                (txp:param-value 'processing-instruction ntest-param-value))
               (node-impl (txp:param-value 'node ntest-param-value))
               (point-impl (txp:param-value 'point ntest-param-value))
               (range-impl (txp:param-value 'range ntest-param-value))
               (brackets
                (lambda (path)
                  (and-let* ((path (sxml:parse-assert "(" path)))
                            (sxml:parse-assert ")" path)))))
          (lambda (path ns-binding add-on)
            (cond
              ((sxml:parse-check-sequence '("comment" "(") path)
               => (lambda (path)
                    (and-let* ((path (sxml:parse-assert ")" path)))
                              (list (comment-impl add-on) path))))
              ((sxml:parse-check-sequence '("text" "(") path)
               => (lambda (path)
                    (and-let* ((path (sxml:parse-assert ")" path)))
                              (list (text-impl add-on) path))))
              ((sxml:parse-check-sequence '("node" "(") path)
               => (lambda (path)
                    (and-let* ((path (sxml:parse-assert ")" path)))
                          (list (node-impl add-on) path))))
              ((sxml:parse-check-sequence '("processing-instruction" "(") path)
               => (lambda (path)
                    (cond
                      ((sxml:parse-check ")" path)
                       => (lambda (path)
                            (list (pi-impl #f add-on) path)))
                      (else
                       (and-let*
                        ((lst (sxml:parse-literal path))
                         (name (car lst))
                         (path (sxml:parse-assert ")" (cadr lst))))
                        (list (pi-impl name add-on) path))))))
              ((sxml:parse-check-sequence '("point" "(") path)
               => (lambda (path)
                    (and-let* ((path (sxml:parse-assert ")" path)))
                              (list (point-impl add-on) path))))
              ((sxml:parse-check-sequence '("range" "(") path)
               => (lambda (path)
                    (and-let* ((path (sxml:parse-assert ")" path)))
                              (list (range-impl add-on) path))))
              ((sxml:parse-check "*" path)
               => (lambda (path)
                    (list (star-impl add-on) path)))
              (else  ; NCName ':' '*'  |  QName
               (and-let*
                ((lst (sxml:parse-ncname path)))
                (let ((path (cadr lst)))
                  (if
                   (or (null? path) (not (char=? (car path) #\:))) ; local name
                   (list (qname-impl #f (car lst) add-on) path)
                   (let* ((name (string->symbol (car lst)))
                          (path (sxml:parse-assert ":" path))
                          (pair (assq name ns-binding)))
                     (cond
                       ((not pair)
                        (sxml:xpointer-parse-error
                         "unknown namespace prefix - " name))
                       ((and (not (null? path)) (char=? (car path) #\*))
                        (list
                         (uri+star-impl (cdr pair) add-on)
                         (sxml:parse-assert "*" path)))
                       (else
                        (and-let*
                         ((lst (sxml:parse-ncname path)))
                         (list
                          (qname-impl (cdr pair) (car lst) add-on)                      
                          (cadr lst))))))))))))))
                
       ; Parses a Step production 
       ; ([4xptr] in XPointer specification, [12] in XPath specification)
       ; [4xptr] Step ::= AxisSpecifier NodeTest Predicate*
       ;                  | AbbreviatedStep
       ;                  | 'range-to' '(' Expr ')' Predicate*
       ; [12]    AbbreviatedStep    ::=    '.'  
       ;                                   | '..' 
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'step
       ;  param-value ::=
       ;   (list
       ;    (list  'common
       ;      (lambda (axis-res node-test-res predicate-res-lst add-on) ...) )
       ;    (list  'range-to
       ;      (lambda (expr-res predicate-res-lst add-on) ...) ))
       (txp:parse-step
        (let* ((step-param-value (txp:param-value 'step txp-params))
               (common-value (txp:param-value 'common step-param-value))
               (range-to-value (txp:param-value 'range-to step-param-value))
               (axis-param-value (txp:param-value 'axis txp-params))
               (self-value (txp:param-value 'self axis-param-value))
               (parent-value (txp:param-value 'parent axis-param-value))
               (ntest-param-value (txp:param-value 'node-test txp-params))
               (node-value (txp:param-value 'node ntest-param-value)))
          (lambda (path ns-binding add-on)
            (cond
              ((sxml:parse-check ".." path)
               (list
                (common-value (parent-value add-on)
                              (node-value add-on) '() add-on)
                (sxml:parse-assert ".." path)))
              ((sxml:parse-check "." path)
               (list
                (common-value (self-value add-on)
                              (node-value add-on) '() add-on)
                (sxml:parse-assert "." path)))
              ((sxml:parse-check "range-to" path)
               (and-let*
                ((path0
                  (sxml:parse-assert "(" (sxml:parse-assert "range-to" path)))
                 (lst (txp:parse-expr path0 ns-binding add-on))
                 (path (sxml:parse-assert ")" (cadr lst))))
                (let ((expr-res (car lst)))
                  (let loop ((path path)
                             (pred-lst '()))
                    (if
                     (sxml:parse-check "[" path)
                     (and-let*
                      ((lst (txp:parse-predicate path ns-binding add-on)))
                      (loop (cadr lst)
                            (cons (car lst) pred-lst)))
                     ; Predicates are over
                     (list
                      (if
                       (apply txp:semantic-errs-detected?
                              (cons expr-res pred-lst))
                       'txp:semantic-error
                       (range-to-value expr-res (reverse pred-lst) add-on))
                      path))))))
              (else  ; common implementation
               (and-let*
                ((lst (txp:parse-axis-specifier path ns-binding add-on)))
                (let ((axis (car lst)))
                  (and-let*
                   ((lst (txp:parse-node-test (cadr lst) ns-binding add-on)))
                   (let ((test (car lst)))
                     (let loop ((preds '())
                                (path (cadr lst)))
                       (if
                        (sxml:parse-check "[" path)
                        (and-let*
                         ((lst (txp:parse-predicate path ns-binding add-on)))
                         (loop (cons (car lst) preds)
                               (cadr lst)))
                        ; No more predicates                   
                        (list
                         (if (or (txp:semantic-errs-detected? axis test)
                                 (apply txp:semantic-errs-detected? preds))
                             'txp:semantic-error
                             (common-value axis test (reverse preds) add-on))
                         path))))))))))))

       ; Parses a RelativeLocationPath production ([3],[11] in
       ; XPath specification)
       ; [3]  RelativeLocationPath  ::=  Step  
       ;                                 | RelativeLocationPath '/' Step  
       ;                                 | AbbreviatedRelativeLocationPath 
       ; [11]  AbbreviatedRelativeLocationPath  ::=
       ;                                    RelativeLocationPath '//' Step
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'relative-lpath
       ;  param-value ::= (lambda (step-res-lst add-on) ...)
       (txp:parse-relative-location-path
        (let* ((relative-lpath-value
                (txp:param-value 'relative-lpath txp-params))
               (step-param-value (txp:param-value 'step txp-params))
               (common-value (txp:param-value 'common step-param-value))
               (axis-param-value (txp:param-value 'axis txp-params))
               (descendant-or-self-value
                (txp:param-value 'descendant-or-self axis-param-value))
               (ntest-param-value (txp:param-value 'node-test txp-params))
               (node-value (txp:param-value 'node ntest-param-value)))
          (lambda (path ns-binding add-on)
            (let loop ((step-res-lst '())
                       (path path))
              (and-let*
               ((lst (txp:parse-step path ns-binding add-on)))
               (let ((step-res (car lst))
                     (path (cadr lst)))
                 (cond
                   ((sxml:parse-check "//" path)
                    (loop
                     (cons
                      ; // = /descendant-or-self::node()/
                      (common-value
                       (descendant-or-self-value add-on)
                       (node-value add-on) '() add-on)
                      (cons step-res step-res-lst))
                     (sxml:parse-assert "//" path)))
                   ((sxml:parse-check "/" path)
                    (loop (cons step-res step-res-lst)
                          (sxml:parse-assert "/" path)))                          
                   (else  ; no more steps
                    (list
                     (if
                      (apply txp:semantic-errs-detected? step-res-lst)
                      'txp:semantic-error
                      (relative-lpath-value
                       (reverse (cons step-res step-res-lst)) add-on))
                     path)))))))))

       ; Parses a LocationPath production ([1],[2],[10] in XPath specification)
       ; [1]    LocationPath    ::=    RelativeLocationPath  
       ;                               | AbsoluteLocationPath  
       ; [2]    AbsoluteLocationPath    ::=   '/' RelativeLocationPath?  
       ;                                      | AbbreviatedAbsoluteLocationPath
       ; [10]    AbbreviatedAbsoluteLocationPath    ::=
       ;                                              '//' RelativeLocationPath
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'location-path
       ;  param-value ::=
       ;   (list
       ;    (list  'bare-slash  (lambda (add-on) ...) )
       ;    (list  'slash  (lambda (relative-lpath-res add-on) ...) )
       ;    (list  'double-slash  (lambda (relative-lpath-res add-on) ...) ))
       (txp:parse-location-path
        (let* ((location-path-value
                (txp:param-value 'location-path txp-params))
               (bare-slash-value
                (txp:param-value 'bare-slash location-path-value))
               (slash-value
                (txp:param-value 'slash location-path-value))
               (double-slash-value
                (txp:param-value 'double-slash location-path-value))               
               (nothing?  ; whether no relative location path follows '/'
                (lambda (path)
                  (let ((path (sxml:skip-ws path)))
                    (cond
                      ((null? path) #t)
                      ((memv (car path)
                             '(#\| #\+ #\- #\< #\> #\= #\) #\] #\,)) #t)
                      ((or (sxml:parse-check "mod" path sxml:delimiter)
                           (sxml:parse-check "div" path sxml:delimiter)
                           (sxml:parse-check "!=" path)
                           (sxml:parse-check "and" path sxml:delimiter)
                           (sxml:parse-check "or" path sxml:delimiter)) #t)
                      (else #f))))))
          (lambda (path ns-binding add-on)
            (cond
              ((sxml:parse-check "//" path)
               (and-let*
                ((lst (txp:parse-relative-location-path
                       (sxml:parse-assert "//" path) ns-binding add-on)))
                (let ((relative-res (car lst))
                      (path (cadr lst)))
                  (list
                   (if (txp:semantic-errs-detected? relative-res)
                       'txp:semantic-error
                       (double-slash-value relative-res add-on))
                   path))))
              ((sxml:parse-check "/" path)
               => (lambda (path)
                    (if (nothing? path)
                        (list (bare-slash-value add-on) path)
                        (and-let*
                         ((lst (txp:parse-relative-location-path
                                path ns-binding add-on)))
                         (let ((relative-res (car lst))
                               (path (cadr lst)))
                           (list
                            (if (txp:semantic-errs-detected? relative-res)
                                'txp:semantic-error
                                (slash-value relative-res add-on))
                            path))))))
              (else  ; Location path is a Relative location path
               (txp:parse-relative-location-path path ns-binding add-on))))))

       ; Parses a Predicate production ([8]-[9] in XPath specification)
       ; [8]    Predicate    ::=    '[' PredicateExpr ']'  
       ; [9]    PredicateExpr    ::=    Expr 
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'predicate
       ;  param-value ::= (lambda (expr-res add-on) ...)
       (txp:parse-predicate
        (let ((predicate-value (txp:param-value 'predicate txp-params)))
          (lambda (path ns-binding add-on)
            (and-let*
             ((path0 (sxml:parse-assert "[" path))
              (lst (txp:parse-expr path0 ns-binding add-on))
              (path (sxml:parse-assert "]" (cadr lst))))
             (list
              (if (txp:semantic-errs-detected? (car lst))
                  'txp:semantic-error
                  (predicate-value (car lst) add-on))
              path)))))

       ; Parses a VariableReference production ([36] in XPath specification)
       ; [36]    VariableReference    ::=    '$' QName 
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'variable-ref
       ;  param-value ::= (lambda (var-name-string add-on) ...)
       (txp:parse-variable-reference  
        (let ((var-ref-value (txp:param-value 'variable-ref txp-params)))
          (lambda (path ns-binding add-on)
            (and-let*
             ((path (sxml:parse-assert "$" path))
              (lst (sxml:parse-qname path)))
             (let ((name              
                    (if (pair? (car lst))  ; contains a prefix-part
                        (string-append (caar lst) ":" (cdar lst))
                        (car lst))))
               (list (var-ref-value name add-on) (cadr lst)))))))

       ; Parses a FunctionCall production ([16],[17],[35] in
       ; XPath specification)
       ; [16]    FunctionCall    ::=    FunctionName 
       ;                                '(' ( Argument ( ',' Argument )* )? ')'
       ; [17]    Argument    ::=    Expr 
       ; [35]    FunctionName    ::=    QName - NodeType
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'function-call
       ;  param-value ::= (lambda (fun-name-string arg-res-lst add-on) ...)
       ;
       ; NOTE: prefix resolution for qualified function names not implemented
       (txp:parse-function-call
        (let ((fun-call-value (txp:param-value 'function-call txp-params))
              (parse-arguments
               ; Returns (list (listof arg-res) new-path)
               (lambda (path ns-binding add-on)
                 (and-let*
                  ((path (sxml:parse-assert "(" path)))
                  (cond	
                    ((sxml:parse-check ")" path)
                      => (lambda (path) (list '() path)))
                    (else
                     (let single-arg ((arg-res-lst '())
                                      (path path))
                       (and-let*
                        ((lst (txp:parse-expr path ns-binding add-on)))
                        (let ((arg-res (car lst))
                              (path (cadr lst)))
                          (cond
                            ((sxml:parse-check ")" path)
                             => (lambda (path)
                                  (list (reverse (cons arg-res arg-res-lst))
                                        path)))
                            (else
                             (and-let*
                              ((path (sxml:parse-assert "," path)))
                              (single-arg
                               (cons arg-res arg-res-lst) path)))))))))))))
          (lambda (path ns-binding add-on)
            (and-let*
             ((lst (sxml:parse-qname path)))
             (let ((fun-name (car lst)))  ; can be a pair
               (and-let*
                ((lst (parse-arguments (cadr lst) ns-binding add-on)))
                (let ((arg-res-lst (car lst))
                      (path (cadr lst)))
                  (list
                   (if (apply txp:semantic-errs-detected? arg-res-lst)
                       'txp:semantic-error
                       (fun-call-value
                        (if (pair? fun-name)  ; a prefix and a local part
                            (string-append (car fun-name) ":" (cdr fun-name))
                            fun-name)
                        arg-res-lst add-on))
                   path))))))))
                     
       ; Parses a PrimaryExpr production ([15] in XPath specification)
       ; [15]    PrimaryExpr    ::=    VariableReference  
       ;                               | '(' Expr ')'  
       ;                               | Literal  
       ;                               | Number  
       ;                               | FunctionCall 
       ; [29]    Literal    ::=    '"' [^"]* '"'  
       ;                           | "'" [^']* "'"  
       ; [30]    Number    ::=    Digits ('.' Digits?)?  
       ;                          | '.' Digits  
       ; [31]    Digits    ::=    [0-9]+ 
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'primary-expr
       ;  param-value ::= 
       ;   (list  (list  'literal  (lambda (literal add-on) ...) )
       ;          (list  'number   (lambda (number add-on) ...)  ))
       (txp:parse-primary-expr
        (let* ((primary-expr-value (txp:param-value 'primary-expr txp-params))
               (literal-value (txp:param-value 'literal primary-expr-value))
               (number-value (txp:param-value 'number primary-expr-value)))
          (lambda (path ns-binding add-on)
            (cond
              ((sxml:parse-check "$" path)  ; a VariableReference
               (txp:parse-variable-reference path ns-binding add-on))
              ((sxml:parse-check "(" path)  ; an '(' Expr ')'
               (and-let*
                ((lst (txp:parse-expr
                       (sxml:parse-assert "(" path) ns-binding add-on))
                 (path (sxml:parse-assert ")" (cadr lst))))
                (let ((expr-res (car lst)))
                  (list expr-res path))))
              ((or (sxml:parse-check "\"" path)
                   (sxml:parse-check "'" path))  ; a Literal
               (and-let*
                ((lst (sxml:parse-literal path)))
                (list
                 (literal-value (car lst) add-on)
                 (cadr lst))))
              ((let ((p (sxml:skip-ws path)))  ; a Number?
                 (cond ((null? p) #f)
                       ((char=? (car p) #\.) #t)
                       ((and (char>=? (car p) #\0) (char<=? (car p) #\9)) #t)
                       (else #f)))
               (and-let*
                ((lst (sxml:parse-number path)))                               
                (list
                 (number-value (car lst) add-on)	   
                 (cadr lst))))
              (else   ; a Function call
               (txp:parse-function-call path ns-binding add-on))))))

       ; Parses a FilterExpr production ([20] in XPath specification)
       ; [20]    FilterExpr    ::=    PrimaryExpr  
       ;                              | FilterExpr Predicate 
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'filter-expr
       ;  param-value ::=
       ;            (lambda (primary-expr-res predicate-res-lst add-on) ...) )
       (txp:parse-filter-expr
        (let ((filter-expr-value (txp:param-value 'filter-expr txp-params)))
          (lambda (path ns-binding add-on)
            (and-let*
             ((lst (txp:parse-primary-expr path ns-binding add-on)))
             (let ((prim-res (car lst)))
               (let loop ((pred-res-lst '())
                          (path (cadr lst)))
                 (cond
                   ((sxml:parse-check "[" path)
                    (and-let*
                     ((lst (txp:parse-predicate path ns-binding add-on)))
                     (loop (cons (car lst) pred-res-lst)
                           (cadr lst))))
                   ; No more predicates
                   ((null? pred-res-lst) (list prim-res path))
                   (else              
                    (list
                     (if
                      (apply txp:semantic-errs-detected?
                             (cons prim-res pred-res-lst))
                      'txp:semantic-error
                      (filter-expr-value prim-res (reverse pred-res-lst) add-on))
                     path)))))))))

       ; Parses a PathExpr production ([19] in XPath specification)
       ; [19]    PathExpr    ::=    LocationPath  
       ;                            | FilterExpr  
       ;                            | FilterExpr '/' RelativeLocationPath  
       ;                            | FilterExpr '//' RelativeLocationPath
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'path-expr
       ;  param-value ::=
       ;   (list
       ;    (list  'slash
       ;           (lambda (filter-expr-res relative-lpath-res add-on) ...) )
       ;    (list  'double-slash
       ;           (lambda (filter-expr-res relative-lpath-res add-on) ...) ))
       (txp:parse-path-expr
         (let ((filter-expr?
                (lambda (path)
                  (let ((path (sxml:skip-ws path)))
                    (cond
                      ((null? path) #f)
                      ((member 
                        (car path) 
                        '(#\$ #\( #\" #\' #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                       #t)
                      ((char=? (car path) #\.)
                       (cond
                         ((null? (cdr path)) #f)
                         ((member
                           (cadr path)
                           '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                          #t)
                         (else #f)))
                      ((member
                        (car path)
                        '(#\) #\< #\> #\[ #\] #\/ #\+ #\* #\, #\= #\| #\! #\@ #\-))
                       #f)
                      (else
                       (let ((lst (sxml:parse-ncname path)))
                         (cond
                           ((not lst) #f)
                           ((sxml:parse-check "::" (cadr lst)) #f)
                           (else
                            (and-let*
                             ((lst (sxml:parse-name path)))
                             (let ((name (car lst))
                                   (new-path (sxml:skip-ws (cadr lst))))
                               (cond
                                 ((string=? name "range-to") #f)
                                 ((string=? name "comment") #f)
                                 ((string=? name "text") #f)
                                 ((string=? name "processing-instruction") #f)
                                 ((string=? name "node") #f)
                                 ((string=? name "point") #f)
                                 ((string=? name "range") #f)
                                 ((null? new-path) #f)
                                 ((char=? (car new-path) #\() #t)
                                 (else #f)))))))))))))
           (let* ((path-expr-value (txp:param-value 'path-expr txp-params))
                  (slash-value (txp:param-value 'slash path-expr-value))
                  (double-slash-value
                   (txp:param-value 'double-slash path-expr-value)))
             (lambda (path ns-binding add-on)
               (if
                (not (filter-expr? path))
                (txp:parse-location-path path ns-binding add-on)
                (and-let*
                 ((lst (txp:parse-filter-expr path ns-binding add-on)))
                 (let ((filter-ex-res (car lst))
                       (path (cadr lst)))
                   (cond
                     ((sxml:parse-check "//" path)
                      (and-let*
                       ((lst2
                         (txp:parse-relative-location-path
                          (sxml:parse-assert "//" path) ns-binding add-on)))
                       (let ((rel-lpath-res (car lst2))
                             (path (cadr lst2)))
                         (list
                          (if
                           (txp:semantic-errs-detected?
                            filter-ex-res rel-lpath-res)
                           'txp:semantic-error
                           (double-slash-value
                            filter-ex-res rel-lpath-res add-on))
                          path))))
                     ((sxml:parse-check "/" path)
                      (and-let*
                       ((lst2
                         (txp:parse-relative-location-path
                          (sxml:parse-assert "/" path) ns-binding add-on)))
                       (let ((rel-lpath-res (car lst2))
                             (path (cadr lst2)))
                         (list
                          (if
                           (txp:semantic-errs-detected?
                            filter-ex-res rel-lpath-res)
                           'txp:semantic-error
                           (slash-value filter-ex-res rel-lpath-res add-on))
                          path))))
                     (else  ; A single filter expression, not followed by lpath
                      lst)))))))))

       ; Parses a UnionExpr production ([18] in XPath specification)
       ; [18]    UnionExpr    ::=    PathExpr  
       ;                             | UnionExpr '|' PathExpr
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'union-expr
       ;  param-value ::= (lambda (path-expr-res-lst add-on) ...)
       (txp:parse-union-expr
        (let ((union-expr-value (txp:param-value 'union-expr txp-params)))              
          (lambda (path ns-binding add-on)
            (let loop ((p-e-res-lst '())
                       (path path))
              (and-let*
               ((lst (txp:parse-path-expr path ns-binding add-on)))
               (let ((p-e-res (car lst))
                     (path (cadr lst)))
                 (let ((new-path (sxml:parse-check "|" path)))
                   (cond
                     (new-path  ; more PathExprs
                      (loop (cons p-e-res p-e-res-lst) new-path))
                     ; no more PathExprs
                     ((null? p-e-res-lst)  ; only one PathExpr                                
                      (list p-e-res path))
                     (else  ; several Path-exprs
                      (list
                       (if
                        (apply txp:semantic-errs-detected?
                               (cons p-e-res p-e-res-lst))
                        'txp:semantic-error
                        (union-expr-value
                         (reverse (cons p-e-res p-e-res-lst)) add-on))
                       path))))))))))
 
       ; Parses a UnaryExpr production ([27] in XPath specification)
       ; [27]    UnaryExpr    ::=    UnionExpr  
       ;                             | '-' UnaryExpr 
       ; Note that the grammar allows multiple unary minuses
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'unary-expr
       ;  param-value ::= (lambda (union-expr-res num-minuses add-on) ...)
       (txp:parse-unary-expr
        (let ((unary-expr-value (txp:param-value 'unary-expr txp-params)))              
          (lambda (path ns-binding add-on)
            (if (not (sxml:parse-check "-" path))
                (txp:parse-union-expr path ns-binding add-on)
                (let loop ((num-minuses 0) (path path))
                  (let ((new-path (sxml:parse-check "-" path)))
                    (if new-path   ; more minuses
                        (loop (+ num-minuses 1) new-path)               
                        (and-let*
                         ((lst (txp:parse-union-expr path ns-binding add-on)))
                         (let ((union-expr-res (car lst))
                               (path (cadr lst)))
                           (list
                            (if
                             (txp:semantic-errs-detected? union-expr-res)
                             'txp:semantic-error
                             (unary-expr-value
                              union-expr-res num-minuses add-on))
                            path))))))))))
                     			
       ; Parses a MultiplicativeExpr production ([26],[34] in
       ; XPath specification)
       ; [26] MultiplicativeExpr  ::=
       ;                 UnaryExpr  
       ;                 | MultiplicativeExpr MultiplyOperator UnaryExpr
       ;                 | MultiplicativeExpr 'div' UnaryExpr  
       ;                 | MultiplicativeExpr 'mod' UnaryExpr 
       ; [34] MultiplyOperator  ::=  '*'
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'mul-expr
       ;  param-value ::= (lambda (unary-expr-res-lst op-lst add-on) ...)
       (txp:parse-multiplicative-expr
        (let* ((mul-expr-value (txp:param-value 'mul-expr txp-params))
               (operations-value (txp:param-value 'operations txp-params))
               (multiply-value (txp:param-value '* operations-value))
               (div-value (txp:param-value 'div operations-value))
               (mod-value (txp:param-value 'mod operations-value)))
          (lambda (path ns-binding add-on)
            (let loop ((unary-expr-res-lst '())
                       (op-lst '())
                       (path path))
              (and-let*
               ((lst (txp:parse-unary-expr path ns-binding add-on)))
               (let ((unary-expr-res (car lst))
                     (path (cadr lst)))
                 (cond
                   ((sxml:parse-check "*" path)
                    (loop (cons unary-expr-res unary-expr-res-lst)
                          (cons (multiply-value add-on) op-lst)
                          (sxml:parse-assert "*" path)))
                   ((sxml:parse-check "div" path sxml:delimiter)
                    (loop (cons unary-expr-res unary-expr-res-lst)
                          (cons (div-value add-on) op-lst)
                          (sxml:parse-assert "div" path)))
                   ((sxml:parse-check "mod" path sxml:delimiter)
                    (loop (cons unary-expr-res unary-expr-res-lst)
                          (cons (mod-value add-on) op-lst)
                          (sxml:parse-assert "mod" path)))
                   ; no more UnaryExprs
                   ((null? unary-expr-res-lst)  ; single UnaryExpr
                    lst)
                   (else   ; several UnaryExprs
                    (list
                     (if
                      (apply txp:semantic-errs-detected?
                             (cons unary-expr-res unary-expr-res-lst))
                      'txp:semantic-error
                      (mul-expr-value
                       (reverse (cons unary-expr-res unary-expr-res-lst))
                       (reverse op-lst) add-on))
                     path)))))))))
              
       ; Parses a AdditiveExpr production ([25] in XPath specification)
       ; [25]    AdditiveExpr    ::=    MultiplicativeExpr  
       ;                                | AdditiveExpr '+' MultiplicativeExpr  
       ;                                | AdditiveExpr '-' MultiplicativeExpr 
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'add-expr
       ;  param-value ::= (lambda (mul-expr-res-lst op-lst add-on) ...)
       (txp:parse-additive-expr
        (let* ((add-expr-value (txp:param-value 'add-expr txp-params))
               (operations-value (txp:param-value 'operations txp-params))
               (plus-value (txp:param-value '+ operations-value))
               (minus-value (txp:param-value '- operations-value)))
          (lambda (path ns-binding add-on)
            (let loop ((mul-expr-res-lst '())
                       (op-lst '())
                       (path path))
              (and-let*
               ((lst (txp:parse-multiplicative-expr path ns-binding add-on)))
               (let ((mul-expr-res (car lst))
                     (path (cadr lst)))
                 (cond
                   ((sxml:parse-check "+" path)
                    (loop (cons mul-expr-res mul-expr-res-lst)
                          (cons (plus-value add-on) op-lst)
                          (sxml:parse-assert "+" path)))
                   ((sxml:parse-check "-" path)
                    (loop (cons mul-expr-res mul-expr-res-lst)
                          (cons (minus-value add-on) op-lst)
                          (sxml:parse-assert "-" path)))
                   ; no more MultiplicativeExprs
                   ((null? mul-expr-res-lst)  ; single MultiplicativeExpr
                    lst)
                   (else   ; several MultiplicativeExprs
                    (list
                     (if
                      (apply txp:semantic-errs-detected?
                             (cons mul-expr-res mul-expr-res-lst))
                      'txp:semantic-error
                      (add-expr-value
                       (reverse (cons mul-expr-res mul-expr-res-lst))
                       (reverse op-lst) add-on))
                     path)))))))))
       
       ; Parses a RelationalExpr production ([24] in XPath specification)
       ; [24]    RelationalExpr    ::=    AdditiveExpr  
       ;                                  | RelationalExpr '<' AdditiveExpr
       ;                                  | RelationalExpr '>' AdditiveExpr
       ;                                  | RelationalExpr '<=' AdditiveExpr
       ;                                  | RelationalExpr '>=' AdditiveExpr
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'relational-expr
       ;  param-value ::=
       ;           (lambda (additive-expr-res-lst cmp-op-lst add-on) ...)
       (txp:parse-relational-expr
        (let* ((rel-expr-value (txp:param-value 'relational-expr txp-params))
               (operations-value (txp:param-value 'operations txp-params))
               (ls-value (txp:param-value '< operations-value))
               (gt-value (txp:param-value '> operations-value))
               (le-value (txp:param-value '<= operations-value))
               (ge-value (txp:param-value '>= operations-value)))                              
          (lambda (path ns-binding add-on)
            (let loop ((add-res-lst '())
                       (cmp-op-lst '())
                       (path path))
              (and-let*
               ((lst (txp:parse-additive-expr path ns-binding add-on)))
               (let ((add-res (car lst))
                     (path (cadr lst)))
                 (cond
                   ((sxml:parse-check "<=" path)
                    (loop (cons add-res add-res-lst)
                          (cons (le-value add-on) cmp-op-lst)
                          (sxml:parse-assert "<=" path)))
                   ((sxml:parse-check ">=" path)
                    (loop (cons add-res add-res-lst)
                          (cons (ge-value add-on) cmp-op-lst)
                          (sxml:parse-assert ">=" path)))
                   ((sxml:parse-check "<" path)
                    (loop (cons add-res add-res-lst)
                          (cons (ls-value add-on) cmp-op-lst)
                          (sxml:parse-assert "<" path)))
                   ((sxml:parse-check ">" path)
                    (loop (cons add-res add-res-lst)
                          (cons (gt-value add-on) cmp-op-lst)
                          (sxml:parse-assert ">" path)))
                   ; no more AdditiveExprs                   
                   ((null? add-res-lst) ; single AdditiveExpr
                    lst)
                   (else   ; several AdditiveExprs
                    (list
                     (if
                      (apply txp:semantic-errs-detected?
                             (cons add-res add-res-lst))
                      'txp:semantic-error
                      (rel-expr-value
                       (reverse (cons add-res add-res-lst))
                       (reverse cmp-op-lst) add-on))
                     path)))))))))
       
       ; Parses an EqualityExpr production ([23] in XPath specification)
       ; [23]    EqualityExpr    ::=    RelationalExpr
       ;                                | EqualityExpr '=' RelationalExpr
       ;                                | EqualityExpr '!=' RelationalExpr
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'equality-expr
       ;  param-value ::=
       ;           (lambda (relational-expr-res-lst cmp-op-lst add-on) ...)
       (txp:parse-equality-expr
        (let* ((equality-expr-value
                (txp:param-value 'equality-expr txp-params))
               (operations-value
                (txp:param-value 'operations txp-params))
               (equal-value (txp:param-value '= operations-value))
               (not-equal-value (txp:param-value '!= operations-value)))
          (lambda (path ns-binding add-on)
            (let loop ((rel-res-lst '())
                       (cmp-op-lst '())
                       (path path))
              (and-let*
               ((lst (txp:parse-relational-expr path ns-binding add-on)))
               (let ((rel-res (car lst))
                     (path (cadr lst)))
                 (cond
                   ((sxml:parse-check "=" path)
                    (loop (cons rel-res rel-res-lst)
                          (cons (equal-value add-on) cmp-op-lst)
                          (sxml:parse-assert "=" path)))
		  ((sxml:parse-check "!=" path)
		   (loop (cons rel-res rel-res-lst)
                         (cons (not-equal-value add-on) cmp-op-lst)
			 (sxml:parse-assert "!=" path)))
		  ; no more RelationalExprs
                  ((null? rel-res-lst) ; only one RelationalExpr
                   lst)
                  (else  ; several RelationalExprs
                   (list
                    (if
                     (apply txp:semantic-errs-detected?
                            (cons rel-res rel-res-lst))
                      'txp:semantic-error
                      (equality-expr-value
                       (reverse (cons rel-res rel-res-lst))
                       (reverse cmp-op-lst) add-on))
                     path)))))))))
                   
       ; Parses an AndExpr production ([22] in XPath specification)
       ; [22]    AndExpr    ::=    EqualityExpr
       ;                           | AndExpr 'and' EqualityExpr
       ; Note that according to 3.4 in XPath specification, the right operand
       ; is not evaluated if the left operand evaluates to false
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'and-expr
       ;  param-value ::= (lambda (equality-expr-res-lst add-on) ...)
       (txp:parse-and-expr
        (let ((and-expr-value (txp:param-value 'and-expr txp-params)))
          (lambda (path ns-binding add-on)
            (let loop ((equality-res-lst '())
                       (path path))
              (and-let*
               ((lst (txp:parse-equality-expr path ns-binding add-on)))
               (let ((equality-res (car lst))
                     (path (cadr lst)))
                 (let ((new-path (sxml:parse-check "and" path sxml:delimiter)))
                   (cond
                     (new-path
                      (loop (cons equality-res equality-res-lst) new-path))
                     ; no more EqualityExprs
                     ((null? equality-res-lst)  ; only one EqualityExpr
                      lst)
                     (else  ; several EqualityExprs
                      (list
                       (if
                        (apply txp:semantic-errs-detected?
                               (cons equality-res equality-res-lst))
                        'txp:semantic-error
                        (and-expr-value
                         (reverse (cons equality-res equality-res-lst))
                         add-on))
                     path))))))))))
                  
       ; Parses an Expr production ([14],[21] in XPath specification)
       ; [14]    Expr    ::=    OrExpr 
       ; [21]    OrExpr    ::=    AndExpr  
       ;                          | OrExpr 'or' AndExpr
       ; Note that according to 3.4 in XPath specification, the right operand
       ; is not evaluated if the left operand evaluates to true
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'or-expr
       ;  param-value ::= (lambda (and-expr-res-lst add-on) ...)
       (txp:parse-expr
        (let ((or-expr-value (txp:param-value 'or-expr txp-params)))
          (lambda (path ns-binding add-on)
            (let loop ((and-res-lst '())
                       (path path))
              (and-let*
               ((lst (txp:parse-and-expr path ns-binding add-on)))
               (let ((and-res (car lst))
                     (path (cadr lst)))
                 (let ((new-path (sxml:parse-check "or" path sxml:delimiter)))
                   (cond
                     (new-path
                      (loop (cons and-res and-res-lst) new-path))
                     ; no more AndExprs
                     ((null? and-res-lst)  ; only one AndExpr
                      lst)
                     (else  ; several AndExprs
                      (list
                       (if
                        (apply txp:semantic-errs-detected?
                               (cons and-res and-res-lst))
                        'txp:semantic-error
                        (or-expr-value
                         (reverse (cons and-res and-res-lst)) add-on))
                      path))))))))))
       
       ;------------------------------------------------
       ; Functions which parse XPointer grammar
       
       ; Parses an FullXPtr production ([3]-[10] in XPointer specification)
       ; [3]    FullXPtr    ::=    XPtrPart (S? XPtrPart)* 
       ; [4]    XPtrPart    ::=    'xpointer' '(' XPtrExpr ')'
       ;                           | 'xmlns' '(' XPtrNsDecl? ')' 
       ;                           | Scheme '(' SchemeSpecificExpr ')' 
       ; [5]    Scheme    ::=    NCName 
       ; [6]    SchemeSpecificExpr    ::=    StringWithBalancedParens 
       ; [7]    StringWithBalancedParens    ::=
       ;                    [^()]* ('(' StringWithBalancedParens ')' [^()]*)*
       ; [8]    XPtrExpr    ::=    Expr
       ; [9]    XPtrNsDecl    ::=    NCName S? '=' S? XPtrNsURI 
       ; [10]    XPtrNsURI    ::=    Char*
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'full-xptr
       ;  param-value ::= (lambda (expr-res-lst add-on) ...)
       (txp:parse-full-xptr
        (let ((full-xptr-value (txp:param-value 'full-xptr txp-params)))
          (lambda (path ns-binding add-on)
            (let loop ((expr-res-lst '())
                       (ns-binding ns-binding)
                       (path path))
              (if
               (null? (sxml:skip-ws path))  ; the string is over               
               (cond
                 ((= (length expr-res-lst) 1)  ; a single XPointer part
                  (car expr-res-lst))
                 ((apply txp:semantic-errs-detected? expr-res-lst)
                  'txp:semantic-error)
                 (else
                  (full-xptr-value (reverse expr-res-lst) add-on)))
               (and-let*
                ((lst (sxml:parse-name path))
                 (name (car lst))
                 (path (cadr lst)))
                (cond
                  ((string=? name "xpointer")  ; xpointer part
                   (and-let*
                    ((path (sxml:parse-assert "(" path))
                     (lst2 (txp:parse-expr path ns-binding add-on)))
                    (let ((expr-res (car lst2))
                          (path (cadr lst2)))
                      (and-let*
                       ((path (sxml:parse-assert ")" path)))
                       (loop (cons expr-res expr-res-lst) ns-binding path)))))
                  ((string=? name "xmlns")  ; xmlns part
                   (and-let*
                    ((path0 (sxml:parse-assert "(" path))
                     (lst2 (sxml:parse-ncname path0))
                     (prefix (string->symbol (car lst2)))
                     (path (sxml:parse-assert "=" (cadr lst2))))
                    (let rpt2 ((path (sxml:skip-ws path)) (uri '()))
                      (cond
                        ((null? path)
                         (sxml:parse-assert ")" path)
                         #f)
                        ((and (char=? (car path) #\)) (null? uri))
                         (sxml:xpointer-parse-error
                          "namespace URI cannot be empty"))
                        ((char=? (car path) #\))
                         (loop expr-res-lst
                               (cons
                                (cons prefix (list->string (reverse uri)))
                                ns-binding)
                               (cdr path)))
                        (else
                         (rpt2 (cdr path) (cons (car path) uri)))))))
                  (else  ; any other XPointer scheme
                   (and-let*
                    ((path (sxml:parse-assert "(" path)))
                    (let rpt3 ((n 1) (path path))
                      (cond
                        ((= n 0)
                         (sxml:xpointer-parse-warning
                          "unknown xpointer schema - " name ". Ignoring")
                         (loop expr-res-lst ns-binding path))
                        ((null? path)
                         (sxml:parse-assert ")" path)
                         #f)
                        ((char=? (car path) #\() (rpt3 (+ n 1) (cdr path)))
                        ((char=? (car path) #\)) (rpt3 (- n 1) (cdr path)))
                        (else (rpt3 n (cdr path))))))))))))))
       
       ; Parses an ChildSeq production ([2] in XPointer specification)
       ; [2]    ChildSeq    ::=    Name? ('/' [1-9] [0-9]* )+
       ;
       ; txp-params are to include the following parameter:
       ;  param-name ::= 'child-seq
       ;  param-value ::=
       ;   (list
       ;    (list  'with-name
       ;           (lambda (name-string number-lst add-on) ...) )
       ;    (list  'without-name
       ;           (lambda (number-lst add-on) ...) ))
       (txp:parse-child-seq
        (let ((helper
               (lambda (path)
                 (let loop ((num-lst '())
                            (path path))
                   (let ((path2 (sxml:parse-check "/" path)))
                     (cond
                       (path2  ; #\/ found
                        (and-let* ((lst (sxml:parse-natural path2)))
                                  (loop (cons (car lst) num-lst)
                                        (cadr lst))))
                       ((null? (sxml:skip-ws path))  ; end of path
                        (reverse num-lst))
                       (else    ; this will cause an error message
                        (sxml:parse-assert "/" path))))))))                         
          (let* ((child-seq-value (txp:param-value 'child-seq txp-params))
                 (with-name-value (txp:param-value 'with-name child-seq-value))
                  (without-name-value
                   (txp:param-value 'without-name child-seq-value)))
            (lambda (path ns-binding add-on)
              (let ((path2 (sxml:parse-check "/" path)))
                (if
                 path2  ; "/" found => no Name supported
                 (and-let*
                  ((number-lst (helper path)))                  
                  (without-name-value number-lst add-on))
                 (and-let*
                  ((lst (sxml:parse-name path))
                   (name (car lst))
                   (number-lst (helper (cadr lst))))
                  (with-name-value name number-lst add-on))))))))
                   
       ;-------------------------------------------------
       ; Higher level functions
       ;  ns-binding - declared namespace prefixes (an optional argument)
       ;  add-on - whatever; may be useful for specific parser
       ; implementations, since this parameter is passed throughout all
       ; grammar rules
       ;
       ;  ns-binding = (listof  (prefix . uri))
       ;  prefix - a symbol
       ;  uri - a string
       
       ; Parses XPath grammar
       ;  path is a string here
       (txp:parse-xpath
        (lambda (path-string ns-binding add-on)
          (let ((res (txp:parse-location-path
                      (string->list path-string) ns-binding add-on)))
            (if (and res  ; no parser errors
                     (sxml:assert-end-of-path (cadr res)))
                (car res)
                'txp:parser-error))))
       
       ; Parses an XPointer production ([1] in XPointer specification)
       ; [1]    XPointer    ::=    Name | ChildSeq | FullXPtr 
       (txp:parse-xpointer
        (lambda (path-string ns-binding add-on)
          (let ((path (string->list path-string)))
            (if (sxml:parse-check "/" path)   ; => ChildSeq
                (txp:parse-child-seq path ns-binding add-on)
                (and-let*
                 ((lst (sxml:parse-name path))
                  (new-path (cadr lst)))
                 (if (sxml:parse-check "(" new-path)  ; FullXPtr production
                     (txp:parse-full-xptr path ns-binding add-on)
                     (txp:parse-child-seq path ns-binding add-on)))))))
       
       ; Parses XPath Expression
       ; [14]    Expr    ::=    OrExpr
       (txp:parse-xpath-expression
        (lambda (path-string ns-binding add-on)
          (let ((res (txp:parse-expr
                      (string->list path-string) ns-binding add-on)))
            (if (and res  ; no parser errors
                     (sxml:assert-end-of-path (cadr res)))
                (car res)
                'txp:parser-error))))
       
       )
        
    `((xpath ,txp:parse-xpath)
      (xpointer ,txp:parse-xpointer)
      (expr ,txp:parse-xpath-expression))
    ))

(provide (all-defined)))
