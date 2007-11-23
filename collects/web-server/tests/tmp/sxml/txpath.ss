; Module header is generated automatically
#cs(module txpath mzscheme
(require (lib "string.ss" "srfi/13"))
(require (lib "ssax.ss" "web-server/tests/tmp/ssax"))
(require "sxpathlib.ss")
(require "sxml-tools.ss")
(require "sxpath-ext.ss")
(require "xpath-parser.ss")

;; Classic TXPath implementation based on sxpathlib, sxpath-ext and txp-parser
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin
;
; XPointer's points and ranges are NOT implemented
;
; Full XPath Core Function Library is supported. That is:
; 4.1 Node Set Functions
;    number last()
;    number position()
;    number count(node-set)
;    node-set id(object)
;    string local-name(node-set?)
;    string namespace-uri(node-set?)
;    string name(node-set?)
; 4.2 String Functions
;    string string(object?)
;    string concat(string, string, string*)
;    boolean starts-with(string, string)
;    boolean contains(string, string)
;    string substring-before(string, string)
;    string substring-after(string, string)
;    string substring(string, number, number?)
;    number string-length(string?)
;    string normalize-space(string?)
;    string translate(string, string, string)
; 4.3 Boolean Functions
;    boolean boolean(object)
;    boolean not(boolean)
;    boolean true()
;    boolean false()
;    boolean lang(string)
; 4.4 Number Functions
;    number number(object?)
;    number sum(node-set)
;    number floor(number)
;    number ceiling(number)
;    number round(number)


;==========================================================================
; Auxilliary

; Runtime errors handler (unbound variable, bad argument, etc).
; It may be re-defined (say, like a warning) without 'exit',  and evaluation will 
; be continued.
; In this case, a default value (usually empty nodeset or 0) is returned by 
; a sub-expression which caused an XPath/XPointer runtime error.
(define (sxml:xpointer-runtime-error . text)
  (apply cerr (append (list "XPath/XPointer runtime error: ") text (list nl)))
  (exit -1))


;--------------------------------------------------------------------------
; Helper functions

; Filter nodeset using preds-list as described in XPath rec. 2.4
; A helper for sxml:parse-step and sxml:parse-filter-expr
(define (sxml:xpath-nodeset-filter preds-list nodeset root-node var-binding)
  (let rpt ((nodeset nodeset)
	    (ps preds-list))
    (if (null? ps) 
      nodeset
      (let lab ((nset nodeset)
		(res '())
		(pos 1)) 
	(if (null? nset)
	  (rpt (reverse res) (cdr ps))
	  (let* ((size (length nodeset))
		 (val ((car ps) 
		       (list (car nset)) 
		       root-node 
		       (cons pos size) 
		       var-binding)))
	    (lab (cdr nset)
		 (if (if (number? val)
		       (= val pos)
		       (sxml:boolean val))
		   (cons (car nset) res)
		   res)
		 (+ pos 1))))))))


; A helper for arithmetic expressions
;   sxml:parse-additive-expr and sxml:parse-multiplicative-expr 
(define (sxml:arithmetic-eval unary-expr-res-lst op-lst add-on)
  (lambda (nodeset root-node context var-binding)
    (let rpt
      ((res (sxml:number
             ((car unary-expr-res-lst) nodeset root-node context var-binding)))
       (fs (cdr unary-expr-res-lst))
       (ops op-lst))
    (if (null? fs)
        res
        (rpt ((car ops)
              res
              (sxml:number ((car fs) nodeset root-node context var-binding)))
             (cdr fs)
             (cdr ops))))))


;==========================================================================
; XPath Core Function Library

;-------------------------------------------------
; 4.1 Node Set Functions

; last()
(define (sxml:core-last)
  (lambda (nodeset root-node context var-binding)
    (cdr context)))

; position()
(define (sxml:core-position)
  (lambda (nodeset root-node context var-binding)
    (car context)))

; count(node-set)
(define (sxml:core-count arg-func)
  (lambda (nodeset root-node context var-binding)
    (let ((res (arg-func nodeset root-node context var-binding)))
      (cond
        ((nodeset? res) (length res))
        (else
         (sxml:xpointer-runtime-error
          "count() function - an argument is not a nodeset")
         0)))))

; id(object)
(define (sxml:core-id arg-func)
  (lambda (nodeset root-node context var-binding)
    (let* ((id-nset ((sxml:child (ntype?? 'id-index))
                     ((sxml:child (ntype?? '@@)) root-node))))
      (if
       (null? id-nset)  ; no id-index
       '()  ; ID function returns an empty nodeset
       ((sxml:id (cdar id-nset))  ; implemented in "sxpath-ext.scm"
        (arg-func nodeset root-node context var-binding))))))

; local-name(node-set?)
(define (sxml:core-local-name . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (cond
          ((null? nodeset) "")
          ((not (pair? (car nodeset))) "")  ; no name
          (else
           (let ((name (symbol->string (caar nodeset))))
             (cond
               ((string-rindex name #\:)
                => (lambda (pos)
                     (substring name (+ pos 1) (string-length name))))
               (else  ; a NCName
                name))))))
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (let ((obj (func nodeset root-node context var-binding)))
            (cond
              ((null? obj) "")  ; an empty nodeset
              ((not (nodeset? obj))
               (sxml:xpointer-runtime-error
                "NAME function - an argument is not a nodeset")              
               "")
              ((not (pair? (car obj))) "")  ; no name
              (else
               (let ((name (symbol->string (caar obj))))
                 (cond
                   ((string-rindex name #\:)
                    => (lambda (pos)
                         (substring
                          name (+ pos 1) (string-length name))))
                   (else  ; a NCName
                    name))))))))))

; namespace-uri(node-set?)
(define (sxml:core-namespace-uri . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (cond
          ((null? nodeset) "")
          ((not (pair? (car nodeset))) "")  ; no name
          (else
           (let ((name (symbol->string (caar nodeset))))
             (cond
               ((string-rindex name #\:)
                => (lambda (pos)
                     (substring name 0 pos)))
               (else ""))))))  ; a NCName
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (let ((obj (func nodeset root-node context var-binding)))
            (cond
              ((null? obj) "")  ; an empty nodeset
              ((not (nodeset? obj))
               (sxml:xpointer-runtime-error
                "NAME function - an argument is not a nodeset")
               "")
              ((not (pair? (car obj))) "")  ; no name
              (else
               (let ((name (symbol->string (caar obj))))
                 (cond
                   ((string-rindex name #\:)
                    => (lambda (pos)
                         (substring name 0 pos)))
                   (else ""))))))))))

; name(node-set?)
(define (sxml:core-name . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (cond
          ((null? nodeset) "")
          ((not (pair? (car nodeset))) "")  ; no name
          (else
           (symbol->string (caar nodeset)))))
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (let ((obj (func nodeset root-node context var-binding)))
            (cond
              ((null? obj) "")  ; an empty nodeset
              ((not (nodeset? obj))
               (sxml:xpointer-runtime-error
                "NAME function - an argument is not a nodeset")
               "")
              ((not (pair? (car obj))) "")  ; no name
              (else
               (symbol->string (caar obj)))))))))


;-------------------------------------------------
; 4.2 String Functions

; string(object?)
(define (sxml:core-string . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (sxml:string nodeset))
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (sxml:string 
           (func nodeset root-node context var-binding))))))

; concat(string, string, string*)
(define (sxml:core-concat . arg-func-lst)
  (lambda (nodeset root-node context var-binding)
    (apply
     string-append
     (map
      (lambda (f)
        (sxml:string (f nodeset root-node context var-binding)))
      arg-func-lst))))

; starts-with(string, string)
(define (sxml:core-starts-with arg-func1 arg-func2)
  (lambda (nodeset root-node context var-binding)
    (let ((str1 (sxml:string
                 (arg-func1 nodeset root-node context var-binding)))
          (str2 (sxml:string
                 (arg-func2 nodeset root-node context var-binding))))
      (string-prefix? str2 str1))))

; contains(string, string)
(define (sxml:core-contains arg-func1 arg-func2)
  (lambda (nodeset root-node context var-binding)
    (let ((str1 (sxml:string
                 (arg-func1 nodeset root-node context var-binding)))
          (str2 (sxml:string
                 (arg-func2 nodeset root-node context var-binding))))
      (if (substring? str2 str1) #t #f)  ; must return a boolean
      )))
  
; substring-before(string, string)
(define (sxml:core-substring-before arg-func1 arg-func2)
  (lambda (nodeset root-node context var-binding)
    (let* ((str1 (sxml:string
                  (arg-func1 nodeset root-node context var-binding)))
           (str2 (sxml:string
                  (arg-func2 nodeset root-node context var-binding)))
           (pos (substring? str2 str1)))
      (if (not pos)  ; STR1 doesn't contain STR2
          ""
          (substring str1 0 pos)))))

; substring-after(string, string)
(define (sxml:core-substring-after arg-func1 arg-func2)
  (lambda (nodeset root-node context var-binding)
    (let* ((str1 (sxml:string
                  (arg-func1 nodeset root-node context var-binding)))
           (str2 (sxml:string
                  (arg-func2 nodeset root-node context var-binding)))
           (pos (substring? str2 str1)))
      (if
       (not pos)  ; STR1 doesn't contain STR2
       ""
       (substring
        str1 (+ pos (string-length str2)) (string-length str1))))))

; substring(string, number, number?)
(define (sxml:core-substring arg-func1 arg-func2 . arg-func3)
  (if (null? arg-func3)  ; no third argument supplied
      (lambda (nodeset root-node context var-binding)
        (let ((str (sxml:string
                    (arg-func1 nodeset root-node context var-binding)))
              (num1 (sxml:number
                     (arg-func2 nodeset root-node context var-binding))))
          (let ((len (string-length str))
                (start (- (inexact->exact (round num1)) 1)))
            (if (> start len)
                ""
                (substring str (if (< start 0) 0 start) len)))))
      (let ((arg-func3 (car arg-func3)))
        (lambda (nodeset root-node context var-binding)
          (let ((str (sxml:string
                      (arg-func1 nodeset root-node context var-binding)))
                (num1 (sxml:number
                       (arg-func2 nodeset root-node context var-binding)))
                (num2 (sxml:number
                       (arg-func3 nodeset root-node context var-binding))))
            (let* ((len (string-length str))
                   (start (- (inexact->exact (round num1)) 1))
                   (fin (+ start (inexact->exact (round num2)))))
              (if (or (> start len) (< fin 0) (< fin start))
                  ""
                  (substring str
                             (if (< start 0) 0 start)
                             (if (> fin len) len fin)))))))))

; string-length(string?)
(define (sxml:core-string-length . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (string-length (sxml:string nodeset)))
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (string-length
           (sxml:string
            (func nodeset root-node context var-binding)))))))

; normalize-space(string?)
(define (sxml:core-normalize-space . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (let rpt ((src (string-split (sxml:string nodeset) sxml:whitespace))
                  (res '()))
          (cond
            ((null? src)
             (apply string-append (reverse res)))
            ((= (string-length (car src)) 0)  ; empty string
             (rpt (cdr src) res))
            ((null? res)
             (rpt (cdr src) (cons (car src) res)))
            (else
             (rpt (cdr src) (cons (car src) (cons " " res)))))))
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (let rpt ((src (string-split
                          (sxml:string
                           (func nodeset root-node context var-binding))
                          sxml:whitespace))
                    (res '()))
            (cond
              ((null? src)
               (apply string-append (reverse res)))
              ((= (string-length (car src)) 0)  ; empty string
               (rpt (cdr src) res))
              ((null? res)
               (rpt (cdr src) (cons (car src) res)))
              (else
               (rpt (cdr src) (cons (car src) (cons " " res))))))))))

; translate(string, string, string)
(define (sxml:core-translate arg-func1 arg-func2 arg-func3)
  (lambda (nodeset root-node context var-binding)
    (let ((str1 (sxml:string
                 (arg-func1 nodeset root-node context var-binding)))
          (str2 (sxml:string
                 (arg-func2 nodeset root-node context var-binding)))
          (str3 (sxml:string
                 (arg-func3 nodeset root-node context var-binding))))
      (let ((alist
             (let while ((lst2 (string->list str2))
                         (lst3 (string->list str3))
                         (alist '()))
               (cond
                 ((null? lst2) (reverse alist))
                 ((null? lst3)
                  (append
                   (reverse alist)
                   (map
                    (lambda (ch) (cons ch #f))
                    lst2)))
                 (else
                  (while
                   (cdr lst2)
                   (cdr lst3)
                   (cons (cons (car lst2) (car lst3)) alist)))))))
        (let rpt ((lst1 (string->list str1))
                  (res '()))
          (cond
            ((null? lst1) (list->string (reverse res)))
            ((assoc (car lst1) alist)
             => (lambda (pair)
                  (if (cdr pair)
                      (rpt (cdr lst1) (cons (cdr pair) res))
                      (rpt (cdr lst1) res))))
            (else
             (rpt (cdr lst1) (cons (car lst1) res)))))))))
  

;-------------------------------------------------
; 4.3 Boolean Functions

; boolean(object)
(define (sxml:core-boolean arg-func)
  (lambda (nodeset root-node context var-binding)
    (sxml:boolean 
     (arg-func nodeset root-node context var-binding))))

; not(boolean)
(define (sxml:core-not arg-func)
  (lambda (nodeset root-node context var-binding)
    (not (sxml:boolean 
          (arg-func nodeset root-node context var-binding)))))

; true()
(define (sxml:core-true)
  (lambda (nodeset root-node context var-binding) #t))

; false()
(define (sxml:core-false)
  (lambda (nodeset root-node context var-binding) #f))

; lang(string)
(define (sxml:core-lang arg-func)
  (lambda (nodeset root-node context var-binding)
    (if
     (null? nodeset)
     #f
     (let ((arg (sxml:string
                 (arg-func nodeset root-node context var-binding)))
           (context-node (car nodeset)))
       (let rpt ((pairs
                  (map
                   (lambda (node) (cons node #f))
                   root-node)))
         (if
          (null? pairs)  ; context node not found
          #f
          (let* ((lng
                  ((sxml:child (ntype?? '*text*))
                   ((sxml:attribute (ntype?? 'xml:lang))
                    (caar pairs))))
                 (lng (if (null? lng) (cdar pairs) (car lng))))
            (if
             (eq? context-node (caar pairs)) ; context node found
             (and
              lng
              (or (string-ci=? arg lng)
                  (string-prefix-ci? (string-append arg "-") lng)))
             (rpt
              (append
               (map
                (lambda (node) (cons node lng))
                ((sxml:attribute (ntype?? '*)) (caar pairs)))
               (map
                (lambda (node) (cons node lng))
                ((sxml:child sxml:node?) (caar pairs)))
               (cdr pairs)))))))))))
  

;-------------------------------------------------
; 4.4 Number Functions

; number(object?)
(define (sxml:core-number . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset root-node context var-binding)
        (sxml:number nodeset))
      (let ((func (car arg-func)))
        (lambda (nodeset root-node context var-binding)
          (sxml:number 
           (func nodeset root-node context var-binding))))))

; sum(node-set)
(define (sxml:core-sum arg-func)
  (lambda (nodeset root-node context var-binding)
    (let ((res (arg-func nodeset root-node context var-binding)))
      (cond
        ((nodeset? res)
         (apply +
                (map
                 (lambda (node)
                   (sxml:number (sxml:string-value node)))
                 res)))
        (else
         (sxml:xpointer-runtime-error
          "SUM function - an argument is not a nodeset")
         0)))))

; floor(number)
(define (sxml:core-floor arg-func)
  (lambda (nodeset root-node context var-binding)
    (inexact->exact
     (floor (sxml:number 
             (arg-func nodeset root-node context var-binding))))))

; ceiling(number)
(define (sxml:core-ceiling arg-func)
  (lambda (nodeset root-node context var-binding)
    (inexact->exact
     (ceiling (sxml:number
               (arg-func nodeset root-node context var-binding))))))

; round(number)
(define (sxml:core-round arg-func)
  (lambda (nodeset root-node context var-binding)
    (inexact->exact
     (round (sxml:number
             (arg-func nodeset root-node context var-binding))))))



;==========================================================================
; Parameters for classic TXPath implementation

(define sxml:classic-params
  `(
    ; For XPath axes, the result is returned in the form of the pair
    ; (cons  (lambda ...)  root-node-required)  
    ;  (lambda ...) - one of the axis functions
    ;  root-node-required - a boolean value
    ; If root-node-required = #t, lambda's signature is
    ;  (lambda (test-pred?)
    ;   (lambda (root-node)
    ;    (lambda (nodeset) ... )))
    ; otherwise
    ;  (lambda (test-pred?)
    ;   (lambda (nodeset) ... ))
    (axis
     ((ancestor
       ,(lambda (add-on) (cons sxml:ancestor #t)))
      (ancestor-or-self
       ,(lambda (add-on) (cons sxml:ancestor-or-self #t)))
      (attribute
       ,(lambda (add-on) (cons sxml:attribute #f)))
      (child
       ,(lambda (add-on) (cons sxml:child #f)))
      (descendant
       ,(lambda (add-on) (cons sxml:descendant #f)))
      (descendant-or-self
       ,(lambda (add-on) (cons sxml:descendant-or-self #f)))
      (following
       ,(lambda (add-on) (cons sxml:following #t)))
      (following-sibling
       ,(lambda (add-on) (cons sxml:following-sibling #t)))
      (namespace
       ,(lambda (add-on) (cons sxml:namespace #f)))
      (parent
       ,(lambda (add-on) (cons sxml:parent #t)))
      (preceding
       ,(lambda (add-on) (cons sxml:preceding #t)))
      (preceding-sibling
       ,(lambda (add-on) (cons sxml:preceding-sibling #t)))
      (self
       ,(lambda (add-on) (cons sxml:filter #f)))))
    
    ; For NodeTests, the result is
    ;  (lambda (node) ...)  - a node test function
    ; or 'txp:semantic-error (namely, for point and range)
    (node-test
     ((star
       ,(lambda (add-on) (ntype?? '*)))
      (uri+star
       ,(lambda (uri add-on) (ntype-namespace-id?? uri)))
      (qname
       ,(lambda (uri local-name add-on)
          (if (not uri)
              (ntype?? (string->symbol local-name))
              (ntype?? (string->symbol (string-append uri ":" local-name))))))
      (comment
       ,(lambda (add-on) (ntype?? '*COMMENT*)))
      (text
       ,(lambda (add-on) (ntype?? '*text*)))
      (processing-instruction
       ,(lambda (literal-string add-on)
          (if (not literal-string)  ; no literal provided
              (lambda (node)
                (and (pair? node) (eq? (car node) '*PI*)))
              (let ((literal (string->symbol literal-string)))
                (lambda (node)
                  (and (pair? node)
                       (eq? (car node) '*PI*)
                       (equal? (cadr node) literal)))))))
      (node
       ,(lambda (add-on) sxml:node?))
      (point
       ,(lambda (add-on)
          (txp:signal-semantic-error
           "point() NodeTest is not supported by this implementation")))
      (range
       ,(lambda (add-on)
          (txp:signal-semantic-error
           "range() NodeTest is not supported by this implementation")))))
    
    ;-------------
    ; The remaining parameter values return the following
    ; (lambda (nodeset root-node context var-binding) - an SXPath-like
    ; function (it transforms a nodeset into a new nodeset)
    ;  nodeset - a current set of nodes
    ;  root-node - the root of a document (a singleton nodeset)
    ;  context - the context of the node; list of two elements - (position size)
    ;  position - context position (a number)
    ;  size - context size (a number)
    
    ; Parse step implementation
    (step
     ((common
       ,(lambda (axis-res node-test-res predicate-res-lst add-on)
          (let ((axis (car axis-res))
                (root-node-required (cdr axis-res)))
            (if
             (null? predicate-res-lst)
             (lambda (nodeset root-node context var-binding)
               (if root-node-required
                   (((axis node-test-res) root-node) nodeset)
                   ((axis node-test-res) nodeset)))
             (lambda (nodeset root-node context var-binding)
               (map-union
                (lambda (node)
                  (sxml:xpath-nodeset-filter 
                   predicate-res-lst
                   ((if root-node-required
                        ((axis node-test-res) root-node)
                        (axis node-test-res))
                    node)
                   root-node var-binding))
                nodeset))))))
      (range-to
       ,(lambda (expr-res predicate-res-lst add-on)
          (txp:signal-semantic-error "range-to function not implemented")))))
    
    ; Relative location path implementation
    (relative-lpath
     ,(lambda (step-res-lst add-on)
        (if
         (null? (cdr step-res-lst))  ; the only step
         (car step-res-lst)
         (lambda (nodeset root-node context var-binding)
           (let rpt ((nset nodeset)
                     (fs step-res-lst))
             (if (null? fs)
                 nset
                 (rpt ((car fs) nset root-node context var-binding)
                      (cdr fs))))))))
    
    ; Location path implementation
    (location-path
     ((bare-slash
       ,(lambda (add-on)
          (lambda (nodeset root-node context var-binding) root-node)))       
      (slash
       ,(lambda (relative-lpath-res add-on)
          (lambda (nodeset root-node context var-binding)
            (relative-lpath-res root-node root-node context var-binding))))
      (double-slash
       ,(lambda (relative-lpath-res add-on)
          (lambda (nodeset root-node context var-binding)
            (relative-lpath-res
             ((sxml:descendant-or-self sxml:node?) root-node)
             root-node context var-binding))))))
    
    ; Predicate implementation
    ; Note that (according to specification) a Predicate must return a number
    ; or a boolean value. However, the return value type is not checked in this
    ; function. This is performed in functions that use 'parse-predicate'
    (predicate
     ,(lambda (expr-res add-on) expr-res))  ; similar to identity function
    
    ; Variable reference implementation
    (variable-ref
     ,(lambda (var-name-string add-on)
        (let ((name (string->symbol var-name-string)))
          (lambda (nodeset root-node context var-binding)
            (cond
              ((assoc name var-binding)
               => cdr)
              (else
               (sxml:xpointer-runtime-error "unbound variable - " name)
               '()))))))
    
    ; Function call implementation
    (function-call
     ,(lambda (fun-name-string arg-res-lst add-on)
        (let ((core-alist
               ; (list fun-name min-num-args max-num-args impl)
               `((last 0 0 ,sxml:core-last)
                 (position 0 0 ,sxml:core-position)
                 (count 1 1 ,sxml:core-count)
                 (id 1 1 ,sxml:core-id)
                 (local-name 0 1 ,sxml:core-local-name)
                 (namespace-uri 0 1 ,sxml:core-namespace-uri)
                 (name 0 1 ,sxml:core-name)
                 (string 0 1 ,sxml:core-string)
                 (concat 2 -1 ,sxml:core-concat)
                 (starts-with 2 2 ,sxml:core-starts-with)
                 (contains 2 2 ,sxml:core-contains)
                 (substring-before 2 2 ,sxml:core-substring-before)
                 (substring-after 2 2 ,sxml:core-substring-after)
                 (substring 2 3 ,sxml:core-substring)
                 (string-length 0 1 ,sxml:core-string-length)
                 (normalize-space 0 1 ,sxml:core-normalize-space)
                 (translate 3 3 ,sxml:core-translate)
                 (boolean 1 1 ,sxml:core-boolean)
                 (not 1 1 ,sxml:core-not)
                 (true 0 0 ,sxml:core-true)
                 (false 0 0 ,sxml:core-false)
                 (lang 1 1 ,sxml:core-lang)
                 (number 0 1 ,sxml:core-number)
                 (sum 1 1 ,sxml:core-sum)
                 (floor 1 1 ,sxml:core-floor)
                 (ceiling 1 1 ,sxml:core-ceiling)
                 (round 1 1 ,sxml:core-round))))
          (cond
           ((assq (string->symbol fun-name-string) core-alist)
            => (lambda (quad)  ; Core function found
                 (cond
                   ((< (length arg-res-lst) (cadr quad))
                    (txp:signal-semantic-error
                     "too few arguments for the Core Function call - "
                     fun-name-string))
                   ((and (> (caddr quad) 0)
                         (> (length arg-res-lst) (caddr quad)))
                    (txp:signal-semantic-error
                     "too many arguments for the Core Function call - "
                     fun-name-string))
                   (else  ; correct number of arguments
                    ; Producing a function implementation
                    (apply (cadddr quad) arg-res-lst)))))
           (else  ; function definition not found
            (txp:signal-semantic-error
             "function call to an unknown function - " fun-name-string))))))
    
    ; Primary expression
    (primary-expr
     ((literal
       ,(lambda (literal add-on)
          (lambda (nodeset root-node context var-binding) literal)))
      (number
       ,(lambda (number add-on)
          (lambda (nodeset root-node context var-binding) number)))))

    ; Filter expression
    (filter-expr
     ,(lambda (primary-expr-res predicate-res-lst add-on)
        (lambda (nodeset root-node context var-binding)
          (let ((nodeset
                 (primary-expr-res nodeset root-node context var-binding)))
            (sxml:xpath-nodeset-filter
             predicate-res-lst
             (cond
               ((nodeset? nodeset) nodeset)
               (else 
                (sxml:xpointer-runtime-error 
                 "expected - nodeset instead of " nodeset)
                '()))
             root-node var-binding)))))
    
    ; Path expression
    (path-expr
     ((slash
       ,(lambda (filter-expr-res relative-lpath-res add-on)
          (lambda (nodeset root-node context var-binding)
            (let ((nset
                   (filter-expr-res nodeset root-node context var-binding)))
              (let ((nset 
                     (cond
                       ((nodeset? nset) nset)
                       (else 
                        (sxml:xpointer-runtime-error 
                         "expected - nodeset instead of " nset)
                        '()))))
                (relative-lpath-res nset root-node context var-binding))))))
      (double-slash
       ,(lambda (filter-expr-res relative-lpath-res add-on)
          (lambda (nodeset root-node context var-binding)
            (let ((nset
                   (filter-expr-res nodeset root-node context var-binding)))
              (let ((nset 
                     (cond
                       ((nodeset? nset) nset)
                       (else 
                        (sxml:xpointer-runtime-error 
                         "expected - nodeset instead of " nset)
                        '()))))
                (let ((nset ((sxml:descendant-or-self sxml:node?) nset)))
                  (relative-lpath-res
                   nset root-node context var-binding)))))))))
    
    ; Union expression
    (union-expr
     ,(lambda (path-expr-res-lst add-on)
        (lambda (nodeset root-node context var-binding)
          (let rpt ((res '())
                    (fs path-expr-res-lst))
            (if
             (null? fs)
             res
             (let ((nset ((car fs) nodeset root-node context var-binding)))
               (rpt
                (append 
                 res
                 (cond
                   ((not (nodeset? nset))
                    (sxml:xpointer-runtime-error 
                     "expected - nodeset instead of " nset)
                    '())
                   (else nset)))
                (cdr fs))))))))
    
    ; Unary expression
    (unary-expr
     ,(lambda (union-expr-res num-minuses add-on)
        (if (even? num-minuses)
            (lambda (nodeset root-node context var-binding)
              (sxml:number
               (union-expr-res nodeset root-node context var-binding)))
            (lambda (nodeset root-node context var-binding)
              (- (sxml:number
                  (union-expr-res nodeset root-node context var-binding)))))))
    
    ; Different operations
    (operations
     ((* ,(lambda (add-on) *))
      (div ,(lambda (add-on) /))
      (mod ,(lambda (add-on) remainder))
      (+ ,(lambda (add-on) +))
      (- ,(lambda (add-on) -))
      (< ,(lambda (add-on) (sxml:relational-cmp <)))
      (> ,(lambda (add-on) (sxml:relational-cmp >)))
      (<= ,(lambda (add-on) (sxml:relational-cmp <=)))
      (>= ,(lambda (add-on) (sxml:relational-cmp >=)))
      (= ,(lambda (add-on) sxml:equal?))
      (!= ,(lambda (add-on) sxml:not-equal?))))
    
    ; Additive and multiplicative expressions
    (mul-expr ,sxml:arithmetic-eval)
    (add-expr ,sxml:arithmetic-eval)
    
    ; Relational expression
    (relational-expr
     ,(lambda (additive-expr-res-lst cmp-op-lst add-on)
        (lambda (nodeset root-node context var-binding)
          (let rpt ((res ((car additive-expr-res-lst)
                          nodeset root-node context var-binding))
                    (fs (cdr additive-expr-res-lst))
                    (ops cmp-op-lst))
            (if (null? fs)
                res
                (rpt ((car ops)
                      res
                      ((car fs) nodeset root-node context var-binding))
                     (cdr fs)
                     (cdr ops)))))))        
    
    ; Equality expression
    (equality-expr
     ,(lambda (relational-expr-res-lst cmp-op-lst add-on)
        (lambda (nodeset root-node context var-binding)
          (let rpt ((res ((car relational-expr-res-lst)
                          nodeset root-node context var-binding))
                    (fs (cdr relational-expr-res-lst))
                    (ops cmp-op-lst))
            (if (null? fs)
                res
                (rpt ((car ops) 
                      res 
                      ((car fs) nodeset root-node context var-binding))
                     (cdr fs)
                     (cdr ops)))))))
    
    ; And-expression
    ; Note that according to 3.4 in XPath specification, the right operand
    ; is not evaluated if the left operand evaluates to false
    (and-expr
     ,(lambda (equality-expr-res-lst add-on)
        (lambda (nodeset root-node context var-binding)
          (let rpt ((fs equality-expr-res-lst))
            (cond
              ((null? fs) #t)
              ((not (sxml:boolean
                     ((car fs) nodeset root-node context var-binding))) #f)
              (else (rpt (cdr fs))))))))
    
    ; Or-expression
    (or-expr
     ,(lambda (and-expr-res-lst add-on)    
        (lambda (nodeset root-node context var-binding)
          (let rpt ((fs and-expr-res-lst))
            (cond
              ((null? fs) #f)
              ((sxml:boolean
                ((car fs) nodeset root-node context var-binding)) #t)
              (else (rpt (cdr fs))))))))
    
    ; Full XPointer
    (full-xptr
     ,(lambda (expr-res-lst add-on)
        (lambda (nodeset root-node context var-binding)
          (let rpt ((fs expr-res-lst))
            (if (null? fs)
                '()
                (let ((nset ((car fs) nodeset root-node context var-binding)))
                  (if (null? nset)
                      (rpt (cdr fs))
                      nset)))))))
    
    ; XPointer child sequence
    (child-seq
     ((with-name
      ,(lambda (name-string number-lst add-on)
         (let ((funcs
                 (apply append
                        (map
                         (lambda (num)
                           (list (sxml:child (ntype?? '*)) (node-pos num)))
                         number-lst))))
           (lambda (nodeset root-node context var-binding)
             (let ((id-nset ((sxml:child (ntype?? 'id-index))
                             ((sxml:child (ntype?? '@@)) root-node))))
               (if
                (null? id-nset)  ; no id-index
                '()
                (let ((nd (sxml:lookup name-string (cdar id-nset))))
                  (if (not nd)
                      '()
                      (let rpt ((nset (list nd))
                                (fs funcs))
                        (if (null? fs)
                            nset
                            (rpt ((car fs) nset) (cdr fs))))))))))))
      (without-name
       ,(lambda (number-lst add-on)
          (let ((funcs
                 (apply append
                        (map
                         (lambda (num)
                           (list (sxml:child (ntype?? '*)) (node-pos num)))
                         number-lst))))
            (lambda (nodeset root-node context var-binding)
              (if (nodeset? nodeset)
                  (let rpt ((nodeset nodeset) (res '()))
                    (if (null? nodeset)
                        res
                        (let rpt2 ((nset (list (car nodeset))) 
                                   (fs funcs))
                          (if (null? fs)
                              (rpt (cdr nodeset) (append res nset))
                              (rpt2 ((car fs) nset) (cdr fs))))))
                  (let rpt ((nodeset nodeset) (fs funcs))
                    (if (null? fs)
                        nodeset
                        (rpt ((car fs) nodeset) (cdr fs)))))))))))                
    ))
     
;=========================================================================
; Highest level API functions

;------------------------------------------------
; 'sxml:xpath' and 'sxml:xpointer' functions
;
;  xpath-string - an XPath location path (a string)
;  ns-binding - declared namespace prefixes (an optional argument)
;  ns-binding = (list  (prefix . uri)
;                      (prefix . uri)
;                      ...)
;  prefix - a symbol
;  uri - a string
;
; The returned result:   (lambda (node . var-binding) ...)  
;                   or   #f
;  #f - signals of a parse error (error message is printed as a side effect
; during parsing)
;  (lambda (node . var-binding) ...)  - an SXPath function
;  node - a node (or a node-set) of the SXML document
;  var-binding - XPath variable bindings (an optional argument)
;  var-binding = (list  (var-name . value)
;                       (var-name . value)
;                       ...)
;  var-name - (a symbol) a name of a variable
;  value - its value. The value can have the following type: boolean, number,
; string, nodeset. NOTE: a node must be represented as a singleton nodeset
; 
; Administrative SXPath variables:
;  *root* - if presented in the 'var-binding', its value (a node or a nodeset)
; specifies the root of the SXML document

(define (sxml:api-helper0 parse-proc)
  (lambda (xpath-string . ns-binding)
    (let ((res (parse-proc
                xpath-string
                (if (null? ns-binding) ns-binding (car ns-binding))
                '())))
      (if (txp:error? res)  ; error detected
          #f
          (lambda (node . var-binding)
            (let ((node (as-nodeset node)))
              (if
               (null? var-binding)  ; no variables supplied
               (res node node (cons 1 1) '())
               (let ((var-binding (car var-binding)))
                 (res
                  node
                  (cond ((assq '*root* var-binding)
                         => (lambda (pair) (as-nodeset (cdr pair))))
                        (else node))
                  (cons 1 1)
                  var-binding)))))))))

(define sxml:classic-res (txp:parameterize-parser sxml:classic-params))

(define (sxml:api-helper parse-proc)
  (lambda (xpath-string . ns-binding)
    (let ((res (parse-proc
                xpath-string
                (if (null? ns-binding) ns-binding (car ns-binding))
                '())))
      (if (txp:error? res)  ; error detected
          #f
          (lambda (node . var-binding)
            (let ((node (as-nodeset node)))
              (if
               (null? var-binding)  ; no variables supplied
               (res node node (cons 1 1) '())
               (let ((var-binding (car var-binding)))
                 (res
                  node
                  (cond ((assq '*root* var-binding)
                         => (lambda (pair) (as-nodeset (cdr pair))))
                        (else node))
                  (cons 1 1)
                  var-binding)))))))))
              
(define sxml:xpath
  (sxml:api-helper (cadr (assq 'xpath sxml:classic-res))))
(define sxml:xpointer
  (sxml:api-helper (cadr (assq 'xpointer sxml:classic-res))))
(define sxml:xpath-expr
  (sxml:api-helper (cadr (assq 'expr sxml:classic-res))))

; Some (deprecated!) aliases for backward compatibility
; which will be eventually removed
(define sxml:xpath+root+vars sxml:xpath)
(define sxml:xpointer+root+vars sxml:xpointer)
(define sxml:xpath+root sxml:xpath)
(define txpath sxml:xpath)


;------------------------------------------------
; 'sxml:xpath+index' and 'sxml:xpointer+index' functions
;
; NOTE: THESE FUNCTIONS ARE JUST STUBS NOW, BECAUSE THEY ALWAYS RETURN #t
; FOR 'index-required'. THESE FUNCTIONS ARE INCLUDED HERE FOR THE SAKE OF
; BACKWARD COMPATIBILITY ONLY.
;
;  xpath-string - an XPath location path (a string)
;  ns-binding - declared namespace prefixes (an optional argument)
;  ns-binding = (list  (prefix . uri)
;                      (prefix . uri)
;                      ...)
;  prefix - a symbol
;  uri - a string
;
; The returned result:   (cons (lambda (node . id-index) ...)  
;                              index-required )
;                   or   #f
;  #f - signals of a parse error (error message is printed as a side effect
; during parsing)
;  (lambda (node) ...)  - an SXPath function
;  node - a root node of the SXML document
;  index-required - a boolean value: whether an id-index is required

(define (sxml:api-index-helper parse-proc)
  (lambda (xpath-string . ns-binding)
    (let ((res (parse-proc
                xpath-string
                (if (null? ns-binding) ns-binding (car ns-binding))
                '())))
      (if (txp:error? res)  ; error detected
          #f
          (cons
           (lambda (node)
             (let ((node (as-nodeset node)))
               (res node node (cons 1 1) '())))
           #t)))))
     
(define sxml:xpath+index
  (sxml:api-index-helper (cadr (assq 'xpath sxml:classic-res))))
(define sxml:xpointer+index
  (sxml:api-index-helper (cadr (assq 'xpointer sxml:classic-res))))

(provide (all-defined)))
