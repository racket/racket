; Module header is generated automatically
#cs(module xpath-ast mzscheme
(require (lib "ssax.ss" "web-server/tests/tmp/ssax"))
(require "xpath-parser.ss")

;; XPath/XPointer -> Abstract Syntax Tree parser
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin


;==========================================================================
; W3C textual XPath/XPointer -> AST

; Writing operations as an S-expression in an infix notation
(define (txp:ast-operation-helper expr-lst op-lst add-on)
  (let ((rev-expr-lst (reverse expr-lst)))
    (let loop ((exprs (cdr rev-expr-lst))
               (ops (reverse op-lst))
               (res (car rev-expr-lst)))
      (if (null? ops)
          res
          (loop (cdr exprs) (cdr ops)
                (list (car ops) (car exprs) res))))))

;-------------------------------------------------
; Parameters for TXPath -> AST implementation

(define txp:ast-params
  `(
    ; Axes
    (axis
     ((ancestor
       ,(lambda (add-on) 'ancestor))
      (ancestor-or-self
       ,(lambda (add-on) 'ancestor-or-self))
      (attribute
       ,(lambda (add-on) 'attribute))
      (child
       ,(lambda (add-on) 'child))
      (descendant
       ,(lambda (add-on) 'descendant))
      (descendant-or-self
       ,(lambda (add-on) 'descendant-or-self))
      (following
       ,(lambda (add-on) 'following))
      (following-sibling
       ,(lambda (add-on) 'following-sibling))
      (namespace
       ,(lambda (add-on) 'namespace))
      (parent
       ,(lambda (add-on) 'parent))
      (preceding
       ,(lambda (add-on) 'preceding))
      (preceding-sibling
       ,(lambda (add-on) 'preceding-sibling))
      (self
       ,(lambda (add-on) 'self))
      ; Addition by XLink
      (arc
       ,(lambda (add-on) 'arc))
      (traverse
       ,(lambda (add-on) 'traverse))
      (traverse-arc
       ,(lambda (add-on) 'traverse-arc))))
        
    ; Node test
    (node-test
     ((star
       ,(lambda (add-on) '((*))))
      (uri+star
       ,(lambda (uri add-on)
          `((namespace-uri ,uri))))
      (qname
       ,(lambda (uri local-name add-on)
          (if (not uri)
              `((local-name ,local-name))
              `((namespace-uri ,uri) (local-name ,local-name)))))      
      (comment
       ,(lambda (add-on) '((comment))))
      (text
       ,(lambda (add-on) '((text))))
      (processing-instruction
       ,(lambda (literal-string add-on)
          (if (not literal-string)  ; no literal provided
              '((pi))
              `((pi ,literal-string)))))
      (node
       ,(lambda (add-on) '((node))))
      (point
       ,(lambda (add-on) '((point))))
      (range
       ,(lambda (add-on) '((range))))))
            
    ; Location step
    (step
     ((common
       ,(lambda (axis-res node-test-res predicate-res-lst add-on)
          `(step
            (axis-specifier (,axis-res))
            (node-test ,@node-test-res)
            ,@predicate-res-lst)))
      (range-to
       ,(lambda (expr-res predicate-res-lst add-on)
          `(range-to
            (expr ,expr-res)
            ,@predicate-res-lst)))))
    
    ; Relative location path
    (relative-lpath
     ,(lambda (step-res-lst add-on)
        (cons 'relative-location-path step-res-lst)))
    
    ; Location path
    (location-path
     ((bare-slash
       ,(lambda (add-on) '(absolute-location-path)))
      (slash
       ,(lambda (relative-lpath-res add-on)
          (cons 'absolute-location-path (cdr relative-lpath-res))))                
      (double-slash
       ,(lambda (relative-lpath-res add-on)
          `(absolute-location-path
            (step
             (axis-specifier (descendant-or-self))
             (node-test (node)))
            ,@(cdr relative-lpath-res))))))
    
    ; Predicate
    (predicate
     ,(lambda (expr-res add-on)
        (list 'predicate expr-res)))
    
    ; Variable reference
    (variable-ref
     ,(lambda (var-name-string add-on)
        `(variable-reference ,var-name-string)))
    
    ; Function call
    (function-call
     ,(lambda (fun-name-string arg-res-lst add-on)
        `(function-call
          (function-name ,fun-name-string)
          ,@(map
             (lambda (arg-res) `(argument ,arg-res))
             arg-res-lst))))
                
    ; Primary expression
    (primary-expr
     ((literal
       ,(lambda (literal add-on)
          `(literal ,literal)))      
      (number
       ,(lambda (number add-on)
          `(number ,number)))))

    ; Filter expression
    (filter-expr
     ,(lambda (primary-expr-res predicate-res-lst add-on)
        `(filter-expr
          (primary-expr ,primary-expr-res)
          ,@predicate-res-lst)))
    
    ; Path expression
    (path-expr
     ((slash
       ,(lambda (filter-expr-res relative-lpath-res add-on)
          `(path-expr
            ,(if (eq? (car filter-expr-res) 'filter-expr)
                 filter-expr-res
                 `(filter-expr (primary-expr ,filter-expr-res)))
            ,@(cdr relative-lpath-res))))
      (double-slash
       ,(lambda (filter-expr-res relative-lpath-res add-on)
          `(path-expr
            ,(if (eq? (car filter-expr-res) 'filter-expr)
                 filter-expr-res
                 `(filter-expr (primary-expr ,filter-expr-res)))
            (step
             (axis-specifier (descendant-or-self))
             (node-test (node)))
            ,@(cdr relative-lpath-res))))))
    
    ; Union expression
    (union-expr
     ,(lambda (path-expr-res-lst add-on)
        (cons 'union-expr path-expr-res-lst)))            
    
    ; Unary expression
    (unary-expr
     ,(lambda (union-expr-res num-minuses add-on)
        (let loop ((n num-minuses)
                   (res union-expr-res))
          (if (= n 0) res
              (loop (- n 1) (list '- res))))))
    
    ; Different operations
    (operations
     ((* ,(lambda (add-on) '*))
      (div ,(lambda (add-on) 'div))
      (mod ,(lambda (add-on) 'mod))
      
      (+ ,(lambda (add-on) '+))
      (- ,(lambda (add-on) '-))
      (< ,(lambda (add-on) '<))
      (> ,(lambda (add-on) '>))
      (<= ,(lambda (add-on) '<=))
      (>= ,(lambda (add-on) '>=))
      (= ,(lambda (add-on) '=))
      (!= ,(lambda (add-on) '!=))))
    
    ; Additive and multiplicative expressions
    (mul-expr ,txp:ast-operation-helper)
    (add-expr ,txp:ast-operation-helper)
    
    ; Relational expression
    (relational-expr ,txp:ast-operation-helper)
    
    ; Equality expression
    (equality-expr ,txp:ast-operation-helper)
    
    ; And-expression
    (and-expr
     ,(lambda (equality-expr-res-lst add-on)
        (cons 'and equality-expr-res-lst)))
    
    ; Or-expression
    (or-expr
     ,(lambda (and-expr-res-lst add-on)
        (cons 'or and-expr-res-lst)))
    
    ; Full XPointer
    (full-xptr
     ,(lambda (expr-res-lst add-on)
        (cons 'full-xptr expr-res-lst)))
    
    ; XPointer child sequence
    (child-seq
     ((with-name
      ,(lambda (name-string number-lst add-on)
         `(child-seq
           (name ,name-string)
           ,@(map
              (lambda (num) (list 'number num))
              number-lst))))
      (without-name
       ,(lambda (number-lst add-on)
          (cons 'child-seq
                (map
                 (lambda (num) (list 'number num))
                 number-lst))))))
    ))
     
(define txp:ast-res (txp:parameterize-parser txp:ast-params))

;-------------------------------------------------
; Highest level API functions
;
;  xpath-string - an XPath location path (a string)
;  ns-binding - declared namespace prefixes (an optional argument)
;  ns-binding = (list  (prefix . uri)
;                      (prefix . uri)
;                      ...)
;  prefix - a symbol
;  uri - a string
;
; The returned result:   abstract-syntax-tree   or   #f
;  abstract-syntax-tree - an S-expression
;  #f - signals of a parse error (error message is printed as a side effect
; during parsing)

(define (txp:ast-api-helper parse-proc)
  (lambda (xpath-string . ns-binding)
    (let ((res (parse-proc
                xpath-string
                (if (null? ns-binding) ns-binding (car ns-binding))
                '())))
      (if (txp:error? res)  ; error detected
          #f res))))
              
(define txp:xpath->ast
  (txp:ast-api-helper (cadr (assq 'xpath txp:ast-res))))
(define txp:xpointer->ast
  (txp:ast-api-helper (cadr (assq 'xpointer txp:ast-res))))
(define txp:expr->ast
  (txp:ast-api-helper (cadr (assq 'expr txp:ast-res))))


;==========================================================================
; SXPath native syntax -> AST
; Additional features added to AST by native SXPath
; Operator += below denotes additional alternatives to AST grammar rules
; {7} <NodeTest> += (node-test (equal?  <SXML-node> ))
;                   | (node-test (eq?  <SXML-node> ))
;                   | (node-test (names  <String>+ ))
;                   | (node-test (not-names  <String>+ ))
; {4} <Step> += (lambda-step  <Lambda> )
;               | <FilterExpr>

(define (txp:sxpath->ast path . ns-binding)
  (let ((ns-binding (if (null? ns-binding) ns-binding (car ns-binding))))
    (if
     (string? path)  ; Just a textual XPath
     (txp:expr->ast path ns-binding)
     (let loop ((ast-steps '())              
                (path path))
       (cond
         ((null? path)  ; parsing is finished
          (if (null? ast-steps)  ; empty path
              '(absolute-location-path)
              (let ((forward-steps (reverse ast-steps)))
                (cons
                 (if (eq? (caar forward-steps) 'filter-expr)
                     'path-expr 'relative-location-path)
                 forward-steps))))
         ((procedure? (car path))
          (loop (cons (list 'lambda-step (car path))
                      ast-steps)
                (cdr path)))
         ((assq (car path) '((// . descendant-or-self) (.. . parent)))          
          => (lambda (pair)
                     (loop (cons
                            `(step (axis-specifier (,(cdr pair)))
                                   (node-test (node)))
                            ast-steps)
                           (cdr path))))
         ((symbol? (car path))
          (loop (cons
                 `(step (axis-specifier (child))
                        (node-test
                         ,(cond
                            ((assq (car path) '((* . (*)) (*text* . (text))))
                             => cdr)
                            (else
                             `(local-name ,(symbol->string (car path)))))))
                 ast-steps)
                (cdr path)))
         ((string? (car path))
          (and-let*   ; only for the location path for the moment
           ((txt-ast (txp:expr->ast (car path))))
           (loop (if (eq? (car txt-ast) 'relative-location-path)
                     (append (reverse (cdr txt-ast)) ast-steps)
                     (cons
                      `(filter-expr (primary-expr ,txt-ast))
                      ast-steps))
                 (cdr path))))
         ((and (pair? (car path)) (not (null? (car path))))
          (cond
            ((assq (caar path) '((*or* . names) (*not* . not-names)))
             => (lambda (pair)
                  (loop
                   (cons
                    `(step (axis-specifier (child))
                           (node-test
                            ,(cons (cdr pair)
                                   (map symbol->string (cdar path)))))
                    ast-steps)
                   (cdr path))))
            ((assq (caar path) '((equal? . equal?) (eq? . eq?)
                                 (ns-id:* . namespace-uri)))
             => (lambda (pair)
                  (loop
                   (cons `(step (axis-specifier (child))
                                (node-test ,(list (cdr pair) (cadar path))))
                         ast-steps)
                   (cdr path))))
            (else
             (let reducer ((reducing-path (cdar path))
                           (filters '()))
               (cond
                 ((null? reducing-path)
                  (if
                   (symbol? (caar path))  ; just a child axis
                   (loop
                    (cons
                     `(step
                       (axis-specifier (child))
                       (node-test (local-name ,(symbol->string (caar path))))
                       ,@(reverse filters))
                     ast-steps)
                    (cdr path))
                   (and-let*
                    ((select (txp:sxpath->ast (caar path) ns-binding)))
                    (loop
                     (cons `(filter-expr
                             (primary-expr ,select)                       
                             ,@(reverse filters))
                           ast-steps)
                     (cdr path)))))
                 ((number? (car reducing-path))
                  (reducer
                   (cdr reducing-path)
                   (cons
                    `(predicate
                      ,(if
                        (negative? (car reducing-path))  ; from end of nodeset
                        `(- (function-call (function-name "last"))
                            (number ,(- -1 (car reducing-path))))
                        `(number ,(car reducing-path))))
                    filters)))
                 (else
                  (and-let*
                   ((pred-ast
                     (txp:sxpath->ast (car reducing-path) ns-binding)))
                   (reducer
                    (cdr reducing-path)
                    (cons `(predicate ,pred-ast) filters)))))))))
          (else
           (cerr "Invalid path step: " (car path))
           #f))))))


;==========================================================================
; Several popular accessors and constructors for AST steps

; Whether a representation for location step
(define (txp:step? op)
  (and (pair? op) (eq? (car op) 'step)))

; Returns the axis specifier of the location step
; Argument: the AST representation of a location step
; Result: either '(child) and the like, or #f if the AST contains syntactic
; error
(define (txp:step-axis op)
  (and (txp:step? op)
       (not (null? (cdr op)))
       (pair? (cadr op)) (eq? (caadr op) 'axis-specifier)
       (cadadr op)))

; Returns the node test of the location step
; Argument: the AST representation of a location step
; Result: either '(*) and the like, or #f if the AST contains syntactic
; error
(define (txp:step-node-test op)
  (and (txp:step? op)
       (not (null? (cdr op))) (not (null? (cddr op)))
       (pair? (caddr op)) (eq? (caaddr op) 'node-test)
       (cadr (caddr op))))

; Returns predicate expressions of the location step
; Argument: the AST representation of a location step
; Result: either (listof  ast-expr)
;         or #f if syntactic error detected in a location step AST
(define (txp:step-preds op)
  (and (txp:step? op)
       (not (null? (cdr op))) (not (null? (cddr op)))
       (null? (filter
               (lambda (sub)  ; not a predicate representation
                 (not (and (pair? sub) (eq? (car sub) 'predicate))))
               (cdddr op)))
       (map cadr (cdddr op))))

; Constructs the AST representation for a given axis, node-test and
; a list of predicate expressions
; axis ::= '(child) and the like
; node-test ::= '(*) and the like
; pred-expr-list ::= (listof ast-expr)
(define (txp:construct-step axis node-test . pred-expr-list)
  `(step (axis-specifier ,axis)
         (node-test ,node-test)
         ,@(map
            (lambda (pred-expr) `(predicate ,pred-expr))
            pred-expr-list)))

(provide (all-defined)))
