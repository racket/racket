#lang scheme/base

(require syntax/parse (for-syntax syntax/parse scheme/base stxclass/util))

(provide (except-out (all-defined-out) define-pred-stxclass))

(define-syntax (parse/get stx)
  (syntax-parse stx
    [(_ arg:expr attr:id pat)
     (let* ([i (generate-temporary)]
            [get-i (datum->syntax 
		    i 
		    (string->symbol 
		     (string-append (symbol->string (syntax-e i)) 
				    "."
				    (symbol->string #'attr.datum))))])
       (quasisyntax/loc stx
         (syntax-parse arg 
           [#,i #:declare #,i pat #'#,get-i])))]))

(define (atom? v)
  (or (number? v) (string? v) (boolean? v) (symbol? v) (keyword? v) (char? v) (bytes? v) (regexp? v)))

(define-syntax-class (3d pred)
  (pattern s           
           #:with datum (syntax-e #'s)
           #:fail-unless (pred #'datum) #f))

(define-syntax-rule (define-pred-stxclass name pred)
  (define-syntax-class name #:attributes ()
    (pattern x
             #:fail-unless (pred (syntax-e #'x)) #f)))

(define-pred-stxclass atom atom?)
(define-pred-stxclass byte-pregexp byte-pregexp?)
(define-pred-stxclass byte-regexp byte-regexp?)
(define-pred-stxclass regexp regexp?)
(define-pred-stxclass bytes bytes?)
