#lang scheme/base

(require "sc.ss"
         "util.ss"
         syntax/stx
         syntax/kerncase
         scheme/struct-info
         scheme/private/contract-helpers
         (for-syntax scheme/base
                     "rep.ss")
         (for-template scheme/base
                       scheme/contract))
(provide (all-defined-out))

(define-syntax-rule (define-pred-stxclass name pred)
  (define-basic-syntax-class name
    () ;; ([datum 0])
    (lambda (x)
      (let ([d (if (syntax? x) (syntax-e x) x)])
        (if (pred d)
            null ;; (list d)
            (fail-sc x #:pattern 'name))))))

(define-pred-stxclass identifier symbol?)
(define-pred-stxclass boolean boolean?)
(define-pred-stxclass str string?)
(define-pred-stxclass character char?)
(define-pred-stxclass keyword keyword?)

(define-pred-stxclass number number?)
(define-pred-stxclass integer integer?)
(define-pred-stxclass exact-integer exact-integer?)
(define-pred-stxclass exact-nonnegative-integer exact-nonnegative-integer?)
(define-pred-stxclass exact-positive-integer exact-positive-integer?)

(define-syntax-rule (define-kw-stxclass name kw)
  (define-basic-syntax-class name
    ()
    (lambda (x)
      (if (and (identifier? x) (free-identifier=? x (quote-syntax kw)))
          null
          (fail-sc x #:pattern 'name)))))

(define-kw-stxclass lambda-kw #%lambda)
(define-kw-stxclass define-values-kw define-values)
(define-kw-stxclass define-syntaxes-kw define-syntaxes)

(define-syntax-class define-values-form
  (pattern (kw:define-values-kw (var:identifier ...) rhs)))
(define-syntax-class define-syntaxes-form
  (pattern (kw:define-syntaxes-kw (var:identifier ...) rhs)))
(define-syntax-class definition-form
  (pattern :define-values-form)
  (pattern :define-syntaxes-form))

(define-basic-syntax-class static
  ([datum 0] [value 0])
  (lambda (x)
    (if (identifier? x)
        (let/ec escape
          (define (bad)
            (escape
             (fail-sc x
                      #:pattern 'static
                      #:reason "not bound as syntax")))
          (let ([value (syntax-local-value x bad)])
            (list (syntax-e x) value)))
        (fail-sc x
                 #:pattern 'static
                 #:reason "not an identifier"))))

(define-basic-syntax-class (static-of name pred)
  ([value 0])
  (lambda (x name pred)
    (let/ec escape
      (define (bad)
        (escape (fail-sc x
                         #:pattern 'name
                         #:reason (format "not bound as ~a" name))))
      (if (identifier? x)
          (let ([value (syntax-local-value x bad)])
            (unless (pred value) (bad))
            (list value))
          (bad)))))

(define-basic-syntax-class struct-name
  ([descriptor 0]
   [constructor 0]
   [predicate 0]
   [accessor 1]
   [super 0]
   [complete? 0])
  (lambda (x)
    (if (identifier? x)
        (let/ec escape
          (define (bad)
            (escape
             (fail-sc x
                      #:pattern 'struct-name
                      #:reason "not bound as a struct name")))
          (let ([value (syntax-local-value x bad)])
            (unless (struct-info? value) (bad))
            (let ([lst (extract-struct-info value)])
              (let ([descriptor (list-ref lst 0)]
                    [constructor (list-ref lst 1)]
                    [predicate (list-ref lst 2)]
                    [accessors (list-ref lst 3)]
                    [super (list-ref lst 5)])
                (let ([r-accessors (reverse accessors)])
                  (list descriptor
                        constructor
                        predicate
                        (if (and (pair? r-accessors) (eq? #f (car r-accessors)))
                            (cdr r-accessors)
                            r-accessors)
                        super
                        (or (null? r-accessors) (not (eq? #f (car r-accessors))))))))))
        (fail-sc x
                 #:pattern 'struct-name
                 #:reason "not bound as a struct name"))))

(define-basic-syntax-class expr/local-expand
  ([expanded 0])
  (lambda (x)
    (list (local-expand x 'expression null))))

(define-basic-syntax-class expr/head-local-expand
  ([expanded 0])
  (lambda (x)
    (list (local-expand x 'expression (kernel-form-identifier-list)))))

(define-basic-syntax-class block/head-local-expand
  ([expanded-block 0]
   [expanded 1]
   [def 1]
   [vdef 1]
   [sdef 1]
   [expr 1])
  (lambda (x)
    (let-values ([(ex1 ex2 defs vdefs sdefs exprs)
                  (head-local-expand-and-categorize-syntaxes x #f #; #t)])
      (list ex1 ex2 defs vdefs sdefs exprs))))

(define-basic-syntax-class internal-definitions
  ([expanded-block 0]
   [expanded 1]
   [def 1]
   [vdef 1]
   [sdef 1]
   [expr 1])
  (lambda (x)
    (let-values ([(ex1 ex2 defs vdefs sdefs exprs)
                  (head-local-expand-and-categorize-syntaxes x #t #; #f)])
      (list ex1 ex2 defs vdefs sdefs exprs))))

(define-syntax-rule (define-contract-stxclass name c)
  (define-basic-syntax-class* (name)
    ([orig-stx 0])
    (lambda (x)
      (list #`(contract c
                        #,x
                        (quote #,(string->symbol (or (build-src-loc-string x) "")))
                        (quote #,(or (current-macro-name) '<this-macro>))
                        (quote-syntax #,(syntax/loc x (<there>))))
            x))))

(define-contract-stxclass expr/num number?)
(define-contract-stxclass expr/num->num (-> number? number?))

(define-basic-syntax-class* (expr)
  ()
  (lambda (x)
    (if (not (keyword? (syntax-e x)))
        (list x)
        (fail-sc x #:pattern 'expr #:reason "keyword"))))

;; FIXME: hack
(define expr/c-use-contracts? (make-parameter #t))

(define-basic-syntax-class* (expr/c contract)
  ([orig-stx 0])
  (lambda (x c)
    (if (not (keyword? (syntax-e x)))
        (if (expr/c-use-contracts?)
            (list #`(contract #,c
                              #,x
                              (quote #,(string->symbol
                                        (or (build-src-loc-string x) "")))
                              (quote #,(or (current-macro-name) '<this-macro>))
                              (quote-syntax #,(syntax/loc x (<there>))))
                  x)
            (list x x))
        (fail-sc x #:pattern 'expr #:reason "keyword"))))

(define-basic-syntax-class (term parser)
  ()
  (lambda (x p) (p x)))

(define-basic-syntax-class (term/pred pred)
  ()
  (lambda (x p)
    (if (p x)
        null
        (fail-sc x #:pattern 'term/pred))))

;; Aliases

(define-syntax id (make-rename-transformer #'identifier))
(define-syntax nat (make-rename-transformer #'exact-nonnegative-integer))
(define-syntax char (make-rename-transformer #'character))
