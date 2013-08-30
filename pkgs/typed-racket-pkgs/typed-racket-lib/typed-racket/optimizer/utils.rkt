#lang racket/base

(require racket/match unstable/sequence
         racket/dict syntax/id-table racket/syntax syntax/stx
         syntax/parse
         racket/promise
         (for-syntax racket/base syntax/parse racket/syntax)
         "../utils/utils.rkt"
         (utils tc-utils)
         (for-template racket/base)
         (types type-table utils subtype)
         (rep type-rep))

(provide *show-optimized-code*
         subtypeof? isoftype?
         mk-unsafe-tbl
         n-ary->binary n-ary-comp->binary
         opt-expr optimize
         define-unsafe-syntax-class
         define-literal-syntax-class
         define-merged-syntax-class
         syntax/loc/origin quasisyntax/loc/origin)

;; for tracking both origin and source location information
(define-syntax-rule (syntax/loc/origin loc op body)
  (syntax-track-origin (syntax/loc loc body) loc op))
(define-syntax-rule (quasisyntax/loc/origin loc op body)
  (syntax-track-origin (quasisyntax/loc loc body) loc op))

;; if set to #t, the optimizer will dump its result to stdout before compilation
(define *show-optimized-code* #f)

;; is the syntax object s's type a subtype of t?
(define (subtypeof? s t)
  (match (type-of s)
    [(tc-result1: (== t (lambda (x y) (subtype y x)))) #t] [_ #f]))
;; similar, but with type equality
(define (isoftype? s t)
  (match (type-of s)
         [(tc-result1: (== t type-equal?)) #t] [_ #f]))

;; generates a table matching safe to unsafe promitives
(define (mk-unsafe-tbl generic safe-pattern unsafe-pattern)
  (for/fold ([h (make-immutable-free-id-table)]) ([g (in-list generic)])
    (let ([f (format-id g safe-pattern g)] [u (format-id g unsafe-pattern g)])
      (dict-set (dict-set h g u) f u))))

;; unlike their safe counterparts, unsafe binary operators can only take 2 arguments
;; this works on operations that are (A A -> A)
(define (n-ary->binary op arg1 arg2 rest)
  (for/fold ([o arg1])
      ([e (in-syntax #`(#,arg2 #,@rest))])
    #`(#,op #,o #,e)))
;; this works on operations that are (A A -> B)
(define (n-ary-comp->binary op arg1 arg2 rest)
  ;; First, generate temps to bind the result of each arg2 args ...
  ;; to avoid computing them multiple times.
  (define lifted (stx-map (lambda (x) (generate-temporary)) #`(#,arg2 #,@rest)))
  ;; Second, build the list ((op arg1 tmp2) (op tmp2 tmp3) ...)
  (define tests
    (let loop ([res  (list #`(#,op #,arg1 #,(car lifted)))]
               [prev (car lifted)]
               [l    (cdr lifted)])
      (cond [(null? l) (reverse res)]
            [else (loop (cons #`(#,op #,prev #,(car l)) res)
                        (car l)
                        (cdr l))])))
  ;; Finally, build the whole thing.
  #`(let #,(for/list ([lhs (in-list lifted)]
                      [rhs (in-syntax #`(#,arg2 #,@rest))])
             #`(#,lhs #,rhs))
      (and #,@tests)))

;; to avoid mutually recursive syntax classes
;; will be set to the actual optimization function at the entry point
;; of the optimizer
(define optimize (make-parameter #f))

(define-syntax-class opt-expr
  #:commit
  #:attributes (opt)
  (pattern e:expr #:attr opt (delay ((optimize) #'e))))


(define-syntax (define-unsafe-syntax-class stx)
  (define-splicing-syntax-class spec
    #:attributes (class-name (literals 1) unsafe-id)
    (pattern (~seq class-name:id (literals:id ...) unsafe-id:id))
    (pattern literal:id
      #:with (literals ...) #'(literal)
      #:with class-name (format-id #'literal "~a^" #'literal)
      #:with unsafe-id (format-id #'literal "unsafe-~a" #'literal)))
  (syntax-parse stx
    [(_ :spec)
     #'(begin
         (define-literal-syntax-class literal (literals ...))
         (define-syntax-class class-name
           (pattern :literal #:with unsafe #'unsafe-id)))]))

(define-syntax (define-literal-syntax-class stx)
  (define-splicing-syntax-class spec
    #:attributes (name (literals 1))
    (pattern (~seq name:id (literals:id ...)))
    (pattern literal:id
             #:with (literals ...) #'(literal)
             #:with name (format-id #'literal "~a^" #'literal)))
  (syntax-parse stx
    ((_ :spec)
     #'(begin
         (define-syntax-class name
           #:commit
           #:literals (literals ...)
           (pattern (~and op (~or literals ...))
                    #:do [(add-disappeared-use (syntax-local-introduce #'op))]))))))

(define-syntax-rule (define-merged-syntax-class name (syntax-classes ...))
  (define-syntax-class name
    #:auto-nested-attributes
    (pattern (~var || syntax-classes)) ...))
