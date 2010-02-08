#lang scheme/base
(require (for-syntax scheme/base
                     scheme/private/sc
                     unstable/syntax
                     unstable/struct
                     "minimatch.ss"
                     "rep-data.ss"
                     "rep.ss")
         scheme/list
         syntax/stx
         "parse.ss"
         "runtime.ss"
         "runtime-prose.ss")

(provide define-syntax-class
         define-splicing-syntax-class

         define-literal-set
         define-conventions
         syntax-class-parse
         syntax-class-attributes
         syntax-class-possible-errors

         debug-rhs
         debug-pattern
         debug-parse

         syntax-parse
         syntax-parser

         pattern
         ~var
         ~datum
         ~literal
         ~and
         ~or
         ~not
         ~seq
         ~between
         ~once
         ~optional
         ~rest
         ~describe
         ~!
         ~bind
         ~fail
         ~parse
         ...+

         attribute
         this-syntax)

(begin-for-syntax
 (define (defstxclass stx name args rhss splicing?)
   (with-syntax ([name name]
                 [(arg ...) args]
                 [rhss rhss])
     (let ([the-rhs
            (parameterize ((current-syntax-context stx))
              (parse-rhs #'rhss #f splicing? #:context stx))])
       (with-syntax ([parser (generate-temporary
                              (format-symbol "parse-~a" (syntax-e #'name)))]
                     [attrs (rhs-attrs the-rhs)]
                     [commit? (rhs-commit? the-rhs)])
         #`(begin (define-syntax name
                    (make stxclass 'name '(arg ...)
                          'attrs
                          ((syntax-local-certifier) (quote-syntax parser))
                          ((syntax-local-certifier) (quote-syntax description))
                          '#,splicing?
                          'commit?))
                  (define-values (parser description)
                    (functions/rhs name (arg ...) attrs rhss #,splicing? #,stx))))))))

(define-syntax (define-syntax-class stx)
  (syntax-case stx ()
    [(define-syntax-class name . rhss)
     (identifier? #'name)
     (defstxclass stx #'name #'() #'rhss #f)]
    [(define-syntax-class (name arg ...) . rhss)
     (andmap identifier? (syntax->list #'(name arg ...)))
     (defstxclass stx #'name #'(arg ...) #'rhss #f)]))

(define-syntax (define-splicing-syntax-class stx)
  (syntax-case stx ()
    [(define-splicing-syntax-class name . rhss)
     (identifier? #'name)
     (defstxclass stx #'name #'() #'rhss #t)]
    [(define-splicing-syntax-class (name arg ...) . rhss)
     (andmap identifier? (syntax->list #'(name arg ...)))
     (defstxclass stx #'name #'(arg ...) #'rhss #t)]))

(define-syntax (define-conventions stx)
  (syntax-case stx ()
    [(define-conventions name rule ...)
     (begin
       (unless (identifier? #'name)
         (raise-syntax-error #f "expected identifier" stx #'name))
       (with-syntax ([([entry (def ...)] ...)
                      (for/list ([line (check-conventions-rules #'(rule ...) stx)])
                        (let ([rx (car line)]
                              [den (cadr line)])
                          (let-values ([(den defs) (create-aux-def den)])
                            (list #`(list (quote #,rx)
                                          (make-den:delayed
                                           (quote-syntax #,(den:delayed-parser den))
                                           (quote-syntax #,(den:delayed-description den))
                                           (quote-syntax #,(den:delayed-class den))))
                                  defs))))])
         #'(begin
             def ... ...
             (define-syntax name
               (make-conventions
                (list entry ...))))))]))

(define-syntax (define-literal-set stx)
  (syntax-case stx ()
    [(define-literal-set name (lit ...))
     (begin
       (unless (identifier? #'name)
         (raise-syntax-error #f "expected identifier" stx #'name))
       (let ([lits (check-literals-list #'(lit ...) stx)])
         (with-syntax ([((internal external) ...) lits])
           #'(define-syntax name
               (make-literalset
                (list (list 'internal (quote-syntax external)) ...))))))]))

;; ----

(define-syntax (functions/rhs stx)
  (syntax-case stx ()
    [(functions/S-rhs name args attrs rhss splicing? ctx)
     (with-disappeared-uses
      (let ([rhs
             (parameterize ((current-syntax-context #'ctx))
               (parse-rhs #'rhss (syntax->datum #'attrs) (syntax-e #'splicing?)
                          #:context #'ctx))])
        #`(let ([get-description
                 (lambda args
                   #,(or (rhs-description rhs)
                         #'(symbol->string 'name)))])
            (values (parse:rhs #,rhs
                               attrs
                               args
                               get-description
                               splicing?)
                    get-description))))]))

(define-syntax (syntax-class-parse stx)
  (syntax-case stx ()
    [(_ s x arg ...)
     (parameterize ((current-syntax-context stx))
       (let* ([arg-count (length (syntax->list #'(arg ...)))]
              [stxclass (get-stxclass/check-arg-count #'s arg-count)]
              [attrs (stxclass-attrs stxclass)])
         (with-syntax ([parser (stxclass-parser-name stxclass)]
                       [(name ...) (map attr-name attrs)]
                       [(depth ...) (map attr-depth attrs)])
           #'(let ([raw (parser x arg ...)])
               (if (ok? raw)
                   (map vector '(name ...) '(depth ...) raw)
                   raw)))))]))

(define-syntax (syntax-class-attributes stx)
  (syntax-case stx ()
    [(_ s)
     (parameterize ((current-syntax-context stx))
       (let ([attrs (stxclass-attrs (get-stxclass #'s))])
         (with-syntax ([(a ...) (map attr-name attrs)]
                       [(depth ...) (map attr-depth attrs)])
           #'(quote ((a depth) ...)))))]))

(define-syntax (syntax-class-possible-errors stx)
  (syntax-case stx ()
    [(_ s)
     (parameterize ((current-syntax-context stx))
       (with-syntax ([p (stxclass-parser-name (get-stxclass #'s))])
         #'(remove-duplicates
            (map interpret-error-expression
                 (parser-errors p)))))]))

(define-syntax (debug-rhs stx)
  (syntax-case stx ()
    [(debug-rhs rhs)
     (let ([rhs (parse-rhs #'rhs #t #:context stx)])
       #`(quote #,rhs))]))

(define-syntax (debug-pattern stx)
  (syntax-case stx ()
    [(debug-pattern p)
     (let ([p (parse-whole-pattern #'p (new-declenv null) #:context stx)])
       #`(quote #,p))]))

(define-syntax-rule (debug-parse x p)
  (let/ec escape
    (parameterize ((current-failure-handler
                    (lambda (_ f)
                      (escape (failure->sexpr f)
                              (failure->sexpr (simplify-failure f))))))
      (syntax-parse x [p 'success]))))

(define-syntax (syntax-parse stx)
  (syntax-case stx ()
    [(syntax-parse stx-expr . clauses)
     (quasisyntax/loc stx
       (let ([x (datum->syntax #f stx-expr)])
         (parse:clauses x clauses #,stx)))]))

(define-syntax (syntax-parser stx)
  (syntax-case stx ()
    [(syntax-parser . clauses)
     (quasisyntax/loc stx
       (lambda (x)
         (let ([x (datum->syntax #f x)])
           (parse:clauses x clauses #,stx))))]))

(define-syntax with-patterns
  (syntax-rules ()
    [(with-patterns () . b)
     (let () . b)]
    [(with-patterns ([p x] . more) . b)
     (syntax-parse x [p (with-patterns more . b)])]))
