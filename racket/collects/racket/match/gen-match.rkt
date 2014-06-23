#lang racket/base

(require "patterns.rkt" "compiler.rkt"
         syntax/stx syntax/parse racket/syntax
         (for-template racket/base (only-in "runtime.rkt" match:error fail)))

(provide go go/one)

;; this transforms `match'-style clauses into ones acceptable to `go'
;; go : syntax syntax syntax -> syntax
(define (go/one parse stx expr clauses)
  (define-syntax-class cl
    #:description "a clause with a pattern and a result"
    (pattern [p . rhs]
             #:with res (syntax/loc this-syntax [(p) . rhs])))
  (syntax-parse clauses
    [(c:cl ...)
     (go parse stx (quasisyntax/loc expr (#,expr))
         #'(c.res ...))]))

;; this parses the clauses using parse, then compiles them
;; go : syntax syntax syntax -> syntax
(define (go parse stx es clauses)
  (syntax-parse clauses
    [([pats . rhs] ...)
     (parameterize ([orig-stx stx])
       (unless (syntax->list es)
         (raise-syntax-error 'match* "expected a sequence of expressions to match" es)))
     (define/with-syntax form-name
       (syntax-case stx ()
         [(fname . _)
          (identifier? #'fname)
          (syntax-e #'fname)]
         [_ 'match]))
     (define len (length (syntax->list es)))
     (define srcloc-list (list #`(quote #,(syntax-source stx))
                               #`(quote #,(syntax-line stx))
                               #`(quote #,(syntax-column stx))
                               #`(quote #,(syntax-position stx))
                               #`(quote #,(syntax-span stx))))
     (define/with-syntax (xs ...) (generate-temporaries es))
     (define/with-syntax (exprs ...) es)
     (define/with-syntax outer-fail (generate-temporary #'fail))
     (define/with-syntax orig-expr (if (= 1 len) (stx-car #'(xs ...)) #'(list xs ...)))
     (define/with-syntax raise-error
       (quasisyntax/loc stx (match:error orig-expr (list (srcloc #,@srcloc-list)) 'form-name)))
     (define parsed-clauses
       (for/list ([clause (syntax->list clauses)]
                  [pats (syntax->list #'(pats ...))]
                  [rhs (syntax->list #'(rhs ...))])
         (unless (syntax->list pats)
           (raise-syntax-error 'match* "expected a sequence of patterns" pats))
         (define lp (length (syntax->list pats)))
         (unless (= len lp)
           (raise-syntax-error 
            'match (format "wrong number of match clauses, expected ~a and got ~a" len lp) pats))
         (define (mk unm rhs)
           (make-Row (for/list ([p (syntax->list pats)]) (parse p))
                     (syntax-property
                      (quasisyntax/loc stx
                        (let () . #,rhs))
                      'feature-profile:pattern-matching 'antimark)
                     unm null))
         (syntax-parse rhs
           [()
            (raise-syntax-error 
             'match
             "expected at least one expression on the right-hand side"
             clause)]
           [(#:when e)
            (raise-syntax-error 
             'match
             "expected at least one expression on the right-hand side after #:when clause"
             clause)]
           [(#:when e rest ...) (mk #f #'((if e (let () rest ...) (fail))))]
           [(((~datum =>) unm) . rhs) (mk #'unm #'rhs)]
           [_ (mk #f rhs)])))
     (define/with-syntax body 
       (compile* (syntax->list #'(xs ...)) parsed-clauses #'outer-fail))
     (define/with-syntax (exprs* ...)
       (for/list ([e (in-list (syntax->list #'(exprs ...)))])
         (syntax-property e 'feature-profile:pattern-matching 'antimark)))
    (syntax-property
     (quasisyntax/loc stx
      (let ([xs exprs*] ...)
        (define (outer-fail) raise-error)
        body))
     'feature-profile:pattern-matching #t)]))
