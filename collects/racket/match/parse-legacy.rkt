#lang racket/base

(require (for-template racket/base)
         syntax/boundmap
         syntax/stx
         racket/struct-info
         "patterns.rkt"
         "compiler.rkt"
         "parse-helper.rkt"
         "parse-quasi.rkt")

(provide parse/legacy/cert)

(define (parse/legacy/cert stx cert)
  (define (parse stx) (parse/legacy/cert stx cert))
  (syntax-case* stx (not $ ? and or = quasiquote quote)
                (lambda (x y) (eq? (syntax-e x) (syntax-e y)))
    [(expander args ...)
     (and (identifier? #'expander)
          (match-expander?
           (syntax-local-value (cert #'expander) (lambda () #f))))
     (match-expander-transform
      parse/legacy/cert cert #'expander stx match-expander-legacy-xform
      "This expander only works with the standard match syntax")]
    [(and p ...)
     (make-And (map parse (syntax->list #'(p ...))))]
    [(or)
     (make-Not (make-Dummy stx))]
    [(or p ps ...)
     (let ([ps (map parse (syntax->list #'(p ps ...)))])
       (all-vars ps stx)
       (make-Or ps))]
    [(not p ...)
     ;; nots are conjunctions of negations
     (let ([ps (map (compose make-Not parse) (syntax->list #'(p ...)))])
       (make-And ps))]
    [bx
     (box? (syntax-e #'bx))
     (make-Box (parse (unbox (syntax-e #'bx))))]
    [#(es ...)
     (ormap ddk? (syntax->list #'(es ...)))
     (make-And (list (make-Pred #'vector?)
                     (make-App #'vector->list
                               (parse (syntax/loc stx (es ...))))))]
    [#(es ...)
     (make-Vector (map parse (syntax->list #'(es ...))))]
    [($ s . pats)
     (parse-struct stx cert parse #'s #'pats)]
    [(? p q1 qs ...)
     (make-And (cons (make-Pred (cert #'p))
                     (map parse (syntax->list #'(q1 qs ...)))))]
    [(? p)
     (make-Pred (cert #'p))]
    [(= f p)
     (make-App #'f (parse (cert #'p)))]
    [(quasiquote p)
     (parse-quasi #'p cert parse/legacy/cert)]
    [(quote . rest)
     (parse-quote stx parse)]
    [() (make-Null (make-Dummy #f))]
    [(..)
     (ddk? #'..)
     (raise-syntax-error 'match "incorrect use of ... in pattern" stx #'..)]
    [(p .. . rest)
     (ddk? #'..)
     (dd-parse parse #'p #'.. #'rest)]
    [(e . es)
     (make-Pair (parse #'e) (parse (syntax/loc stx es)))]
    [x
     (identifier? #'x)
     (parse-id #'x)]
    [v
     (or (parse-literal (syntax-e #'v))
         (raise-syntax-error 'match "syntax error in pattern" stx))]))
