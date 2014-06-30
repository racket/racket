#lang racket/base

(require (for-template racket/base)
         "patterns.rkt"
         "parse-helper.rkt"
         "parse-quasi.rkt")

(provide parse/legacy)

(define orig-insp (variable-reference->module-declaration-inspector
                   (#%variable-reference)))

(define (parse/legacy stx)
  (define (rearm new-stx) (syntax-rearm new-stx stx))
  (define (parse stx) (parse/legacy (rearm stx)))
  (define disarmed-stx (syntax-disarm stx orig-insp))
  (syntax-case* disarmed-stx (not $ ? and or = quasiquote quote)
                (lambda (x y) (eq? (syntax-e x) (syntax-e y)))
    [(expander args ...)
     (and (identifier? #'expander)
          (legacy-match-expander?
           (syntax-local-value #'expander (λ () #f))))
     (match-expander-transform
      parse #'expander disarmed-stx legacy-match-expander-proc
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
                               (list (parse (syntax/loc stx (es ...)))))))]
    [#(es ...)
     (make-Vector (map parse (syntax->list #'(es ...))))]
    [($ s . pats)
     (parse-struct disarmed-stx parse #'s #'pats)]
    [(? p q1 qs ...)
     (make-And (cons (make-Pred #'p)
                     (map parse (syntax->list #'(q1 qs ...)))))]
    [(? p)
     (make-Pred (rearm #'p))]
    [(= f p)
     (make-App #'f (list (parse #'p)))]
    [(quasiquote p)
     (parse-quasi #'p parse)]
    [(quote . rest)
     (parse-quote disarmed-stx parse)]
    [() (make-Null (make-Dummy #f))]
    [(..)
     (ddk? #'..)
     (raise-syntax-error 'match "incorrect use of ... in pattern" stx #'..)]
    [(p .. . rest)
     (ddk? #'..)
     (dd-parse parse #'p #'.. #'rest #'list?)]
    [(e . es)
     (make-Pair (parse #'e) (parse (syntax/loc stx es)))]
    [x
     (identifier? #'x)
     (parse-id #'x)]
    [v
     (or (parse-literal (syntax-e #'v))
         (raise-syntax-error 'match "syntax error in pattern" stx))]))
