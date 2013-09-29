#lang racket/base

(require racket/struct-info
         "patterns.rkt"
         "parse-helper.rkt"
         "parse-quasi.rkt"
         (for-template (only-in "runtime.rkt" matchable?)
                       racket/base))

(provide parse)

(define (ht-pat-transform p)
  (syntax-case p ()
    [(a b) #'(list a b)]
    [x (identifier? #'x) #'x]))

(define orig-insp (variable-reference->module-declaration-inspector
                   (#%variable-reference)))

;; parse : syntax -> Pat
;; compile stx into a pattern, using the new syntax
(define (parse stx)
  (define (rearm new-stx) (syntax-rearm new-stx stx))
  (define (rearm+parse new-stx) (parse (rearm new-stx)))
  (define disarmed-stx (syntax-disarm stx orig-insp))
  (syntax-case* disarmed-stx (not var struct box cons list vector ? and or quote app
                                  regexp pregexp list-rest list-no-order hash-table
                                  quasiquote mcons list* mlist)
                (lambda (x y) (eq? (syntax-e x) (syntax-e y)))
    [(expander args ...)
     (and (identifier? #'expander)
          (match-expander? (syntax-local-value #'expander
                                               (lambda () #f))))
     (match-expander-transform
      rearm+parse #'expander disarmed-stx match-expander-proc
      "This expander only works with the legacy match syntax")]
    [(var v)
     (identifier? #'v)
     (Var (rearm #'v))]
    [(and p ...)
     (And (map rearm+parse (syntax->list #'(p ...))))]
    [(or)
     (Not (Dummy stx))]
    [(or p ps ...)
     (let ([ps (map rearm+parse (syntax->list #'(p ps ...)))])
       (all-vars ps stx)
       (Or ps))]
    [(not p ...)
     ;; nots are conjunctions of negations
     (let ([ps (map (compose Not rearm+parse) (syntax->list #'(p ...)))])
       (And ps))]
    [(regexp r)
     (trans-match #'matchable?
                  (rearm #'(lambda (e) (regexp-match r e)))
                  (Pred #'values))]
    [(regexp r p)
     (trans-match #'matchable? #'(lambda (e) (regexp-match r e)) (parse #'p))]
    [(pregexp r)
     (trans-match #'matchable?
                  (rearm
                   #'(lambda (e)
                       (regexp-match (if (pregexp? r) r (pregexp r)) e)))
                  (Pred #'values))]
    [(pregexp r p)
     (trans-match #'matchable?
                  (rearm 
                   #'(lambda (e)
                       (regexp-match (if (pregexp? r) r (pregexp r)) e)))
                  (rearm+parse #'p))]
    [(box e) (Box (parse #'e))]
    [(vector es ...)
     (ormap ddk? (syntax->list #'(es ...)))
     (trans-match #'vector?
                  #'vector->list
                  (rearm+parse (syntax/loc stx (list es ...))))]
    [(vector es ...)
     (Vector (map rearm+parse (syntax->list #'(es ...))))]
    [(hash-table p ... dd)
     (ddk? #'dd)
     (trans-match
      #'hash?
      #'(lambda (e) (hash-map e list))
      (with-syntax ([(elems ...)
                     (map ht-pat-transform (syntax->list #'(p ...)))])
        (rearm+parse (syntax/loc stx (list-no-order elems ... dd)))))]
    [(hash-table p ...)
     (ormap ddk? (syntax->list #'(p ...)))
     (raise-syntax-error
      'match "dot dot k can only appear at the end of hash-table patterns" stx
      (ormap (lambda (e) (and (ddk? e) e)) (syntax->list #'(p ...))))]
    [(hash-table p ...)
     (trans-match #'hash?
                  #'(lambda (e) (hash-map e list))
                  (with-syntax ([(elems ...)
                                 (map ht-pat-transform
                                      (syntax->list #'(p ...)))])
                    (rearm+parse (syntax/loc stx (list-no-order elems ...)))))]
    [(hash-table . _)
     (raise-syntax-error 'match "syntax error in hash-table pattern" stx)]
    [(list-no-order p ... lp dd)
     (ddk? #'dd)
     (let* ([count (ddk? #'dd)]
            [min (if (number? count) count #f)]
            [max (if (number? count) count #f)]
            [ps (syntax->list #'(p ...))])
       (GSeq (cons (list (rearm+parse #'lp))
                        (for/list ([p ps]) (list (parse p))))
                  (cons min (map (lambda _ 1) ps))
                  (cons max (map (lambda _ 1) ps))
                  ;; vars in lp are lists, vars elsewhere are not
                  (cons #f (map (lambda _ #t) ps))
                  (Null (Dummy (syntax/loc stx _)))
                  #f))]
    [(list-no-order p ...)
     (ormap ddk? (syntax->list #'(p ...)))
     (raise-syntax-error
      'match "dot dot k can only appear at the end of unordered match patterns"
      stx
      (ormap (lambda (e) (and (ddk? e) e)) (syntax->list #'(p ...))))]
    [(list-no-order p ...)
     (let ([ps (syntax->list #'(p ...))])
       (GSeq (for/list ([p ps]) (list (rearm+parse p)))
                  (map (lambda _ 1) ps)
                  (map (lambda _ 1) ps)
                  ;; all of these patterns get bound to only one thing
                  (map (lambda _ #t) ps)
                  (Null (Dummy (syntax/loc stx _)))
                  #f))]
    [(list) (Null (Dummy (syntax/loc stx _)))]
    [(mlist) (Null (Dummy (syntax/loc stx _)))]
    [(list ..)
     (ddk? #'..)
     (raise-syntax-error 'match "incorrect use of ... in pattern" stx #'..)]
    [(mlist ..)
     (ddk? #'..)
     (raise-syntax-error 'match "incorrect use of ... in pattern" stx #'..)]
    [(list p .. . rest)
     (ddk? #'..)
     (dd-parse rearm+parse #'p #'.. (syntax/loc stx (list . rest)))]
    [(mlist p .. . rest)
     (ddk? #'..)
     (dd-parse rearm+parse #'p #'.. (syntax/loc stx (list . rest)) #:mutable #t)]
    [(list e es ...)
     (Pair (rearm+parse #'e) (rearm+parse (syntax/loc stx (list es ...))))]
    [(mlist e es ...)
     (MPair (rearm+parse #'e) (rearm+parse (syntax/loc stx (mlist es ...))))]
    [(list* . rest)
     (rearm+parse (syntax/loc stx (list-rest . rest)))]
    [(list-rest e)
     (rearm+parse #'e)]
    [(list-rest p dd . rest)
     (ddk? #'dd)
     (dd-parse rearm+parse #'p #'dd (syntax/loc stx (list-rest . rest)))]
    [(list-rest e . es)
     (Pair (rearm+parse #'e) (rearm+parse (syntax/loc #'es (list-rest . es))))]
    [(cons e1 e2) (Pair (rearm+parse #'e1) (rearm+parse #'e2))]
    [(mcons e1 e2) (MPair (rearm+parse #'e1) (rearm+parse #'e2))]
    [(struct s pats)
     (parse-struct disarmed-stx rearm+parse #'s #'pats)]
    [(s . pats)
     (and (identifier? #'s) (struct-info? (syntax-local-value #'s (lambda () #f))))
     (parse-struct disarmed-stx rearm+parse #'s #'pats)]
    [(? p q1 qs ...)
     (OrderedAnd 
      (list (Pred (rearm #'p))
            (And (map rearm+parse (syntax->list #'(q1 qs ...))))))]
    [(? p)
     (Pred (rearm #'p))]
    [(app f ps ...) ;; only make a list for more than one pattern
     (App #'f (map rearm+parse (syntax->list #'(ps ...))))]
    [(quasiquote p)
     (parse-quasi #'p rearm+parse)]
    [(quasiquote . _)
     (raise-syntax-error 'match "illegal use of quasiquote")]
    [(quote . _)
     (parse-quote disarmed-stx rearm+parse)]
    [x
     (identifier? #'x)
     (parse-id (rearm #'x))]
    [v
     (or (parse-literal (syntax-e #'v))
         (raise-syntax-error 'match "syntax error in pattern" disarmed-stx))]))

;; (trace parse)
