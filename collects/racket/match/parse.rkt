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
     (make-Var (rearm #'v))]
    [(and p ...)
     (make-And (map rearm+parse (syntax->list #'(p ...))))]
    [(or)
     (make-Not (make-Dummy stx))]
    [(or p ps ...)
     (let ([ps (map rearm+parse (syntax->list #'(p ps ...)))])
       (all-vars ps stx)
       (make-Or ps))]
    [(not p ...)
     ;; nots are conjunctions of negations
     (let ([ps (map (compose make-Not rearm+parse) (syntax->list #'(p ...)))])
       (make-And ps))]
    [(regexp r)
     (trans-match #'matchable?
                  (rearm #'(lambda (e) (regexp-match r e)))
                  (make-Pred #'values))]
    [(regexp r p)
     (trans-match #'matchable? #'(lambda (e) (regexp-match r e)) (parse #'p))]
    [(pregexp r)
     (trans-match #'matchable?
                  (rearm
                   #'(lambda (e)
                       (regexp-match (if (pregexp? r) r (pregexp r)) e)))
                  (make-Pred #'values))]
    [(pregexp r p)
     (trans-match #'matchable?
                  (rearm 
                   #'(lambda (e)
                       (regexp-match (if (pregexp? r) r (pregexp r)) e)))
                  (rearm+parse #'p))]
    [(box e) (make-Box (parse #'e))]
    [(vector es ...)
     (ormap ddk? (syntax->list #'(es ...)))
     (trans-match #'vector?
                  #'vector->list
                  (rearm+parse (syntax/loc stx (list es ...))))]
    [(vector es ...)
     (make-Vector (map rearm+parse (syntax->list #'(es ...))))]
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
       (make-GSeq (cons (list (rearm+parse #'lp))
                        (for/list ([p ps]) (list (parse p))))
                  (cons min (map (lambda _ 1) ps))
                  (cons max (map (lambda _ 1) ps))
                  ;; vars in lp are lists, vars elsewhere are not
                  (cons #f (map (lambda _ #t) ps))
                  (make-Null (make-Dummy (syntax/loc stx _)))
                  #f))]
    [(list-no-order p ...)
     (ormap ddk? (syntax->list #'(p ...)))
     (raise-syntax-error
      'match "dot dot k can only appear at the end of unordered match patterns"
      stx
      (ormap (lambda (e) (and (ddk? e) e)) (syntax->list #'(p ...))))]
    [(list-no-order p ...)
     (let ([ps (syntax->list #'(p ...))])
       (make-GSeq (for/list ([p ps]) (list (rearm+parse p)))
                  (map (lambda _ 1) ps)
                  (map (lambda _ 1) ps)
                  ;; all of these patterns get bound to only one thing
                  (map (lambda _ #t) ps)
                  (make-Null (make-Dummy (syntax/loc stx _)))
                  #f))]
    [(list) (make-Null (make-Dummy (syntax/loc stx _)))]
    [(mlist) (make-Null (make-Dummy (syntax/loc stx _)))]
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
     (make-Pair (rearm+parse #'e) (rearm+parse (syntax/loc stx (list es ...))))]
    [(mlist e es ...)
     (make-MPair (rearm+parse #'e) (rearm+parse (syntax/loc stx (mlist es ...))))]
    [(list* . rest)
     (rearm+parse (syntax/loc stx (list-rest . rest)))]
    [(list-rest e)
     (rearm+parse #'e)]
    [(list-rest p dd . rest)
     (ddk? #'dd)
     (dd-parse rearm+parse #'p #'dd (syntax/loc stx (list-rest . rest)))]
    [(list-rest e . es)
     (make-Pair (rearm+parse #'e) (rearm+parse (syntax/loc #'es (list-rest . es))))]
    [(cons e1 e2) (make-Pair (rearm+parse #'e1) (rearm+parse #'e2))]
    [(mcons e1 e2) (make-MPair (rearm+parse #'e1) (rearm+parse #'e2))]
    [(struct s pats)
     (parse-struct disarmed-stx rearm+parse #'s #'pats)]
    [(s . pats)
     (and (identifier? #'s) (struct-info? (syntax-local-value #'s (lambda () #f))))
     (parse-struct disarmed-stx rearm+parse #'s #'pats)]
    [(? p q1 qs ...)
     (make-And (cons (make-Pred (rearm #'p))
                     (map rearm+parse (syntax->list #'(q1 qs ...)))))]
    [(? p)
     (make-Pred (rearm #'p))]
    [(app f p)
     (make-App #'f (rearm+parse #'p))]
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
