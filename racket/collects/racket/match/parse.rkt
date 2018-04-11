#lang racket/base

(require racket/struct-info
         racket/syntax
         "patterns.rkt"
         "parse-helper.rkt"
         "parse-quasi.rkt"
         (for-template (only-in "runtime.rkt" matchable? mlist? mlist->list)
                       racket/base))

(provide parse)

(define (ht-pat-transform p)
  (syntax-case p ()
    [(a b) #'(list a b)]
    [x (identifier? #'x) #'x]))

(define orig-insp (variable-reference->module-declaration-inspector
                   (#%variable-reference)))

(define (literal-pat? p)
  (syntax-case p ()
    [(_quote e)
     (eq? 'quote (syntax-e #'_quote))
     (parse-literal (syntax-e #'e))]
    [_ (parse-literal (syntax-e p))]))

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
          (syntax-local-value/record #'expander match-expander?))
     (match-expander-transform
      rearm+parse #'expander disarmed-stx match-expander-proc
      "This expander only works with the legacy match syntax")]
    [(var v)
     (identifier? #'v)
     (Var (rearm #'v))]
    [(and p ...)
     (OrderedAnd (map rearm+parse (syntax->list #'(p ...))))]
    [(or)
     (Not (Dummy stx))]
    [(or p ps ...)
     (let ([ps (map rearm+parse (syntax->list #'(p ps ...)))])
       (all-vars ps stx)
       (Or ps))]
    [(not p ...)
     ;; nots are conjunctions of negations
     (let ([ps (map (compose Not rearm+parse) (syntax->list #'(p ...)))])
       (OrderedAnd ps))]
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
    [(hash-table (k0 v0) (k1 v1) ...)
     (andmap (λ (p) (and (literal-pat? p) (not (identifier? p)))) (syntax->list #'(k0 k1 ...)))
     (with-syntax ([(k ...) #'(k0 k1 ...)]
                   [(v ...) #'(v0 v1 ...)])
       (let ([keys (map Exact-v (map literal-pat? (syntax->list #'(k ...))))])
         (trans-match*
          (cons #'hash? (for/list ([k (in-list keys)]) (λ (e) #`(hash-has-key? #,e '#,k))))
          (for/list ([k (in-list keys)]) (λ (e) #`(hash-ref #,e '#,k)))
          (map parse (syntax->list #'(v ...))))))]
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
            [ps (syntax->list #'(p ...))]
            ;; parsed versions of ps and lp
            [parsed-ps (map parse ps)]
            [parsed-lp (rearm+parse #'lp)])
       ;; duplicates within *one* of the ps is fine, but duplicates
       ;; *accross multiple* of the ps is an error, at least for now
       (check-list-no-order-duplicates (cons parsed-lp parsed-ps))
       (GSeq (cons (list parsed-lp)
                   (for/list ([p parsed-ps]) (list p)))
             (cons min (map (lambda _ 1) ps))
             (cons #f (map (lambda _ 1) ps))
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
     (let* ([ps (syntax->list #'(p ...))]
            ;; parsed versions of ps
            [parsed-ps (map rearm+parse ps)])
       ;; duplicates within *one* of the ps is fine, but duplicates
       ;; *accross multiple* of the ps is an error, at least for now
       (check-list-no-order-duplicates parsed-ps)
       (GSeq (for/list ([p parsed-ps]) (list p))
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
     (dd-parse rearm+parse #'p #'.. (syntax/loc stx (list . rest)) #'list?)]
    [(mlist p .. . rest)
     (ddk? #'..)
     (dd-parse rearm+parse #'p #'.. (syntax/loc stx (list . rest)) #'mlist? #:to-list #'mlist->list #:mutable #t)]
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
     (dd-parse rearm+parse #'p #'dd (syntax/loc stx (list-rest . rest)) #'list?)]
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
            (OrderedAnd (map rearm+parse (syntax->list #'(q1 qs ...))))))]
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

;; --------------------------------------------------------------

;; check-list-no-order-duplicates : [Listof Pat] -> Void
(define (check-list-no-order-duplicates pats)
  ;; Duplicate identifiers within *one* pat is fine, but
  ;; duplicate identifiers across multiple pats is an error.
  ;; Using the `bound-vars` function on each pat separately
  ;; should merge duplicate identifiers within each *one*.
  ;; So, duplicate identifiers in the appended list must be
  ;; duplicates across multiple.
  (define vars (apply append (map bound-vars pats)))
  (define dup (check-duplicate-identifier vars))
  (when dup
    (raise-syntax-error 'list-no-order "unexpected duplicate identifier" dup)))

;; --------------------------------------------------------------

;; (trace parse)
