#lang racket/base

(require racket/struct-info
         racket/syntax
         racket/list
         "patterns.rkt"
         "parse-helper.rkt"
         "parse-quasi.rkt"
         (only-in "stxtime.rkt" current-form-name)
         (for-template (only-in "runtime.rkt" matchable? pregexp-matcher mlist? mlist->list
                                undef user-def undef? user-def? hash-remove-safe
                                hash-remove-safe!)
                       (only-in racket/unsafe/ops unsafe-vector-ref)
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

;; underscore? :: syntax? -> boolean?
(define (underscore? x) (eq? '_ (syntax-e x)))

;; one-wildcard-ddk? :: (listof syntax?) -> boolean?
;; return #t if there is exactly one ddk and the preceding pattern before
;; ddk is underscore
(define (one-wildcard-ddk? es)
  (and (= 1 (count ddk? es))
       ;; the fact that count = 1 means that rest is always valid
       (for/first ([be (in-list es)]
                   [e (in-list (rest es))]
                   #:when (ddk? e))
         (underscore? be))))

;; split-one-wildcard-ddk :: (listof syntax?) ->
;;                           (listof syntax?) (listof syntax?) number?
;; precondition: es satisfies one-wildcard-ddk?
(define (split-one-wildcard-ddk es)
  ;; the prefix has underscore, the suffix has ddk
  (define-values (prefix suffix) (splitf-at es (λ (e) (not (ddk? e)))))
  (define ddk-size (ddk? (first suffix)))
  (values (drop-right prefix 1)
          (rest suffix)
          (if (eq? ddk-size #t) 0 ddk-size)))


;; gen-closed-mode :: syntax?
;;                    (listof identifier?)
;;                    (listof identifier?)
;;                    -> (listof syntax?)
(define (gen-closed-mode mode k-ids ref-ids)
  (cond
    [(eq? mode #t)
     (list #`(λ ()
               (define seen (hash-copy-clear e #:kind 'mutable))
               (define cnt
                 (+ #,@(for/list ([k-id (in-list k-ids)]
                                  [ref-id (in-list ref-ids)])
                         #`(cond
                             [(or (hash-has-key? seen #,k-id)
                                  (user-def? #,ref-id))
                              0]
                             [else
                              (hash-set! seen #,k-id #t)
                              1]))))
               (= (hash-count e) cnt)))]
    [else '()]))

;; do-hash :: syntax? stx-list? (or/c #t #f syntax?) -> syntax?
(define (do-hash stx kvps mode)
  (define kvp-list (syntax->list kvps))
  (define-values (k-exprs v-pats def-exprs def-ids)
    (for/fold ([k-exprs '()]
               [v-pats '()]
               [def-exprs '()]
               [def-ids '()]
               #:result (values (reverse k-exprs)
                                (reverse v-pats)
                                (reverse def-exprs)
                                (reverse def-ids)))
              ([kvp (in-list kvp-list)])
      (syntax-case kvp ()
        [(k-expr v-pat #:default def-expr)
         (values (cons #'k-expr k-exprs)
                 (cons #'v-pat v-pats)
                 (cons #'def-expr def-exprs)
                 (cons #'user-def def-ids))]
        [(k-expr v-pat)
         (values (cons #'k-expr k-exprs)
                 (cons #'v-pat v-pats)
                 (cons #'undef def-exprs)
                 (cons #'undef def-ids))]
        [_ (raise-syntax-error #f "expect a key-value group" stx kvp)])))

  (define k-ids (generate-temporaries k-exprs))
  (define ref-ids (generate-temporaries k-exprs))

  (with-syntax ([(k-id ...) k-ids]
                [(ref-id ...) ref-ids]
                [(k-expr ...) k-exprs]
                [(def-id ...) def-ids])

    (define activate-fun
      #'(λ (p) (p)))

    ;; we use let explicitly to prevent macro expander from nesting too much
    (define main-fun
      #`(λ (e)
          ;; initially assign k-ids and ref-ids to #f, so that
          ;; if section 1 short-circuits, there's no need to
          ;; evaluate all k-exprs and hash-refs
          (let ([k-id #f] ... [ref-id #f] ...)
            (values
             ;; SECTION 1: predicate
             (or (undef? (begin
                           (set! k-id k-expr)
                           (set! ref-id (hash-ref e k-id def-id))
                           ref-id))
                 ...)

             ;; henceforth, we can assume ref-ids are not undef

             ;; SECTION 2: full mode predicate
             #,@(gen-closed-mode mode k-ids ref-ids)

             ;; SECTION 3: values
             ref-id ...

             ;; SECTION 4: rest
             #,@(cond
                  [(syntax? mode)
                   (list #`(λ ()
                             (cond
                               [(immutable? e)
                                #,(for/fold ([stx #'e])
                                            ([k (in-list k-ids)])
                                    #`(hash-remove-safe #,stx #,k))]
                               [else
                                (define e* (hash-copy e))
                                #,@(for/list ([k (in-list k-ids)])
                                     #`(hash-remove-safe! e* #,k))
                                e*])))]
                  [else '()])))))

    (OrderedAnd
     (list (Pred #'hash?)
           (App main-fun
                (append
                 ;; SECTION 1
                 (list (Exact #f))

                 ;; SECTION 2
                 (cond
                   [(eq? mode #t) (list (App activate-fun (list (Exact #t))))]
                   [else '()])

                 ;; SECTION 3
                 (for/list ([ref-id (in-list ref-ids)]
                            [def-expr (in-list def-exprs)]
                            [v-pat (in-list v-pats)])
                   (with-syntax ([ref-id ref-id]
                                 [def-expr def-expr])
                     (App #'(λ (ref-id)
                              (if (user-def? ref-id)
                                  def-expr
                                  ref-id))
                          (list (parse v-pat)))))

                 ;; SECTION 4
                 (cond
                   [(syntax? mode)
                    (list (App activate-fun (list (parse mode))))]
                   [else '()])))))))

(define (make-kvps stx xs)
  (let loop ([xs (syntax->list xs)] [acc '()])
    (cond
      [(empty? xs) (reverse acc)]
      [(empty? (rest xs))
       (raise-syntax-error #f "key does not have a value" stx)]
      [else (loop (rest (rest xs)) (cons (list (first xs) (second xs)) acc))])))

;; parse : syntax -> Pat
;; compile stx into a pattern, using the new syntax
(define (parse stx)
  (define (rearm new-stx) (syntax-rearm new-stx stx))
  (define (rearm+parse new-stx) (parse (rearm new-stx)))
  (define disarmed-stx (syntax-disarm stx orig-insp))
  (syntax-case* disarmed-stx (not var struct box cons list vector ? and or quote app
                                  regexp pregexp list-rest list-no-order hash-table
                                  quasiquote mcons list* mlist
                                  hash hash*)
                (lambda (x y) (eq? (syntax-e x) (syntax-e y)))
    [(expander . args)
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
                  #`(pregexp-matcher r '#,(current-form-name))
                  (Pred #'values))]
    [(pregexp r p)
     (trans-match #'matchable?
                  #`(pregexp-matcher r '#,(current-form-name))
                  (rearm+parse #'p))]
    [(box e) (Box (parse #'e))]
    [(vector es ...)
     (one-wildcard-ddk? (syntax->list #'(es ...)))
     (let-values ([(prefix suffix ddk-size) (split-one-wildcard-ddk (syntax->list #'(es ...)))])
       (define prefix-len (length prefix))
       (define suffix-len (length suffix))
       (define pre+suf-len (+ prefix-len suffix-len))
       (trans-match*
        (list #`(λ (e)
                  (and (vector? e)
                       (>= (vector-length e) #,(+ pre+suf-len ddk-size)))))
        (for/list ([idx (in-range pre+suf-len)])
          #`(λ (e)
              (define vec-len (vector-length e))
              (unsafe-vector-ref
               e
               #,(if (< idx prefix-len)
                     idx
                     #`(+ vec-len #,(- idx pre+suf-len))))))
        (map parse (append prefix suffix))))]
    [(vector es ...)
     (ormap ddk? (syntax->list #'(es ...)))
     (trans-match #'vector?
                  #'vector->list
                  (rearm+parse (syntax/loc stx (list es ...))))]
    [(vector es ...)
     (Vector (map rearm+parse (syntax->list #'(es ...))))]

    ;; Hash table patterns
    [(hash* kvp ... #:rest rest-pat)
     (eq? (syntax-e #'rest-pat) '_)
     (do-hash stx #'(kvp ...) #f)]
    [(hash* kvp ... #:rest rest-pat) (do-hash stx #'(kvp ...) #'rest-pat)]
    [(hash* kvp ... #:closed) (do-hash stx #'(kvp ...) #t)]
    [(hash* kvp ... #:open) (do-hash stx #'(kvp ...) #f)]
    [(hash* kvp ...) (do-hash stx #'(kvp ...) #f)]

    [(hash es ... #:rest rest-pat)
     (with-syntax ([(kvp ...) (make-kvps stx #'(es ...))])
       (parse (syntax/loc stx (hash* kvp ... #:rest rest-pat))))]
    [(hash es ... #:closed)
     (with-syntax ([(kvp ...) (make-kvps stx #'(es ...))])
       (do-hash stx #'(kvp ...) #t))]
    [(hash es ... #:open)
     (with-syntax ([(kvp ...) (make-kvps stx #'(es ...))])
       (do-hash stx #'(kvp ...) #f))]
    [(hash es ...)
     (with-syntax ([(kvp ...) (make-kvps stx #'(es ...))])
       (do-hash stx #'(kvp ...) #t))]

    ;; Deprecated hash table patterns
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
       ;; *across multiple* of the ps is an error, at least for now
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
       ;; *across multiple* of the ps is an error, at least for now
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
