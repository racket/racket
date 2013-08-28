#lang racket/base
(require (for-syntax racket/base)
         racket/list
         racket/match
         racket/function
         racket/vector
         racket/set)
(provide (all-defined-out))

; same hash
(define (make-immutable-hash*) (make-immutable-hash empty))
(define (make-immutable-hasheqv*) (make-immutable-hasheqv empty))
(define (make-immutable-hasheq*) (make-immutable-hasheq empty))
(define (same-hash-make ht)
  (match ht
    [(? immutable?)
     (match ht
       [(? hash-equal?) make-immutable-hash*]
       [(? hash-eqv?) make-immutable-hasheqv*]
       [(? hash-eq?) make-immutable-hasheq*])]
    [(? hash-weak?)
     (match ht
       [(? hash-equal?) make-weak-hash]
       [(? hash-eqv?) make-weak-hasheqv]
       [(? hash-eq?) make-weak-hasheq])]
    [_
     (match ht
       [(? hash-equal?) make-hash]
       [(? hash-eqv?) make-hasheqv]
       [(? hash-eq?) make-hasheq])]))
(define (same-hash-kind? x y)
  (eq? (same-hash-make x) (same-hash-make y)))
(define (same-hash-map f ht)
  (define new-ht ((same-hash-make ht)))
  (if (immutable? ht)
      (for/fold ([new-ht new-ht])
        ([(k v) (in-hash ht)])
        (hash-set new-ht k (f v)))
      (begin 
        (for ([(k v) (in-hash ht)])
          (hash-set! new-ht k (f v)))
        new-ht)))

; compound structs
(require unstable/sequence)
(define (in-compound-struct s)
  (define-values (stype _) (struct-info s))
  (define-values (name init-field-cnt auto-field-cnt accessor-proc mutator-proc immutable-k-list super-type skipped?) (struct-type-info stype))
  (define total-field-cnt (+ init-field-cnt)
    #;(compound-struct-type-field-cnt stype))
  (sequence-lift (curry accessor-proc s) (in-range total-field-cnt)))

(define (compound-struct-map f s)
  (define-values (stype _) (struct-info s))
  (define make (struct-type-make-constructor stype))
  (apply make 
         (for/list ([e (in-compound-struct s)])
           (f e))))
(define (compound-struct-ormap f s)
  (for/or ([e (in-compound-struct s)])
    (f e)))
(define (compound-struct-andmap f s)
  (for/and ([e (in-compound-struct s)])
    (f e)))
(define (compound-struct-same? x y)
  (define-values (xtype _) (struct-info x))
  ((struct-type-make-predicate xtype) y))  
(define (compound-struct-cmp x y =)
  (and (compound-struct-same? x y)
       (for/and ([ex (in-compound-struct x)]
                 [ey (in-compound-struct y)])
         (= ex ey))))

(define-struct logic-var (val) #:mutable)

(define *unbound* '_)

;;unbound refs point to themselves
(define (make-ref [val *unbound*])
  (make-logic-var val))

(define _ make-ref)
(define (unbound-logic-var? r)
  (and (logic-var? r) (eq? (logic-var-val r) *unbound*)))
(define (unbind-ref! r)
  (set-logic-var-val! r *unbound*))

(define-struct frozen (val))
(define (freeze-ref r)
  (make-ref (make-frozen r)))
(define (thaw-frozen-ref r)
  (frozen-val (logic-var-val r)))
(define (frozen-logic-var? r)
  (frozen? (logic-var-val r)))

(define-syntax (uni-match stx)
  (syntax-case
      stx (? logic-var? cons mcons box vector? hash? compound-struct? atom? else)
    [(_ v
        [(? logic-var? lv) logic-var-expr ...]
        [(cons cl cr) cons-expr ...]
        [(mcons mcl mcr) mcons-expr ...]
        [(box bv) box-expr ...]
        [(? vector? vec) vector-expr ...]
        [(? hash? hash) hash-expr ...]
        [(? compound-struct? cs) cs-expr ...]
        [(? atom? x) atom-expr ...])
     (syntax/loc stx
       (match v
         [(? logic-var? lv) logic-var-expr ...]
         [(cons cl cr) cons-expr ...]
         [(mcons mcl mcr) mcons-expr ...]
         [(box bv) box-expr ...]
         [(? vector? vec) vector-expr ...]
         [(? hash? hash) hash-expr ...]
         [(? compound-struct? cs) cs-expr ...]
         [(? atom? x) atom-expr ...]))]
    [(_ v
        [(? logic-var? lv) logic-var-expr ...]
        [(cons cl cr) cons-expr ...]
        [(mcons mcl mcr) mcons-expr ...]
        [(box bv) box-expr ...]
        [(? vector? vec) vector-expr ...]
        [(? hash? hash) hash-expr ...]
        [(? compound-struct? cs) cs-expr ...]
        [(? atom? x) atom-expr ...]
        [else else-expr ...])
     (syntax/loc stx
       (match v
         [(? logic-var? lv) logic-var-expr ...]
         [(cons cl cr) cons-expr ...]
         [(mcons mcl mcr) mcons-expr ...]
         [(box bv) box-expr ...]
         [(? vector? vec) vector-expr ...]
         [(? hash? hash) hash-expr ...]
         [(? compound-struct? cs) cs-expr ...]
         [(? atom? x) atom-expr ...]
         [else else-expr ...]))]))

(define (logic-var-val* v)
  (uni-match 
   v
   [(? logic-var? s)
    (if (frozen-logic-var? s) s
        (logic-var-val* (logic-var-val s)))]
   [(cons l r)
    (cons (logic-var-val* l) (logic-var-val* r))]
   [(mcons l r)
    (mcons (logic-var-val* l) (logic-var-val* r))]
   [(box v) (box (logic-var-val* v))]
   [(? vector? v)
    (vector-map logic-var-val* v)]
   [(? hash? v) (same-hash-map logic-var-val* v)]
   [(? compound-struct? v) (compound-struct-map logic-var-val* v)]
   [(? atom? s) s]))

(define use-occurs-check? (make-parameter #f))

(define (occurs-in? var term)
  (and (use-occurs-check?)
       (let loop ([term term])
         (or (eqv? var term)
             (uni-match 
              term
              [(? logic-var? term)
               (cond [(unbound-logic-var? term) #f]
                     [(frozen-logic-var? term) #f]
                     [else (loop (logic-var-val term))])]
              [(cons l r)
               (or (loop l) (loop r))]
              [(mcons l r)
               (or (loop l) (loop r))]
              [(box v) (loop v)]
              [(? vector? v)
               (for/or ([e (in-vector v)]) (loop e))]
              [(? hash? ht)
               (for/or ([(k v) (in-hash ht)]) (or (loop k) (loop v)))]
              [(? compound-struct? cs) (compound-struct-ormap loop cs)]
              [(? atom? x) #f])))))

(define (constant? x)
  (uni-match 
   x
   [(? logic-var? x)
    (cond [(unbound-logic-var? x) #f]
          [(frozen-logic-var? x) #t]
          [else (constant? (logic-var-val x))])]
   [(cons l r) #f]
   [(mcons l r) #f]
   [(box v) #f]
   [(? vector? v) #f]
   [(? hash? v) #f]
   [(? compound-struct? v) #f]
   [(? atom? x) #t]))

(define (is-compound? x)
  (uni-match 
   x
   [(? logic-var? x)
    (cond [(unbound-logic-var? x) #f]
          [(frozen-logic-var? x) #f]
          [else (is-compound? (logic-var-val x))])]
   [(cons l r) #t]
   [(mcons l r) #t]
   [(box v) #t]
   [(? vector? v) #t]
   [(? hash? v) #t]
   [(? compound-struct? v) #t]
   [(? atom? x) #f]))

(define (var? x)
  (uni-match 
   x
   [(? logic-var? x)
    (cond [(unbound-logic-var? x) #t]
          [(frozen-logic-var? x) #f]
          [else (var? (logic-var-val x))])]
   [(cons l r) (or (var? l) (var? r))]
   [(mcons l r) (or (var? l) (var? r))]
   [(box v) (var? v)]
   [(? vector? v)
    (for/or ([e (in-vector v)]) (var? e))]
   [(? hash? ht)
    (for/or ([(k v) (in-hash ht)]) (var? v))]
   [(? compound-struct? cs) (compound-struct-ormap var? cs)]
   [(? atom? x) #f]))

(define (freeze v)
  (define dict (make-hasheq))
  (define (loop s)
    (uni-match 
     s
     [(? logic-var? s)
      (if (or (unbound-logic-var? s) (frozen-logic-var? s))
          (hash-ref! dict s
                     (lambda ()
                       (freeze-ref s)))
          (loop (logic-var-val s)))]
     [(cons l r)
      (cons (loop l) (loop r))]
     [(mcons l r)
      (mcons (loop l) (loop r))]
     [(box v) (box (loop v))]
     [(? vector? v)
      (vector-map loop v)]
     [(? hash? v)
      (same-hash-map loop v)]
     [(? compound-struct? cs) (compound-struct-map loop cs)]
     [(? atom? s) s]))
  (loop v))

(define (melt f)
  (uni-match 
   f
   [(? logic-var? f)
    (cond [(unbound-logic-var? f) f]
          [(frozen-logic-var? f) (thaw-frozen-ref f)]
          [else (melt (logic-var-val f))])]
   [(cons l r)
    (cons (melt l) (melt r))]
   [(mcons l r)
    (mcons (melt l) (melt r))]
   [(box v) (box (melt v))]
   [(? vector? v)
    (vector-map melt v)]
   [(? hash? v)
    (same-hash-map melt v)]
   [(? compound-struct? cs) (compound-struct-map melt cs)]
   [(? atom? s) s]))

(define (melt-new f)
  (define dict (make-hasheq))
  (define (loop s)
    (uni-match 
     s
     [(? logic-var? f)
      (cond [(unbound-logic-var? f) f]
            [(frozen-logic-var? f)
             (hash-ref! dict f _)]
            [else (loop (logic-var-val f))])]
     [(cons l r)
      (cons (loop l) (loop r))]
     [(mcons l r)
      (mcons (loop l) (loop r))]
     [(box v) (box (loop v))]
     [(? vector? v)
      (vector-map loop v)]
     [(? hash? v)
      (same-hash-map loop v)]
     [(? compound-struct? cs)
      (compound-struct-map loop cs)]
     [(? atom? s) s]))
  (loop f))

(define (copy s)
  (melt-new (freeze s)))

(define (ident? x y)
  (uni-match 
   x
   [(? logic-var? x) 
    (cond [(unbound-logic-var? x)
           (cond [(logic-var? y)
                  (cond [(unbound-logic-var? y) (eq? x y)]
                        [(frozen-logic-var? y) #f]
                        [else (ident? x (logic-var-val y))])]
                 [else #f])]
          [(frozen-logic-var? x)
           (cond [(logic-var? y)
                  (cond [(unbound-logic-var? y) #f]
                        [(frozen-logic-var? y) (eq? x y)]
                        [else (ident? x (logic-var-val y))])]
                 [else #f])]
          [else (ident? (logic-var-val x) y)])]
   [(cons xl xr)
    (uni-match 
     y
     [(? logic-var? y)
      (cond [(unbound-logic-var? y) #f]
            [(frozen-logic-var? y) #f]
            [else (ident? x (logic-var-val y))])]
     [(cons yl yr) 
      (and (ident? xl yl) (ident? xr yr))]
     [(mcons yl yr) #f]
     [(box v) #f]
     [(? vector? y) #f]
     [(? hash? y) #f]
     [(? compound-struct? y) #f]
     [(? atom? y) #f])]
   [(mcons xl xr)
    (uni-match 
     y
     [(? logic-var? y)
      (cond [(unbound-logic-var? y) #f]
            [(frozen-logic-var? y) #f]
            [else (ident? x (logic-var-val y))])]
     [(cons yl yr) #f]
     [(mcons yl yr) 
      (and (ident? xl yl) (ident? xr yr))]
     [(box v) #f]
     [(? vector? y) #f]
     [(? hash? y) #f]
     [(? compound-struct? y) #f]
     [(? atom? y) #f])]
   [(box xv)
    (uni-match 
     y
     [(? logic-var? y)
      (cond [(unbound-logic-var? y) #f]
            [(frozen-logic-var? y) #f]
            [else (ident? x (logic-var-val y))])]
     [(cons yl yr) #f]
     [(mcons yl yr) #f]
     [(box yv) (ident? xv yv)]
     [(? vector? y) #f]
     [(? hash? y) #f]
     [(? compound-struct? y) #f]
     [(? atom? y) #f])]
   [(? vector? x)
    (uni-match 
     y
     [(? logic-var? y)
      (cond [(unbound-logic-var? y) #f]
            [(frozen-logic-var? y) #f]
            [else (ident? x (logic-var-val y))])]
     [(cons yl yr) #f]
     [(mcons yl yr) #f]
     [(box v) #f]
     [(? vector? y)
      (if (= (vector-length x)
             (vector-length y))
          (for/and ([xe (in-vector x)]
                    [ye (in-vector y)])
            (ident? xe ye))
          #f)]
     [(? hash? y) #f]
     [(? compound-struct? y) #f]
     [(? atom? y) #f])]
   [(? hash? x)
    (uni-match 
     y
     [(? logic-var? y)
      (cond [(unbound-logic-var? y) #f]
            [(frozen-logic-var? y) #f]
            [else (ident? x (logic-var-val y))])]
     [(cons yl yr) #f]
     [(mcons yl yr) #f]
     [(box v) #f]
     [(? vector? y) #f]
     [(? hash? y)
      (and (same-hash-kind? x y)
           (= (hash-count x) (hash-count y))
           (for/and ([(xk xv) (in-hash x)])
             ; XXX not using ident? for key comparison
             (and (hash-has-key? y xk)
                  (ident? xv (hash-ref y xk)))))]
     [(? compound-struct? y) #f]
     [(? atom? y) #f])]
   [(? compound-struct? x)
    (uni-match 
     y
     [(? logic-var? y)
      (cond [(unbound-logic-var? y) #f]
            [(frozen-logic-var? y) #f]
            [else (ident? x (logic-var-val y))])]
     [(cons yl yr) #f]
     [(mcons yl yr) #f]
     [(box v) #f]
     [(? vector? y) #f]
     [(? hash? y) #f]
     [(? compound-struct? y) 
      (compound-struct-cmp x y ident?)]
     [(? atom? y) #f])]
   [(? atom? x)
    (uni-match 
     y
     [(? logic-var? y)
      (cond [(unbound-logic-var? y) #f]
            [(frozen-logic-var? y) #f]
            [else (ident? x (logic-var-val y))])]
     [(cons yl yr) #f]
     [(mcons yl yr) #f]
     [(box v) #f]
     [(? vector? y) #f]
     [(? hash? y) #f]
     [(? compound-struct? y) #f]
     [(? atom? y) (eqv? x y)])]))

(define (unify t1 t2)
  (define iu (inner-unify t1 t2))
  (λ (fk)
    (define-values (cleanup k)
      (iu fk))
    k))

(define (inner-unify t1 t2)
  (lambda (fk)
    (define (cleanup s)
      (for-each unbind-ref! s))
    (define (cleanup-n-fail s)
      (cleanup s)
      (fk 'fail))
    (define (unify1 t1 t2 s)
      (cond [(eqv? t1 t2) s]
            [(logic-var? t1)
             (cond [(unbound-logic-var? t1)
                    (cond [(occurs-in? t1 t2)
                           (cleanup-n-fail s)]
                          [else 
                           (set-logic-var-val! t1 t2)
                           (list* t1 s)])]
                   [(frozen-logic-var? t1)
                    (cond [(logic-var? t2)
                           (cond [(unbound-logic-var? t2)
                                  (unify1 t2 t1 s)]
                                 [(frozen-logic-var? t2)
                                  (cleanup-n-fail s)]
                                 [else
                                  (unify1 t1 (logic-var-val t2) s)])]
                          [else (cleanup-n-fail s)])]
                   [else 
                    (unify1 (logic-var-val t1) t2 s)])]
            [(logic-var? t2) (unify1 t2 t1 s)]
            [(and (pair? t1) (pair? t2))
             (unify1 (cdr t1) (cdr t2)
                     (unify1 (car t1) (car t2) s))]
            [(and (mpair? t1) (mpair? t2))
             (unify1 (mcdr t1) (mcdr t2)
                     (unify1 (mcar t1) (mcar t2) s))]
            [(and (box? t1) (box? t2))
             (unify1 (unbox t1) (unbox t2) s)]
            [(and (vector? t1) (vector? t2))
             (if (= (vector-length t1)
                    (vector-length t2))
                 (for/fold ([s s])
                   ([v1 (in-vector t1)]
                    [v2 (in-vector t2)])
                   (unify1 v1 v2 s))
                 (cleanup-n-fail s))]
            [(and (hash? t1) (hash? t2))
             (if (and (same-hash-kind? t1 t2)
                      (= (hash-count t1) (hash-count t2)))
                 (for/fold ([s s])
                   ([(xk xv) (in-hash t1)])
                   (if (hash-has-key? t2 xk)
                       (unify1 xv (hash-ref t2 xk) s)
                       (cleanup-n-fail s)))
                 (cleanup-n-fail s))]
            [(and (compound-struct? t1) (compound-struct? t2))
             (if (compound-struct-same? t1 t2)
                 (for/fold ([s s])
                   ([e1 (in-compound-struct t1)]
                    [e2 (in-compound-struct t2)])
                   (unify1 e1 e2 s))
                 (cleanup-n-fail s))]
            [(and (atom? t1) (atom? t2))
             (if (equal? t1 t2) s
                 (cleanup-n-fail s))]
            [else
             (cleanup-n-fail s)]))
    (define s (unify1 t1 t2 empty))
    (values
     (λ () (cleanup s))
     (lambda (d)
       (cleanup-n-fail s)))))

(define-syntax-rule (or* x f ...)
  (or (f x) ...))

(define (atomic-struct? v)
  (not (compound-struct? v)))
(define (compound-struct? v)
  (let-values ([(stype skipped?) (struct-info v)])
    (and stype (not skipped?))))

(define (atom? x)
  (or* x boolean? number? string? bytes? char? symbol?
       regexp? pregexp? byte-regexp? byte-pregexp?
       keyword? null? procedure? void? generic-set?
       atomic-struct?))
(define (compound? x)
  (or* x pair? vector? mpair? box? hash? compound-struct?))

(define (answer-value? x)
  (uni-match 
   x
   [(? logic-var? x) #f]
   [(cons l r) (and (answer-value? l) (answer-value? r))]
   [(mcons l r) (and (answer-value? l) (answer-value? r))]
   [(box v) (answer-value? v)]
   [(? vector? v) (for/and ([e (in-vector v)]) (answer-value? e))]
   [(? hash? ht) (for/and ([(k v) (in-hash ht)]) (and (answer-value? k) (answer-value? v)))]
   [(? compound-struct? cs) (compound-struct-andmap answer-value? cs)]
   [(? atom? x) #t]
   [else #f]))
(define answer?
  (match-lambda
    [#f #t]
    [(list (cons (? symbol?) (? answer-value?)) ...) #t]
    [_ #f]))
(define (unifiable? x)
  (uni-match 
   x
   [(? logic-var? x) #t]
   [(cons l r) (and (unifiable? l) (unifiable? r))]
   [(mcons l r) (and (unifiable? l) (unifiable? r))]
   [(box v) (unifiable? v)]
   [(? vector? v) (for/and ([e (in-vector v)]) (unifiable? e))]
   [(? hash? ht) (for/and ([(k v) (in-hash ht)]) (and #;(answer-value? k) ; No constraint, but won't be used XXX
                                                      (unifiable? v)))]
   [(? compound-struct? cs) (compound-struct-andmap unifiable? cs)]
   [(? atom? x) #t]
   [else #f]))
