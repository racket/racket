#lang racket/base
(require racket/contract/base
         racket/dict
         racket/match)

#|
eomap = "extend-only" mapping

Used to represent persistent mappings that are typically extended only once,
where both lookup and extend must be fast, but no other operations are needed.

Like association list (sharing, fast extend), but lookup is fast too.
Like immutable hash (fast lookup), but extend (hopefully) involves less allocation.

----

An eomap[K,V] is (eomap store[K,V] timestamp (box timestamp)).
A store[K,V] is mutable-dict[K, (listof (cons timestamp V))].

A timestamp is either
  - nat
  - (cons nat (cons symbol timestamp))

Timestamps are arranged into Branches. The main Branch goes 1, 2, 3 ....
A box stores the latest timestamp in a Branch.
If another eomap is branched off of 2, that Branch is tagged (g0 . 2) and
its successor for 2 is (1 g0 . 2).

Timestamps and branches are compared using eqv?.

Comparison:
  t1 < nat2 if 
    - t1 = nat1 < nat2
  t1 < (nat2 sym2 . bt2) if
    - t1 = (nat1 sym2 . bt2)    ;; same branch
      and nat1 < nat2
    - t1 < bt2, or              ;; less than branch point

====

The data structure works best, of course, when a mapping is extended only once,
so we can stick to simple numbered timestamps.

An alternative is to make eomaps enforce the extend-once property; so
instead of branching a second extension just errors.

TODO: check if macro stepper strictly follows extend-once discipline.

|#

(struct eomap (store ts latest))

(define (empty-eomap [store (make-hasheq)])
  (eomap store 1 (box 1)))

(define (eomap-bump eom)
  (match eom
    [(eomap store ts latest)
     (let-values ([(ts* latest*) (successor ts latest)])
       (eomap store ts* latest*))]))

(define (eomap-add eom to-add)
  (let ([eom* (eomap-bump eom)])
    (match eom*
      [(eomap store ts latest)
       (for ([(key value) (in-dict to-add)])
         (dict-set! store key
                    (cons (cons ts value) (dict-ref store key null))))])
    eom*))

(define (eomap-set* eom keys value)
  (let ([eom* (eomap-bump eom)])
    (match eom*
      [(eomap store ts latest)
       (for ([key (in-list keys)])
         (dict-set! store key
                    (cons (cons ts value) (dict-ref store key null))))])
    eom*))

(define (eomap-ref eom key [default not-given])
  (match eom
    [(eomap store ts _latest)
     (let loop ([ts+value-list (dict-ref store key null)])
       (cond [(pair? ts+value-list)
              (let ([entry-ts (car (car ts+value-list))]
                    [entry-value (cdr (car ts+value-list))])
                (cond [(t<=? entry-ts ts)
                       entry-value]
                      [else (loop (cdr ts+value-list))]))]
             [else
              (cond [(eq? default not-given)
                     (error 'eomap-ref "key not found: ~e" key)]
                    [(procedure? default) (default)]
                    [else default])]))]))

(define not-given (gensym 'not-given))

;; Timestamps

(define (successor ts latest)
  (define (tadd1 ts)
    (cond [(pair? ts) (cons (add1 (car ts)) (cdr ts))]
          [else (add1 ts)]))
  (cond [(eqv? ts (unbox latest))
         (let ([ts+1 (tadd1 ts)])
           (set-box! latest ts+1)
           (values ts+1 latest))]
        [else
         (let* ([tag (cons (gensym) ts)]
                [next (cons 1 tag)])
           (values next (box next)))]))

(define (t<=? x y)
  (or (eqv? x y) (t<? x y)))

(define (t<? x y)
  (match y
    [(cons yn ytag)
     (match x
       [(cons xn (? (lambda (v) (eqv? v ytag))))
        (< xn yn)]
       [x
        (t<? x (cdr ytag))])]
    [yn
     (and (number? x)
          (< x yn))]))

;; ----

(provide/contract
 [eomap?
  (-> any/c boolean?)]
 [empty-eomap
  (->* () (dict?) eomap?)]
 [eomap-add
  (-> eomap? dict? eomap?)]
 [eomap-set*
  (-> eomap? list? any/c eomap?)]
 [eomap-ref
  (->* (eomap? any/c) (any/c) any)])
