#lang racket/unit

(require "../utils/utils.rkt")
(require (rep type-rep)
         (types union subtype remove-intersect resolve utils)
         "signatures.rkt"
         unstable/sequence
         racket/match)

(import infer^)
(export restrict^)

(define current-seen (make-parameter null))

;; Type Type -> Pair<Seq, Seq>
;; construct a pair for the set of seen type pairs
(define (seen-before s t)
  (cons (Type-seq s) (Type-seq t)))

;; Type Type Seen -> Seen
;; Add the type pair to the set of seen type pairs
(define/cond-contract (remember s t A)
  ((or/c AnyValues? Values/c ValuesDots?) (or/c AnyValues? Values/c ValuesDots?)
   (listof (cons/c exact-nonnegative-integer?
                   exact-nonnegative-integer?))
   . -> .
   (listof (cons/c exact-nonnegative-integer?
                   exact-nonnegative-integer?)))
 (cons (seen-before s t) A))

;; Type Type -> Boolean
;; Check if a given type pair have been seen before
(define/cond-contract (seen? s t cs)
  ((or/c AnyValues? Values/c ValuesDots?) (or/c AnyValues? Values/c ValuesDots?)
   (listof (cons/c exact-nonnegative-integer?
                   exact-nonnegative-integer?))
   . -> . any/c)
 (member (seen-before s t) cs))


;; we don't use union map directly, since that might produce too many elements
  (define (union-map f l)
    (match l
      [(Union: es)
       (let ([l (map f es)])
         (apply Un l))]))

(define (restrict-fields flds flds* f)
  (for/list ([fld (in-sequence-forever (in-list flds) #f)]
             [fld* (in-list flds*)])
    (if fld
        (match* (fld fld*)
         [((fld: t _ _) (fld: t* acc mut))
          (make-fld (restrict t t* f) acc mut)])
        fld*)))


;; NEW IMPL
;; restrict t1 to be a subtype of t2
;; if `f' is 'new, use t2 when giving up, otherwise use t1
(define (restrict* t1 t2 [f 'new])
  (define default (if (eq? f 'new) t2 t1))
  (define cs (current-seen))

  (if (seen? t1 t2 cs)
      default
      (parameterize ([current-seen (remember t1 t2 cs)])
        (cond
          [(subtype t1 t2) t1] ;; already a subtype
          [(match t2
             [(Poly: vars t)
              (and (infer vars null (list t1) (list t) #f) t1)]
             [_ #f])]
          [(Union? t1) (union-map (lambda (e) (restrict* e t2 f)) t1)]
          [(Union? t2) (union-map (lambda (e) (restrict* t1 e f)) t2)]
          [(needs-resolving? t1) (restrict* (resolve-once t1) t2 f)]
          [(needs-resolving? t2) (restrict* t1 (resolve-once t2) f)]
          [(and (Struct? t1) (Struct? t2) (ancestor-struct-type? t1 t2))
           (match* (t1 t2)
            [((Struct: _ _ flds _ _ _) (Struct: name parent flds* proc poly pred-id))
             (define new-flds (restrict-fields flds flds* f))
             (make-Struct name (and parent (restrict* t1 parent f)) new-flds proc poly pred-id)])]
          [(and (Struct? t1) (Struct? t2) (ancestor-struct-type? t2 t1))
           (match* (t2 t1)
            [((Struct: _ _ flds _ _ _) (Struct: name parent flds* proc poly pred-id))
             (define new-flds (restrict-fields flds flds* (if (eq? f 'new) 'old 'new)))
             (make-Struct name (and parent (restrict* parent t2 f)) new-flds proc poly pred-id)])]
          [(subtype t2 t1) t2] ;; we don't actually want this - want something that's a part of t1
          [(not (overlap t1 t2)) (Un)] ;; there's no overlap, so the restriction is empty
          [else default])))) ;; t2 and t1 have a complex relationship, so we punt

(define restrict restrict*)
