#lang scheme/base
(require "rep-attrs.ss"
         unstable/struct
         (for-syntax scheme/base
                     syntax/stx
                     unstable/syntax))
(provide (all-defined-out))

#|
A Base is (listof IAttr)
  If P = (make-pattern Attrs ...) and A is in Attrs,
  the depth of A is with respect to P,
  not with respect to the entire enclosing pattern.
|#

#|
A SinglePattern is one of
  (make-pat:any Base)
  (make-pat:var Base id id (listof stx) (listof IAttr) bool)
  (make-pat:literal Base identifier ct-phase)
  (make-pat:datum Base datum)
  (make-pat:ghost Base GhostPattern SinglePattern)
  (make-pat:head Base HeadPattern SinglePattern)
  (make-pat:dots Base (listof EllipsisHeadPattern) SinglePattern)
  (make-pat:and Base (listof SinglePattern))
  (make-pat:or Base (listof SinglePattern))
  (make-pat:not Base SinglePattern)
  (make-pat:compound Base Kind (listof SinglePattern))
  (make-pat:describe Base stx boolean SinglePattern)

A ListPattern is a subtype of SinglePattern; one of
  (make-pat:datum Base '())
  (make-pat:ghost Base GhostPattern ListPattern)
  (make-pat:head Base HeadPattern ListPattern)
  (make-pat:compound Base '#:pair (list SinglePattern ListPattern))
  (make-pat:dots Base EllipsisHeadPattern SinglePattern)
|#

(define-struct pat:any (attrs) #:prefab)
(define-struct pat:var (attrs name parser args nested-attrs commit?) #:prefab)
(define-struct pat:literal (attrs id phase) #:prefab)
(define-struct pat:datum (attrs datum) #:prefab)
(define-struct pat:ghost (attrs ghost inner) #:prefab)
(define-struct pat:head (attrs head tail) #:prefab)
(define-struct pat:dots (attrs heads tail) #:prefab)
(define-struct pat:and (attrs patterns) #:prefab)
(define-struct pat:or (attrs patterns) #:prefab)
(define-struct pat:not (attrs pattern) #:prefab)
(define-struct pat:compound (attrs kind patterns) #:prefab)
(define-struct pat:describe (attrs description transparent? pattern) #:prefab)

#|
A GhostPattern is one of
  (make-ghost:cut Base)
  (make-ghost:fail Base bool stx stx)
  (make-ghost:bind Base (listof clause:attr))
* (make-ghost:and Base (listof GhostPattern))
  (make-ghost:parse Base SinglePattern stx)

ghost:and is desugared below in create-* procedures
|#

(define-struct ghost:cut (attrs) #:prefab)
(define-struct ghost:fail (attrs early? when message) #:prefab)
(define-struct ghost:bind (attrs clauses) #:prefab)
(define-struct ghost:and (attrs patterns) #:prefab)
(define-struct ghost:parse (attrs pattern expr) #:prefab)

#|
A HeadPattern is one of 
  (make-hpat:var Base id id (listof stx) (listof IAttr) bool)
  (make-hpat:seq Base ListPattern)
  (make-hpat:ghost Base GhostPattern HeadPattern)
  (make-hpat:and Base HeadPattern SinglePattern)
  (make-hpat:or Base (listof HeadPattern))
  (make-hpat:optional Base HeadPattern (listof clause:attr))
  (make-hpat:describe Base stx/#f boolean HeadPattern)
|#

(define-struct hpat:var (attrs name parser args nested-attrs commit?) #:prefab)
(define-struct hpat:seq (attrs inner) #:prefab)
(define-struct hpat:ghost (attrs ghost inner) #:prefab)
(define-struct hpat:and (attrs head single) #:prefab)
(define-struct hpat:or (attrs patterns) #:prefab)
(define-struct hpat:optional (attrs inner defaults) #:prefab)
(define-struct hpat:describe (attrs description transparent? pattern) #:prefab)

#|
An EllipsisHeadPattern is
  (make-ehpat Base HeadPattern RepConstraint)

A RepConstraint is one of
  (make-rep:once stx stx stx)
  (make-rep:optional stx stx (listof clause:attr))
  (make-rep:bounds nat/#f nat/#f stx stx stx)
  #f
|#

(define-struct ehpat (attrs head repc) #:prefab)
(define-struct rep:once (name under-message over-message) #:prefab)
(define-struct rep:optional (name over-message defaults) #:prefab)
(define-struct rep:bounds (min max name under-message over-message) #:prefab)


#|
A Kind is one of
  '#:pair
  '#:box
  '#:vector
  (list '#:pstruct prefab-struct-key)
|#

(define (pattern? x)
  (or (pat:any? x)
      (pat:var? x)
      (pat:literal? x)
      (pat:datum? x)
      (pat:ghost? x)
      (pat:head? x)
      (pat:dots? x)
      (pat:and? x)
      (pat:or? x)
      (pat:not? x)
      (pat:compound? x)
      (pat:describe? x)))

(define (ghost-pattern? x)
  (or (ghost:cut? x)
      (ghost:bind? x)
      (ghost:fail? x)
      (ghost:and? x)
      (ghost:parse? x)))

(define (head-pattern? x)
  (or (hpat:var? x)
      (hpat:seq? x)
      (hpat:ghost? x)
      (hpat:and? x)
      (hpat:or? x)
      (hpat:optional? x)
      (hpat:describe? x)))

(define (ellipsis-head-pattern? x)
  (ehpat? x))

(define single-pattern? pattern?)

(define (single-or-head-pattern? x)
  (or (single-pattern? x)
      (head-pattern? x)))

(define pattern-attrs
  (let ()
    (define-syntax (mk-get-attrs stx)
      (syntax-case stx ()
        [(_ struct ...)
         (with-syntax
             ([([pred accessor] ...)
               (for/list ([s (stx->list #'(struct ...))])
                 (list (format-id s "~a?" (syntax-e s))
                       (format-id s "~a-attrs" (syntax-e s))))])
           #'(lambda (x)
               (cond [(pred x) (accessor x)] ...
                     [else (raise-type-error 'pattern-attrs "pattern" x)])))]))
    (mk-get-attrs pat:any pat:var pat:datum pat:literal pat:ghost pat:head
                  pat:dots pat:and pat:or pat:not pat:compound pat:describe
                  ghost:cut ghost:bind ghost:fail ghost:and ghost:parse
                  hpat:var hpat:seq hpat:ghost hpat:and hpat:or hpat:describe
                  hpat:optional
                  ehpat)))


;; ----

;; Helpers to handle attribute calculations
;; Too complicated for a few pattern forms; those are handled in rep.ss

(define (create-pat:any)
  (make pat:any null))

(define (create-pat:var name parser args nested-attrs commit?)
  (let ([attrs
         (if name (cons (make attr name 0 #t) nested-attrs) nested-attrs)])
    (make pat:var attrs name parser args nested-attrs commit?)))

(define (create-pat:datum datum)
  (make pat:datum null datum))

(define (create-pat:literal literal phase)
  (make pat:literal null literal phase))

(define (create-pat:ghost g sp)
  (cond [(ghost:and? g)
         (for/fold ([sp sp]) ([g (reverse (ghost:and-patterns g))])
           (create-pat:ghost g sp))]
        [else
         (let ([attrs (append-iattrs (map pattern-attrs (list g sp)))])
           (make pat:ghost attrs g sp))]))

(define (create-pat:head headp tailp)
  (let ([attrs (append-iattrs (map pattern-attrs (list headp tailp)))])
    (make pat:head attrs headp tailp)))

(define (create-pat:compound kind ps)
  (make pat:compound (append-iattrs (map pattern-attrs ps)) kind ps))

(define (create-pat:describe description transparent? p)
  (make pat:describe (pattern-attrs p) description transparent? p))

(define (create-pat:and patterns)
  (let ([attrs (append-iattrs (map pattern-attrs patterns))])
    (make pat:and attrs patterns)))

(define (create-pat:or patterns)
  (let ([attrs (union-iattrs (map pattern-attrs patterns))])
    (make pat:or attrs patterns)))

(define (create-pat:not pattern)
  (make pat:not null pattern))

(define (create-pat:dots headps tailp)
  (let ([attrs (append-iattrs (map pattern-attrs (cons tailp headps)))])
    (make pat:dots attrs headps tailp)))

;; ----

(define (create-ghost:cut)
  (make ghost:cut null))

(define (create-ghost:fail early? condition message)
  (make ghost:fail null early? condition message))

(define (create-ghost:and patterns)
  (let ([attrs (append-iattrs (map pattern-attrs patterns))])
    (make ghost:and attrs patterns)))

(define (create-ghost:parse pattern expr)
  (make ghost:parse (pattern-attrs pattern) pattern expr))

;; ----

(define (create-hpat:var name parser args nested-attrs commit?)
  (let ([attrs
         (if name (cons (make attr name 0 #t) nested-attrs) nested-attrs)])
    (make hpat:var attrs name parser args nested-attrs commit?)))

(define (create-hpat:seq lp)
  (make hpat:seq (pattern-attrs lp) lp))

(define (create-hpat:ghost g hp)
  (cond [(ghost:and? g)
         (for/fold ([hp hp]) ([g (reverse (ghost:and-patterns g))])
           (create-hpat:ghost g hp))]
        [else
         (let ([attrs (append-iattrs (map pattern-attrs (list g hp)))])
           (make hpat:ghost attrs g hp))]))

(define (create-hpat:describe description transparent? p)
  (make hpat:describe (pattern-attrs p) description transparent? p))

(define (create-hpat:and hp sp)
  (make hpat:and (append-iattrs (map pattern-attrs (list hp sp))) hp sp))

(define (create-hpat:or patterns)
  (let ([attrs (union-iattrs (map pattern-attrs patterns))])
    (make hpat:or attrs patterns)))

;; ----

(define (ghost/head-pattern->list-pattern p)
  (cond [(ghost-pattern? p)
         (create-pat:ghost p (create-pat:any))]
        [(hpat:seq? p)
         ;; simplification: just extract list pattern from hpat:seq
         (hpat:seq-inner p)]
        [else
         (create-pat:head p (create-pat:datum '()))]))

(define (ghost-pattern->single-pattern gp)
  (create-pat:ghost gp (create-pat:any)))
