#lang racket/base
(require syntax/parse/private/residual-ct ;; keep abs. path
         "rep-attrs.rkt"
         "kws.rkt"
         "make.rkt"
         (for-syntax racket/base
                     syntax/stx
                     racket/syntax))
(provide (all-defined-out))

#|
Uses Arguments from kws.rkt
|#

#|
A Base is (listof IAttr)
  If P = (make-pattern Attrs ...) and A is in Attrs,
  the depth of A is with respect to P,
  not with respect to the entire enclosing pattern.
|#

#|
A SinglePattern is one of
  (pat:any Base)
  (pat:var Base id id Arguments (listof IAttr) nat/#f bool stx)
  (pat:literal Base identifier ct-phase ct-phase)
  (pat:datum Base datum)
  (pat:action Base ActionPattern SinglePattern)
  (pat:head Base HeadPattern SinglePattern)
  (pat:dots Base (listof EllipsisHeadPattern) SinglePattern)
  (pat:and Base (listof SinglePattern))
  (pat:or Base (listof SinglePattern))
  (pat:not Base SinglePattern)
  (pat:pair Base boolean SinglePattern SinglePattern)
  (pat:vector Base SinglePattern)
  (pat:box Base SinglePattern)
  (pat:pstruct Base key SinglePattern)
  (pat:describe Base SinglePattern stx boolean stx)
  (pat:delimit Base SinglePattern)
  (pat:commit Base SinglePattern)
  (pat:reflect Base stx Arguments (listof SAttr) id (listof IAttr))
  (pat:post Base SinglePattern)
  (pat:integrated Base id/#f id string stx)

A ListPattern is a subtype of SinglePattern; one of
  (pat:datum Base '())
  (pat:action Base ActionPattern ListPattern)
  (pat:head Base HeadPattern ListPattern)
  (pat:pair Base #t SinglePattern ListPattern)
  (pat:dots Base EllipsisHeadPattern SinglePattern)
|#

(define-struct pat:any (attrs) #:prefab)
(define-struct pat:var (attrs name parser argu nested-attrs attr-count commit? role) #:prefab)
(define-struct pat:literal (attrs id input-phase lit-phase) #:prefab)
(define-struct pat:datum (attrs datum) #:prefab)
(define-struct pat:action (attrs action inner) #:prefab)
(define-struct pat:head (attrs head tail) #:prefab)
(define-struct pat:dots (attrs heads tail) #:prefab)
(define-struct pat:and (attrs patterns) #:prefab)
(define-struct pat:or (attrs patterns) #:prefab)
(define-struct pat:not (attrs pattern) #:prefab)
(define-struct pat:pair (attrs proper? head tail) #:prefab)
(define-struct pat:vector (attrs pattern) #:prefab)
(define-struct pat:box (attrs pattern) #:prefab)
(define-struct pat:pstruct (attrs key pattern) #:prefab)
(define-struct pat:describe (attrs pattern description transparent? role) #:prefab)
(define-struct pat:delimit (attrs pattern) #:prefab)
(define-struct pat:commit (attrs pattern) #:prefab)
(define-struct pat:reflect (attrs obj argu attr-decls name nested-attrs) #:prefab)
(define-struct pat:post (attrs pattern) #:prefab)
(define-struct pat:integrated (attrs name predicate description role) #:prefab)

#|
A ActionPattern is one of
  (action:cut Base)
  (action:fail Base stx stx)
  (action:bind Base (listof clause:attr))
* (action:and Base (listof ActionPattern))
  (action:parse Base SinglePattern stx)
  (action:do Base (listof stx))
  (action:post Base ActionPattern)

action:and is desugared below in create-* procedures
|#

(define-struct action:cut (attrs) #:prefab)
(define-struct action:fail (attrs when message) #:prefab)
(define-struct action:bind (attrs clauses) #:prefab)
(define-struct action:and (attrs patterns) #:prefab)
(define-struct action:parse (attrs pattern expr) #:prefab)
(define-struct action:do (attrs stmts) #:prefab)
(define-struct action:post (attrs pattern) #:prefab)

#|
A HeadPattern is one of 
  (hpat:var Base id id Arguments (listof IAttr) nat/#f bool stx)
  (hpat:seq Base ListPattern)
  (hpat:action Base ActionPattern HeadPattern)
  (hpat:and Base HeadPattern SinglePattern)
  (hpat:or Base (listof HeadPattern))
  (hpat:optional Base HeadPattern (listof clause:attr))
  (hpat:describe Base HeadPattern stx/#f boolean stx)
  (hpat:delimit Base HeadPattern)
  (hpat:commit Base HeadPattern)
  (hpat:reflect Base stx Arguments (listof SAttr) id (listof IAttr))
  (hpat:post Base HeadPattern)
  (hpat:peek Base HeadPattern)
  (hpat:peek-not Base HeadPattern)
|#

(define-struct hpat:var (attrs name parser argu nested-attrs attr-count commit? role) #:prefab)
(define-struct hpat:seq (attrs inner) #:prefab)
(define-struct hpat:action (attrs action inner) #:prefab)
(define-struct hpat:and (attrs head single) #:prefab)
(define-struct hpat:or (attrs patterns) #:prefab)
(define-struct hpat:optional (attrs inner defaults) #:prefab)
(define-struct hpat:describe (attrs pattern description transparent? role) #:prefab)
(define-struct hpat:delimit (attrs pattern) #:prefab)
(define-struct hpat:commit (attrs pattern) #:prefab)
(define-struct hpat:reflect (attrs obj argu attr-decls name nested-attrs) #:prefab)
(define-struct hpat:post (attrs pattern) #:prefab)
(define-struct hpat:peek (attrs pattern) #:prefab)
(define-struct hpat:peek-not (attrs pattern) #:prefab)

#|
An EllipsisHeadPattern is
  (ehpat Base HeadPattern RepConstraint)

A RepConstraint is one of
  (rep:once stx stx stx)
  (rep:optional stx stx (listof clause:attr))
  (rep:bounds nat/#f nat/#f stx stx stx)
  #f
|#

(define-struct ehpat (attrs head repc) #:prefab)
(define-struct rep:once (name under-message over-message) #:prefab)
(define-struct rep:optional (name over-message defaults) #:prefab)
(define-struct rep:bounds (min max name under-message over-message) #:prefab)


#|
A SideClause is one of
  (clause:fail stx stx)
  (clause:with pattern stx (listof stx))
  (clause:attr IAttr stx)
  (clause:do (listof stx))
|#
(define-struct clause:fail (condition message) #:prefab)
(define-struct clause:with (pattern expr definitions) #:prefab)
(define-struct clause:attr (attr expr) #:prefab)
(define-struct clause:do (stmts) #:prefab)

(define (pattern? x)
  (or (pat:any? x)
      (pat:var? x)
      (pat:literal? x)
      (pat:datum? x)
      (pat:action? x)
      (pat:head? x)
      (pat:dots? x)
      (pat:and? x)
      (pat:or? x)
      (pat:not? x)
      (pat:pair? x)
      (pat:vector? x)
      (pat:box? x)
      (pat:pstruct? x)
      (pat:describe? x)
      (pat:delimit? x)
      (pat:commit? x)
      (pat:reflect? x)
      (pat:post? x)
      (pat:integrated? x)))

(define (action-pattern? x)
  (or (action:cut? x)
      (action:bind? x)
      (action:fail? x)
      (action:and? x)
      (action:parse? x)
      (action:do? x)
      (action:post? x)))

(define (head-pattern? x)
  (or (hpat:var? x)
      (hpat:seq? x)
      (hpat:action? x)
      (hpat:and? x)
      (hpat:or? x)
      (hpat:optional? x)
      (hpat:describe? x)
      (hpat:delimit? x)
      (hpat:commit? x)
      (hpat:reflect? x)
      (hpat:post? x)
      (hpat:peek? x)
      (hpat:peek-not? x)))

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
               (for/list ([s (in-list (stx->list #'(struct ...)))])
                 (list (format-id s "~a?" (syntax-e s))
                       (format-id s "~a-attrs" (syntax-e s))))])
           #'(lambda (x)
               (cond [(pred x) (accessor x)] ...
                     [else (raise-type-error 'pattern-attrs "pattern" x)])))]))
    (mk-get-attrs pat:any pat:var pat:datum pat:literal pat:action pat:head
                  pat:dots pat:and pat:or pat:not pat:describe
                  pat:pair pat:vector pat:box pat:pstruct
                  pat:delimit pat:commit pat:reflect pat:post pat:integrated
                  action:cut action:bind action:fail action:and action:parse
                  action:do action:post
                  hpat:var hpat:seq hpat:action hpat:and hpat:or hpat:describe
                  hpat:optional hpat:delimit hpat:commit hpat:reflect hpat:post
                  hpat:peek hpat:peek-not
                  ehpat)))

;; ----

;; Helpers to handle attribute calculations
;; Too complicated for a few pattern forms; those are handled in rep.rkt

(define (create-pat:any)
  (make pat:any null))

(define (create-pat:var name parser argu nested-attrs attr-count commit? role)
  (let ([attrs
         (if name (cons (make attr name 0 #t) nested-attrs) nested-attrs)])
    (make pat:var attrs name parser argu nested-attrs attr-count commit? role)))

(define (create-pat:reflect obj argu attr-decls name nested-attrs)
  (let ([attrs
         (if name (cons (make attr name 0 #t) nested-attrs) nested-attrs)])
    (make pat:reflect attrs obj argu attr-decls name nested-attrs)))

(define (create-pat:datum datum)
  (make pat:datum null datum))

(define (create-pat:literal literal input-phase lit-phase)
  (make pat:literal null literal input-phase lit-phase))

(define (create-pat:action g sp)
  (cond [(action:and? g)
         (for/fold ([sp sp]) ([g (in-list (reverse (action:and-patterns g)))])
           (create-pat:action g sp))]
        [else
         (let ([attrs (append-iattrs (map pattern-attrs (list g sp)))])
           (make pat:action attrs g sp))]))

(define (create-pat:head headp tailp)
  (let ([attrs (append-iattrs (map pattern-attrs (list headp tailp)))])
    (make pat:head attrs headp tailp)))

(define (create-pat:pair headp tailp)
  (let ([attrs (append-iattrs (map pattern-attrs (list headp tailp)))]
        [proper? (proper-list-pattern? tailp #t)])
    (make pat:pair attrs proper? headp tailp)))

(define (create-pat:vector pattern)
  (make pat:vector (pattern-attrs pattern) pattern))

(define (create-pat:box pattern)
  (make pat:box (pattern-attrs pattern) pattern))

(define (create-pat:pstruct key pattern)
  (make pat:pstruct (pattern-attrs pattern) key pattern))

(define (create-pat:describe p description transparent? role)
  (make pat:describe (pattern-attrs p) p description transparent? role))

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

(define (create-pat:delimit pattern)
  (make pat:delimit (pattern-attrs pattern) pattern))

(define (create-pat:commit pattern)
  (make pat:commit (pattern-attrs pattern) pattern))

(define (create-pat:post pattern)
  (make pat:post (pattern-attrs pattern) pattern))

(define (create-pat:integrated name predicate description role)
  (let ([attrs (if name (list (make attr name 0 #t)) null)])
    (make pat:integrated attrs name predicate description role)))

;; ----

(define (create-action:cut)
  (make action:cut null))

(define (create-action:fail condition message)
  (make action:fail null condition message))

(define (create-action:bind clauses)
  (make action:bind (map clause:attr-attr clauses) clauses))

(define (create-action:and patterns)
  (let ([attrs (append-iattrs (map pattern-attrs patterns))])
    (make action:and attrs patterns)))

(define (create-action:parse pattern expr)
  (make action:parse (pattern-attrs pattern) pattern expr))

(define (create-action:do stmts)
  (make action:do null stmts))

(define (create-action:post pattern)
  (make action:post (pattern-attrs pattern) pattern))

;; ----

(define (create-hpat:var name parser argu nested-attrs attr-count commit? role)
  (let ([attrs
         (if name (cons (make attr name 0 #t) nested-attrs) nested-attrs)])
    (make hpat:var attrs name parser argu nested-attrs attr-count commit? role)))

(define (create-hpat:reflect obj argu attr-decls name nested-attrs)
  (let ([attrs
         (if name (cons (make attr name 0 #t) nested-attrs) nested-attrs)])
    (make hpat:reflect attrs obj argu attr-decls name nested-attrs)))

(define (create-hpat:seq lp)
  (make hpat:seq (pattern-attrs lp) lp))

(define (create-hpat:action g hp)
  (cond [(action:and? g)
         (for/fold ([hp hp]) ([g (in-list (reverse (action:and-patterns g)))])
           (create-hpat:action g hp))]
        [else
         (let ([attrs (append-iattrs (map pattern-attrs (list g hp)))])
           (make hpat:action attrs g hp))]))

(define (create-hpat:describe p description transparent? role)
  (make hpat:describe (pattern-attrs p) p description transparent? role))

(define (create-hpat:and hp sp)
  (make hpat:and (append-iattrs (map pattern-attrs (list hp sp))) hp sp))

(define (create-hpat:or patterns)
  (let ([attrs (union-iattrs (map pattern-attrs patterns))])
    (make hpat:or attrs patterns)))

(define (create-hpat:delimit pattern)
  (make hpat:delimit (pattern-attrs pattern) pattern))

(define (create-hpat:commit pattern)
  (make hpat:commit (pattern-attrs pattern) pattern))

(define (create-hpat:post pattern)
  (make hpat:post (pattern-attrs pattern) pattern))

(define (create-hpat:peek pattern)
  (make hpat:peek (pattern-attrs pattern) pattern))

(define (create-hpat:peek-not pattern)
  (make hpat:peek-not null pattern))

;; ----

(define (create-ehpat head repc)
  (let* ([iattrs0 (pattern-attrs head)]
         [iattrs (repc-adjust-attrs iattrs0 repc)])
    (ehpat iattrs head repc)))

(define (repc-adjust-attrs iattrs repc)
  (cond [(rep:once? repc)
         iattrs]
        [(rep:optional? repc)
         (map attr-make-uncertain iattrs)]
        [(or (rep:bounds? repc) (eq? #f repc))
         (map increase-depth iattrs)]
        [else
         (error 'repc-adjust-attrs "INTERNAL ERROR: unexpected: ~e" repc)]))

;; ----

(define (action/head-pattern->list-pattern p)
  (cond [(action-pattern? p)
         (create-pat:action p (create-pat:any))]
        [(hpat:seq? p)
         ;; simplification: just extract list pattern from hpat:seq
         (hpat:seq-inner p)]
        [else
         (create-pat:head p (create-pat:datum '()))]))

(define (action-pattern->single-pattern gp)
  (create-pat:action gp (create-pat:any)))

(define (proper-list-pattern? p trust-pair?)
  (or (and (pat:datum? p) (eq? (pat:datum-datum p) '()))
      (and (pat:pair? p)
           (if trust-pair?
               (pat:pair-proper? p)
               (proper-list-pattern? (pat:pair-tail p))))
      (and (pat:head? p) (proper-list-pattern? (pat:head-tail p) trust-pair?))
      (and (pat:dots? p) (proper-list-pattern? (pat:dots-tail p) trust-pair?))
      (and (pat:action? p) (proper-list-pattern? (pat:action-inner p) trust-pair?))))
