#lang scheme/base
(require "rep-attrs.ss"
         "../util.ss"
         (for-syntax scheme/base
                     syntax/stx
                     "../util.ss"))
(provide (all-defined-out))

#|
A PBase/HPBase/EHPBase is (listof IAttr)
  If P = (make-pattern Attrs ...) and A is in Attrs,
  the depth of A is with respect to P,
  not with respect to the entire enclosing pattern.

An IdPrefix is an identifier/#f
If #f, it means bind no attributes
If identifier, it already includes the colon part, unless epsilon
|#


#|
A SinglePattern is one of
  (make-pat:any SPBase)
  (make-pat:var SPBase id id (listof stx) (listof IAttr))
  (make-pat:datum SPBase datum)
  (make-pat:literal SPBase identifier)
  (make-pat:head SPBase HeadPattern SinglePattern)
  (make-pat:dots SPBase (listof EllipsisHeadPattern) SinglePattern)
  (make-pat:and SPBase (listof SinglePattern))
  (make-pat:or SPBase (listof SinglePattern))
  (make-pat:not SPBase SinglePattern)
  (make-pat:compound SPBase Kind (listof SinglePattern))
  (make-pat:cut SPBase SinglePattern)
  (make-pat:describe SPBase stx boolean SinglePattern)
  (make-pat:fail SPBase stx stx)
  (make-pat:bind SPBase (listof clause:attr))

A ListPattern is a subtype of SinglePattern; one of
  (make-pat:datum SPBase '())
  (make-pat:head SPBase HeadPattern ListPattern)
  (make-pat:compound SPBase '#:pair (list SinglePattern ListPattern))
  (make-pat:dots SPBase EllipsisHeadPattern SinglePattern)
  (make-pat:cut SPBase ListPattern)
|#

(define-struct pat:any (attrs) #:prefab)
(define-struct pat:var (attrs name parser args nested-attrs) #:prefab)
(define-struct pat:datum (attrs datum) #:prefab)
(define-struct pat:literal (attrs id) #:prefab)
(define-struct pat:head (attrs head tail) #:prefab)
(define-struct pat:dots (attrs heads tail) #:prefab)
(define-struct pat:and (attrs patterns) #:prefab)
(define-struct pat:or (attrs patterns) #:prefab)
(define-struct pat:not (attrs pattern) #:prefab)
(define-struct pat:compound (attrs kind patterns) #:prefab)
(define-struct pat:cut (attrs pattern) #:prefab)
(define-struct pat:describe (attrs description transparent? pattern) #:prefab)
(define-struct pat:fail (attrs when message) #:prefab)
(define-struct pat:bind (attrs clauses) #:prefab)

#|
A HeadPattern is one of 
  (make-hpat:var SPBase id id (listof stx) (listof IAttr))
  (make-hpat:seq HPBase ListPattern)
  (make-hpat:and HPBase HeadPattern SinglePattern)
  (make-hpat:or HPBase (listof HeadPattern))
  (make-hpat:describe HPBase stx/#f boolean HeadPattern)
  (make-hpat:optional HPBase HeadPattern (listof clause:attr))
|#

(define-struct hpat:var (attrs name parser args nested-attrs) #:prefab)
(define-struct hpat:seq (attrs inner) #:prefab)
(define-struct hpat:or (attrs patterns) #:prefab)
(define-struct hpat:and (attrs head single) #:prefab)
(define-struct hpat:describe (attrs description transparent? pattern) #:prefab)
(define-struct hpat:optional (attrs inner defaults) #:prefab)

#|
An EllipsisHeadPattern is
  (make-ehpat EHPBase HeadPattern RepConstraint)

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
      (pat:datum? x)
      (pat:literal? x)
      (pat:head? x)
      (pat:dots? x)
      (pat:and? x)
      (pat:or? x)
      (pat:not? x)
      (pat:compound? x)
      (pat:cut? x)
      (pat:describe? x)
      (pat:bind? x)
      (pat:fail? x)))

(define (head-pattern? x)
  (or (hpat:var? x)
      (hpat:seq? x)
      (hpat:and? x)
      (hpat:or? x)
      (hpat:describe? x)
      (hpat:optional? x)))

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
                 (list (datum->syntax
                        s (format-symbol "~a?" (syntax-e s)))
                       (datum->syntax
                        s (format-symbol "~a-attrs" (syntax-e s)))))])
           #'(lambda (x)
               (cond [(pred x) (accessor x)] ...
                     [else (raise-type-error 'pattern-attrs "pattern" x)])))]))
    (mk-get-attrs pat:any pat:var pat:datum pat:literal pat:head pat:dots
                  pat:and pat:or pat:not pat:compound
                  pat:cut pat:describe pat:bind pat:fail
                  hpat:var hpat:seq hpat:and hpat:or hpat:describe
                  hpat:optional
                  ehpat)))


;; ----

;; Helpers to handle attribute calculations
;; Too complicated for a few pattern forms; those are handled in rep.ss

(define (create-pat:any)
  (make pat:any null))

(define (create-pat:var name parser args nested-attrs)
  (let ([attrs
         (if name (cons (make attr name 0 #t) nested-attrs) nested-attrs)])
    (make pat:var attrs name parser args nested-attrs)))

(define (create-pat:datum datum)
  (make pat:datum null datum))

(define (create-pat:literal literal)
  (make pat:literal null literal))

(define (create-pat:compound kind ps)
  (make pat:compound (append-iattrs (map pattern-attrs ps)) kind ps))

(define (create-pat:cut inner)
  (make pat:cut (pattern-attrs inner) inner))

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

(define (create-pat:fail condition message)
  (make pat:fail null condition message))

(define (create-pat:head headp tailp)
  (let ([attrs (append-iattrs (map pattern-attrs (list headp tailp)))])
    (make pat:head attrs headp tailp)))

;; ----

(define (create-hpat:var name parser args nested-attrs)
  (let ([attrs
         (if name (cons (make attr name 0 #t) nested-attrs) nested-attrs)])
    (make hpat:var attrs name parser args nested-attrs)))

(define (create-hpat:seq lp)
  (make hpat:seq (pattern-attrs lp) lp))

(define (create-hpat:describe description transparent? p)
  (make hpat:describe (pattern-attrs p) description transparent? p))

(define (create-hpat:and hp sp)
  (make hpat:and (append-iattrs (map pattern-attrs (list hp sp))) hp sp))

(define (create-hpat:or patterns)
  (let ([attrs (union-iattrs (map pattern-attrs patterns))])
    (make hpat:or attrs patterns)))

;; ----

(define (head-pattern->list-pattern hp)
  ;; simplification: just extract list pattern from hpat:seq
  (if (hpat:seq? hp)
      (hpat:seq-inner hp)
      (create-pat:head hp (create-pat:datum '()))))
