#lang scheme/base
(require (for-syntax scheme/base
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
  (make-pat:name SPBase SinglePattern (listof identifier))
  (make-pat:any SPBase)
  (make-pat:sc SPBase id id boolean boolean)
  (make-pat:datum SPBase datum)
  (make-pat:literal SPBase identifier)
  (make-pat:head SPBase HeadPattern SinglePattern)
  (make-pat:dots SPBase (listof EllipsisHeadPattern) SinglePattern)
  (make-pat:and SPBase (listof SinglePattern))
  (make-pat:or SPBase (listof SinglePattern))
  (make-pat:compound SPBase Kind (listof SinglePattern))
  (make-pat:cut SPBase SinglePattern)
  (make-pat:describe SPBase stx SinglePattern)
  (make-pat:bind SPBase (listof clause:attr))
  (make-pat:fail SPBase stx stx)

A ListPattern is a subtype of SinglePattern; one of
  (make-pat:datum SPBase '())
  (make-pat:head SPBase HeadPattern ListPattern)
  (make-pat:compound SPBase '#:pair (list SinglePattern ListPattern))
  (make-pat:dots SPBase EllipsisHeadPattern SinglePattern)
  (make-pat:cut SPBase ListPattern)
|#

(define-struct pat:name (attrs pattern names) #:prefab)
(define-struct pat:any (attrs) #:prefab)
(define-struct pat:sc (attrs parser description bind-term? bind-attrs?) #:prefab)
(define-struct pat:datum (attrs datum) #:prefab)
(define-struct pat:literal (attrs id) #:prefab)
(define-struct pat:head (attrs head tail) #:prefab)
(define-struct pat:dots (attrs heads tail) #:prefab)
(define-struct pat:and (attrs patterns) #:prefab)
(define-struct pat:or (attrs patterns) #:prefab)
(define-struct pat:compound (attrs kind patterns) #:prefab)
(define-struct pat:cut (attrs pattern) #:prefab)
(define-struct pat:describe (attrs description pattern) #:prefab)
(define-struct pat:bind (attrs clauses) #:prefab)
(define-struct pat:fail (attrs when message) #:prefab)


#|
A HeadPattern is one of
  (make-hpat:ssc HPBase id id boolean boolean)
  (make-hpat:seq HPBase ListPattern)
  (make-hpat:or HPBase (listof HeadPattern))
  (make-hpat:describe HPBase stx/#f HeadPattern)
|#

(define-struct hpat:ssc (attrs parser description bind-term? bind-attrs?) #:prefab)
(define-struct hpat:seq (attrs inner) #:prefab)
(define-struct hpat:or (attrs patterns) #:prefab)
(define-struct hpat:describe (attrs description pattern) #:prefab)

#|
An EllipsisHeadPattern is
  (make-ehpat EHPBase HeadPattern RepConstraint)

A RepConstraint is one of
  (make-rep:once stx stx stx)
  (make-rep:optional stx stx)
  (make-rep:bounds nat/#f nat/#f stx stx stx)
  #f
|#
(define-struct ehpat (attrs head repc) #:prefab)
(define-struct rep:once (name under-message over-message) #:prefab)
(define-struct rep:optional (name over-message) #:prefab)
(define-struct rep:bounds (min max name under-message over-message) #:prefab)


#|
A Kind is one of
  '#:pair
  '#:box
  '#:vector
  (list '#:pstruct prefab-struct-key)
|#

(define (pattern? x)
  (or (pat:name? x)
      (pat:any? x)
      (pat:sc? x)
      (pat:datum? x)
      (pat:literal? x)
      (pat:head? x)
      (pat:dots? x)
      (pat:and? x)
      (pat:or? x)
      (pat:compound? x)
      (pat:cut? x)
      (pat:describe? x)
      (pat:bind? x)
      (pat:fail? x)))

(define (head-pattern? x)
  (or (hpat:ssc? x)
      (hpat:seq? x)
      (hpat:or? x)
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
                 (list (datum->syntax
                        s (format-symbol "~a?" (syntax-e s)))
                       (datum->syntax
                        s (format-symbol "~a-attrs" (syntax-e s)))))])
           #'(lambda (x)
               (cond [(pred x) (accessor x)] ...
                     [else (raise-type-error 'pattern-attrs "pattern" x)])))]))
    (mk-get-attrs pat:name pat:any pat:sc pat:datum pat:literal pat:head
                  pat:dots pat:and pat:or pat:compound pat:cut pat:describe
                  pat:bind pat:fail
                  hpat:ssc hpat:seq hpat:or hpat:describe
                  ehpat)))
