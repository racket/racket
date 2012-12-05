#lang racket/base

(require syntax/parse racket/dict
         "../utils/utils.rkt"
         (for-template racket/base racket/fixnum racket/unsafe/ops)
         (utils tc-utils)
         (types numeric-tower)
         (optimizer utils logging))

(provide fixnum-expr fixnum-opt-expr)


(define (mk-fixnum-tbl generic [fx-specific-too? #t])
  (mk-unsafe-tbl generic (if fx-specific-too? "fx~a" "~a") "unsafe-fx~a"))

;; due to undefined behavior when results are out of the fixnum range, only some
;; fixnum operations can be optimized
;; the following must be closed on fixnums
(define binary-fixnum-ops
  (dict-set
   (dict-set
    (dict-set
     (dict-set
      (dict-set
       (dict-set
        (mk-fixnum-tbl (list #'min #'max))
        #'bitwise-and #'unsafe-fxand)
       #'fxand #'unsafe-fxand)
      #'bitwise-ior #'unsafe-fxior)
     #'fxior #'unsafe-fxior)
    #'bitwise-xor #'unsafe-fxxor)
   #'fxxor #'unsafe-fxxor))

(define binary-fixnum-comps (mk-fixnum-tbl (list #'= #'<= #'< #'> #'>=)))

(define-syntax-class fixnum-unary-op
  #:commit
  (pattern (~or (~literal bitwise-not) (~literal fxnot))
           #:with unsafe (begin (add-disappeared-use this-syntax)
                                #'unsafe-fxnot)))

;; closed on fixnums, but 2nd argument must not be 0
(define-syntax-class nonzero-fixnum-binary-op
  #:commit
  ;; quotient is not closed. (quotient most-negative-fixnum -1) is not a fixnum
  (pattern (~or (~literal modulo)    (~literal fxmodulo))
           #:with unsafe (begin (add-disappeared-use this-syntax)
                                #'unsafe-fxmodulo))
  (pattern (~or (~literal remainder) (~literal fxremainder))
           #:with unsafe (begin (add-disappeared-use this-syntax)
                                #'unsafe-fxremainder)))

;; these operations are not closed on fixnums, but we can sometimes guarantee
;; that results will be within fixnum range
;; if their return type is a subtype of Fixnum, we can optimize
;; obviously, we can't include fx-specific ops here, since their return type is
;; always Fixnum, and we rely on the error behavior if that would be violated
(define potentially-bounded-fixnum-ops
  (mk-fixnum-tbl (list #'+ #'- #'*) #f))
(define potentially-bounded-nonzero-fixnum-ops
  (mk-fixnum-tbl (list #'quotient #'remainder) #f))

(define-syntax-class (fixnum-op tbl)
  #:commit
  (pattern i:id
           #:when (dict-ref tbl #'i #f)
           #:with unsafe (begin (add-disappeared-use #'i)
                                (dict-ref tbl #'i))))


(define-syntax-class fixnum-expr
  #:commit
  (pattern e:expr
           #:when (subtypeof? #'e -Fixnum)
           #:with opt ((optimize) #'e)))
(define-syntax-class nonzero-fixnum-expr
  #:commit
  (pattern e:expr
           #:when (or (subtypeof? #'e -PosFixnum) (subtypeof? #'e -NegFixnum))
           #:with opt ((optimize) #'e)))

(define fixnum-opt-msg "Fixnum arithmetic specialization.")

(define (log-fixnum-missed-opt stx)
  (log-missed-optimization
   "out of fixnum range"
   "This expression consists of all fixnum arguments but is not guaranteed to produce a fixnum. Therefore it cannot be safely optimized. Constraining the arguments to be of Byte or Index types may help."
   stx))

;; general-purpose safety check for fixnum opts
;; some operations have different definitions of safe and have their own checks
;; if we make it this far, an optimization is likely to be expected by the
;; user, so we report a missed opt if the check fails
(define (check-if-safe stx)
  (let ([safe-to-opt? (subtypeof? stx -Fixnum)])
    (unless safe-to-opt?
      (log-fixnum-missed-opt stx))
    safe-to-opt?))

(define-syntax-class fixnum-opt-expr
  #:commit
  (pattern (#%plain-app op:fixnum-unary-op n:fixnum-expr)
           #:with opt
           (begin (log-optimization "unary fixnum" fixnum-opt-msg this-syntax)
                  #'(op.unsafe n.opt)))
  (pattern (#%plain-app (~var op (fixnum-op binary-fixnum-ops))
                        n1:fixnum-expr
                        n2:fixnum-expr
                        ns:fixnum-expr ...)
           #:with opt
           (begin (log-optimization "binary fixnum" fixnum-opt-msg this-syntax)
                  (n-ary->binary #'op.unsafe #'n1.opt #'n2.opt #'(ns.opt ...))))
  (pattern (#%plain-app (~var op (fixnum-op binary-fixnum-comps))
                        n1:fixnum-expr n2:fixnum-expr)
           #:with opt
           (begin (log-optimization "binary fixnum comp" fixnum-opt-msg this-syntax)
                  #'(op.unsafe n1.opt n2.opt)))
  (pattern (#%plain-app (~var op (fixnum-op binary-fixnum-comps))
                        n1:fixnum-expr
                        n2:fixnum-expr
                        ns:fixnum-expr ...)
           #:with opt
           (begin (log-optimization "multi fixnum comp" fixnum-opt-msg this-syntax)
                  (n-ary-comp->binary #'op.unsafe #'n1.opt #'n2.opt #'(ns.opt ...))))

  (pattern (#%plain-app op:nonzero-fixnum-binary-op
                        n1:fixnum-expr
                        n2:nonzero-fixnum-expr)
           #:with opt
           (begin (log-optimization "binary nonzero fixnum" fixnum-opt-msg this-syntax)
                  #'(op.unsafe n1.opt n2.opt)))

  (pattern (#%plain-app (~and op (~literal -)) f:fixnum-expr)
           #:with opt
           (begin (log-optimization "unary fixnum" fixnum-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'(unsafe-fx- 0 f.opt)))

  (pattern (#%plain-app (~and op (~literal exact->inexact)) n:fixnum-expr)
           #:with opt
           (begin (log-optimization "fixnum to float" fixnum-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'(unsafe-fx->fl n.opt)))

  (pattern (#%plain-app (~and op (~literal zero?)) n:fixnum-expr)
           #:with opt
           (begin (log-optimization "fixnum zero?" fixnum-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'(unsafe-fx= n.opt 0)))

  ;; The following are not closed on fixnums, but we can guarantee that results
  ;; won't exceed fixnum range in some cases.
  ;; (if they typecheck with return type Fixnum)
  (pattern (#%plain-app (~var op (fixnum-op potentially-bounded-fixnum-ops))
                        n1:fixnum-expr n2:fixnum-expr ns:fixnum-expr ...)
           #:when (check-if-safe this-syntax)
           #:with opt
           (begin (log-optimization "fixnum bounded expr" fixnum-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  (let ([post-opt (syntax->list #'(n1.opt n2.opt ns.opt ...))])
                    (n-ary->binary #'op.unsafe
                                   (car post-opt) (cadr post-opt) (cddr post-opt)))))
  (pattern (#%plain-app (~var op (fixnum-op potentially-bounded-nonzero-fixnum-ops))
                        n1:fixnum-expr n2:nonzero-fixnum-expr)
           #:when (check-if-safe this-syntax)
           #:with opt
           (begin (log-optimization "nonzero fixnum bounded expr" fixnum-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'(op.unsafe n1.opt n2.opt)))
  ;; for fx-specific ops, we need to mimic the typing rules of their generic
  ;; counterparts, since fx-specific ops rely on error behavior for typechecking
  ;; and thus their return type cannot be used directly for optimization
  ;; Note: We don't log near misses for those, too many false positives.
  ;;  If someone is using `fx+' in the first place, they should know about `unsafe-fx+'.
  (pattern (#%plain-app (~and op (~literal fx+)) n1:fixnum-expr n2:fixnum-expr)
           #:when (or (and (subtypeof? #'n1 -Index) (subtypeof? #'n2 -Index))
                      (and (subtypeof? #'n1 -NonNegFixnum) (subtypeof? #'n2 -NonPosFixnum))
                      (and (subtypeof? #'n1 -NonPosFixnum) (subtypeof? #'n2 -NonNegFixnum)))
           #:with opt
           (begin (log-optimization "fixnum fx+" fixnum-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'(unsafe-fx+ n1.opt n2.opt)))
  (pattern (#%plain-app (~and op (~literal fx-)) n1:fixnum-expr n2:fixnum-expr)
           #:when (and (subtypeof? #'n1 -NonNegFixnum)
                       (subtypeof? #'n2 -NonNegFixnum))
           #:with opt
           (begin (log-optimization "fixnum fx-" fixnum-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'(unsafe-fx- n1.opt n2.opt)))
  (pattern (#%plain-app (~and op (~literal fx*)) n1:fixnum-expr n2:fixnum-expr)
           #:when (and (subtypeof? #'n1 -Byte)
                       (subtypeof? #'n2 -Byte))
           #:with opt
           (begin (log-optimization "fixnum fx*" fixnum-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'(unsafe-fx* n1.opt n2.opt)))
  (pattern (#%plain-app (~and op (~literal fxquotient)) n1:fixnum-expr n2:fixnum-expr)
           #:when (and (subtypeof? #'n1 -NonNegFixnum)
                       (subtypeof? #'n2 -Fixnum))
           #:with opt
           (begin (log-optimization "fixnum fxquotient" fixnum-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'(unsafe-fxquotient n1.opt n2.opt)))
  (pattern (#%plain-app (~and op (~or (~literal fxabs) (~literal abs))) n:fixnum-expr)
           #:when (subtypeof? #'n -NonNegFixnum)
           #:with opt
           (begin (log-optimization "fixnum fxabs" fixnum-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'(unsafe-fxabs n.opt)))

  (pattern (#%plain-app (~and op (~literal add1)) n:fixnum-expr)
           #:when (check-if-safe this-syntax)
           #:with opt
           (begin (log-optimization "fixnum add1" fixnum-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'(unsafe-fx+ n.opt 1)))
  (pattern (#%plain-app (~and op (~literal sub1)) n:fixnum-expr)
           #:when (check-if-safe this-syntax)
           #:with opt
           (begin (log-optimization "fixnum sub1" fixnum-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'(unsafe-fx- n.opt 1))))
