#lang racket/base

(require syntax/parse racket/dict
         syntax/parse/experimental/specialize
         "../utils/utils.rkt"
         (for-template racket/base racket/fixnum racket/unsafe/ops)
         (for-syntax racket/base syntax/parse racket/syntax)
         (utils tc-utils)
         (types numeric-tower union)
         (optimizer utils logging))

(provide fixnum-expr fixnum-opt-expr)

(begin-for-syntax
  (define (format-ids id . args)
    (for/list ((arg (in-list args)))
      (format-id id arg id))))

(define-syntax (define-fx-syntax-classes stx)
  (define-syntax-class spec
    [pattern name:id
       #:with v
         (with-syntax ([(class-name safe-fx unsafe-fx)
                        (format-ids #'name "~a^" "fx~a" "unsafe-fx~a")])
           #'(define-unsafe-syntax-class class-name (name safe-fx) unsafe-fx))])

  (syntax-parse stx
    ((_ (name:spec ...))
     #'(begin name.v ...))))

(define-syntax (define-bitwise-syntax-class stx)
  (define-syntax-class spec
    [pattern name:id
       #:with v
         (with-syntax ([(class-name safe-reg safe-fx unsafe-fx)
                        (format-ids #'name "~a^" "bitwise-~a" "fx~a" "unsafe-fx~a")])
           #'(define-unsafe-syntax-class class-name (safe-reg safe-fx) unsafe-fx))])

  (syntax-parse stx
    [(_ (name:spec ...))
     #'(begin name.v ...)]))

(define-syntax (define-split-fx-syntax-class stx)
  (define-syntax-class spec
    [pattern name:id
       #:with v
         (with-syntax ([(class-name safe-fx unsafe-fx)
                        (format-ids #'name "~a^" "fx~a" "unsafe-fx~a")])
           #'(begin
               (define-unsafe-syntax-class safe-fx)
               (define-unsafe-syntax-class class-name (name) unsafe-fx)))])

  (syntax-parse stx
    [(_ (name:spec ...))
     #'(begin name.v ...)]))



(define-fx-syntax-classes (= < > >= <= min max abs))
;; Seperated out because of potentially-bounded-*-op
(define-split-fx-syntax-class (+ - * quotient modulo remainder))
(define-bitwise-syntax-class (and ior xor not))

(define-unsafe-syntax-class exact->inexact^ (exact->inexact) unsafe-fx->fl)
(define-literal-syntax-class add1)
(define-literal-syntax-class sub1)
(define-literal-syntax-class zero?)

;; Due to undefined behavior when results are out of the fixnum range, only some
;; fixnum operations can be optimized

(define-merged-syntax-class fixnum-unary-op (not^))

;; closed on fixnums
(define-merged-syntax-class fixnum-binary-op (min^ max^ and^ ior^ xor^))

;; closed on fixnums, but 2nd argument must not be 0
;; quotient is not closed. (quotient most-negative-fixnum -1) is not a fixnum
(define-merged-syntax-class nonzero-fixnum-binary-op
  (modulo^ remainder^ fxmodulo^ fxremainder^))

(define-merged-syntax-class fixnum-binary-comp (=^ <^ >^ <=^ >=^))

;; these operations are not closed on fixnums, but we can sometimes guarantee
;; that results will be within fixnum range
;; if their return type is a subtype of Fixnum, we can optimize
;; obviously, we can't include fx-specific ops here, since their return type is
;; always Fixnum, and we rely on the error behavior if that would be violated
(define-merged-syntax-class potentially-bounded-fixnum-op (+^ -^ *^))
(define-merged-syntax-class potentially-bounded-nonzero-fixnum-op (quotient^ remainder^))


(define-syntax-class/specialize byte-expr
  (subtyped-expr -Byte))
(define-syntax-class/specialize index-expr
  (subtyped-expr -Index))
(define-syntax-class/specialize fixnum-expr
  (subtyped-expr -Fixnum))
(define-syntax-class/specialize nonneg-fixnum-expr
  (subtyped-expr -NonNegFixnum))
(define-syntax-class/specialize nonpos-fixnum-expr
  (subtyped-expr -NonPosFixnum))
(define-syntax-class/specialize nonzero-fixnum-expr
  (subtyped-expr (Un -PosFixnum -NegFixnum)))

(define-syntax-rule (log-fx-opt opt-label)
  (log-opt opt-label "Fixnum arithmetic specialization."))

(define (log-fixnum-missed-opt stx)
  (log-missed-optimization
   "out of fixnum range"
   "This expression consists of all fixnum arguments but is not guaranteed to produce a fixnum. Therefore it cannot be safely optimized. Constraining the arguments to be of Byte or Index types may help."
   stx))
(define (log-fixnum-hidden-cost stx)
  (log-optimization-info
   "non-optimized fixnum op" ; e.g. fx+ that couldn't be turned into unsafe-fx+
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
  #:literal-sets (kernel-literals)
  #:attributes (opt)
  (pattern (#%plain-app . :inner-fixnum-opt-expr))
  (pattern (#%plain-app . (~var || (inner-checked-fixnum-opt-expr this-syntax)))))


(define-syntax-class inner-fixnum-opt-expr
  #:commit
  #:attributes (opt)
  (pattern (op:fixnum-unary-op n:fixnum-expr)
    #:do [(log-fx-opt "unary fixnum")]
    #:with opt #'(op.unsafe n.opt))
  (pattern (op:fixnum-binary-op (~between ns:fixnum-expr 2 +inf.0) ...)
    #:do [(log-fx-opt "binary fixnum")]
    #:with opt (n-ary->binary #'op.unsafe #'(ns.opt ...)))
  (pattern (op:fixnum-binary-comp n1:fixnum-expr n2:fixnum-expr)
    #:do [(log-fx-opt "binary fixnum comp")]
    #:with opt #'(op.unsafe n1.opt n2.opt))
  (pattern (op:fixnum-binary-comp n1:fixnum-expr n2:fixnum-expr ns:fixnum-expr ...)
    #:do [(log-fx-opt "multi fixnum comp")]
    #:with opt (n-ary-comp->binary #'op.unsafe #'n1.opt #'n2.opt #'(ns.opt ...)))

  (pattern (op:nonzero-fixnum-binary-op n1:fixnum-expr n2:nonzero-fixnum-expr)
    #:do [(log-fx-opt "binary nonzero fixnum")]
    #:with opt #'(op.unsafe n1.opt n2.opt))

  (pattern (op:-^ f:nonneg-fixnum-expr)
    ;; Invalid for `(- <most-negative-fixnum>)'.
    #:do [(log-fx-opt "unary fixnum")]
    #:with opt #'(op.unsafe 0 f.opt))

  (pattern (op:exact->inexact^ n:fixnum-expr)
    #:do [(log-fx-opt "fixnum to float")]
    #:with opt #'(op.unsafe n.opt))

  (pattern (op:zero?^ n:fixnum-expr)
    #:do [(log-fx-opt "fixnum zero?")]
    #:with opt #'(unsafe-fx= n.opt 0))

  ;; for fx-specific ops, we need to mimic the typing rules of their generic
  ;; counterparts, since fx-specific ops rely on error behavior for typechecking
  ;; and thus their return type cannot be used directly for optimization
  ;; Note: We don't log near misses for those, too many false positives.
  ;;  If someone is using `fx+' in the first place, they should know about
  ;;  `unsafe-fx+'. However, it makes sense to log it as a hidden cost, which
  ;;  won't get shown except in hot code, where it may be worth reporting (see
  ;;  below).
  (pattern (op:fx+^
             (~or (~seq n1:index-expr n2:index-expr)
                  (~seq n1:nonneg-fixnum-expr n2:nonpos-fixnum-expr)
                  (~seq n1:nonpos-fixnum-expr n2:nonneg-fixnum-expr)))
    #:do [(log-fx-opt "fixnum fx+")]
    #:with opt #'(op.unsafe n1.opt n2.opt))
  (pattern (op:fx-^ n1:nonneg-fixnum-expr n2:nonneg-fixnum-expr)
    #:do [(log-fx-opt "fixnum fx-")]
    #:with opt #'(op.unsafe n1.opt n2.opt))
  (pattern (op:fx*^ n1:byte-expr n2:byte-expr)
    #:do [(log-fx-opt "fixnum fx*")]
    #:with opt #'(op.unsafe n1.opt n2.opt))
  (pattern (op:fxquotient^ n1:nonneg-fixnum-expr n2:nonzero-fixnum-expr)
    #:do [(log-fx-opt "fixnum fxquotient")]
    #:with opt #'(op.unsafe n1.opt n2.opt))
  (pattern (op:abs^ n:nonneg-fixnum-expr)
    #:do [(log-fx-opt "fixnum fxabs")]
    #:with opt #'(op.unsafe n.opt)))

;; The following are not closed on fixnums, but we can guarantee that results
;; won't exceed fixnum range in some cases.
;; (if they typecheck with return type Fixnum)
(define-syntax-class (inner-checked-fixnum-opt-expr stx)
  #:attributes (opt)
  (pattern (op:potentially-bounded-fixnum-op (~between ns:fixnum-expr 2 +inf.0) ...)
    #:when (check-if-safe stx)
    #:do [(log-fx-opt "fixnum bounded expr")]
    #:with opt (n-ary->binary #'op.unsafe #'(ns.opt ...)))
  (pattern (op:potentially-bounded-nonzero-fixnum-op n1:fixnum-expr n2:nonzero-fixnum-expr)
    #:when (check-if-safe stx)
    #:do [(log-fx-opt "nonzero fixnum bounded expr")]
    #:with opt #'(op.unsafe n1.opt n2.opt))
  (pattern (op:add1^ n:fixnum-expr)
    #:when (check-if-safe stx)
    #:do [(log-fx-opt "fixnum add1")]
    #:with opt #'(unsafe-fx+ n.opt 1))
  (pattern (op:sub1^ n:fixnum-expr)
    #:when (check-if-safe stx)
    #:do [(log-fx-opt "fixnum sub1")]
    #:with opt #'(unsafe-fx- n.opt 1))
  ;; Report potentially bounded fixnum ops (fixnum versions only) that couldn't
  ;; be proved bounded as hidden costs.
  (pattern ((~or op:fx+^ op:fx-^ op:fx*^ op:fxquotient^
                 op:fxmodulo^ op:fxremainder^)
            n1:opt-expr n2:opt-expr)
    #:do [(log-fixnum-hidden-cost this-syntax)]
    #:with opt #'(op n1.opt n2.opt)))
