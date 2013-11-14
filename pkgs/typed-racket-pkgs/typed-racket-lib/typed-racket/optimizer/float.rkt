#lang racket/base

(require syntax/parse unstable/sequence racket/dict racket/flonum racket/promise
         syntax/parse/experimental/specialize
         (for-template racket/base racket/flonum racket/unsafe/ops racket/math)
         "../utils/utils.rkt"
         (utils tc-utils)
         (types numeric-tower union abbrev)
         (optimizer utils numeric-utils logging fixnum))

(provide float-opt-expr float-arg-expr int-expr)


(define (mk-float-tbl generic)
  (mk-unsafe-tbl generic "fl~a" "unsafe-fl~a"))

(define binary-float-ops
  (mk-float-tbl (list #'+ #'- #'* #'/ #'min #'max)))
(define binary-float-comps
  (dict-set
   (dict-set
    (mk-float-tbl (list #'= #'<= #'< #'> #'>=))
    ;; not a comparison, but takes 2 floats and does not return a float,
    ;; unlike binary-float-ops
    #'make-rectangular #'unsafe-make-flrectangular)
   #'make-flrectangular #'unsafe-make-flrectangular))
(define unary-float-ops
  (mk-float-tbl (list #'abs #'sin #'cos #'tan #'asin #'acos #'atan #'log #'exp
                      #'sqrt #'round #'floor #'ceiling #'truncate)))

(define-literal-syntax-class -)
(define-literal-syntax-class /)
(define-literal-syntax-class sqr)
(define-literal-syntax-class zero?)
(define-literal-syntax-class add1)
(define-literal-syntax-class sub1)
(define-literal-syntax-class ->float^ (exact->inexact real->double-flonum))
(define-literal-syntax-class ->single-float^ (exact->inexact real->single-flonum))

(define-literal-syntax-class random)
(define-literal-syntax-class flrandom)
(define-merged-syntax-class random-op (random^ flrandom^))

(define-syntax-class (float-op tbl)
  #:commit
  (pattern i:id
           #:when (dict-ref tbl #'i #f)
           #:with unsafe (begin (add-disappeared-use #'i)
                                (dict-ref tbl #'i))))

(define-syntax-class/specialize float-expr (subtyped-expr -Flonum))
(define-syntax-class/specialize single-float-expr (subtyped-expr -SingleFlonum))
(define-syntax-class/specialize int-expr (subtyped-expr -Integer))
(define-syntax-class/specialize real-expr (subtyped-expr -Real))
(define-syntax-class/specialize unary-float-op (float-op unary-float-ops))
(define-syntax-class/specialize binary-float-op (float-op binary-float-ops))
(define-syntax-class/specialize binary-float-comp (float-op binary-float-comps))

;; if the result of an operation is of type float, its non float arguments
;; can be promoted, and we can use unsafe float operations
;; note: none of the unary operations have types where non-float arguments
;;  can result in float (as opposed to real) results
(define-syntax-class float-arg-expr
  #:commit
  #:attributes (opt)
  ;; we can convert literals right away
  (pattern (quote n)
    #:when (and (real?  (syntax->datum #'n))
                (exact? (syntax->datum #'n)))
    #:with opt #`'#,(exact->inexact (syntax->datum #'n)))
  (pattern e:fixnum-expr
    #:attr opt (delay #'(unsafe-fx->fl e.opt)))
  (pattern e:int-expr
    #:attr opt (delay #'(->fl e.opt)))
  (pattern :float-expr)
  ;; reals within float expressions are not always valid to optimize because
  ;; of the exact 0 problem, but since float-opt-expr checks whether the
  ;; surrounding expressing is of type Float and not just Real, this is safe
  (pattern e:real-expr
     #:attr opt (delay #'(real->double-flonum e.opt))))

(define (log-float-real-missed-opt stx irritants)
  (log-missed-optimization
   "all args float-arg-expr, result not Float"
   (string-append
    "This expression has a Real type. The optimizer could optimize it if it had type Float."
    (if (null? irritants)
        ""
        " To fix, change the highlighted expression(s) to have Float type(s)."))
   stx irritants))

(define-syntax-rule (log-fl-opt opt-label)
  (log-opt opt-label "Float arithmetic specialization."))

(define (maybe-exact-rational? stx)
  (and (subtypeof? stx -Real)
       (not (subtypeof? stx -Flonum))
       (not (subtypeof? stx -Int))))


(define-syntax-class float-opt-expr
  #:commit
  #:literal-sets (kernel-literals)
  (pattern (#%plain-app op:unary-float-op f:float-arg-expr)
           #:when (let* ([safe-to-opt? (subtypeof? this-syntax -Flonum)]
                         [missed-optimization? (and (not safe-to-opt?)
                                                    (in-real-layer? this-syntax))])
                    (when missed-optimization?
                      (log-float-real-missed-opt this-syntax (list #'f)))
                    safe-to-opt?)
           #:do [(log-fl-opt "unary float")]
           #:with opt #'(op.unsafe f.opt))
  (pattern (#%plain-app op:binary-float-op
                        ;; for now, accept anything that can be coerced to float
                        ;; finer-grained checking is done below
                        (~between fs:float-arg-expr 2 +inf.0) ...)
           #:when (let* ([safe-to-opt?
                          ;; For it to be safe, we need:
                          ;; - the result to be a float, in which case coercing args to floats
                          ;;   won't change the result type
                          ;; - all non-float arguments need to be provably non-zero
                          ;;   otherwise, we may hit corner cases like (* 0 <float>) => 0
                          ;;   or (+ 0 -0.0) => -0.0 (while (+ 0.0 -0.0) => 0.0)
                          ;; - only one argument can be coerced. If more than one needs
                          ;;   coercion, we could end up turning exact (or single-float)
                          ;;   operations into float operations by accident.
                          ;;   (Note: could allow for more args, if not next to each other, but
                          ;;    probably not worth the trouble (most ops have 2 args anyway))
                          (and (subtypeof? this-syntax -Flonum)
                               (for/and ([a (in-syntax #'(fs ...))])
                                 ;; flonum or provably non-zero
                                 (or (subtypeof? a -Flonum)
                                     (subtypeof? a (Un -PosReal -NegReal))))
                               (>= 1
                                   (for/sum ([a (in-syntax #'(fs ...))]
                                             #:when (not (subtypeof? a -Flonum)))
                                     1)))]
                         ;; if we don't have a return type of float, or if the return type is
                         ;; float, but we can't optimizer for some other reason, we missed an
                         ;; optimization opportunity, report it
                         ;; ignore operations that stay within integers or rationals, since
                         ;; these have nothing to do with float optimizations
                         [missed-optimization? (and (not safe-to-opt?)
                                                    (or (in-real-layer? this-syntax)
                                                        (in-float-layer? this-syntax)))])
                    (when missed-optimization?
                      (log-float-real-missed-opt
                       this-syntax
                       (for/list ([x (in-syntax #'(fs ...))]
                                  #:unless (subtypeof? x -Flonum))
                         x)))
                    ;; If an optimization was expected (whether it was safe or not doesn't matter),
                    ;; report subexpressions doing expensive exact arithmetic (Exact-Rational and
                    ;; Real arithmetic), since that extra precision would be "lost" by going to
                    ;; floating-point in this expression.
                    ;; Since this exact behavior can be desirable, it's invalid to optimize it away,
                    ;; but it's more likely to be there by accident. I can't really think of many
                    ;; use cases for computing exact intermediate results, then converting them to
                    ;; floats at the end.
                    (when (or safe-to-opt? missed-optimization?)
                      (define extra-precision-subexprs
                        (filter
                         values
                         (for/list ([subexpr (in-syntax #'(fs ...))]
                                    #:when (or (and (in-real-layer? subexpr)
                                                    ;; exclude single-flonums
                                                    (not (subtypeof? subexpr -InexactReal)))
                                               (in-rational-layer? subexpr)))
                           (syntax-parse subexpr
                             ;; Only warn about subexpressions that actually perform exact arithmetic.
                             ;; There's not much point in warning about literals/variables that will
                             ;; be coerced anyway, or about things like:
                             ;; (vector-ref vector-of-rationals x)
                             ;; which don't perform arithmetic despite returning numbers.
                             [(~and e:arith-expr (op args ...))
                              ;; if a subexpression has any float args, it will be reported as a
                              ;; float-real mix missed opt, so this report would be redundant

                              #:when (for/and ([s (in-syntax #'(args ...))])
                                       (not (in-float-layer? s)))
                              #'e]
                             [_ #f]))))
                      (when (and (not (null? extra-precision-subexprs))
                                 (subtypeof? this-syntax -InexactReal))
                        (log-missed-optimization
                         "exact ops inside float expr"
                         "This expression has a Float type, but the highlighted subexpression(s) use exact arithmetic. The extra precision of the exact arithmetic will be lost. Using Float types in these subexpression(s) may result in performance gains without significant precision loss."
                         this-syntax extra-precision-subexprs)))
                    safe-to-opt?)
           #:do [(log-fl-opt "binary float")]
           #:with opt (n-ary->binary #'op.unsafe #'(fs.opt ...)))
  (pattern (#%plain-app op:binary-float-comp f1:float-expr f2:float-expr)
    #:do [(log-fl-opt "binary float comp")]
    #:with opt #'(op.unsafe f1.opt f2.opt))
  (pattern (#%plain-app op:binary-float-comp
                        f1:float-expr
                        f2:float-expr
                        fs:float-expr ...)
    #:do [(log-fl-opt "multi float comp")]
    #:with opt (n-ary-comp->binary #'op.unsafe #'f1.opt #'f2.opt #'(fs.opt ...)))

  (pattern (#%plain-app op:-^ f:float-expr)
    #:do [(log-fl-opt "unary float")]
    #:with opt #'(unsafe-fl* -1.0 f.opt))
  (pattern (#%plain-app op:/^ f:float-expr)
    #:do [(log-fl-opt "unary float")]
    #:with opt #'(unsafe-fl/ 1.0 f.opt))
  (pattern (#%plain-app op:sqr^ f:float-expr)
    #:do [(log-fl-opt "unary float")]
    #:with opt #'(let ([tmp f.opt]) (unsafe-fl* tmp tmp)))

  ;; we can optimize exact->inexact if we know we're giving it an Integer
  (pattern (#%plain-app op:->float^ n:int-expr)
    #:do [(log-fl-opt "int to float")]
    #:with opt #'(->fl n.opt))
  ;; we can get rid of it altogether if we're giving it a float
  (pattern (#%plain-app op:->float^ f:float-expr)
    #:do [(log-fl-opt "float to float")]
    #:with opt #'f.opt)
  ;; same for single-flonums
  (pattern (#%plain-app op:->single-float^ f:single-float-expr)
    #:do [(log-fl-opt "single-float to single-float")]
    #:with opt #'f.opt)

  (pattern (#%plain-app op:zero?^ f:float-expr)
    #:do [(log-fl-opt "float zero?")]
    #:with opt #'(unsafe-fl= f.opt 0.0))

  (pattern (#%plain-app op:add1^ n:float-expr)
    #:do [(log-fl-opt "float add1")]
    #:with opt #'(unsafe-fl+ n.opt 1.0))
  (pattern (#%plain-app op:sub1^ n:float-expr)
    #:do [(log-fl-opt "float sub1")]
    #:with opt #'(unsafe-fl- n.opt 1.0))

  (pattern (#%plain-app op:random-op prng:opt-expr)
    #:when (subtypeof? #'prng -Pseudo-Random-Generator)
    #:do [(log-fl-opt "float random")]
    #:with opt #'(unsafe-flrandom prng.opt))
  (pattern (#%plain-app op:random^) ; random with no args
    #:do [(log-fl-opt "float 0-arg random")
          ;; We introduce a reference to `current-pseudo-random-generator',
          ;; but, by optimizing, we're preventing the hidden cost reports
          ;; from triggering down the line (see hidden-cost.rkt), so we need
          ;; to do the logging ourselves.
          (log-optimization-info "hidden parameter (random)" #'op)]
    #:with opt #'(unsafe-flrandom (current-pseudo-random-generator)))

  ;; warn about (potentially) exact real arithmetic, in general
  ;; Note: These patterns don't perform optimization. They only produce logging
  ;;  for consumption by Optimization Coach.
  (pattern (#%plain-app op:binary-float-op n:opt-expr ...)
    #:when (maybe-exact-rational? this-syntax)
    #:do [(log-opt-info "exact real arith")]
    #:with opt #'(op n.opt ...))
  (pattern (#%plain-app op:binary-float-comp n:opt-expr ...)
     ;; can't look at return type, since it's always bool
     #:when (andmap maybe-exact-rational? (syntax->list #'(n ...)))
     #:do [(log-opt-info "exact real arith")]
     #:with opt #'(op n.opt ...))
  (pattern (#%plain-app op:unary-float-op n:opt-expr ...)
     #:when (maybe-exact-rational? this-syntax)
     #:do [(log-opt-info "exact real arith")]
     #:with opt #'(op n.opt ...))
  )

