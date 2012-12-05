#lang racket/base

(require syntax/parse
         racket/dict racket/flonum
         (for-template racket/base racket/flonum racket/unsafe/ops racket/math)
         "../utils/utils.rkt"
         (utils tc-utils)
         (types numeric-tower type-table union)
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

(define-syntax-class (float-op tbl)
  #:commit
  (pattern i:id
           #:when (dict-ref tbl #'i #f)
           #:with unsafe (begin (add-disappeared-use #'i)
                                (dict-ref tbl #'i))))

(define-syntax-class float-expr
  #:commit
  (pattern e:expr
           #:when (subtypeof? #'e -Flonum)
           #:with opt ((optimize) #'e)))
(define-syntax-class single-float-expr
  #:commit
  (pattern e:expr
           #:when (subtypeof? #'e -SingleFlonum)
           #:with opt ((optimize) #'e)))
(define-syntax-class int-expr
  #:commit
  (pattern e:expr
           #:when (subtypeof? #'e -Integer)
           #:with opt ((optimize) #'e)))
(define-syntax-class real-expr
  #:commit
  (pattern e:expr
           #:when (subtypeof? #'e -Real)
           #:with opt ((optimize) #'e)))


;; if the result of an operation is of type float, its non float arguments
;; can be promoted, and we can use unsafe float operations
;; note: none of the unary operations have types where non-float arguments
;;  can result in float (as opposed to real) results
(define-syntax-class float-arg-expr
  #:commit
  ;; we can convert literals right away
  (pattern (quote n)
           #:when (and (real?  (syntax->datum #'n))
                       (exact? (syntax->datum #'n)))
           #:with opt
           (datum->syntax #'here (exact->inexact (syntax->datum #'n))))
  (pattern e:fixnum-expr
           #:with opt #'(unsafe-fx->fl e.opt))
  (pattern e:int-expr
           #:with opt #'(->fl e.opt))
  (pattern e:float-expr
           #:with opt #'e.opt)
  ;; reals within float expressions are not always valid to optimize because
  ;; of the exact 0 problem, but since float-opt-expr checks whether the
  ;; surrounding expressing is of type Float and not just Real, this is safe
  (pattern e:real-expr
           #:with opt #'(exact->inexact e)))

(define (log-float-real-missed-opt stx irritants)
  (log-missed-optimization
   "all args float-arg-expr, result not Float"
   (string-append
    "This expression has a Real type. The optimizer could optimize it if it had type Float."
    (if (null? irritants)
        ""
        " To fix, change the highlighted expression(s) to have Float type(s)."))
   stx irritants))

(define float-opt-msg "Float arithmetic specialization.")

(define-syntax-class float-opt-expr
  #:commit
  (pattern (#%plain-app (~var op (float-op unary-float-ops)) f:float-arg-expr)
           #:when (let* ([safe-to-opt? (subtypeof? this-syntax -Flonum)]
                         [missed-optimization? (and (not safe-to-opt?)
                                                    (in-real-layer? this-syntax))])
                    (when missed-optimization?
                      (log-float-real-missed-opt this-syntax (list #'f)))
                    safe-to-opt?)
           #:with opt
           (begin (log-optimization "unary float" float-opt-msg this-syntax)
                  #'(op.unsafe f.opt)))
  (pattern (#%plain-app (~var op (float-op binary-float-ops))
                        ;; for now, accept anything that can be coerced to float
                        ;; finer-grained checking is done below
                        f1:float-arg-expr
                        f2:float-arg-expr
                        fs:float-arg-expr ...)
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
                               (for/and ([a (in-list (syntax->list #'(f1 f2 fs ...)))])
                                 ;; flonum or provably non-zero
                                 (or (subtypeof? a -Flonum)
                                     (subtypeof? a (Un -PosReal -NegReal))))
                               (>= 1
                                   (for/sum ([a (in-list (syntax->list #'(f1 f2 fs ...)))]
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
                       (for/list ([x (in-list (syntax->list #'(f1 f2 fs ...)))]
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
                         (for/list ([subexpr (in-list (syntax->list #'(f1 f2 fs ...)))]
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

                              #:when (for/and ([s (in-list (syntax->list #'(args ...)))])
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
           #:with opt
           (begin (log-optimization "binary float" float-opt-msg this-syntax)
                  (n-ary->binary #'op.unsafe #'f1.opt #'f2.opt #'(fs.opt ...))))
  (pattern (#%plain-app (~var op (float-op binary-float-comps))
                        f1:float-expr
                        f2:float-expr)
           #:with opt
           (begin (log-optimization "binary float comp" float-opt-msg this-syntax)
                  #'(op.unsafe f1.opt f2.opt)))
  (pattern (#%plain-app (~var op (float-op binary-float-comps))
                        f1:float-expr
                        f2:float-expr
                        fs:float-expr ...)
           #:with opt
           (begin (log-optimization "multi float comp" float-opt-msg this-syntax)
                  (n-ary-comp->binary #'op.unsafe #'f1.opt #'f2.opt #'(fs.opt ...))))

  (pattern (#%plain-app (~and op (~literal -)) f:float-expr)
           #:with opt
           (begin (log-optimization "unary float" float-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'(unsafe-fl* -1.0 f.opt)))
  (pattern (#%plain-app (~and op (~literal /)) f:float-expr)
           #:with opt
           (begin (log-optimization "unary float" float-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'(unsafe-fl/ 1.0 f.opt)))
  (pattern (#%plain-app (~and op (~literal sqr)) f:float-expr)
           #:with opt
           (begin (log-optimization "unary float" float-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'(let ([tmp f.opt]) (unsafe-fl* tmp tmp))))

  ;; we can optimize exact->inexact if we know we're giving it an Integer
  (pattern (#%plain-app (~and op (~or (~literal exact->inexact)
                                      (~literal real->double-flonum)))
                        n:int-expr)
           #:with opt
           (begin (log-optimization "int to float" float-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'(->fl n.opt)))
  ;; we can get rid of it altogether if we're giving it a float
  (pattern (#%plain-app (~and op (~or (~literal exact->inexact)
                                      (~literal real->double-flonum)))
                        f:float-expr)
           #:with opt
           (begin (log-optimization "float to float" float-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'f.opt))
  ;; same for single-flonums
  (pattern (#%plain-app (~and op (~or (~literal exact->inexact)
                                      (~literal real->single-flonum)))
                        f:single-float-expr)
           #:with opt
           (begin (log-optimization "single-float to single-float"
                                    float-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'f.opt))

  (pattern (#%plain-app (~and op (~literal zero?)) f:float-expr)
           #:with opt
           (begin (log-optimization "float zero?" float-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'(unsafe-fl= f.opt 0.0)))

  (pattern (#%plain-app (~and op (~literal add1)) n:float-expr)
           #:with opt
           (begin (log-optimization "float add1" float-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'(unsafe-fl+ n.opt 1.0)))
  (pattern (#%plain-app (~and op (~literal sub1)) n:float-expr)
           #:with opt
           (begin (log-optimization "float sub1" float-opt-msg this-syntax)
                  (add-disappeared-use #'op)
                  #'(unsafe-fl- n.opt 1.0))))
