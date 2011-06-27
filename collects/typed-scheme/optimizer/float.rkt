#lang scheme/base

(require syntax/parse
         racket/dict racket/flonum
         (for-template racket/base racket/flonum racket/unsafe/ops racket/math)
         "../utils/utils.rkt"
         (types numeric-tower type-table)
         (optimizer utils numeric-utils logging fixnum))

(provide float-opt-expr float-arg-expr)


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
           #:with unsafe (dict-ref tbl #'i)))

(define-syntax-class float-expr
  #:commit
  (pattern e:expr
           #:when (subtypeof? #'e -Flonum)
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
   (format "This expression has type ~a. It would be better optimized if it had a Float type. To fix this, change the irritant~a to have~a Float type~a."
           (print-res (type-of stx))
           (if (> (length irritants) 1) "s" "")
           (if (> (length irritants) 1) ""  " a")
           (if (> (length irritants) 1) "s" "")) ; plural
   stx irritants))

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
           (begin (log-optimization "unary float" this-syntax)
                  #'(op.unsafe f.opt)))
  (pattern (#%plain-app (~var op (float-op binary-float-ops))
                        f1:float-arg-expr
                        f2:float-arg-expr
                        fs:float-arg-expr ...)
           ;; if the result is a float, we can coerce integers to floats and optimize
           #:when (let* ([safe-to-opt? (subtypeof? this-syntax -Flonum)]
                         ;; if we don't have a return type of float, we missed an optimization
                         ;; opportunity, report it
                         ;; ignore operations that stay within integers or rationals, since
                         ;; these have nothing to do with float optimizations
                         [missed-optimization? (and (not safe-to-opt?)
                                                    (in-real-layer? this-syntax))])
                    (when missed-optimization?
                      (log-float-real-missed-opt
                       this-syntax
                       (for/list ([x (in-list (syntax->list #'(f1 f2 fs ...)))]
                                  #:when (not (subtypeof? x -Flonum)))
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
                      (for ([subexpr (in-list (syntax->list #'(f1 f2 fs ...)))]
                            #:when (or (in-real-layer? subexpr)
                                       (in-rational-layer? subexpr)))
                        (syntax-parse subexpr
                          ;; Only warn about subexpressions that actually perform exact arithmetic.
                          ;; There's not much point in warning about literals/variables that will
                          ;; be coerced anyway, or about things like:
                          ;; (vector-ref vector-of-rationals x)
                          ;; which don't perform arithmetic despite returning numbers.
                          [e:arith-expr
                           (log-missed-optimization
                            "exact arithmetic subexpression inside a float expression, extra precision discarded"
                            this-syntax subexpr)]
                          [_ #f])))
                    safe-to-opt?)
           #:with opt
           (begin (log-optimization "binary float" this-syntax)
                  (n-ary->binary #'op.unsafe #'f1.opt #'f2.opt #'(fs.opt ...))))
  (pattern (#%plain-app (~var op (float-op binary-float-comps))
                        f1:float-expr
                        f2:float-expr
                        fs:float-expr ...)
           #:with opt
           (begin (log-optimization "binary float comp" this-syntax)
                  (n-ary->binary #'op.unsafe #'f1.opt #'f2.opt #'(fs.opt ...))))

  (pattern (#%plain-app (~and op (~literal -)) f:float-expr)
           #:with opt
           (begin (log-optimization "unary float" this-syntax)
                  #'(unsafe-fl- 0.0 f.opt)))
  (pattern (#%plain-app (~and op (~literal /)) f:float-expr)
           #:with opt
           (begin (log-optimization "unary float" this-syntax)
                  #'(unsafe-fl/ 1.0 f.opt)))
  (pattern (#%plain-app (~and op (~literal sqr)) f:float-expr)
           #:with opt
           (begin (log-optimization "unary float" this-syntax)
                  #'(let ([tmp f.opt]) (unsafe-fl* tmp tmp))))

  ;; we can optimize exact->inexact if we know we're giving it an Integer
  (pattern (#%plain-app (~and op (~literal exact->inexact)) n:int-expr)
           #:with opt
           (begin (log-optimization "int to float" this-syntax)
                  #'(->fl n.opt)))
  ;; we can get rid of it altogether if we're giving it a float
  (pattern (#%plain-app (~and op (~literal exact->inexact)) f:float-expr)
           #:with opt
           (begin (log-optimization "float to float" this-syntax)
                  #'f.opt))

  (pattern (#%plain-app (~and op (~literal zero?)) f:float-expr)
           #:with opt
           (begin (log-optimization "float zero?" this-syntax)
                  #'(unsafe-fl= f.opt 0.0)))

  (pattern (#%plain-app (~and op (~literal add1)) n:float-expr)
           #:with opt
           (begin (log-optimization "float add1" this-syntax)
                  #'(unsafe-fl+ n.opt 1.0)))
  (pattern (#%plain-app (~and op (~literal sub1)) n:float-expr)
           #:with opt
           (begin (log-optimization "float sub1" this-syntax)
                  #'(unsafe-fl- n.opt 1.0))))
