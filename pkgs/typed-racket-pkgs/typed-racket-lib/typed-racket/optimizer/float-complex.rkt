#lang racket/base

(require syntax/parse syntax/stx racket/promise
         racket/syntax racket/match syntax/parse/experimental/specialize
         "../utils/utils.rkt" racket/unsafe/ops unstable/sequence
         (for-template racket/base racket/math racket/flonum racket/unsafe/ops)
         (types numeric-tower subtype type-table utils)
         (optimizer utils numeric-utils logging float unboxed-tables))

(provide float-complex-opt-expr
         float-complex-expr
         binding-names
         float-complex-arith-expr
         unboxed-float-complex-opt-expr
         float-complex-call-site-opt-expr arity-raising-opt-msg)

(define-literal-syntax-class +)
(define-literal-syntax-class -)
(define-literal-syntax-class *)
(define-literal-syntax-class /)
(define-literal-syntax-class conjugate)
(define-literal-syntax-class magnitude)
(define-literal-syntax-class make-polar)
(define-literal-syntax-class exp)

(define-literal-syntax-class make-rectangular^ (make-rectangular unsafe-make-flrectangular))
(define-literal-syntax-class real-part^ (real-part flreal-part unsafe-flreal-part))
(define-literal-syntax-class imag-part^ (imag-part flimag-part unsafe-flimag-part))
(define-merged-syntax-class projection^ (real-part^ imag-part^))

(define-merged-syntax-class float-complex-op (+^ -^ *^ conjugate^ exp^))

(define-syntax-class/specialize number-expr (subtyped-expr -Number))
(define-syntax-class/specialize float-expr (subtyped-expr -Flonum))
(define-syntax-class/specialize float-complex-expr (subtyped-expr -FloatComplex))

(define (binding-names)
  (generate-temporaries (list "unboxed-real-" "unboxed-imag-")))

(define arity-raising-opt-msg "Complex number arity raising.")
(define-syntax-rule (log-unboxing-opt opt-label)
  (log-opt opt-label "Complex number unboxing."))
(define-syntax-rule (log-arity-raising-opt opt-label)
  (log-opt opt-label arity-raising-opt-msg))
(define-syntax-rule (log-missed-complex-expr)
  (log-missed-optimization
    "non-complex value in complex arithmetic"
    (string-append
      "This expression has a non-float Complex number type. "
      "The optimizer could optimize it better if it had type Float-Complex.")
    this-syntax))

;; If a part is 0.0?
(define (0.0? stx)
  (equal? (syntax->datum stx) 0.0))


;; a+bi / c+di, names for real and imag parts of result -> one let-values binding clause
(define (unbox-one-complex-/ a b c d res-real res-imag)
  (define both-real? (and (0.0? b) (0.0? d)))
  ;; we have the same cases as the Racket `/' primitive (except for the non-float ones)
  (define d=0-case
    #`(values (unsafe-fl+ (unsafe-fl/ #,a #,c)
                          (unsafe-fl* #,d #,b))
              (unsafe-fl- (unsafe-fl/ #,b #,c)
                          (unsafe-fl* #,d #,a))))
  (define c=0-case
    #`(values (unsafe-fl+ (unsafe-fl/ #,b #,d)
                          (unsafe-fl* #,c #,a))
              (unsafe-fl- (unsafe-fl* #,c #,b)
                          (unsafe-fl/ #,a #,d))))

  (define general-case
    #`(let* ([cm    (unsafe-flabs #,c)]
             [dm    (unsafe-flabs #,d)]
             [swap? (unsafe-fl< cm dm)]
             [a     (if swap? #,b #,a)]
             [b     (if swap? #,a #,b)]
             [c     (if swap? #,d #,c)]
             [d     (if swap? #,c #,d)]
             [r     (unsafe-fl/ c d)]
             [den   (unsafe-fl+ d (unsafe-fl* c r))]
             [i     (if swap?
                        (unsafe-fl/ (unsafe-fl- a (unsafe-fl* b r)) den)
                        (unsafe-fl/ (unsafe-fl- (unsafe-fl* b r) a) den))])
        (values (unsafe-fl/ (unsafe-fl+ b (unsafe-fl* a r)) den)
                i)))
  (cond [both-real?
         #`[(#,res-real #,res-imag)
            (values (unsafe-fl/ #,a #,c)
                    0.0)]] ; currently not propagated
        [else
         #`[(#,res-real #,res-imag)
            (cond [(unsafe-fl= #,d 0.0) #,d=0-case]
                  [(unsafe-fl= #,c 0.0) #,c=0-case]
                  [else                 #,general-case])]]))

;; a+bi / c+di, names for real and imag parts of result -> one let-values binding clause
;; b = exact 0
;; a,c,d are floats (!= exact 0)
(define (unbox-one-float-complex-/ a c d res-real res-imag)
  ;; TODO: In what cases is the negation in the d=0 case useful
  (define d=0-case
    #`(values (unsafe-fl/ #,a #,c)
              (unsafe-fl* -1.0 (unsafe-fl* #,d #,a))))
  (define c=0-case
    #`(values (unsafe-fl* #,c #,a)
              (unsafe-fl* -1.0 (unsafe-fl/ #,a #,d))))


  (define general-case
    #`(let* ([cm    (unsafe-flabs #,c)]
             [dm    (unsafe-flabs #,d)]
             [swap? (unsafe-fl< cm dm)]
             [a     #,a]
             [c     (if swap? #,d #,c)]
             [d     (if swap? #,c #,d)]
             [r     (unsafe-fl/ c d)]
             [den   (unsafe-fl+ d (unsafe-fl* c r))]
             [i     (if swap?
                        (unsafe-fl/ (unsafe-fl* -1.0 (unsafe-fl* a r)) den)
                        (unsafe-fl/ (unsafe-fl* -1.0 a) den))]
             [j     (if swap? a (unsafe-fl* a r))])
          (values (unsafe-fl/ j den) i)))
  #`[(#,res-real #,res-imag)
     (cond [(unsafe-fl= #,d 0.0) #,d=0-case]
           [(unsafe-fl= #,c 0.0) #,c=0-case]
           [else                 #,general-case])])


;; it's faster to take apart a complex number and use unsafe operations on
;; its parts than it is to use generic operations
;; we keep the real and imaginary parts unboxed as long as we stay within
;; complex operations
(define-syntax-class unboxed-float-complex-opt-expr
  #:commit
  #:attributes (real-binding imag-binding (bindings 1))
  #:literal-sets (kernel-literals)

  ;; We let racket's optimizer handle optimization of 0.0s
  (pattern (#%plain-app op:+^ (~between cs:unboxed-float-complex-opt-expr 2 +inf.0) ...)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:with (real-binding imag-binding) (binding-names)
    #:do [(log-unboxing-opt "unboxed binary float complex")]
    #:with (bindings ...)
      #`(cs.bindings ... ...
         #,@(let ()
               (define (fl-sum cs) (n-ary->binary #'unsafe-fl+ cs))
               (list
                #`((real-binding) #,(fl-sum #'(cs.real-binding ...)))
                #`((imag-binding) #,(fl-sum #'(cs.imag-binding ...)))))))
  (pattern (#%plain-app op:+^ :unboxed-float-complex-opt-expr)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:do [(log-unboxing-opt "unboxed unary float complex")])


  (pattern (#%plain-app op:-^ (~between cs:unboxed-float-complex-opt-expr 2 +inf.0) ...)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:with (real-binding imag-binding) (binding-names)
    #:do [(log-unboxing-opt "unboxed binary float complex")]
    #:with (bindings ...)
      #`(cs.bindings ... ...
         #,@(let ()
              (define (fl-subtract cs) (n-ary->binary #'unsafe-fl- cs))
              (list
               #`((real-binding) #,(fl-subtract #'(cs.real-binding ...)))
               #`((imag-binding) #,(fl-subtract #'(cs.imag-binding ...)))))))
  (pattern (#%plain-app op:-^ c1:unboxed-float-complex-opt-expr) ; unary -
    #:when (subtypeof? this-syntax -FloatComplex)
    #:with (real-binding imag-binding) (binding-names)
    #:do [(log-unboxing-opt "unboxed unary float complex")]
    #:with (bindings ...)
      #`(c1.bindings ...
         [(real-binding) (unsafe-fl* -1.0 c1.real-binding)]
         [(imag-binding) (unsafe-fl* -1.0 c1.imag-binding)]))

  (pattern (#%plain-app op:*^
                        c1:unboxed-float-complex-opt-expr
                        c2:unboxed-float-complex-opt-expr
                        cs:unboxed-float-complex-opt-expr ...)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:with (real-binding imag-binding) (binding-names)
    #:do [(log-unboxing-opt "unboxed binary float complex")]
    #:with (bindings ...)
      #`(c1.bindings ... c2.bindings ... cs.bindings ... ...
         ;; we want to bind the intermediate results to reuse them
         ;; the final results are bound to real-binding and imag-binding
         #,@(let ((lr (syntax->list #'(c1.real-binding c2.real-binding cs.real-binding ...)))
                  (li (syntax->list #'(c1.imag-binding c2.imag-binding cs.imag-binding ...))))
              (let loop ([o1 (car lr)]
                         [o2 (car li)]
                         [e1 (cdr lr)]
                         [e2 (cdr li)]
                         [rs (append (stx-map (lambda (x) (generate-temporary "unboxed-real-"))
                                              #'(cs.real-binding ...))
                                     (list #'real-binding))]
                         [is (append (stx-map (lambda (x) (generate-temporary "unboxed-imag-"))
                                              #'(cs.imag-binding ...))
                                     (list #'imag-binding))]
                         [res '()])
                (if (null? e1)
                    (reverse res)
                    (loop (car rs) (car is) (cdr e1) (cdr e2) (cdr rs) (cdr is)
                          ;; complex multiplication, imag part, then real part (reverse)
                          ;; we eliminate operations on the imaginary parts of reals
                          (let ((o-real? (0.0? o2))
                                (e-real? (0.0? (car e2))))
                            (list* #`((#,(car is))
                                      #,(cond ((and o-real? e-real?) #'0.0)
                                              (o-real? #`(unsafe-fl* #,o1 #,(car e2)))
                                              (e-real? #`(unsafe-fl* #,o2 #,(car e1)))
                                              (else
                                               #`(unsafe-fl+ (unsafe-fl* #,o2 #,(car e1))
                                                             (unsafe-fl* #,o1 #,(car e2))))))
                                   #`((#,(car rs))
                                      #,(cond ((or o-real? e-real?)
                                               #`(unsafe-fl* #,o1 #,(car e1)))
                                              (else
                                               #`(unsafe-fl- (unsafe-fl* #,o1 #,(car e1))
                                                             (unsafe-fl* #,o2 #,(car e2))))))
                                 res))))))))
  (pattern (#%plain-app op:*^ :unboxed-float-complex-opt-expr)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:do [(log-unboxing-opt "unboxed unary float complex")])

  (pattern (#%plain-app op:/^
                        c1:unboxed-float-complex-opt-expr
                        c2:unboxed-float-complex-opt-expr
                        cs:unboxed-float-complex-opt-expr ...)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:with (real-binding imag-binding) (binding-names)
    #:with reals #'(c1.real-binding c2.real-binding cs.real-binding ...)
    #:with imags #'(c1.imag-binding c2.imag-binding cs.imag-binding ...)
    #:do [(log-unboxing-opt "unboxed binary float complex")]
    #:with (bindings ...)
      #`(c1.bindings ... c2.bindings ... cs.bindings ... ...
         ;; we want to bind the intermediate results to reuse them
         ;; the final results are bound to real-binding and imag-binding
         #,@(let loop ([a  (stx-car #'reals)]
                       [b  (stx-car #'imags)]
                       [e1 (cdr (syntax->list #'reals))]
                       [e2 (cdr (syntax->list #'imags))]
                       [rs (append (stx-map (lambda (x) (generate-temporary "unboxed-real-"))
                                            #'(cs.real-binding ...))
                                   (list #'real-binding))]
                       [is (append (stx-map (lambda (x) (generate-temporary "unboxed-imag-"))
                                            #'(cs.imag-binding ...))
                                   (list #'imag-binding))]
                       [res '()])
              (if (null? e1)
                  (reverse res)
                  (loop (car rs) (car is) (cdr e1) (cdr e2) (cdr rs) (cdr is)
                        (cons (unbox-one-complex-/ a b (car e1) (car e2) (car rs) (car is))
                              res))))))
  (pattern (#%plain-app op:/^ c1:unboxed-float-complex-opt-expr) ; unary /
    #:when (subtypeof? this-syntax -FloatComplex)
    #:with (real-binding imag-binding) (binding-names)
    #:do [(log-unboxing-opt "unboxed unary float complex")]
    #:with (bindings ...)
      #`(c1.bindings ...
         #,(unbox-one-float-complex-/ #'1.0 #'c1.real-binding #'c1.imag-binding
                                      #'real-binding #'imag-binding)))

  (pattern (#%plain-app op:conjugate^ c:unboxed-float-complex-opt-expr)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:with real-binding #'c.real-binding
    #:with imag-binding (generate-temporary "unboxed-imag-")
    #:do [(log-unboxing-opt "unboxed unary float complex")]
    #:with (bindings ...)
      #`(c.bindings ...
         ((imag-binding) (unsafe-fl* -1.0 c.imag-binding))))


  (pattern (#%plain-app op:exp^ c:unboxed-float-complex-opt-expr)
    #:when (or (subtypeof? this-syntax -FloatComplex)
               (and (log-missed-complex-expr) #f))
    #:with (real-binding imag-binding) (binding-names)
    #:with scaling-factor (generate-temporary "unboxed-scaling-")
    #:do [(log-unboxing-opt "unboxed unary float complex")]
    #:with (bindings ...)
      #`(c.bindings ...
         ((scaling-factor) (unsafe-flexp c.real-binding))
         ((real-binding) (unsafe-fl* (unsafe-flcos c.imag-binding) scaling-factor))
         ((imag-binding) (unsafe-fl* (unsafe-flsin c.imag-binding) scaling-factor))))


  ;; we can eliminate boxing that was introduced by the user
  (pattern (#%plain-app op:make-rectangular^ real:float-arg-expr imag:float-arg-expr)
    #:when (or (subtypeof? this-syntax -FloatComplex)
               (and (log-missed-complex-expr) #f))
    #:with (real-binding imag-binding) (binding-names)
    #:do [(log-unboxing-opt "make-rectangular elimination")]
    #:with (bindings ...)
      #'(((real-binding) real.opt)
         ((imag-binding) imag.opt)))
  (pattern (#%plain-app op:make-polar^ r:float-arg-expr theta:float-arg-expr)
    #:when (or (subtypeof? this-syntax -FloatComplex)
               (and (log-missed-complex-expr) #f))
    #:with radius       (generate-temporary)
    #:with angle        (generate-temporary)
    #:with (real-binding imag-binding) (binding-names)
    #:do [(log-unboxing-opt "make-rectangular elimination")]
    #:with (bindings ...)
      #'(((radius)       r.opt)
         ((angle)        theta.opt)
         ((real-binding) (unsafe-fl* radius (unsafe-flcos angle)))
         ((imag-binding) (unsafe-fl* radius (unsafe-flsin angle)))))

  ;; if we see a variable that's already unboxed, use the unboxed bindings
  (pattern :unboxed-var
    #:do [(log-unboxing-opt "leave var unboxed")]
    #:with (bindings ...) #'())

  ;; else, do the unboxing here

  ;; we can unbox literals right away
  (pattern (quote n*:number)
    #:do [(define n (syntax->datum #'n*))]
    #:when (not (equal? (imag-part n) 0))
    #:with (real-binding imag-binding) (binding-names)
    #:do [(log-unboxing-opt "unboxed literal")]
    #:with (bindings ...)
      #`(((real-binding) '#,(real->double-flonum (real-part n)))
         ((imag-binding) '#,(real->double-flonum (imag-part n)))))

  (pattern e:float-complex-expr
    #:with e* (generate-temporary)
    #:with (real-binding imag-binding) (binding-names)
    #:do [(log-unboxing-opt "unbox float-complex")]
    #:with (bindings ...)
      #`(((e*) e.opt)
         ((real-binding) (unsafe-flreal-part e*))
         ((imag-binding) (unsafe-flimag-part e*))))

  ;; The following optimization is incorrect and causes bugs because it turns exact numbers into inexact
  (pattern e:number-expr
    #:with e* (generate-temporary)
    #:with (real-binding imag-binding) (binding-names)
    #:do [(log-unboxing-opt
            (if (subtypeof? #'e -Flonum)
                "float in complex ops"
                "non float complex in complex ops"))]
    #:with real-binding-value
      (cond
        [(subtypeof? #'e -Flonum) #'e*]
        [(subtypeof? #'e -Real) #'(real->double-flonum e*)]
        [else #'(real->double-flonum (real-part e*))])
    #:with imag-binding-value
      (cond
        [(subtypeof? #'e -Real) #'0.0]
        [else #'(real->double-flonum (imag-part e*))])

    #:with (bindings ...)
      #'(((e*) e.opt)
         ((real-binding) real-binding-value)
         ((imag-binding) imag-binding-value)))
  (pattern e:expr
    #:do [(error (format "non exhaustive pattern match ~a" #'e))]
    #:with (bindings ...) (list)
    #:with real-binding #f
    #:with imag-binding #f))


(define-syntax-class float-complex-opt-expr
  #:commit
  #:attributes (opt)
  #:literal-sets (kernel-literals)
  ;; Dummy pattern that can't actually match.
  ;; We just want to detect "unexpected" Complex _types_ that come up.
  ;; (not necessarily complex _values_, in fact, most of the time this
  ;; case would come up, no actual complex values will be generated,
  ;; but the type system has to play it safe, and must assume that it
  ;; could happen. ex: (sqrt Integer), if the type system can't prove
  ;; that the argument is non-negative, it must assume that complex
  ;; results can happen, even if it never does in the user's program.
  ;; This is exactly what makes complex types like this "unexpected")
  ;; We define unexpected as: the whole expression has a Complex type,
  ;; but none of its subexpressions do. Since our definition of
  ;; arithmetic expression (see the arith-expr syntax class) exclude
  ;; constructors (like make-rectangular) and coercions, this is a
  ;; reasonable definition.
  (pattern e:arith-expr
           #:when (when (and (in-complex-layer? #'e)
                             (for/and ([subexpr (in-syntax #'(e.args ...))])
                               (subtypeof? subexpr -Real)))
                    (log-missed-optimization
                     "unexpected complex type"
                     (string-append
                      "This expression has a Complex type, despite all its "
                      "arguments being reals. If you do not want or expect "
                      "complex numbers as results, you may want to restrict "
                      "the type of the arguments or use float-specific "
                      "operations (e.g. flsqrt), which may have a beneficial "
                      "impact on performance.")
                     this-syntax))
           ;; We don't actually want to match.
           #:when #f
           ;; required, otherwise syntax/parse is not happy
           #:with opt #'#f)


  (pattern (#%plain-app op:make-polar^ r theta)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:with exp:unboxed-float-complex-opt-expr this-syntax
    #:do [(log-unboxing-opt "make-polar")]
    #:with opt #`(let*-values (exp.bindings ...)
                   (unsafe-make-flrectangular exp.real-binding exp.imag-binding)))

  (pattern (#%plain-app op:unboxed-fun .
             (~var call (float-complex-call-site-opt-expr #'op.unboxed-info)))
    #:do [(log-unboxing-opt "unboxed call site")
          (log-arity-raising-opt "call to fun with unboxed args")]
    #:with opt ((attribute call.opt-app) #'op))

  (pattern :float-complex-arith-opt-expr))

;; Supports not optimizing in order to support using it to check for optimizable expressions.
;; Thus side effects are hidden behind the optimizing argument and referencing the opt attribute.
(define-syntax-class (float-complex-arith-expr* optimizing)
  #:commit
  #:attributes (opt)
  #:literal-sets (kernel-literals)

  ;; we can optimize taking the real of imag part of an unboxed complex
  ;; hopefully, the compiler can eliminate unused bindings for the other part if it's not used
  (pattern (#%plain-app _:projection^ _:float-complex-expr)
    #:attr opt
      (delay
        (syntax-parse this-syntax
          [(#%plain-app op:projection^ c:unboxed-float-complex-opt-expr)
           (log-unboxing-opt "complex accessor elimination")
           #`(let*-values (c.bindings ...)
               #,(if (or (free-identifier=? #'op #'real-part)
                         (free-identifier=? #'op #'flreal-part)
                         (free-identifier=? #'op #'unsafe-flreal-part))
                     #'c.real-binding
                     #'c.imag-binding))])))

  (pattern (#%plain-app _:magnitude^ _:float-complex-expr)
    #:attr opt
      (delay
        (syntax-parse this-syntax
          [(#%plain-app op:magnitude^ c:unboxed-float-complex-opt-expr)
           (log-unboxing-opt "unboxed unary float complex")
           #`(let*-values (c.bindings ...)
               (unsafe-flsqrt
                 (unsafe-fl+ (unsafe-fl* c.real-binding c.real-binding)
                             (unsafe-fl* c.imag-binding c.imag-binding))))])))


  (pattern (#%plain-app op:float-complex-op e:expr ...)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:attr opt
      (delay
        (syntax-parse this-syntax
          (exp:unboxed-float-complex-opt-expr
           #'(let*-values (exp.bindings ...)
               (unsafe-make-flrectangular exp.real-binding exp.imag-binding))))))

  ;; division is special. can only optimize if none of the arguments can be exact 0.
  ;; otherwise, optimization is unsound (we'd give a result where we're supposed to throw an error)
  (pattern (#%plain-app op:/^ e:expr ...)
    #:when (subtypeof? this-syntax -FloatComplex)
    #:when (let ([irritants
                  (for/list ([c (in-syntax #'(e ...))]
                             #:when (match (type-of c)
                                      [(tc-result1: t)
                                       (subtype -Zero t)]
                                      [_ #t]))
                    c)])
             (define safe-to-opt? (null? irritants))
             ;; result is Float-Complex, but unsafe to optimize, missed optimization
             (when (and optimizing (not safe-to-opt?))
               (log-missed-optimization
                "Float-Complex division, potential exact 0s on the rhss"
                (string-append
                 "This expression has a Float-Complex type, but cannot be safely unboxed. "
                 "The second (and later) arguments could potentially be exact 0."
                 (if (null? irritants)
                     ""
                     "\nTo fix, change the highlighted expression(s) to have Float (or Float-Complex) type(s)."))
                this-syntax irritants))
             safe-to-opt?)
    #:attr opt
      (delay
        (syntax-parse this-syntax
          (exp:unboxed-float-complex-opt-expr
           #'(let*-values (exp.bindings ...)
               (unsafe-make-flrectangular exp.real-binding exp.imag-binding))))))

  (pattern (~and :unboxed-var v:float-complex-expr)
    ;; unboxed variable used in a boxed fashion, we have to box
    #:attr opt
      (delay
       (log-unboxing-opt "unboxed complex variable")
       #'(unsafe-make-flrectangular real-binding imag-binding))))


(define-syntax-class possibly-unboxed
  #:attributes ([bindings 1] [real-binding 1] [imag-binding 1] [boxed-binding 1])
  (pattern (#t arg:unboxed-float-complex-opt-expr)
    #:with (bindings ...) #'(arg.bindings ...)
    #:with (real-binding ...) #'(arg.real-binding)
    #:with (imag-binding ...) #'(arg.imag-binding)
    #:with (boxed-binding ...) #'())
  (pattern (#f arg:opt-expr)
    #:with binding-name (generate-temporary 'boxed-binding)
    #:with (bindings ...) #'(((binding-name) arg.opt))
    #:with (real-binding ...) #'()
    #:with (imag-binding ...) #'()
    #:with (boxed-binding ...) #'(binding-name)))

;; takes as argument a structure describing which arguments will be unboxed
;; We cannot log opt here because this doesn't see the full original syntax
(define-syntax-class (float-complex-call-site-opt-expr unboxed-info)
  #:commit
  #:attributes (opt-app)
  ;; call site of a function with unboxed parameters
  ;; the calling convention is: real parts of unboxed, imag parts, boxed
  (pattern (orig-args:expr ...)
    #:with (unboxed-args ...) unboxed-info
    #:attr opt-app
      (λ (op)
        (syntax-parse #'((unboxed-args orig-args) ...)
          [(e:possibly-unboxed ...)
           #`(let*-values (e.bindings ... ...)
               (#,op e.real-binding ... ...
                     e.imag-binding ... ...
                     e.boxed-binding ... ...))]))))


(define-syntax-class/specialize float-complex-arith-opt-expr (float-complex-arith-expr* #t))
(define-syntax-class/specialize float-complex-arith-expr (float-complex-arith-expr* #f))
