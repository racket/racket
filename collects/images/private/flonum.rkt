#lang typed/racket/base

(require (for-syntax typed/racket/base)
         racket/flonum
         (except-in racket/fixnum fl->fx fx->fl)
         racket/math
         (except-in racket/unsafe/ops unsafe-flvector-ref unsafe-flvector-set!)
         (prefix-in old- (only-in racket/unsafe/ops unsafe-flvector-ref unsafe-flvector-set!))
         )

(provide (all-defined-out))

(define-predicate nonnegative-fixnum? Nonnegative-Fixnum)

(: unsafe-flvector-ref (FlVector Integer -> Flonum))
(define unsafe-flvector-ref flvector-ref)

(: unsafe-flvector-set! (FlVector Integer Flonum -> Void))
(define unsafe-flvector-set! flvector-set!)

(define-syntax-rule (fl->fx x)
  (let ([i  (fl->exact-integer x)])
    (with-asserts ([i  fixnum?])
      i)))

(define-syntax-rule (fx->fl i)
  (->fl i))

(define-syntax-rule (flrational? x)
  (let: ([x* : Flonum  x])
    ;; if x = +nan.0, both tests return #f
    (and (x . > . -inf.0) (x . < . +inf.0))))

(define-syntax-rule (fl-convex-combination dv sv sa)
  (let: ([sa* : Flonum  sa])
    (+ (fl* sv sa*) (fl* dv (- 1.0 sa*)))))

(define-syntax-rule (fl-alpha-blend dca sca sa)
  (+ sca (* dca (- 1.0 sa))))

(define-syntax-rule (flgaussian x s)
  (let: ([x/s : Flonum  (fl/ x s)])
    (/ (exp (* -0.5 (* x/s x/s)))
       (fl* (sqrt (* 2.0 pi)) s))))

(define-syntax-rule (flsigmoid x)
  (/ 1.0 (+ 1.0 (exp (fl- 0.0 x)))))

(define-syntax-rule (inline-build-flvector size f)
  (let: ([n : Integer  size])
    (with-asserts ([n  nonnegative-fixnum?])
      (let: ([vs : FlVector  (make-flvector n)])
        (let: loop : FlVector ([i : Nonnegative-Fixnum  0])
          (cond [(i . fx< . n)  (old-unsafe-flvector-set! vs i (f i))
                                (loop (unsafe-fx+ i 1))]
                [else  vs]))))))

;; ===================================================================================================
;; 3-vectors

(define-syntax-rule (fl3dot x1 y1 z1 x2 y2 z2)
  (+ (fl* x1 x2) (fl* y1 y2) (fl* z1 z2)))

(define-syntax (fl3* stx)
  (syntax-case stx ()
    [(_ x y z c)
     (syntax/loc stx
       (let: ([c* : Flonum  c])
         (values (fl* x c*) (fl* y c*) (fl* z c*))))]
    [(_ x1 y1 z1 x2 y2 z2)
     (syntax/loc stx
       (values (fl* x1 x2) (fl* y1 y2) (fl* z1 z2)))]))

(define-syntax-rule (fl3+ x1 y1 z1 x2 y2 z2)
  (values (fl+ x1 x2) (fl+ y1 y2) (fl+ z1 z2)))

(define-syntax (fl3- stx)
  (syntax-case stx ()
    [(_ x y z)
     (syntax/loc stx
       (values (fl- 0.0 x) (fl- 0.0 y) (fl- 0.0 z)))]
    [(_ x1 y1 z1 x2 y2 z2)
     (syntax/loc stx
       (values (fl- x1 x2) (fl- y1 y2) (fl- z1 z2)))]))

(define-syntax-rule (fl3mag^2 x y z)
  (let: ([x* : Flonum  x] [y* : Flonum  y] [z* : Flonum  z])
    (+ (* x* x*) (* y* y*) (* z* z*))))

(define-syntax-rule (fl3mag x y z)
  (flsqrt (fl3mag^2 x y z)))

(define-syntax-rule (fl3dist x1 y1 z1 x2 y2 z2)
  (fl3mag (fl- x1 x2) (fl- y1 y2) (fl- z1 z2)))

(define-syntax-rule (fl3normalize x y z)
  (let: ([x* : Flonum  x] [y* : Flonum  y] [z* : Flonum  z])
    (let: ([d : Flonum  (fl3mag x* y* z*)])
      (values (/ x* d) (/ y* d) (/ z* d)))))

(define-syntax-rule (fl3-half-norm x1 y1 z1 x2 y2 z2)
  (fl3normalize (fl+ x1 x2) (fl+ y1 y2) (fl+ z1 z2)))
