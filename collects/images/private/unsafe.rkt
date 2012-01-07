#lang racket

(require racket/flonum
         ;racket/unsafe/ops
         (prefix-in unsafe- (combine-in racket/flonum racket/fixnum racket/base))
         )

(provide (all-defined-out)
         ;(all-from-out racket/unsafe/ops)
         (combine-out (all-from-out racket/flonum racket/fixnum)
                      unsafe-bytes-ref unsafe-bytes-set! unsafe-bytes-length
                      unsafe-flvector-ref unsafe-flvector-set! unsafe-flvector-length
                      unsafe-vector-ref unsafe-vector-set! unsafe-vector-length))

;; ===================================================================================================
;; flonum ops

(define-syntax-rule (unsafe-flneg x) (unsafe-fl- 0.0 x))
(define-syntax-rule (unsafe-flsqr x) (let ([y x]) (unsafe-fl* y y)))

(define-syntax-rule (unsafe-flrational? x)
  ;; if x = +nan.0, both tests return #f
  (and (unsafe-fl> x -inf.0) (unsafe-fl< x +inf.0)))

(define-syntax unsafe-flsum
  (syntax-rules ()
    [(_)            0.0]
    [(_ v1)         v1]
    [(_ v1 vs ...)  (unsafe-fl+ v1 (unsafe-flsum vs ...))]))

(define-syntax unsafe-flproduct
  (syntax-rules ()
    [(_)            1.0]
    [(_ v1)         v1]
    [(_ v1 vs ...)  (unsafe-fl* v1 (unsafe-flproduct vs ...))]))

(define-syntax unsafe-flmin*
  (syntax-rules ()
    [(_)            1.0]
    [(_ v1)         v1]
    [(_ v1 vs ...)  (unsafe-flmin v1 (unsafe-flmin* vs ...))]))

(define-syntax unsafe-flmax*
  (syntax-rules ()
    [(_)            1.0]
    [(_ v1)         v1]
    [(_ v1 vs ...)  (unsafe-flmax v1 (unsafe-flmax* vs ...))]))

(define-syntax-rule (unsafe-fl->byte x)
  (unsafe-fl->fx* (unsafe-flround (unsafe-flmax (unsafe-flmin x 255.0) 0.0))))

(define-syntax-rule (unsafe-fl-convex-combination dv sv sa)
  (let ([sa* sa])
    (unsafe-fl+ (unsafe-fl* sa* sv) (unsafe-fl* dv (unsafe-fl- 1.0 sa*)))))

(define-syntax-rule (unsafe-fl-alpha-blend dca sca sa)
  (unsafe-fl+ sca (unsafe-fl* dca (unsafe-fl- 1.0 sa))))

(define-syntax-rule (unsafe-flsigmoid x)
  (unsafe-fl/ 1.0 (unsafe-fl+ 1.0 (unsafe-flexp (unsafe-fl- 0.0 x)))))

(define-syntax-rule (unsafe-flgaussian x s)
  (let* ([s* s] [x*  (unsafe-fl/ x s*)])
    (unsafe-fl/ (unsafe-flexp (unsafe-fl* -0.5 (unsafe-fl* x* x*)))
                (unsafe-fl* (sqrt (* 2.0 pi)) s*))))

;; ===================================================================================================
;; flvector ops

(define-syntax-rule (unsafe-flvector-3ref vs i)
  (let ([j i])
    (values (unsafe-flvector-ref vs j)
            (unsafe-flvector-ref vs (unsafe-fx+ j 1))
            (unsafe-flvector-ref vs (unsafe-fx+ j 2)))))

(define-syntax-rule (unsafe-flvector-3set! vs i x y z)
  (let ([j i])
    (unsafe-flvector-set! vs j x)
    (unsafe-flvector-set! vs (unsafe-fx+ j 1) y)
    (unsafe-flvector-set! vs (unsafe-fx+ j 2) z)))

(define-syntax-rule (unsafe-flvector-4ref vs i)
  (let ([j i])
    (values (unsafe-flvector-ref vs j)
            (unsafe-flvector-ref vs (unsafe-fx+ j 1))
            (unsafe-flvector-ref vs (unsafe-fx+ j 2))
            (unsafe-flvector-ref vs (unsafe-fx+ j 3)))))

(define-syntax-rule (unsafe-flvector-4set! vs i a r g b)
  (let ([j i])
    (unsafe-flvector-set! vs j a)
    (unsafe-flvector-set! vs (unsafe-fx+ j 1) r)
    (unsafe-flvector-set! vs (unsafe-fx+ j 2) g)
    (unsafe-flvector-set! vs (unsafe-fx+ j 3) b)))

(define-syntax-rule (unsafe-build-flvector len f)
  (let ([n len])
    (define vs (make-flvector n))
    (let loop ([i 0])
      (cond [(i . unsafe-fx< . n)  (unsafe-flvector-set! vs i (f i))
                                   (loop (unsafe-fx+ i 1))]
            [else  vs]))))

(define-syntax-rule (unsafe-flvector-sum vs)
  (let ([vs* vs])
    (let ([n  (unsafe-flvector-length vs*)])
      (let loop ([i 0] [sum 0.0])
        (cond [(unsafe-fx= i n)  sum]
              [else  (loop (unsafe-fx+ i 1) (unsafe-fl+ sum (unsafe-flvector-ref vs* i)))])))))

;; ===================================================================================================
;; fixnum ops

(define (unsafe-fl->fx* x) (if (unsafe-flrational? x) (unsafe-fl->fx x) 0))

(define-syntax-rule (unsafe-fxneg x) (unsafe-fx- 0 x))

(define-syntax unsafe-fxsum
  (syntax-rules ()
    [(_)            0]
    [(_ v1)         v1]
    [(_ v1 vs ...)  (unsafe-fx+ v1 (unsafe-fxsum vs ...))]))

(define-syntax unsafe-fxproduct
  (syntax-rules ()
    [(_)            1]
    [(_ v1)         v1]
    [(_ v1 vs ...)  (unsafe-fx* v1 (unsafe-fxproduct vs ...))]))

(define-syntax-rule (unsafe-byte-blend x y α)
  (unsafe-fxquotient (unsafe-fx+ (unsafe-fx* x α) (unsafe-fx* (unsafe-fx- 255 α) y)) 255))

(define-syntax-rule (unsafe-fx->byte x)
  (unsafe-fxmax (unsafe-fxmin x 255) 0))

(define-syntax-rule (unsafe-fx-dst-over-alpha sa da)
  (let ([sa* sa] [da* da])
    (unsafe-fxquotient (unsafe-fx+ 127 (unsafe-fx- (unsafe-fx* (unsafe-fx+ sa* da*) 255)
                                                   (unsafe-fx* sa* da*)))
                       255)))

(define-syntax-rule (unsafe-fx-dst-over-color sa sc da dc)
  (let ([da* da])
    (unsafe-fxquotient (unsafe-fxsum 32512
                                     (unsafe-fxproduct da* dc 255)
                                     (unsafe-fxproduct sa sc (unsafe-fx- 255 da*)))
                       65025)))

;; ===================================================================================================
;; bytes ops

(define-syntax-rule (unsafe-bytes-3ref bs i)
  (let ([j i])
    (values (unsafe-bytes-ref bs j)
            (unsafe-bytes-ref bs (unsafe-fx+ j 1))
            (unsafe-bytes-ref bs (unsafe-fx+ j 2)))))

(define-syntax-rule (unsafe-bytes-3set! bs i r g b)
  (let ([j i])
    (unsafe-bytes-set! bs j r)
    (unsafe-bytes-set! bs (unsafe-fx+ j 1) g)
    (unsafe-bytes-set! bs (unsafe-fx+ j 2) b)))

(define-syntax-rule (unsafe-bytes-4ref bs i)
  (let ([j i])
    (values (unsafe-bytes-ref bs j)
            (unsafe-bytes-ref bs (unsafe-fx+ j 1))
            (unsafe-bytes-ref bs (unsafe-fx+ j 2))
            (unsafe-bytes-ref bs (unsafe-fx+ j 3)))))

(define-syntax-rule (unsafe-bytes-4set! bs i a r g b)
  (let ([j i])
    (unsafe-bytes-set! bs j a)
    (unsafe-bytes-set! bs (unsafe-fx+ j 1) r)
    (unsafe-bytes-set! bs (unsafe-fx+ j 2) g)
    (unsafe-bytes-set! bs (unsafe-fx+ j 3) b)))

;; ===================================================================================================
;; 2-flonum-values ops

(define-syntax-rule (unsafe-fl2dot x1 y1 x2 y2)
  (unsafe-fl+ (unsafe-fl* x1 x2) (unsafe-fl* y1 y2)))

;; ===================================================================================================
;; 3-flonum-values ops

(define-syntax-rule (unsafe-fl3+ x1 y1 z1 x2 y2 z2)
  (values (unsafe-fl+ x1 x2) (unsafe-fl+ y1 y2) (unsafe-fl+ z1 z2)))

(define-syntax-rule (unsafe-fl3- x1 y1 z1 x2 y2 z2)
  (values (unsafe-fl- x1 x2) (unsafe-fl- y1 y2) (unsafe-fl- z1 z2)))

(define-syntax unsafe-fl3*
  (syntax-rules ()
    [(_ x y z c)            (values (unsafe-fl* x c) (unsafe-fl* y c) (unsafe-fl* z c))]
    [(_ x1 y1 z1 x2 y2 z2)  (values (unsafe-fl* x1 x2) (unsafe-fl* y1 y2) (unsafe-fl* z1 z2))]))

(define-syntax unsafe-fl3/
  (syntax-rules ()
    [(_ x y z c)            (values (unsafe-fl/ x c) (unsafe-fl/ y c) (unsafe-fl/ z c))]
    [(_ x1 y1 z1 x2 y2 z2)  (values (unsafe-fl/ x1 x2) (unsafe-fl/ y1 y2) (unsafe-fl/ z1 z2))]))

(define-syntax unsafe-fl3ma
  (syntax-rules ()
    [(_ x y z dx dy dz t)
     (values (unsafe-fl+ x (unsafe-fl* dx t))
             (unsafe-fl+ y (unsafe-fl* dy t))
             (unsafe-fl+ z (unsafe-fl* dz t)))]
    [(_ x y z dx dy dz tx ty tz)
     (values (unsafe-fl+ x (unsafe-fl* dx tx))
             (unsafe-fl+ y (unsafe-fl* dy ty))
             (unsafe-fl+ z (unsafe-fl* dz tz)))]))

(define-syntax-rule (unsafe-fl3neg x y z)
  (values (unsafe-flneg x) (unsafe-flneg y) (unsafe-flneg z)))

(define-syntax-rule (unsafe-fl3dot x1 y1 z1 x2 y2 z2)
  (unsafe-fl+ (unsafe-fl+ (unsafe-fl* x1 x2) (unsafe-fl* y1 y2))
              (unsafe-fl* z1 z2)))

(define-syntax-rule (unsafe-fl3mag^2 dx dy dz)
  (unsafe-fl3dot dx dy dz dx dy dz))

(define-syntax-rule (unsafe-fl3mag dx dy dz)
  (unsafe-flsqrt (unsafe-fl3mag^2 dx dy dz)))

(define-syntax-rule (unsafe-fl3dist x1 y1 z1 x2 y2 z2)
  (unsafe-fl3mag (unsafe-fl- x1 x2) (unsafe-fl- y1 y2) (unsafe-fl- z1 z2)))

(define-syntax-rule (unsafe-fl3normalize x1 y1 z1)
  (let ([i1 x1] [j1 y1] [k1 z1])
    (define d (unsafe-fl3mag i1 j1 k1))
    (values (unsafe-fl/ i1 d) (unsafe-fl/ j1 d) (unsafe-fl/ k1 d))))

(define-syntax-rule (unsafe-fl3-half-norm x1 y1 z1 x2 y2 z2)
  (unsafe-fl3normalize (unsafe-fl+ x1 x2) (unsafe-fl+ y1 y2) (unsafe-fl+ z1 z2)))

(define-syntax-rule (unsafe-fl3-convex-combination x1 y1 z1 x2 y2 z2 α)
  (let* ([a    α]
         [1-a  (unsafe-fl- 1.0 a)])
    (values (unsafe-fl+ (unsafe-fl* 1-a x1) (unsafe-fl* a x2))
            (unsafe-fl+ (unsafe-fl* 1-a y1) (unsafe-fl* a y2))
            (unsafe-fl+ (unsafe-fl* 1-a z1) (unsafe-fl* a z2)))))
