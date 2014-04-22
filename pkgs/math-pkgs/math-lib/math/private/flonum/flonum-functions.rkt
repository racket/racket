#lang typed/racket/base

(require racket/flonum
         (only-in racket/math pi)
         racket/performance-hint
         (for-syntax racket/base)
         "flonum-constants.rkt"
         "flonum-bits.rkt")

(provide (all-from-out racket/flonum)
         fl
         flsubnormal? flrational? flinfinite? flnan? flinteger?
         flnext* flprev*
         flulp-error
         fleven? flodd? flsgn flhypot fllog/base
         flprobability?
         flsinpix flcospix fltanpix flcscpix flsecpix flcotpix)

(module syntax-defs racket/base
  (require (for-syntax racket/base)
           racket/flonum)
  (provide fl)
  (define-syntax (fl stx)
    ;; can't use a rename transformer: get error:
    ;; "unsealed local-definition or module context found in syntax object"
    (syntax-case stx ()
      [(_ . args)  (syntax/loc stx (real->double-flonum . args))]
      [_  (syntax/loc stx real->double-flonum)])))

(require 'syntax-defs)

(begin-encourage-inline
  
  (: flsubnormal? (Flonum -> Boolean))
  (define (flsubnormal? x)
    (and ((flabs x) . fl<= . +max-subnormal.0)
         (not (fl= x 0.0))))
  
  (define flrational?
    (λ: ([x : Flonum])
      (fl< (flabs x) +inf.0)))
  
  (define flinfinite?
    (λ: ([x : Flonum])
      (fl= (flabs x) +inf.0)))
  
  (define flnan?
    (λ: ([x : Flonum])
      (not (fl<= (flabs x) +inf.0))))
  
  (define flinteger?
    (λ: ([x : Flonum])
      (fl= x (fltruncate x))))
  
  (: flsubnormal-next* (Flonum -> Flonum))
  (define (flsubnormal-next* x)
    (fl/ (fl+ (fl* x (flexpt 2.0 1022.0)) epsilon.0)
         (flexpt 2.0 1022.0)))
  
  (: flsubnormal-prev* (Flonum -> Flonum))
  (define (flsubnormal-prev* x)
    (fl/ (fl- (fl* x (flexpt 2.0 1022.0)) epsilon.0)
         (flexpt 2.0 1022.0)))

)  ; begin-encourage-inline

(: flnext* (Flonum -> Flonum))
(define (flnext* x)
  (cond [(x . fl< . 0.0)  (fl- 0.0 (flprev* (fl- 0.0 x)))]
        [(fl= x 0.0)  +min.0]
        [(fl= x +inf.0)  +inf.0]
        [else  (define next-x (fl+ x (fl* x (fl* 0.5 epsilon.0))))
               (cond [(fl= next-x x)  (fl+ x (fl* x epsilon.0))]
                     [else  next-x])]))

(: flprev* (Flonum -> Flonum))
(define (flprev* x)
  (cond [(x . fl< . 0.0)  (fl- 0.0 (flnext* (fl- 0.0 x)))]
        [(fl= x 0.0)  -min.0]
        [(fl= x +inf.0)  +max.0]
        [else  (define prev-x (fl- x (fl* x (fl* 0.5 epsilon.0))))
               (cond [(fl= prev-x x)  (fl- x (fl* x epsilon.0))]
                     [else  prev-x])]))

;; ===================================================================================================
;; Error measurement

(: flulp-error (Flonum Real -> Flonum))
(define (flulp-error x r)
  (define r.0 (fl r))
  (cond [(eqv? x r)  0.0]
        [(and (fl= x 0.0) (fl= r.0 0.0))  0.0]
        [(and (fl= x +inf.0) (fl= r.0 +inf.0))  0.0]
        [(and (fl= x -inf.0) (fl= r.0 -inf.0))  0.0]
        [(zero? r)  +inf.0]
        [(and (flrational? x) (flrational? r.0))
         (flabs (fl (/ (- (inexact->exact x) (inexact->exact r))
                       (inexact->exact (flmax +min.0 (flulp r.0))))))]
        [else  +inf.0]))

;; ===================================================================================================
;; More floating-point functions

(begin-encourage-inline
  
  (: flsgn (Flonum -> Flonum))
  (define (flsgn x)
    (cond [(fl< x 0.0) -1.0]
          [(fl< 0.0 x)  1.0]
          [else  0.0]))
  
  (: fleven? (Flonum -> Boolean))
  (define (fleven? x)
    (let ([x  (flabs x)])
      (or (fl= x 0.0)
          (and (x . fl>= . 2.0)
               (let ([0.5x  (fl* 0.5 x)])
                 (fl= (truncate 0.5x) 0.5x))))))
  
  (define last-odd (fl- (flexpt 2.0 53.0) 1.0))
  
  (: flodd? (Flonum -> Boolean))
  (define (flodd? x)
    (let ([x  (flabs x)])
      (and (x . fl>= . 1.0) (x . fl<= . last-odd)
           (let ([0.5x  (fl* 0.5 (fl+ 1.0 x))])
             (fl= (truncate 0.5x) 0.5x)))))
  
  (: flhypot (Flonum Flonum -> Flonum))
  (define (flhypot x y)
    (define xa (flabs x))
    (define ya (flabs y))
    (let ([xa  (flmin xa ya)]
          [ya  (flmax xa ya)])
      (cond [(fl= xa 0.0)  ya]
            [else  (define u (fl/ xa ya))
                   (fl* ya (flsqrt (fl+ 1.0 (fl* u u))))])))
  
  ;; todo: overflow not likely; underflow likely
  (: fllog/base (Flonum Flonum -> Flonum))
  (define (fllog/base b x)
    (fl/ (fllog x) (fllog b)))
  
  (: flprobability? (case-> (Flonum -> Boolean)
                            (Flonum Any -> Boolean)))
  (define (flprobability? p [log? #f])
    (cond [log?  (and (p . fl>= . -inf.0) (p . fl<= . 0.0))]
          [else  (and (p . fl>= . 0.0) (p . fl<= . 1.0))]))
  
  )  ; begin-encourage-inline

(: flsinpix (Flonum -> Flonum))
;; Computes sin(pi*x) accurately; i.e. error <= 2 ulps but almost always <= 1 ulp
(define (flsinpix x)
  (cond [(fl= x 0.0)  x]
        [(and (x . fl> . -inf.0) (x . fl< . +inf.0))
         (let*-values
             ([(x s)  (if (x . fl< . 0.0) (values (- x) -1.0) (values x 1.0))]
              [(x)    (fl- x (fl* 2.0 (fltruncate (fl* 0.5 x))))]
              [(x s)  (if (x . fl> . 1.0) (values (fl- x 1.0) (fl* s -1.0)) (values x s))]
              [(x)    (if (x . fl> . 0.5) (fl- 1.0 x) x)])
           (fl* s (flsin (fl* pi x))))]
        [else  +nan.0]))

(: flcospix (Flonum -> Flonum))
;; Computes cos(pi*x) accurately; i.e. error <= 1 ulps
(define (flcospix x)
  (cond [(and (x . fl> . -inf.0) (x . fl< . +inf.0))
         (let*-values
             ([(x)  (flabs x)]
              [(x)  (fl- x (fl* 2.0 (fltruncate (fl* 0.5 x))))]
              [(x)  (if (x . fl> . 1.0) (fl- 2.0 x) x)]
              [(x s)  (if (x . fl> . 0.5) (values (fl- 1.0 x) -1.0) (values x 1.0))])
           (cond [(x . fl> . 0.25)  (fl* (fl* s -1.0) (flsin (fl* pi (fl- x 0.5))))]
                 [else  (fl* s (flcos (fl* pi x)))]))]
        [else  +nan.0]))

(: fltanpix (Flonum -> Flonum))
;; Computes tan(pi*x) accurately; i.e. error <= 2 ulps but almost always <= 1 ulp
(define (fltanpix x)
  (cond [(fl= x 0.0)  x]
        [(and (x . fl> . -inf.0) (x . fl< . +inf.0))
         (let*-values 
             ([(x s)  (if (x . fl< . 0.0) (values (- x) -1.0) (values x 1.0))]
              [(x)    (fl- x (fltruncate x))]
              [(x s)  (if (x . fl> . 0.5) (values (fl- 1.0 x) (fl* s -1.0)) (values x s))])
           (cond [(x . fl= . 0.5)  +nan.0]
                 [(x . fl> . 0.25)  (fl/ s (fltan (fl* pi (fl- 0.5 x))))]
                 [else  (fl* s (fltan (fl* pi x)))]))]
        [else  +nan.0]))

(: flcscpix (Flonum -> Flonum))
(define (flcscpix x)
  (cond [(and (not (zero? x)) (flinteger? x))  +nan.0]
        [else  (/ 1.0 (flsinpix x))]))

(: flsecpix (Flonum -> Flonum))
(define (flsecpix x)
  (cond [(and (x . fl> . 0.0) (flinteger? (fl- x 0.5)))  +nan.0]
        [(and (x . fl< . 0.0) (flinteger? (fl+ x 0.5)))  +nan.0]
        [else  (/ 1.0 (flcospix x))]))

(: flcotpix (Flonum -> Flonum))
;; Computes 1/tan(pi*x) accurately; i.e. error <= 2 ulps but almost always <= 1 ulp
(define (flcotpix x)
  (cond [(fl= x 0.0)  (fl/ 1.0 x)]
        [(and (x . fl> . -inf.0) (x . fl< . +inf.0))
         (let*-values 
             ([(x s)  (if (x . fl< . 0.0) (values (- x) -1.0) (values x 1.0))]
              [(x)    (fl- x (fltruncate x))]
              [(x s)  (if (x . fl> . 0.5) (values (fl- 1.0 x) (fl* s -1.0)) (values x s))])
           (cond [(x . fl= . 0.0)  +nan.0]
                 [(x . fl< . 0.25)  (fl/ s (fltan (fl* pi x)))]
                 [else  (fl* s (fltan (fl* pi (fl- 0.5 x))))]))]
        [else  +nan.0]))
