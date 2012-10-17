#lang typed/racket/base

(require racket/flonum
         racket/performance-hint
         (for-syntax racket/base)
         "flonum-constants.rkt"
         "flonum-bits.rkt")

(provide (all-from-out racket/flonum)
         fl
         flsubnormal?
         flnext* flprev*
         flulp-error
         float-complex? (rename-out [inline-number->float-complex number->float-complex])
         find-least-flonum
         fleven? flodd? flsgn flhypot fllog/base
         flprobability?)

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
  (cond [(eqv? r +nan.0)  (if (eqv? x +nan.0) 0.0 +nan.0)]
        [(= r +inf.0)     (if (fl= x +inf.0)  0.0 +inf.0)]
        [(= r -inf.0)     (if (fl= x -inf.0)  0.0 +inf.0)]
        [(zero? r)        (if (zero? x)       0.0 +inf.0)]
        [(eqv? x +nan.0)  +nan.0]
        [(fl= x +inf.0)   +inf.0]
        [(fl= x -inf.0)   +inf.0]
        [(zero? x)        +inf.0]
        [else  (flabs (real->double-flonum
                       (/ (- (inexact->exact x) (inexact->exact r))
                          (inexact->exact (flulp x)))))]))

;; ===================================================================================================
;; Types, conversion

(define-predicate float-complex? Float-Complex)

(define-syntax (inline-number->float-complex stx)
  (syntax-case stx ()
    [(_ z-expr)  (syntax/loc stx
                   (let: ([z : Number  z-expr])
                     (make-rectangular (real->double-flonum (real-part z))
                                       (real->double-flonum (imag-part z)))))]
    [(_ e ...)  (syntax/loc stx (number->float-complex e ...))]
    [_  (syntax/loc stx number->float-complex)]))

(: number->float-complex (Number -> Float-Complex))
(define (number->float-complex z) (inline-number->float-complex z))

;; ===================================================================================================
;; Search

(define +inf-ordinal (flonum->ordinal +inf.0))

(: find-least-flonum (case-> ((Flonum -> Any) Flonum -> (U Flonum #f))
                             ((Flonum -> Any) Flonum Flonum -> (U Flonum #f))))

(define find-least-flonum
  (case-lambda
    [(pred? x-start)
     (when (eqv? +nan.0 x-start)
       (raise-argument-error 'find-least-flonum "non-NaN Flonum" 1 pred? x-start))
     (let loop ([n-end  (flonum->ordinal x-start)] [step 1])
       (define x-end (ordinal->flonum n-end))
       (cond [(pred? x-end)  (find-least-flonum pred? x-start x-end)]
             [(fl= x-end +inf.0)  #f]
             [else  (loop (min +inf-ordinal (+ n-end step)) (* step 2))]))]
    [(pred? x-start x-end)
     (when (eqv? x-start +nan.0)
       (raise-argument-error 'find-least-flonum "non-NaN Flonum" 1 pred? x-start x-end))
     (when (eqv? x-end +nan.0)
       (raise-argument-error 'find-least-flonum "non-NaN Flonum" 2 pred? x-start x-end))
     (cond [(pred? x-start)  x-start]
           [(not (pred? x-end))  #f]
           [else
            (let loop ([n-start  (flonum->ordinal x-start)] [n-end  (flonum->ordinal x-end)])
              (cond [(= n-start n-end)  (define x (ordinal->flonum n-end))
                                        (if (pred? x) x #f)]
                    [else
                     (define n-mid (quotient (+ n-start n-end) 2))
                     (cond [(pred? (ordinal->flonum n-mid))
                            (loop n-start n-mid)]
                           [else
                            (loop (+ n-mid 1) n-end)])]))])]))

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
