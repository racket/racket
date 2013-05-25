#lang typed/racket/base

(require racket/fixnum
         "../../flonum.rkt"
         "../unsafe.rkt"
         "erf.rkt"
         "beta.rkt")

(provide fllog-del-ratio flbeta-lower-regularized-asym)

;; This must be even:
(define num-asym-iters 20)

(define e0 1.12837916709551)  ; 2/sqrt(pi)
(define e1 .353553390593274)  ; 2^(-3/2)
(define log-e0 0.120782237635245)

(define c0 .0833333333333333)
(define c1 -.00277777777760991)
(define c2 7.9365066682539e-4)
(define c3 -5.9520293135187e-4)
(define c4 8.37308034031215e-4)
(define c5 -.00165322962780713)

(: fllog-del-ratio (Flonum Flonum -> Flonum))
;; Computes del(a) + del(b) - del(a+b) where del(x) is the polynomial part of Stirling's series for x
(define (fllog-del-ratio a b)
  (let ([a  (flmin a b)]
        [b  (flmax a b)])
    (define h (fl/ a b))
    (define c (fl/ h (fl+ h 1.0)))
    (define x (fl/ 1.0 (fl+ h 1.0)))
    (define x2 (fl* x x))
    
    ;; sn = (1-x^n)/(1-x)
    (define s3 (fl+ (fl+ x x2) 1.0))
    (define s5 (fl+ (fl+ x (fl* x2 s3)) 1.0))
    (define s7 (fl+ (fl+ x (fl* x2 s5)) 1.0))
    (define s9 (fl+ (fl+ x (fl* x2 s7)) 1.0))
    (define s11 (fl+ (fl+ x (fl* x2 s9)) 1.0))
    
    ;; w = del(b) - del(a+b)
    (define 1/b (fl/ 1.0 b))
    (define w
      (fl* ((make-flpolyfun
             (c0 (fl* c1 s3) (fl* c2 s5) (fl* c3 s7) (fl* c4 s9) (fl* c5 s11)))
            (fl* 1/b 1/b))
           (fl/ c b)))
    
    ;; del(a) + w
    (define 1/a (fl/ 1.0 a))
    (fl+ w (fl/ ((make-flpolyfun (c0 c1 c2 c3 c4 c5))
                 (fl* 1/a 1/a))
                a))))

(: make-thread-local-flvector (Integer -> (-> FlVector)))
(define (make-thread-local-flvector dims)
  (let: ([val : (Thread-Cellof (U #f FlVector)) (make-thread-cell #f)])
    (λ () (or (thread-cell-ref val)
              (let: ([v : FlVector  (make-flvector dims)])
                (thread-cell-set! val v)
                v)))))

(define thread-local-as (make-thread-local-flvector (+ num-asym-iters 1)))
(define thread-local-bs (make-thread-local-flvector (+ num-asym-iters 1)))
(define thread-local-cs (make-thread-local-flvector (+ num-asym-iters 1)))
(define thread-local-ds (make-thread-local-flvector (+ num-asym-iters 1)))

(: flbeta-lower-regularized-asym (Flonum Flonum Flonum Any -> Flonum))
(define (flbeta-lower-regularized-asym a b l log?)
  (define f (fl+ (fl* a (- (fllog1pmx (- (fl/ l a)))))
                 (fl* b (- (fllog1pmx (fl/ l b))))))
  (cond [(and (not log?) (fl= 0.0 (flexp (- f))))  0.0]
        [else  (flbeta-lower-regularized-asym/f a b f log?)]))

(: flbeta-lower-regularized-asym/f (Flonum Flonum Flonum Any -> Flonum))
(define (flbeta-lower-regularized-asym/f a b f log?)
  (define as (thread-local-as))
  (define bs (thread-local-bs))
  (define cs (thread-local-cs))
  (define ds (thread-local-ds))
  
  (define z0 (flsqrt f))
  (define z (fl* 0.5 (fl/ z0 e1)))
  (define z2 (fl* 2.0 f))
  
  (define-values (h r0 r1 w0)
    (cond [(a . fl< . b)
           (let* ([h   (fl/ a b)]
                  [r0  (fl/ 1.0 (fl+ h 1.0))]
                  [r1  (fl/ (fl- b a) b)]
                  [w0  (fl/ 1.0 (flsqrt (fl* a (fl+ h 1.0))))])
             (values h r0 r1 w0))]
          [else
           (let* ([h   (fl/ b a)]
                  [r0  (fl/ 1.0 (fl+ h 1.0))]
                  [r1  (fl/ (fl- b a) a)]
                  [w0  (fl/ 1.0 (flsqrt (fl* b (fl+ h 1.0))))])
             (values h r0 r1 w0))]))
  
  (define a0 (fl* r1 #i2/3))
  (define c0 (fl* -0.5 a0))
  (define d0 (- c0))
  
  (unsafe-flvector-set! as 0 a0)
  (unsafe-flvector-set! bs 0 0.0)
  (unsafe-flvector-set! cs 0 c0)
  (unsafe-flvector-set! ds 0 d0)
  
  (define j0 (fl* (fl/ 0.5 e0) (flerfc*expsqr z0)))
  (define h^2 (fl* h h))
  
  (define: sum : Flonum
    (let: loop : Flonum ([sum : Flonum  (fl+ j0 (fl* (fl* d0 w0) e1))]
                         [s : Flonum  1.0]
                         [hn : Flonum  1.0]
                         [j0 : Flonum  j0]
                         [j1 : Flonum  e1]
                         [znm1 : Flonum  z]
                         [zn : Flonum  z2]
                         [w : Flonum  w0]
                         [n : Positive-Fixnum  1])
      (cond [(n . fx< . num-asym-iters)
             (define new-hn (fl* h^2 hn))
             (define new-s (fl+ s new-hn))
             (unsafe-flvector-set! as n (fl/ (fl* (fl* r0 2.0) (fl+ (fl* h new-hn) 1.0))
                                             (+ n 3.0)))
             (unsafe-flvector-set! as (fx+ n 1) (fl/ (fl* (fl* r1 2.0) new-s) (+ n 4.0)))
             
             (for ([i  (in-range n (fx+ n 2))])
               (define r (fl* -0.5 (+ i 2.0)))
               (unsafe-flvector-set! bs 0 (fl* r a0))
               (for ([m  (in-range 1 (fx+ i 1))])
                 (define bsum
                   (for/fold: ([bsum : Flonum  0.0]) ([j  (in-range 0 m)])
                     (define mmj (- m j))
                     (fl+ bsum (fl* (fl* (- (fl* (+ j 1.0) r) mmj)
                                         (unsafe-flvector-ref as j))
                                    (unsafe-flvector-ref bs (fx- mmj 1))))))
                 (unsafe-flvector-set! bs m (fl+ (fl* r (unsafe-flvector-ref as m))
                                                 (fl/ bsum (+ m 1.0)))))
               (unsafe-flvector-set! cs i (fl/ (unsafe-flvector-ref bs i)
                                               (+ i 2.0)))
               
               (define dsum
                 (for/fold: ([dsum : Flonum  0.0]) ([j  (in-range 0 i)])
                   (fl+ dsum (fl* (unsafe-flvector-ref ds (fx- (fx- i j) 1))
                                  (unsafe-flvector-ref cs j)))))
               (unsafe-flvector-set! ds i (- (fl+ dsum (unsafe-flvector-ref cs i)))))
             
             (let* ([j0  (fl+ (fl* e1 znm1) (* n j0))]
                    [j1  (fl+ (fl* e1 zn) (fl* (+ n 1.0) j1))]
                    [znm1  (fl* z2 znm1)]
                    [zn  (fl* z2 zn)]
                    [w  (fl* w0 w)]
                    [t0  (fl* (fl* (unsafe-flvector-ref ds n) w) j0)]
                    [w  (fl* w0 w)]
                    [t1  (fl* (fl* (unsafe-flvector-ref ds (+ n 1)) w) j1)]
                    [sum  (fl+ sum (fl+ t0 t1))])
               (cond [((fl+ (flabs t0) (flabs t1)) . fl<= . (* epsilon.0 sum))  sum]
                     [else
                      (loop sum new-s new-hn j0 j1 znm1 zn w (fx+ n 2))]))]
            [else  sum])))
  
  (cond [log?  (flsum (list log-e0 (- f) (- (fllog-del-ratio a b)) (fllog sum)))]
        [else  (fl* (fl* (fl* e0 (flexp (- f)))
                         (flexp (- (fllog-del-ratio a b))))
                    sum)]))
