;; /*
;;   Based on
;; 
;;    Implementation of SRFI-27 core generator in C for Racket.
;;    dvanhorn@cs.uvm.edu
;; 
;;   and
;; 
;;    54-BIT (double) IMPLEMENTATION IN C OF THE "MRG32K3A" GENERATOR
;;    ===============================================================
;; 
;;    Sebastian.Egner@philips.com, Mar-2002, in ANSI-C and Scheme 48 0.57
;; 
;;    This code is a C-implementation of Pierre L'Ecuyer's MRG32k3a generator.
;;    The code uses (double)-arithmetics, assuming that it covers the range
;;    {-2^53..2^53-1} exactly (!). The code of the generator is based on the
;;    L'Ecuyer's own implementation of the generator. Please refer to the
;;    file 'mrg32k3a.scm' for more information about the method.
;; */

;; The Generator
;; =============

;; moduli of the components
(define Im1 #xffffff2f)
(define Im2 #xffffa6bb)
(define m1 4294967087.0)
(define m2 4294944443.0)

;; recursion coefficients of the components
(define a12  1403580.0)
(define a13n  810728.0)
(define a21   527612.0)
(define a23n 1370589.0)

;; normalization factor 1/(m1 + 1)
(define norm 2.328306549295728e-10)

;; the actual generator

(define-record-type (pseudo-random-generator new-pseudo-random-generator pseudo-random-generator?)
  (fields (mutable x10) (mutable x11) (mutable x12) (mutable x20) (mutable x21) (mutable x22))
  (nongenerative))

(define (mrg32k3a s) ;; -> flonum in {0..m1-1}
  ;; component 1
  (let* ([x10 (fl- (fl* a12 (pseudo-random-generator-x11 s))
                   (fl* a13n (pseudo-random-generator-x12 s)))]
         [k10 (fltruncate (fl/ x10 m1))]
         [x10 (fl- x10 (fl* k10 m1))]
         [x10 (if (fl< x10 0.0)
                  (fl+ x10 m1)
                  x10)])
    (pseudo-random-generator-x12-set! s (pseudo-random-generator-x11 s))
    (pseudo-random-generator-x11-set! s (pseudo-random-generator-x10 s))
    (pseudo-random-generator-x10-set! s x10)
    
    ;; component 2
    (let* ([x20 (fl- (fl* a21 (pseudo-random-generator-x20 s))
                     (fl* a23n (pseudo-random-generator-x22 s)))]
           [k20 (fltruncate (fl/ x20 m2))]
           [x20 (fl- x20 (fl* k20 m2))]
           [x20 (if (fl< x20 0.0)
                    (fl+ x20 m2)
                    x20)])
      (pseudo-random-generator-x22-set! s (pseudo-random-generator-x21 s))
      (pseudo-random-generator-x21-set! s (pseudo-random-generator-x20 s))
      (pseudo-random-generator-x20-set! s x20)

      ;; combination of components
      (let* ([y (fl- x10 x20)])
        (if (fl< y 0.0)
            (fl+ y m1)
            y)))))

(define (make-pseudo-random-generator)
  (let ([s (new-pseudo-random-generator 1.0 1.0 1.0 1.0 1.0 1.0)])
    (pseudo-random-generator-seed! s (current-milliseconds))
    s))

(define (pseudo-random-generator-seed! s x)
  ;; Initial values are from Sebastian Egner's implementation:
  (pseudo-random-generator-x10-set! s 1062452522.0)
  (pseudo-random-generator-x11-set! s 2961816100.0)
  (pseudo-random-generator-x12-set! s 342112271.0)
  (pseudo-random-generator-x20-set! s 2854655037.0)
  (pseudo-random-generator-x21-set! s 3321940838.0)
  (pseudo-random-generator-x22-set! s 3542344109.0)
  (srand-half! s (bitwise-and x #xFFFF))
  (srand-half! s (bitwise-and (bitwise-arithmetic-shift-right x 16) #xFFFF)))

(define (srand-half! s x)
  (let* ([x (random-n! x
                       (- Im1 1)
                       (lambda (z)
                         (pseudo-random-generator-x10-set!
                          s
                          (exact->inexact
                           (+ 1 (modulo
                                 (+ (inexact->exact (pseudo-random-generator-x10 s))
                                    z)
                                 (- Im1 1)))))))]
         [x (random-n! x
                       Im1
                       (lambda (z)
                         (pseudo-random-generator-x11-set!
                          s
                          (exact->inexact
                           (+ 1 (modulo
                                 (+ (inexact->exact (pseudo-random-generator-x11 s))
                                    z)
                                 Im1))))))]
         [x (random-n! x
                       Im1
                       (lambda (z)
                         (pseudo-random-generator-x12-set!
                          s
                          (exact->inexact
                           (+ 1 (modulo
                                 (+ (inexact->exact (pseudo-random-generator-x12 s))
                                    z)
                                 Im1))))))]
         [x (random-n! x
                       (- Im2 1)
                       (lambda (z)
                         (pseudo-random-generator-x20-set!
                          s
                          (exact->inexact
                           (+ 1 (modulo
                                 (+ (inexact->exact (pseudo-random-generator-x20 s))
                                    z)
                                 (- Im2 1)))))))]
         [x (random-n! x
                       Im2
                       (lambda (z)
                         (pseudo-random-generator-x21-set!
                          s
                          (exact->inexact
                           (+ 1 (modulo
                                 (+ (inexact->exact (pseudo-random-generator-x21 s))
                                    z)
                                 Im2))))))]
         [x (random-n! x
                       Im2
                       (lambda (z)
                         (pseudo-random-generator-x22-set!
                          s
                          (exact->inexact
                           (+ 1 (modulo
                                 (+ (inexact->exact (pseudo-random-generator-x22 s))
                                    z)
                                 Im2))))))])
    (void)))

(define (random-n! x Im k)
  (let* ([y1 (bitwise-and x #xFFFF)]
         [x (+ (* 30903 y1) (bitwise-arithmetic-shift-right x 16))]
         [y2 (bitwise-and x #xFFFF)]
         [x (+ (* 30903 y2) (bitwise-arithmetic-shift-right x 16))])
    (k (modulo (+ (arithmetic-shift y1 16) y2) Im))
    x))

(define/who (pseudo-random-generator->vector s)
  (check who pseudo-random-generator? s)
  (vector (inexact->exact (pseudo-random-generator-x10 s))
          (inexact->exact (pseudo-random-generator-x11 s))
          (inexact->exact (pseudo-random-generator-x12 s))
          (inexact->exact (pseudo-random-generator-x20 s))
          (inexact->exact (pseudo-random-generator-x21 s))
          (inexact->exact (pseudo-random-generator-x22 s))))

(define (pseudo-random-generator-vector? v)
  (let ([in-range?
         (lambda (i mx)
           (let ([x (vector-ref v i)])
             (and (exact-nonnegative-integer? x)
                  (<= x mx))))]
        [nonzero?
         (lambda (i)
           (not (zero? (vector-ref v i))))])
    (and (vector? v)
         (= 6 (vector-length v))
         (in-range? 0 4294967086)
         (in-range? 1 4294967086)
         (in-range? 2 4294967086)
         (in-range? 3 4294944442)
         (in-range? 4 4294944442)
         (in-range? 5 4294944442)
         (or (nonzero? 0) (nonzero? 1) (nonzero? 2))
         (or (nonzero? 3) (nonzero? 4) (nonzero? 5)))))

(define/who (vector->pseudo-random-generator orig-v)
  (let ([iv (and (vector? orig-v)
                 (= 6 (vector-length orig-v))
                 (vector->immutable-vector orig-v))])
    (check who pseudo-random-generator-vector? iv)
    (let ([r (lambda (i) (exact->inexact (vector-ref iv i)))])
      (new-pseudo-random-generator (r 0)
                                   (r 1)
                                   (r 2)
                                   (r 3)
                                   (r 4)
                                   (r 5)))))

(define/who (vector->pseudo-random-generator! s orig-v)
  (check who pseudo-random-generator? s)
  (let ([iv (and (vector? orig-v)
                 (= 6 (vector-length orig-v))
                 (vector->immutable-vector orig-v))])
    (unless (pseudo-random-generator-vector? iv)
      (raise-argument-error 'vector->pseudo-random-generator! "pseudo-random-generator-vector?" orig-v))
    (let ([r (lambda (i) (exact->inexact (vector-ref iv i)))])
      (pseudo-random-generator-x10-set! s (r 0))
      (pseudo-random-generator-x11-set! s (r 1))
      (pseudo-random-generator-x12-set! s (r 2))
      (pseudo-random-generator-x20-set! s (r 3))
      (pseudo-random-generator-x21-set! s (r 4))
      (pseudo-random-generator-x22-set! s (r 5)))))

(define (pseudo-random-generator-integer! s n)
  ;; generate result in {0..n-1} using the rejection method
  (let* ([n (exact->inexact n)]
         [q (fltruncate (fl/ m1 n))]
         [qn (fl* q n)]
         [x (let loop ()
              (let ([x (mrg32k3a s)])
                (if (fl>= x qn)
                    (loop)
                    x)))]
         [xq (fl/ x q)])
    (inexact->exact (flfloor xq))))

(define (pseudo-random-generator-real! s)
  (fl* (fl+ (mrg32k3a s) 1.0) norm))

;; ----------------------------------------

(define/who current-pseudo-random-generator
  (make-parameter (make-pseudo-random-generator)
                  (lambda (v)
                    (check who pseudo-random-generator? v)
                    v)))

(define/who random
  (case-lambda
   [() (pseudo-random-generator-real! (|#%app| current-pseudo-random-generator))]
   [(n)
    (cond
     [(pseudo-random-generator? n)
      (pseudo-random-generator-real! n)]
     [else
      (check who
             :test (and (integer? n)
                        (exact? n)
                        (<= 1 n 4294967087))
             :contract "(or/c (integer-in 1 4294967087) pseudo-random-generator?)"
             n)
      (pseudo-random-generator-integer! (|#%app| current-pseudo-random-generator) n)])]
   [(n prg)
    (check who
           :test (and (integer? n)
                      (exact? n)
                      (<= 1 n 4294967087))
           :contract "(or/c (integer-in 1 4294967087) pseudo-random-generator?)"
           n)
    (check who pseudo-random-generator? prg)
    (pseudo-random-generator-integer! prg n)]))

(define/who (random-seed k)
  (check who
         :test (and (exact-nonnegative-integer? k)
                    (<= k (sub1 (expt 2 31))))
         :contract "(integer-in 0 (sub1 (expt 2 31)))"
         k)
  (pseudo-random-generator-seed! (|#%app| current-pseudo-random-generator) k))
