#|

;; This Scheme implentation forms reasonably well, with suitable unboxing
;; of floating-point calculations. But C compilers are still a little
;; better at floating-point (e.g., using instructions that work on
;; multiple values at once), so we still use a C implementation in
;; the Chez Scheme kernel for now.

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

(define-record pseudo-random-generator
  ((mutable double x10) (mutable double x11) (mutable double x12)
   (mutable double x20) (mutable double x21) (mutable double x22))
  ()
  ((constructor new-pseudo-random-generator)))

;; We can use
;;   (fixnum->flonum (flonum->fixnum x))
;; as a fast fltruncate below because x is always in fixnum range:
;; each element of a random generator is between 0 and 4294967086, so
;; (fl/ (fl- (fl* a12 x_i) (fl* a13n x_j)) m1) is between 0 and
;; 1403580, where the upper bound is x_i = 4294967086 and x_j = 0.

(define-syntax-rule (mrg32k3a s-expr) ;; -> flonum in {0..m1-1}
  ;; component 1
  (let* ([s s-expr]
         [x10 (fl- (fl* a12 (pseudo-random-generator-x11 s))
                   (fl* a13n (pseudo-random-generator-x12 s)))]
         [k10 (fixnum->flonum (flonum->fixnum (fl/ x10 m1)))] ; fast fltruncate (see above)
         [x10 (fl- x10 (fl* k10 m1))]
         [x10 (if (fl< x10 0.0)
                  (fl+ x10 m1)
                  x10)])
    (set-pseudo-random-generator-x12! s (pseudo-random-generator-x11 s))
    (set-pseudo-random-generator-x11! s (pseudo-random-generator-x10 s))
    (set-pseudo-random-generator-x10! s x10)
    
    ;; component 2
    (let* ([x20 (fl- (fl* a21 (pseudo-random-generator-x20 s))
                     (fl* a23n (pseudo-random-generator-x22 s)))]
           [k20 (fixnum->flonum (flonum->fixnum (fl/ x20 m2)))]
           [x20 (fl- x20 (fl* k20 m2))]
           [x20 (if (fl< x20 0.0)
                    (fl+ x20 m2)
                    x20)])
      (set-pseudo-random-generator-x22! s (pseudo-random-generator-x21 s))
      (set-pseudo-random-generator-x21! s (pseudo-random-generator-x20 s))
      (set-pseudo-random-generator-x20! s x20)

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
  (set-pseudo-random-generator-x10! s 1062452522.0)
  (set-pseudo-random-generator-x11! s 2961816100.0)
  (set-pseudo-random-generator-x12! s 342112271.0)
  (set-pseudo-random-generator-x20! s 2854655037.0)
  (set-pseudo-random-generator-x21! s 3321940838.0)
  (set-pseudo-random-generator-x22! s 3542344109.0)
  (srand-half! s (bitwise-and x #xFFFF))
  (srand-half! s (bitwise-and (bitwise-arithmetic-shift-right x 16) #xFFFF)))

(define (srand-half! s x)
  (let* ([u32+ (lambda (a b)
                 (bitwise-and (+ a b) #xFFFFFFFF))]
         [x (random-n! x
                       (- Im1 1)
                       (lambda (z)
                         (set-pseudo-random-generator-x10!
                          s
                          (exact->inexact
                           (+ 1 (modulo
                                 (u32+ (inexact->exact (pseudo-random-generator-x10 s))
                                       z)
                                 (- Im1 1)))))))]
         [x (random-n! x
                       Im1
                       (lambda (z)
                         (set-pseudo-random-generator-x11!
                          s
                          (exact->inexact
                           (modulo
                            (u32+ (inexact->exact (pseudo-random-generator-x11 s))
                                  z)
                            Im1)))))]
         [x (random-n! x
                       Im1
                       (lambda (z)
                         (set-pseudo-random-generator-x12!
                          s
                          (exact->inexact
                           (modulo
                            (u32+ (inexact->exact (pseudo-random-generator-x12 s))
                                  z)
                            Im1)))))]
         [x (random-n! x
                       (- Im2 1)
                       (lambda (z)
                         (set-pseudo-random-generator-x20!
                          s
                          (exact->inexact
                           (+ 1 (modulo
                                 (u32+ (inexact->exact (pseudo-random-generator-x20 s))
                                       z)
                                 (- Im2 1)))))))]
         [x (random-n! x
                       Im2
                       (lambda (z)
                         (set-pseudo-random-generator-x21!
                          s
                          (exact->inexact
                           (modulo
                            (u32+ (inexact->exact (pseudo-random-generator-x21 s))
                                  z)
                            Im2)))))]
         [x (random-n! x
                       Im2
                       (lambda (z)
                         (set-pseudo-random-generator-x22!
                          s
                          (exact->inexact
                           (modulo
                            (u32+ (inexact->exact (pseudo-random-generator-x22 s))
                                  z)
                            Im2)))))])
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
      (set-pseudo-random-generator-x10! s (r 0))
      (set-pseudo-random-generator-x11! s (r 1))
      (set-pseudo-random-generator-x12! s (r 2))
      (set-pseudo-random-generator-x20! s (r 3))
      (set-pseudo-random-generator-x21! s (r 4))
      (set-pseudo-random-generator-x22! s (r 5)))))

(define (pseudo-random-generator-integer! s k)
  ;; generate result in {0..n-1} using the rejection method
  (let* ([n (real->flonum k)]
         [q (fltruncate (fl/ m1 n))]
         [qn (fl* q n)])
    (let loop ()
      (let ([x (mrg32k3a s)])
        (if (fl>= x qn)
            (loop)
            (let ([xq (fl/ x q)])
              (if (fixnum? k) ; => result is fixnum
                  (flonum->fixnum xq)
                  (inexact->exact (flfloor xq)))))))))

(define (pseudo-random-generator-real! s)
  (fl* (fl+ (mrg32k3a s) 1.0) norm))

;; ----------------------------------------

(define/who current-pseudo-random-generator
  (make-parameter (make-pseudo-random-generator)
                  (lambda (v)
                    (check who pseudo-random-generator? v)
                    v)
                  'current-pseudo-random-generator))

(define/who random
  (case-lambda
   [() (pseudo-random-generator-real! (current-pseudo-random-generator))]
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
      (pseudo-random-generator-integer! (current-pseudo-random-generator) n)])]
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
  (pseudo-random-generator-seed! (current-pseudo-random-generator) k))

|#

(define/who current-pseudo-random-generator
  (make-parameter (make-pseudo-random-generator)
                  (lambda (v)
                    (check who pseudo-random-generator? v)
                    v)
                  'current-pseudo-random-generator))

(define/who random
  (case-lambda
   [() (pseudo-random-generator-next! (current-pseudo-random-generator))]
   [(n)
    (cond
     [(pseudo-random-generator? n)
      (pseudo-random-generator-next! n)]
     [else
      (check who
             :test (and (exact-integer? n)
                        (<= 1 n 4294967087))
             :contract "(or/c (integer-in 1 4294967087) pseudo-random-generator?)"
             n)
      (pseudo-random-generator-next! (current-pseudo-random-generator) n)])]
   [(n prg)
    (check who
           :test (and (exact-integer? n)
                      (<= 1 n 4294967087))
           :contract "(or/c (integer-in 1 4294967087) pseudo-random-generator?)"
           n)
    (check who pseudo-random-generator? prg)
    (pseudo-random-generator-next! prg n)]))

(define/who (random-seed k)
  (check who
         :test (and (exact-nonnegative-integer? k)
                    (<= k (sub1 (expt 2 31))))
         :contract "(integer-in 0 (sub1 (expt 2 31)))"
         k)
  (pseudo-random-generator-seed! (current-pseudo-random-generator) k))

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

(define (vector->pseudo-random-generator v)
  (#%vector->pseudo-random-generator (unwrap-pseudo-random-generator-vector v)))

(define (vector->pseudo-random-generator! s v)
  (#%vector->pseudo-random-generator! s (if (pseudo-random-generator? s)
                                            (unwrap-pseudo-random-generator-vector v)
                                            v)))

;; convert a vector for chaperoned form:
(define (unwrap-pseudo-random-generator-vector v)
  (cond
    [(and (not (#%vector? v))
          (vector? v)
          (= 6 (vector-length v)))
     (vector (vector-ref v 0)
             (vector-ref v 1)
             (vector-ref v 2)
             (vector-ref v 3)
             (vector-ref v 4)
             (vector-ref v 5))]
    [else v]))
