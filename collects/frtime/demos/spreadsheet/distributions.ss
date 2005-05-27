(module distributions mzscheme
  
  (require (lib "math.ss")
           )
  
  (define RANDOM-GRAIN (sub1 (expt 2 31)))  
  
  ;;  A distribution has a:
  ;;    bool continuous?          : Is this distribution continuous? (#t if yes, #f if discrete)
  ;;    (num -> num) density-f    : Probability density function f(y)= p(y)
  ;;    num mean                  : mean of distribution
  ;;    num variance              : variance of distribution
  ;;                 I need to think about this next one:
  ;;    (num -> num)? mgf         : moment generating function.  Forgive the acronym.
  (define-struct distribution (continuous? 
                               d-func 
                               mean 
                               variance 
                               rand
                               ))
  
  (define (print-distribution dist)
    (print (format "continuous?: ~a, mean: ~a, variance: ~a"
                   (distribution-continuous? dist)
                   (distribution-mean dist)
                   (distribution-variance dist))))
  
  (define (ln n)
    (/ (log n)
       (log e)))
  
  
  (define fac
    (lambda (n)
      (if (zero? n)
          1
          (* n (fac (- n 1))))))
  
  (define choose
    (lambda (a b)
      (/ (fac a)
         (* (fac b)
            (fac (- a b))))))
  
  (define myround
    (lambda (n)
      (- n
         (- n (round n)))))
  
  
  
  ;; Ultimately, mgf might be a struct, with make-uniform-mgf, make-normal-mgf, etc.
  ;; be special constructors for that particular struct.  For now, I'll leave
  ;; this as the actual formula
  (define (make-uniform-mgf theta1 theta2)
    (lambda (t)
      (/ (- (exp (* t theta2))
            (exp (* t theta1)))
         (* t
            (- theta2 theta1)))))
  (define (make-normal-mgf mu sigma)
    (lambda (t)
      (exp (+ (* mu t)
              (/ (* (sqr t) (sqr sigma))
                 2)))))
  
  ;;Do I need these? Dammit!
  (define (make-exponential-mgf bete)
    'dummy)
  (define (make-binomial-mgf n p)
    'dummy)
  
  
  
  
  
  (define (make-uniform-distribution theta1 theta2)
    (make-distribution #t
                       (lambda (y)
                         ;;is it appropriate to limit the domains of the 
                         ;; probability functions with conditionals like
                         ;; this?
                         (if (or (< y theta1)
                                 (< theta2 y))
                             0
                             (/ (- theta2 theta1))))
                       (/ (+ theta1 theta2)
                          2)
                       (/ (sqr (- theta2 theta1))
                          12)
                       (lambda ()
                         (uniform-random theta1 theta2))
                       ))
  
  (define (make-normal-distribution mu sigma)
    (make-distribution #t
                       (lambda (y)
                         (* (/ (* sigma (sqrt (* 2 pi))))
                            (exp (- (* (/ (* 2 (sqr sigma)))
                                       (sqr (- y mu)))))))
                       mu
                       (sqr sigma)
                       (lambda ()
                         (normal-random mu sigma))
                       ))
  
  (define (make-exponential-distribution beta)
    ;;Can I asser here that beta > 0??
    (make-distribution #t
                       (lambda (y)
                         (if (< 0 y)
                             (* (/ beta)
                                (exp (/ (- y)
                                        beta)))
                             0))
                       beta
                       (sqr beta)
                       (lambda ()
                         (exponential-random beta))
                       ))
  
  ;; FUCK! The Gamma function is a big integral, equal to factorial if the
  ;; argument is an integer, but otherwise...crap on a stick!
  
  ; (define (make-gamma-distribution alpha beta)
  ;  (make-distribution #t
  ;                    (lambda (y)
  
  
  (define (make-binomial-distribution n p)
    (make-distribution #f
                       (lambda (y)
                         ;;should assert whole-number-ness here or something
                         (* (choose n y)
                            (expt p y)
                            (expt (- 1 p)
                                  (- n y))))
                       (* n p)
                       (* n p (- 1 p))
                       (lambda ()
                         (binomial-random n p))
                       ))
;  
;  (define (make-gamma-distribution alpha beta)
;    (make-distribution #f
;                       (lambda (y)
;                         (if (<= y 0)
;                             0
;                             (* (expt y (sub1 alpha))
;                                (exp (/ (- y) beta))
;                                (/ (* (exp (gammln alpha))
;                                      (expt beta alpha))))))
;                       (* alpha beta)
;                       (* alpha beta beta)
;                       (lambda () 
;                         (gamma-random alpha beta))))
  
  
  
  
  
  (define (uniform-random theta1 theta2)
    (+ theta1 (* (/ (- theta2 theta1) (sub1 RANDOM-GRAIN)) (random RANDOM-GRAIN))))
  
  (define (open-uniform-random theta1 theta2)
    (+ theta1 (* (/ (- theta2 theta1) RANDOM-GRAIN) (add1 (random (sub1 RANDOM-GRAIN))))))
  
  
  
  
  ;; Although it might be the root of all evil, and make no sense in this context,
  ;; I'm going to use the Marsaglia Polar Method here with the flip-flopping
  ;; indicator switch because it's HOT. 
  (define r 0)
  (define ind 1)
  
  (define (ranf)
    (open-uniform-random 0 1))
  
  (define (standard-normal-random-guts)
    (let* ([x1 (- (* 2 (ranf)) 1)]
           [x2 (- (* 2 (ranf)) 1)]
           [w  (+ (* x1 x1) (* x2 x2))])
      (if (> w 1)
          (standard-normal-random-guts)
          (let ([w2 (sqrt (/ (* (ln w) -2) w))])
            (set! r (* x2 w2))
            (* x1 w2)))))
  
  (define (standard-normal-random)
    (set! ind (- ind))
    (if (> ind 0)
        (standard-normal-random-guts)
        r))
  
  (define (normal-random mu sigma)
    (+ (* sigma (standard-normal-random)) mu))
  
  (define (exponential-random beta)
    (let ([rand (/ (add1 (random (sub1 RANDOM-GRAIN))) RANDOM-GRAIN)])
      (* (- (ln rand)) beta)))
  
  
  
  ;(define (testrand args make-dist rand target n m)
  ;  (/ (let loopa ([t 0])
  ;       (if (> t target)
  ;           0
  ;           (+ (let loop ([i m])
  ;                (if (> 0 i)
  ;                    0
  ;                    (+ (if (= (apply rand
  ;                                     (cons n
  ;                                           args))
  ;                              t)
  ;                           1
  ;                           0)
  ;                       (loop (sub1 i)))))
  ;(loopa (add1 t)))))
  ;((distribution-density-f (apply make-dist args)) target)
  ;))
  
  
  ;;;pronlems if alpha is a non-integer
  ;(define (standard-gamma-random-guts alpha)
  ;  (let ([v1 (ranf RANDOM-GRAIN)]
  ;        [v2 (- (* 2 (ranf RANDOM-GRAIN)) 1)])
  ;    (if (> (+ (* v1 v1) (* v2 v2)) 1)
  ;        (standard-gamma-random-guts alpha)
  ;        (let*
  ;            ([y (/ v2 v1)]
  ;             [am (- alpha 1)]
  ;             [s (sqrt (+ (* 2 am) 1))]
  ;             [x (+ (* s y) am)])
  ;          (if (<= x 0)
  ;              (standard-gamma-random-guts alpha)
  ;              (if (> (ranf)
  ;                     (* (+ 1 (* y y))
  ;                        (exp (- (* am
  ;                                   (ln (/ x am)))
  ;                                (* s y)))))
  ;                  (standard-gamma-random-guts alpha)
  ;                  x))))))
  ;
  ;;;problems if alpha is a non-integer, I think
  ;(define (standard-gamma-random alpha)
  ;  (cond [(< alpha 1) (error "Bad alpha value for standard-gamma-random")]
  ;        [(< alpha 6) (let loop ([x 1] [i 0])
  ;                       (if (> i alpha)
  ;                           (- (ln x))
  ;                           (loop (* x (ranf)) (add1 i))))] ; the random number here
  ;        ; might have to be over the
  ;        ; whole number line.
  ;        [else (standard-gamma-random-guts alpha)]))
  ;
  ;
  ;;;problems if alpha is a non-integer
  ;(define (gamma-random alpha beta)
  ;  (* beta (standard-gamma-random alpha)))
  
  ;; PROBLEMS with GAMMA RANDOM:
  ;; (1) The random gamma number generator should only take positive integers as
  ;;     argument according to the spec in Numerical Recipes.  However, according to
  ;;     Mathematical Statistics and Applications, the alpha parameter of a gamma function
  ;;     may not always be an integer.
  ;; (2) Using the code for the random number generator above seems to produce data with mean of
  ;;     ((alpha + 1) * beta), and variance of ((alpha + 1) * beta ^ 2).  I don't know what
  ;;     is wrong with the code, but I hesitated to just throwing in something subtracting 1 from
  ;;     alpha before running calculations because that seems unsound.
  
  
  (define (gammln xx)
    ;Returns the value ln[(xx)] for xx > 0.
    (let* ([ser 1.000000000190015]
           [cof1 76.18009172947146]
           [cof2 -86.50532032941677]
           [cof3 24.01409824083091]
           [cof4 -1.231739572450155]
           [cof5 0.001208650973866179]
           [cof6 -0.000005395239384953]
           [x xx]
           [y xx]
           [tmp (- (+ x 5.5)
                   (* (+ x 0.5)
                      (ln (+ x 5.5))))]
           [ser2 (+ ser
                    (/ cof1 (+ y 1))
                    (/ cof2 (+ y 2))
                    (/ cof3 (+ y 3))
                    (/ cof4 (+ y 4))
                    (/ cof5 (+ y 5))
                    (/ cof6 (+ y 6)))])
      (+ (- tmp)
         (ln (* 2.5066282746310005
                (/ ser2
                   x))))))
  
  
  
  ;;more sketchy mutative stuff recommended by Numerical Recipes to optimize this craziness.
  (define pg 0)
  (define poldm -1.0)
  (define psq -1)
  (define palxm -1)
  
  
  (define (poisson-random xm)
    (if (> 12 xm)
        (begin
          (unless (= xm poldm)
            (set! poldm xm)
            (set! pg (exp (- xm))))
          (let loop ([em 0] [t (ranf)])
            (if (> t pg)
                (loop (add1 em) (* t (ranf)))
                em)))
        (begin
          (unless (= xm poldm)
            (set! poldm xm)
            (set! psq (sqrt (* 2.0 xm)))
            (set! palxm (ln xm))
            (set! pg (- (* xm palxm) (gammln (+ xm 1)))))
          (poisson-random-guts xm))))
  
  (define (poisson-random-guts xm)
    (let loop ([y (tan (* (ranf) pi))])
      (let ([em (+ (* psq y)
                   xm)])
        (if (< em 0)
            (loop (tan (* (ranf) pi)))
            (let* ([fem (floor em)]
                   [t (* 0.9
                         (+ 1.0 (* y y))
                         (exp (- (* fem palxm)
                                 (gammln (+ fem 1))
                                 pg)))])
              (if (> (ranf) t)
                  (loop (tan (* pi (ranf))))
                  fem))))))
  
  
  
  
  ;;These are mutating variables that are meant to store computed values to speed up sampling
  (define bnold -1)
  (define bpold -1)
  (define boldg -1)
  
  (define (binomial-random n pp)
    ;Returns as a floating-point number an integer value that is a random deviate drawn from
    ;a binomial distribution of n trials each of probability pp, using ran1(idum) as a source of
    ;uniform random deviates.  
    (let* ([p (if (<= pp 0.5)
                  pp
                  (- 1 pp))]
           [am (* n p)])
      (if (< n 25)
          (let loop ([bnl 0] [j 1])
            (if (<= n j)
                (if (= p pp)
                    bnl
                    (- n bnl))
                (if (< (ranf) p)
                    (loop (add1 bnl) (add1 j))
                    (loop bnl (add1 j)))))
          (if (< am 1)
              (let ([g (exp (- am))])
                (let loop ([t 1] [j 0])
                  (if (not (<= j n))
                      (if (= p pp)
                          n
                          0)
                      (let ([t2 (* t (ranf))])
                        (if (< t g)
                            (if (< t g)
                                (if (= p pp)
                                    j
                                    (- n j))
                                (loop t2 (add1 j))))))))
              
              (begin
                (unless (= n bnold)
                  (set! boldg (gammln (+ n 1)))
                  (set! bnold n))
                (let* ([en n]
                       [pc (- 1 p)]
                       [plog (ln p)]              ; If sampling is too slow
                       [pclog (ln pc)]            ; I can gain speed by storing some of these
                       [sq (sqrt (* 2 am pc))]    ; in mutating, out of function variables
                       )
                  (let loop ([angle (* pi (ranf))])
                    (let* ([y (tan angle)]
                           [em (+ (* sq y) am)])
                      (if (or (< em 0) (>= em (+ en 1)))
                          (loop (* pi (ranf)))
                          (let ([fem (floor em)]
                                [t (* 1.2 
                                      sq  
                                      (+ (* y y) 1)
                                      (exp (+ boldg
                                              (- (gammln (+ em 1)))
                                              (- (gammln (+ en (- em) 1)))
                                              (* em plog)
                                              (* (- en em) pclog))))])
                            (if (> (ranf) t)
                                (loop (* pi (ranf)))
                                (if (= p pp)
                                    em
                                    (- n em)))))))))))))
  
  
  
  (provide (all-defined))
  )

;(let loop ([x (round (binomial-random 100000 10 .4))])
;  (loop (round (binomial-random 100000 10 .4))))




;(define (accumulate-mean sampler-event) 
;  (collect-e sampler-event
;             (cons 777777 0)  ;;;The number 7777777 never gets used
;             (lambda (datum mean-count)
;               (let ([new-count (add1 (cdr mean-count))])
;                 (cons (+ (/ datum new-count)
;                          (* (/ (cdr mean-count)
;                                new-count)
;                             (car mean-count)))
;                       new-count)))))

;(accumulate-mean ((changes milliseconds) . ==> . (lambda (_) (normal-random 1000000 5 30))))
