#!r6rs

(library (tests r6rs arithmetic flonums)
  (export run-arithmetic-flonums-tests)
  (import (rnrs)
          (tests r6rs test))

  (define (try-flonums proc)
    (proc 0.0)
    (proc 1.0)
    (proc -1.0)
    (proc +inf.0)
    (proc -inf.0)
    (proc +nan.0))

  (define-syntax try-bad-divs
    (syntax-rules ()
      [(_ op)
       'nothing
       ;; The spec is unclear whether the following
       ;; are allowed to raise exceptions.
       #;
       (begin
         (test/unspec (op 1.0 0.0))
         (test/unspec (op +inf.0 1.0))
         (test/unspec (op -inf.0 1.0))
         (test/unspec (op +nan.0 1.0)))]))

  (define (run-arithmetic-flonums-tests)

    (test (fl=? +inf.0 +inf.0)            #t)
    (test (fl=? -inf.0 +inf.0)            #f)
    (test (fl=? -inf.0 -inf.0)            #t)
    (test (fl=? 0.0 -0.0)                 #t)
    (test (fl<? 0.0 -0.0)                 #f)
    (try-flonums
     (lambda (fl)
       (test (fl=? +nan.0 fl)                #f)
       (test (fl<? +nan.0 fl)                #f)))
    
    (test (flnegative? -0.0)   #f)
    (test (flfinite? +inf.0)   #f)
    (test (flfinite? 5.0)      #t)
    (test (flinfinite? 5.0)    #f)
    (test (flinfinite? +inf.0) #t)
    (test (flinfinite? -inf.0) #t)
    (test (flinfinite? +nan.0) #f)
    
    (test (fl+ +inf.0 -inf.0)       +nan.0)
    (try-flonums 
     (lambda (fl)
       (test (fl+ +nan.0 fl)           +nan.0)
       (test (fl* +nan.0 fl)           +nan.0)))

    (test (fl- +inf.0 +inf.0)       +nan.0)
    
    (test (fl/ 1.0 0.0)  +inf.0)
    (test (fl/ -1.0 0.0) -inf.0)
    (test (fl/ 0.0 0.0)  +nan.0)

    (test (flnumerator +inf.0)            +inf.0)
    (test (flnumerator -inf.0)            -inf.0)
    (test (fldenominator +inf.0)          1.0)
    (test (fldenominator -inf.0)          1.0)
    
    ;; (test (flnumerator 0.75)              3.0) ; probably
    ;; (test (fldenominator 0.75)            4.0) ; probably

    (test (flnumerator -0.0)             -0.0)
    
    (test (flfloor +inf.0)                        +inf.0)
    (test (flceiling -inf.0)                      -inf.0)
    (test (fltruncate +nan.0)                     +nan.0)
    
    (test (flexp +inf.0)                +inf.0)
    (test (flexp -inf.0)                0.0)
    (test (fllog +inf.0)                +inf.0)
    (test (fllog 0.0)                   -inf.0)
    (test/unspec (fllog -0.0)) ; if -0.0 is distinguished
    (test (fllog -inf.0)                +nan.0)
    (test/approx (flatan -inf.0)               -1.5707963267948965)
    (test/approx (flatan +inf.0)               1.5707963267948965)

    (test (flsqrt +inf.0)                +inf.0)
    (test (flsqrt -0.0)                  -0.0)

    ;; ----------------------------------------
    
    (let ([test-ordered
           (lambda (a b c)
             (test (fl=? a a) #t)
             (test (fl=? b b) #t)
             (test (fl=? c c) #t)

             (test (fl=? a b) #f)
             (test (fl=? b a) #f)
             (test (fl=? b c) #f)
             (test (fl=? c b) #f)

             (test (fl=? a c b) #f)
             (test (fl=? a a b) #f)
             (test (fl=? a b b) #f)

             (let ([test-lt
                    (lambda (fl<? fl<=? a b c)
                      (test (fl<? a b) #t)
                      (test (fl<? b c) #t)
                      (test (fl<? a c) #t)
                      (test (fl<? a b c) #t)

                      (test (fl<? b a) #f)
                      (test (fl<? c b) #f)
                      (test (fl<? a c b) #f)
                      
                      (test (fl<=? a a) #t)
                      (test (fl<=? a b) #t)
                      (test (fl<=? a c) #t)
                      (test (fl<=? b b) #t)
                      (test (fl<=? b c) #t)
                      (test (fl<=? c c) #t)
                      (test (fl<=? a c c) #t)
                      (test (fl<=? a b c) #t)
                      (test (fl<=? b b c) #t)

                      (test (fl<=? c a) #f)
                      (test (fl<=? b a) #f)
                      (test (fl<=? a c b) #f)
                      (test (fl<=? b c a) #f))])
               (test-lt fl<? fl<=? a b c)
               (test-lt fl>? fl>=? c b a))
             
             ;; Since b is between a and c, we can add or subtract 1:
             (test (fl=? (+ b 1) (+ b 1)) #t)
             (test (fl<? b (+ b 1)) #t)
             (test (fl<=? b (+ b 1)) #t)
             (test (fl>? b (+ b 1)) #f)
             (test (fl>=? b (+ b 1)) #f)
             (test (fl=? (- b 1) (- b 1)) #t)
             (test (fl<? b (- b 1)) #f)
             (test (fl<=? b (- b 1)) #f)
             (test (fl>? b (- b 1)) #t)
             (test (fl>=? b (- b 1)) #t)
             
             ;; Check min & max while we have ordered values:
             (test (flmin a b) a)
             (test (flmin b c) b)
             (test (flmin a c) a)
             (test (flmin b a c) a)
             (test (flmax a b) b)
             (test (flmax b c) c)
             (test (flmax a c) c)
             (test (flmax b c a) c))])
      (test-ordered 1.0 2.0 3.0)
      (test-ordered -1.0 0.0 1.0)
      (test-ordered -1.0e5 0.0 1.0e-5))
    
    (test (flinteger? 4.0) #t)
    (test (flinteger? 4.1) #f)
    (test (flzero? 4.1) #f)
    (test (flzero? 0.0) #t)
    (test (flzero? -4.1) #f)
    (test (flpositive? 4.1) #t)
    (test (flpositive? 0.0) #f)
    (test (flpositive? -4.1) #f)
    (test (flnegative? 4.1) #f)
    (test (flnegative? 0.0) #f)
    (test (flnegative? -4.1) #t)
    
    (test (fleven? 2.0) #t)
    (test (fleven? -2.0) #t)
    (test (fleven? 0.0) #t)
    (test (fleven? -0.0) #t)
    (test (fleven? 3.0) #f)
    (test (fleven? -3.0) #f)

    (test (flodd? 3.0) #t)
    (test (flodd? -3.0) #t)
    (test (flodd? 0.0) #f)
    (test (flodd? -0.0) #f)
    (test (flodd? 2.0) #f)
    (test (flodd? -2.0) #f)

    (test (flnan? +inf.0) #f)
    (test (flnan? 0.0) #f)
    (test (flnan? -0.0) #f)
    (test (flnan? -inf.0) #f)
    (test (flnan? +nan.0) #t)

    (test (fl+) +0.0)
    (test (fl+ 2.3) 2.3)
    (test/approx (fl+ 2.3 3.1) 5.4)
    (test/approx (fl+ 2.3 3.1 -1.1) 4.3)
    (test/approx (fl+ 2.3e2 3.1e1) 261)

    (test (fl*) 1.0)
    (test (fl* 2.3) 2.3)
    (test/approx (fl* 2.3 2.1) 4.83)
    (test/approx (fl* 2.3 2.1 1.1) 5.313)
    (test/approx (fl* 2.3 2.1 -1.1) -5.313)

    (test/approx (fl- 0.0 2.3) -2.3)
    (test/approx (fl- 0.0 2.3 -1.1) -1.2)
    (test/approx (fl- 2.3) -2.3)
    (test (fl- 0.0) -0.0)

    (test/approx (fl/ 5.0 2.0) 2.5)
    (test/approx (fl/ 5.0 2.0 2.5) 1.0)
    (test/approx (fl/ 2.0) 0.5)
    (test/approx (fl/ -2.0) -0.5)

    (test (flabs 0.0) 0.0)
    (test/approx (flabs 1.0) 1.0)
    (test/approx (flabs -1.0) 1.0)
    (test/approx (flabs -0.1) 0.1)

    (test (fldiv 123.0 10.0) 12.0)
    (test (flmod 123.0 10.0) 3.0)
    (test (fldiv 123.0 -10.0) -12.0)
    (test (flmod 123.0 -10.0) 3.0)
    (test (fldiv -123.0 10.0) -13.0)
    (test (flmod -123.0 10.0) 7.0)
    (test (fldiv -123.0 -10.0) 13.0)
    (test (flmod -123.0 -10.0) 7.0)

    (test/values (fldiv-and-mod -123.0 10.0) -13.0 7.0)

    (try-bad-divs fldiv)
    (try-bad-divs flmod)
    (try-bad-divs fldiv-and-mod)

    (test (fldiv0 123.0 10.0) 12.0)
    (test (flmod0 123.0 10.0) 3.0)
    (test (fldiv0 123.0 -10.0) -12.0)
    (test (flmod0 123.0 -10.0) 3.0)
    (test (fldiv0 -123.0 10.0) -12.0)
    (test (flmod0 -123.0 10.0) -3.0)
    (test (fldiv0 -123.0 -10.0) 12.0)
    (test (flmod0 -123.0 -10.0) -3.0)

    (test/values (fldiv0-and-mod0 -123.0 10.0) -12.0 -3.0)

    (try-bad-divs fldiv0)
    (try-bad-divs flmod0)
    (try-bad-divs fldiv0-and-mod0)

    (test (flfloor 3.1) 3.0)
    (test (flfloor -3.1) -4.0)
    (test (flceiling 3.1) 4.0)
    (test (flceiling -3.1) -3.0)
    (test (fltruncate 3.1) 3.0)
    (test (fltruncate -3.1) -3.0)
    (test (flround 3.1) 3.0)
    (test (flround -3.1) -3.0)
    (test (flround 3.8) 4.0)
    (test (flround -3.8) -4.0)
    ;; (test (flround 3.5) 4.0) ; probably
    ;; (test (flround -3.5) -4.0) ; probably
    ;; (test (flround 2.5) 2.0) ; probably
    ;; (test (flround -2.5) -2.0) ; probably

    (test/approx (flexp 2.0) 7.389)
    (test/approx (fllog 7.389) 2.0)
    (test/approx (fllog 1024.0 2.0) 10.0)

    (test/approx (flsin 0.0) 0.0)
    (test/approx (flsin 1.570796) 1.0)
    (test/approx (flcos 1.570796) 0.0)
    (test/approx (flcos 0.0) 1.0)
    (test/approx (flatan 0.0 1.0) 0.0)
    (test/approx (flatan 0.0 -1.0) (* 1.570796 2.0))
    (test/approx (flatan 1.0 0.0) 1.570796)
    (test/approx (flatan -1.0 0.0) -1.570796)
    (test/approx (flatan 1.0 1.0) (/ 1.570796 2.0))
    (test/approx (flatan -1.0 1.0) (/ -1.570796 2.0))
    (test/approx (flatan 0.0) 0.0)
    (test/approx (flatan 1.0) (/ 1.570796 2.0))
    (test/approx (flatan 10.0) 1.47113)
    (test/approx (flatan 0.1) 0.0996687)

    (test/approx (flsqrt 4.0) 2.0)
    (test/approx (flsqrt 5.0) 2.23607)

    (test/approx (flexpt 2.0 3.0) 8.0)
    (test/approx (flexpt 10.0 3.0) 1000.0)

    (test (no-infinities-violation? (make-no-infinities-violation)) #t)
    (test ((record-predicate (record-type-descriptor &no-infinities)) (make-no-infinities-violation)) #t)
    (test (no-nans-violation? (make-no-nans-violation)) #t)
    (test ((record-predicate (record-type-descriptor &no-nans)) (make-no-nans-violation)) #t)

    (test/approx (fixnum->flonum 2) 2.0)

    ;;
    ))

