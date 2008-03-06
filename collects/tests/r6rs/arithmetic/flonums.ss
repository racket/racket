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
    
    ; (test (flnumerator 0.75)              3.0) ; probably
    ; (test (fldenominator 0.75)            4.0) ; probably

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
    
    ;;
    ))

