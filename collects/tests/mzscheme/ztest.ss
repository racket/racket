;; rudimentary test harness for complex math routines in 
;; zmath.ss 

(require (lib "zmath.ss"))

(define ztest
  (lambda (z)
    (printf "z = ~a~n" z)
    (printf " zabs(z) = ~a~n" (zabs z))
    (printf " zlog(z) = ~a~n" (zlog z))
    (printf " zexp(z) = ~a~n" (zexp z))
    (printf " zsqrt(z) = ~a~n" (zsqrt z))
    (printf " zsin(z) = ~a~n" (zsin z))
    (printf " zcos(z) = ~a~n" (zcos z))
    (printf " ztan(z) = ~a~n" (ztan z))
    (printf " zasin(z) = ~a~n" (zasin z))
    (printf " zacos(z) = ~a~n" (zacos z))
    (printf " zatan(z) = ~a~n" (zatan z))))

(ztest 0.5)
