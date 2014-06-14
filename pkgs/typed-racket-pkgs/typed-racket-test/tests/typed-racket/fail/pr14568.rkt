#;
(exn-pred 1)
#lang typed/racket

(: rational->fle (-> Exact-Rational (Values Flonum Flonum)))
(define (rational->fle r)
  (let-values ([(s r)  (values (sgn r) (abs r))])
    (define e
      (- (integer-length (numerator r))
         (integer-length (denominator r))))
    (let loop ([r  (/ r (expt 2 e))] [e e])
      (cond [(< r 1)  (loop (* r 2) (- e 1))]
            [(>= r 2)  (loop (/ r 2) (+ e 1))]
            [else
             (ann r Positive-Byte) ; should not typecheck
             (values (* (real->double-flonum s)
                        (real->double-flonum r))  ; optimizer changes to ->fl
                     (real->double-flonum e))]))))

(rational->fle 1/7)  ; optimization causes contract violation
