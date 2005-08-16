(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)])

;; calc : AE -> number
;; to define a calculator for AEs

(define (calc an-ae)
  (type-case AE an-ae
             [num (n) n]
             [add (l r) (+ (calc l) (calc r))]))

(test (calc (num 3)) 3)
(test (calc (add (num 3) (num 4))) 7)
(test (calc (add (add (num 3) (num 4)) (num 7))) 14)

