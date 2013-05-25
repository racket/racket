#lang typed/racket

;Doesn't TypeCheck
(struct: foo ())
(struct: foo-num foo ((v : Number)))
(struct: foo-str foo ((v : String)))

#|
;TypeChecks
(struct: foo-num ((v : Number)))
(struct: foo-str ((v : String)))
|#


(: extract-foo (case-lambda
                 (foo-num -> Number)
                 (foo-str -> String)))

(define (extract-foo foo)
  (cond
    ((foo-num? foo) (foo-num-v foo))
    ((foo-str? foo) (foo-str-v foo))))
