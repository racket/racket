#lang typed/racket

(: opt (case-lambda ( -> Void)
                    (Integer -> Void)))
(define opt
  (opt-lambda: ((n : Integer 0))
    (display n)))
(opt)
(opt 1)
