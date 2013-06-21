#;#;
#<<END
TR missed opt: invalid-derived-pair.rkt 14:2 (cadr x) -- car/cdr on a potentially empty list -- caused by: 14:2 (cadr x)
TR missed opt: invalid-derived-pair.rkt 19:6 (cadr x) -- car/cdr on a potentially empty list -- caused by: 19:6 (cadr x)

END
""

#lang typed/racket #:optimize

;; can't optimize, the lists may not be long enough
(: f ((Listof Integer) -> Integer))
(define (f x)
  (cadr x))
(: g ((Listof Integer) -> Integer))
(define (g x)
  (if (null? x)
      0
      (cadr x)))
