#;#;
#<<END
TR missed opt: invalid-derived-pair.rkt 10:6 (cadr x) -- car/cdr on a potentially empty list -- caused by: 10:6 (cadr x)
TR missed opt: invalid-derived-pair.rkt 5:2 (cadr x) -- car/cdr on a potentially empty list -- caused by: 5:2 (cadr x)
END
""
#lang typed/racket #:optimize
#reader tests/typed-racket/optimizer/reset-port

;; can't optimize, the lists may not be long enough
(: f ((Listof Integer) -> Integer))
(define (f x)
  (cadr x))
(: g ((Listof Integer) -> Integer))
(define (g x)
  (if (null? x)
      0
      (cadr x)))
