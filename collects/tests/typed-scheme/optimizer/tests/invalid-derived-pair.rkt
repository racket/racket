#;
(
TR missed opt: invalid-derived-pair.rkt 12:2 (#%app car (#%app cdr x)) -- car/cdr on a potentially empty list -- caused by: 12:2 (#%app cdr x)
TR missed opt: invalid-derived-pair.rkt 17:6 (#%app car (#%app cdr x)) -- car/cdr on a potentially empty list -- caused by: 17:6 (#%app cdr x)
 )

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
