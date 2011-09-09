#lang typed/scheme

(: g ((U Integer #f) -> Integer))
(define (g x)
  (cond
    [(or (equal? x 0) (equal? x #f)) 0]
    [else x]))

(define-type-alias Source (U Symbol #f))

(: source? (Any -> Boolean : Source))
(define (source? x)
  (if (false? x)
      #t
      (symbol? x)))
