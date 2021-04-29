#lang racket/base

(provide (struct-out faslable-correlated-linklet)
         (struct-out faslable-correlated)
         strip-correlated)

(struct faslable-correlated-linklet (expr name)
  #:prefab)

(struct faslable-correlated (e source position line column span props)
  #:prefab)

(define (strip-correlated v)
  (let strip ([v v])
    (cond
      [(pair? v)
       (cons (strip (car v))
             (strip (cdr v)))]
      [(faslable-correlated? v)
       (strip (faslable-correlated-e v))]
      [else v])))
