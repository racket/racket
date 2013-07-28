#lang typed/racket

(define-struct: [e f] doll ((inside : (Option (doll e f)))
                            (elt1 : e)
                                                        (elt2 : f)))

(: singleton (All (e f) (e f -> (doll e f))))
(define (singleton e f)
  (make-doll #f e f))
