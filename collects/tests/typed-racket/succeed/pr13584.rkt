#lang typed/racket

(: f (case->
      (-> Boolean)
      (Input-Port -> Boolean)
      (Bytes -> Boolean)))

(define (f (p #f))
  (cond ((input-port? p) #t)
        ((bytes? p) #f)
        (else #f)))

(assert (call-with-input-bytes #"port" f))
