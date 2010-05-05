#lang racket/base

;; The posn struct for the teaching languages
(provide (struct-out posn) make-posn)

(struct posn (x y) #:mutable #:transparent)

;; We define a separate function so tha it has the 
;; name `make-posn':
(define (make-posn x y) (posn x y))
