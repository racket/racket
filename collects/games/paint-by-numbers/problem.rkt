#lang racket
(provide (struct-out problem))
(define-struct problem (name rows cols solution) #:mutable)
