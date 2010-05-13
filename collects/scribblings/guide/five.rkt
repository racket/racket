#lang racket/base

(provide read read-syntax)

(define (read in) (read-string 5 in))
(define (read-syntax src in) (read-string 5 in))
