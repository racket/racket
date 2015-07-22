#lang racket/base

(provide (rename-out [read-bstr read]
                     [read-syntax-bstr read-syntax]))

(define (read-bstr port)
  (read (open-input-bytes (read port))))

(define (read-syntax-bstr src port)
  (read-syntax src (open-input-bytes (read port))))
