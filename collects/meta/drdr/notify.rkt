#lang racket
(define (notify! fmt . args)
  (log-info (format "[~a] ~a" (current-seconds) (apply format fmt args))))

(provide/contract
 [notify! ((string?) () #:rest (listof any/c) . ->* . void)])
