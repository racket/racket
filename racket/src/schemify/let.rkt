#lang racket/base

(provide make-let*)

(define (make-let* bindings body)
  (if (null? bindings)
      body
      `(let* ,bindings ,body)))
