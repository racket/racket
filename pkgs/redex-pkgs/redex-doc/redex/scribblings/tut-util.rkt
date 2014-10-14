#lang at-exp racket/base

(require scribble/base 
         scribble/core)
(provide exercise exref)

(define i 0)
(define (exercise [id #f])
  (set! i (+ i 1))
  (when id (hash-set! ex-ids id i))
  (element (style 'bold '()) 
           (format "Exercise ~a" i)))
(define ex-ids (make-hash))
(define (exref id) (format "~a" (hash-ref ex-ids id)))

