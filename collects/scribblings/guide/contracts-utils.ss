#lang scheme/base
(require scribble/basic
         scribble/manual)

(provide ctc-section
         ctc-link
         exercise
         solution)

(define (ctc-section #:tag [tag #f] . rest)
  (keyword-apply section
                 '(#:tag)
                 (list (and tag (str->tag tag)))
                 rest))

(define (ctc-link tag . rest) (apply seclink (str->tag tag) rest))

(define (str->tag tag) (format "contracts-~a" tag))

(define exercise-number 0)
(define (exercise)
  (set! exercise-number (+ exercise-number 1))
  (bold (format "Exercise ~a" exercise-number)))

(define (solution)
  (bold (format "Solution to exercise ~a" exercise-number)))

