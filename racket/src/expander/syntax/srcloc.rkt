#lang racket/base
(require "syntax.rkt")

(provide syntax-source
         syntax-line
         syntax-column
         syntax-position
         syntax-span
         
         encoded-srcloc?
         to-srcloc-stx)

;; As a macro so that the procedures have a nice name:
(define-syntax-rule (syntax-source-accessor who srcloc-accessor)
  (lambda (s)
    (unless (syntax? s)
      (raise-argument-error who "syntax?" s))
    (define srcloc (syntax-srcloc s))
    (and srcloc
         (srcloc-accessor srcloc))))

(define syntax-source (syntax-source-accessor 'syntax-source srcloc-source))
(define syntax-line (syntax-source-accessor 'syntax-line srcloc-line))
(define syntax-column (syntax-source-accessor 'syntax-column srcloc-column))
(define syntax-position (syntax-source-accessor 'syntax-position srcloc-position))
(define syntax-span (syntax-source-accessor 'syntax-span srcloc-span))

(define (encoded-srcloc? v)
  (or (and (list? v)
           (= (length v) 5)
           (srcloc-vector? (list->vector v)))
      (and (vector? v)
           (= (vector-length v) 5)
           (srcloc-vector? v))))

(define (srcloc-vector? v)
  (and (or (not (vector-ref v 1))
           (exact-positive-integer? (vector-ref v 1)))
       (or (not (vector-ref v 2))
           (exact-nonnegative-integer? (vector-ref v 2)))
       (or (not (vector-ref v 3))
           (exact-positive-integer? (vector-ref v 3)))
       (or (not (vector-ref v 4))
           (exact-nonnegative-integer? (vector-ref v 4)))))

(define (to-srcloc-stx v)
  (cond
   [(srcloc? v) (struct-copy syntax empty-syntax
                             [srcloc v])]
   [(pair? v) (to-srcloc-stx (list->vector v))]
   [(vector? v)
    (struct-copy syntax empty-syntax
                 [srcloc (srcloc (vector-ref v 0)
                                 (vector-ref v 1)
                                 (vector-ref v 2)
                                 (vector-ref v 3)
                                 (vector-ref v 4))])]
   [else v]))
