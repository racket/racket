#lang typed/racket/base

(require racket/future)

(provide max-math-threads
         dft-convention
         dft-inverse-convention)

(: max-math-threads (Parameterof Positive-Integer))
(define max-math-threads (make-parameter (max 1 (processor-count))))

(: dft-convention (Parameterof (List Real Real)))
(define dft-convention (make-parameter (list 1 -1)))

(: dft-inverse-convention (-> (List Real Real)))
(define (dft-inverse-convention)
  (define c (dft-convention))
  (define a (car c))
  (define b (cadr c))
  (list (- a) (- b)))
