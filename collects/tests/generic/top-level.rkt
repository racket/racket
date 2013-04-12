#lang racket/base

;; check that generics work at the top-level

(require racket/generic
         rackunit)

(define ns (make-base-namespace))

(check-not-exn
 (λ ()
  (eval '(require racket/generic) ns)
  (eval '(define-generics foobar [foo foobar a1]) ns)
  (eval '(struct inst ()
                 ;; make sure `gen:foobar` doesn't cause an
                 ;; error here
                 #:methods gen:foobar
                 [(define (foo foobar a1) 0)])
        ns)))

