#lang racket/base

;; check that generics work at the top-level

(require racket/generic
         rackunit)

(define ns (make-base-namespace))

(check-not-exn
 (λ ()
  (eval '(require racket/generic) ns)
  (eval '(define-generics foobar [foo foobar a1] [bar foobar a1]
           #:defaults ([keyword?
                        (define/generic gbar bar)
                        (define (bar foobar a1) (gbar a1 '#:dummy))])
           #:fallbacks [(define/generic gfoo foo)
                        (define (foo foobar a1) 'foo)
                        (define (bar foobar a1) (gfoo a1 foobar))])
        ns)
  (eval '(struct inst ()
                 ;; make sure `gen:foobar` doesn't cause an
                 ;; error here
                 #:methods gen:foobar
                 [(define (foo foobar a1) 0)])
        ns)))

