#lang racket

(require racket/private/generics)

(define-generics (echoable gen:echoable prop:echo echo?
                           #:defined-table dummy
                           #:coerce-method-table list->vector
                           #:prop-defined-already? #f)
  (echo echoable))

(struct echo1 (s)
        #:methods gen:echoable
        ;; defined the "new" way
        ((define (echo x) (echo1-s x))))

(struct echo2 (s)
        #:property prop:echo
        ;; defined the "old" way
        (list (lambda (x) (echo2-s x))))

(struct echo3 (s)
        #:property prop:echo
        ;; happens to get a valid method table, we're good
        (vector (lambda (x) (echo3-s x))))

(module+ test
  (require rackunit)

  (define e1 (echo1 "a"))
  (check-equal? (echo e1) "a")
  (define e2 (echo2 "b"))
  (check-equal? (echo e2) "b")
  (define e3 (echo3 "c"))
  (check-equal? (echo e3) "c"))
