#lang scheme/base

(require scheme/class
         "test-engine.scm")

(define scheme-test-data (make-parameter (list #f #f #f)))
(define scheme-test-format (make-parameter (lambda (v) (format "~a" v))))

(define scheme-test%
  (class* test-engine% ()
    (super-instantiate ())
    (inherit-field test-info test-display)
    (inherit setup-info)

    (field [tests null]
           [test-objs null])

    (define/public (add-test tst)
      (set! tests (cons tst tests)))
    (define/public (get-info)
      (unless test-info (send this setup-info 'check-require))
      test-info)

    (define/augment (run)
      (inner (void) run)
      (for ([t (reverse tests)]) (run-test t)))

    (define/augment (run-test test)
      (test)
      (inner (void) run-test test))))

(provide scheme-test% scheme-test-data scheme-test-format)
