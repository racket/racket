#lang racket
(require racket/sandbox
         racket/gui/dynamic)

(define-syntax-rule (test expect expr)
  (do-test 'expr expect expr))

(define (do-test which expect got)
  (unless (equal? expect got)
    (error 'test "failed: ~s expect: ~e got: ~e" which expect got)))

;; GUI is initialled allowed by sandbox, but not initially available:

(test #t (sandbox-gui-available))
(test #f (gui-available?))

;; Create a pre-GUI evaluator:

(define e (call-with-trusted-sandbox-configuration
           (lambda ()
             (make-evaluator 'racket))))
(test (void) (e '(require racket/gui/dynamic)))
(test #f (e '(gui-available?)))

;; Load GUI library

(test (void) (dynamic-require 'racket/gui #f))

;; Now the GUI is available:

(test #t (gui-available?))

;; Create a post-GUI evaluator:

(define ge (call-with-trusted-sandbox-configuration
            (lambda ()
              (make-evaluator 'racket))))
(test (void) (ge '(require racket/gui/dynamic)))
(test #t (ge '(gui-available?)))

;; A post-GUI evaluator, but with GUI disabled:

(define pe (parameterize ([sandbox-gui-available #f])
             (call-with-trusted-sandbox-configuration
              (lambda ()
                (make-evaluator 'racket)))))
(test (void) (pe '(require racket/gui/dynamic)))
(test #f (pe '(gui-available?)))
