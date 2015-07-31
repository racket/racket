#lang racket/base
(require rackunit)

(define ns (make-base-namespace))
(eval '(require syntax/macro-testing) ns)

(define-check (check-expand/eval term)
  ;; Checks that term expands w/o error, and that expanded form is writable...
  (check-not-exn (lambda () (format "~s" (parameterize ((current-namespace ns)) (expand term)))))
  ;; ...then checks that term evals raising an error.
  (check-exn (lambda _ #t) (lambda () (eval term ns))))

(test-case "convert-*-error"
  (check-expand/eval '(convert-syntax-error (lambda)))
  (check-expand/eval '(convert-compile-time-error (lambda)))
  ;; syntax exn for term with unsealed intdefctx:
  (check-expand/eval '(convert-syntax-error (let () (define x) x)))
  (check-expand/eval '(convert-compile-time-error (let () (define x) x))))

(test-case "phase1-eval"
  (eval '(require (for-syntax racket racket/struct-info)) ns)
  (eval '(struct point (x y)) ns)
  (define ct-expr '(extract-struct-info (syntax-local-value #'point)))
  (check-pred list? (eval `(phase1-eval ,ct-expr) ns))
  (check-pred syntax? (eval `(phase1-eval ,ct-expr #:quote quote-syntax) ns)))
