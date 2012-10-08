#lang racket/base
(require rackunit
         macro-debugger/model/debug
         "../test-setup.rkt")
(provide policy-tests)

(define ns (make-base-namespace))
(eval '(require (prefix-in k: '#%kernel)) ns)
(eval '(require (prefix-in base: racket/base)) ns)
(eval '(require (prefix-in scheme: racket)) ns)

(define (make-test-id sym)
  (parameterize ((current-namespace ns))
    (namespace-symbol->identifier sym)))

(define-syntax-rule (test-policy policy name show?)
  (test-case (format "~s" 'name)
    (check-eq? (policy (make-test-id 'name))
               show?)))
(define-syntax-rule (test-standard name show?)
  (test-policy standard-policy name show?))
(define-syntax-rule (test-base name show?)
  (test-policy base-policy name show?))

(define policy-tests
  (test-suite "Policy tests"
    (test-suite "Base policy"
      ;; Kernel forms
      (test-base k:define-values #f)
      (test-base k:lambda #f)
      (test-base k:if #f)

      ;; racket/base forms
      (test-base base:define #f)
      (test-base base:lambda #f)
      (test-base base:#%app #f)
      (test-base base:if #f)

      ;; Other racket/* forms
      (test-base scheme:match #f)
      (test-base scheme:unit #f)
      (test-base scheme:class #f)

      ;; Unbound names
      (test-base no-such-name #t)
      )
    (test-suite "Standard policy"
      ;; Kernel forms
      (test-standard k:define-values #f)
      (test-standard k:lambda #f)
      (test-standard k:if #f)

      ;; racket/base forms
      (test-standard base:define #f)
      (test-standard base:lambda #f)
      (test-standard base:#%app #f)
      (test-standard base:if #f)

      ;; Other racket/* forms
      (test-standard scheme:match #f)
      (test-standard scheme:unit #f)
      (test-standard scheme:class #f)

      ;; Unbound names
      (test-standard no-such-name #t))))
