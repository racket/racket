#lang scheme/base

(require (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
         macro-debugger/model/debug
         "../test-setup.ss")
(provide policy-tests)

(define ns (make-base-namespace))
(eval '(require (prefix-in k: '#%kernel)) ns)
(eval '(require (prefix-in base: scheme/base)) ns)
(eval '(require (prefix-in scheme: scheme)) ns)

(define-syntax-rule (test-policy policy name show?)
  (test-case (format "~s" 'name)
    (check-eq? (policy
                (parameterize ((current-namespace ns))
                  (namespace-symbol->identifier 'name)))
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

      ;; Scheme/base forms
      (test-base base:define #f)
      (test-base base:lambda #f)
      (test-base base:#%app #f)
      (test-base base:if #f)

      ;; Other Scheme/* forms
      (test-base scheme:match #t)
      (test-base scheme:unit #t)
      (test-base scheme:class #t)

      ;; Unbound names
      (test-base no-such-name #t)
      )
    (test-suite "Standard policy"
      ;; Kernel forms
      (test-standard k:define-values #f)
      (test-standard k:lambda #f)
      (test-standard k:if #f)

      ;; Scheme/base forms
      (test-standard base:define #f)
      (test-standard base:lambda #f)
      (test-standard base:#%app #f)
      (test-standard base:if #f)

      ;; Other Scheme/* forms
      (test-standard scheme:match #f)
      (test-standard scheme:unit #f)
      (test-standard scheme:class #f)

      ;; Unbound names
      (test-standard no-such-name #t))))
