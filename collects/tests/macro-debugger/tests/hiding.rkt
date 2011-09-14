#lang racket/base
(require rackunit
         macro-debugger/model/debug
         "../test-setup.rkt")
(provide specialized-hiding-tests)

;; == Macro hiding

(define-syntax test-hiding/policy
  (syntax-rules ()
    [(th form hidden-e2 policy)
     (test-case (format "~s" 'form)
       (let-values ([(steps binders uses stx exn)
                     (parameterize ((macro-policy policy))
                       (reductions+ (trace/k 'form)))])
         (check-pred syntax? stx)
         (check-equal? (syntax->datum stx) 'hidden-e2)))]))

(define-syntax test-trivial-hiding
  (syntax-rules ()
    [(tth form hidden-e2)
     (test-hiding/policy form hidden-e2 (lambda (m) #t))]))
(define-syntax test-trivial-hiding/id
  (syntax-rules ()
    [(tthi form)
     (test-trivial-hiding form form)]))

(define-syntax-rule (test-T-hiding form hidden-e2)
  (test-hiding/policy form hidden-e2 T-policy))
(define-syntax-rule (test-T-hiding/id form)
  (test-T-hiding form form))

(define-syntax-rule (test-Tm-hiding form hidden-e2)
  (test-hiding/policy form hidden-e2 Tm-policy))
(define-syntax-rule (test-Tm-hiding/id form)
  (test-Tm-hiding form form))

(define specialized-hiding-tests
  (test-suite "Specialized macro hiding tests"

    (test-suite "Result tests for trivial hiding"
      (test-suite "Atomic expressions"
        (test-trivial-hiding/id *)
        (test-trivial-hiding 1 '1)
        (test-trivial-hiding (#%datum . 1) '1)
        (test-trivial-hiding unbound-var (#%top . unbound-var)))
      (test-suite "Basic expressions"
        (test-trivial-hiding/id (if * * *))
        (test-trivial-hiding/id (with-continuation-mark * * *))
        (test-trivial-hiding/id (define-values (x) *))
        (test-trivial-hiding/id (define-syntaxes (x) *)))
      (test-suite "Binding expressions"
        (test-trivial-hiding/id (lambda (x) *))
        (test-trivial-hiding/id (case-lambda [(x) *] [(x y) *]))
        (test-trivial-hiding/id (let-values ([(x) *]) *))
        (test-trivial-hiding/id (letrec-values ([(x) *]) *)))
      (test-suite "Blocks"
        ;; Internal definitions no longer expand into straightforward letrec exprs;
        ;; now they can also produce multiple nested lets/letrec forms
        (test-trivial-hiding/id (lambda (x y) x y))
        (test-trivial-hiding (lambda (x y z) (begin x y) z)
                             (lambda (x y z) x y z))
        (test-trivial-hiding (lambda (x y z) x (begin y z))
                             (lambda (x y z) x y z))
        (test-trivial-hiding (lambda (x) (define-values (y) x) y)
                             (lambda (x) (let-values ([(y) x]) y)))
        (test-trivial-hiding (lambda (x) (begin (define-values (y) x)) y)
                             (lambda (x) (let-values ([(y) x]) y)))
        (test-trivial-hiding (lambda (x) (begin (define-values (y) x) y) x)
                             (lambda (x) (let-values ([(y) x]) y x)))
        (test-trivial-hiding (lambda (x) (id (define-values (y) x)) x)
                             (lambda (x) (let-values ([(y) x]) x)))
        (test-trivial-hiding (lambda (x) (id (begin (define-values (y) x) x)))
                             (lambda (x) (let-values ([(y) x]) x)))
        (test-trivial-hiding (lambda (x) (define-values (y) (id x)) y)
                             (lambda (x) (let-values ([(y) x]) y)))
        (test-trivial-hiding (lambda (x y) x (id y))
                             (lambda (x y) x y))
        (test-trivial-hiding (lambda (x) (define-values (y) (id x)) y)
                             (lambda (x) (let-values ([(y) x]) y))))
      #|
      ;; Old hiding mechanism never did letrec transformation (unless forced)
      (test-suite "Block normalization"
        (test-trivial-hiding/id (lambda (x y) x y))
        (test-trivial-hiding/id (lambda (x y z) (begin x y) z))
        (test-trivial-hiding/id (lambda (x y z) x (begin y z)))
        (test-trivial-hiding/id (lambda (x) (define-values (y) x) y))
        (test-trivial-hiding/id (lambda (x) (begin (define-values (y) x)) y))
        (test-trivial-hiding/id (lambda (x) (begin (define-values (y) x) y) x))
        (test-trivial-hiding (lambda (x) (id x))
                             (lambda (x) x))
        (test-trivial-hiding (lambda (x) (id (begin (define-values (y) x) x)))
                             (lambda (x) (begin (define-values (y) x) x)))
        (test-trivial-hiding (lambda (x) (define-values (y) (id x)) y)
                             (lambda (x) (define-values (y) x) y)))
      |#
      )

    (test-suite "Result tests for T hiding"
      (test-suite "Atomic expressions"
        (test-T-hiding/id *)
        (test-T-hiding/id 1)
        (test-T-hiding/id unbound-var))
      (test-suite "Basic expressions"
        (test-T-hiding/id (if 1 2 3))
        (test-T-hiding/id (with-continuation-mark 1 2 3))
        (test-T-hiding/id (define-values (x) 1))
        (test-T-hiding/id (define-syntaxes (x) 1)))
      (test-suite "Opaque macros"
        (test-T-hiding/id (id '1))
        (test-T-hiding/id (id 1))
        (test-T-hiding/id (id (id '1)))
        ;; app is hidden:
        (test-T-hiding/id (+ '1 '2)))
      (test-suite "Transparent macros"
        (test-T-hiding (Tlist x)
                       (list x))
        (test-T-hiding (Tid x) x)
        (test-T-hiding (Tlist (id x))
                       (list (id x)))
        (test-T-hiding (Tid (id x))
                       (id x))
        (test-T-hiding (id (Tlist x))
                       (id (list x)))
        (test-T-hiding (id (Tid x))
                       (id x)))
      (test-suite "Blocks"
        ;; See note about about internal definition expansion
        (test-T-hiding/id (lambda (x y) x y))
        (test-T-hiding (lambda (x y z) (begin x y) z)
                       (lambda (x y z) x y z))
        (test-T-hiding (lambda (x y z) x (begin y z))
                       (lambda (x y z) x y z))
        (test-T-hiding (lambda (x) (define-values (y) x) y)
                       (lambda (x) (let-values ([(y) x]) y)))
        (test-T-hiding (lambda (x) (begin (define-values (y) x)) y)
                       (lambda (x) (let-values ([(y) x]) y)))
        (test-T-hiding (lambda (x) (begin (define-values (y) x) y) x)
                       (lambda (x) (let-values ([(y) x]) y x)))
        (test-T-hiding (lambda (x) (id x))
                       (lambda (x) (id x)))
        (test-T-hiding (lambda (x) (Tid x))
                       (lambda (x) x))
        (test-T-hiding/id (lambda (x) (id (define-values (y) x)) x))
        (test-T-hiding (lambda (x) (id (define-values (y) x)) (Tid x))
                       (lambda (x) (id (define-values (y) x)) x))
        (test-T-hiding/id (lambda (x) (id (begin (define-values (y) x) x))))
        (test-T-hiding (lambda (x) (begin (id (define-values (y) x)) y))
                       (lambda (x) (id (define-values (y) x)) y))
        (test-T-hiding (lambda (x) (id (begin (Tid (define-values (y) x)))) (Tid y))
                       (lambda (x) (id (begin (define-values (y) x))) y))
        (test-T-hiding (lambda (x) (id (begin (Tid (define-values (y) x)))) x (Tid y))
                       (lambda (x) (id (begin (define-values (y) x))) x y))
        (test-T-hiding (lambda (x) (define-values (y) (id x)) y)
                       (lambda (x) (letrec-values ([(y) (id x)]) y)))
        (test-T-hiding (lambda (x y) x (id y))
                       (lambda (x y) x (id y)))
        (test-T-hiding (lambda (x y) x (Tid y))
                       (lambda (x y) x y))
        (test-T-hiding (lambda (x) (id (define-values (y) x)) x (Tid y))
                       (lambda (x) (id (define-values (y) x)) x y))
        (test-T-hiding/id (lambda (x) (id (define-values (y) (id x))) y))
        #|
        FIXME
        (test-T-hiding (lambda (x) (id (define-values (y) (Tid x))) y)
                       (lambda (x) (id (define-values (y) x)) y))
        |#)
      (test-suite "Binding expressions"
        (test-T-hiding/id (lambda (x) x))
        (test-T-hiding/id (lambda (x) (id x))))
      (test-suite "Module declarations"
        (test-T-hiding (module m mzscheme
                         (require 'helper)
                         (define x 1))
                       (module m mzscheme
                         (require 'helper)
                         (define x 1)))
        (test-Tm-hiding (module m mzscheme
                          (require 'helper)
                          (define x 1))
                        (module m mzscheme
                          (#%module-begin
                           (require 'helper)
                           (define x 1))))

        (test-T-hiding (module m mzscheme
                         (require 'helper)
                         (define x (Tlist 1)))
                       (module m mzscheme
                         (require 'helper)
                         (define x (list 1))))
        (test-Tm-hiding (module m mzscheme
                          (require 'helper)
                          (define x (Tlist 1)))
                        (module m mzscheme
                          (#%module-begin
                           (require 'helper)
                           (define x (list 1)))))

        (test-T-hiding (module m mzscheme
                         (#%plain-module-begin
                          (require 'helper)
                          (define x (Tlist 1))))
                       (module m mzscheme
                         (#%plain-module-begin
                          (require 'helper)
                          (define x (list 1)))))
        (test-Tm-hiding (module m mzscheme
                          (#%plain-module-begin
                           (require 'helper)
                           (define x (Tlist 1))))
                        (module m mzscheme
                          (#%plain-module-begin
                           (require 'helper)
                           (define x (list 1)))))))))
