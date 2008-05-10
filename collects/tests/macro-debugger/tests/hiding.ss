
#lang scheme/base
(require (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
         (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 2 8))
         macro-debugger/model/debug
         "../test-setup.ss")
(provide specialized-hiding-tests)

;; == Macro hiding

(define-syntax test-hiding/policy
  (syntax-rules ()
    [(th form hidden-e2 policy)
     (test-case (format "~s" 'form)
       (let-values ([(steps defs stx exn)
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

(define-syntax test-simple-hiding
  (syntax-rules ()
    [(tsh form hidden-e2)
     (test-hiding/policy form hidden-e2 simple-policy)]))
(define-syntax test-simple-hiding/id
  (syntax-rules ()
    [(tshi form) (test-simple-hiding form form)]))

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
        (test-trivial-hiding/id (lambda (x y) x y))
        (test-trivial-hiding (lambda (x y z) (begin x y) z)
                             (lambda (x y z) x y z))
        (test-trivial-hiding/id (lambda (x y z) x (begin y z))) ;; expression begin!
        (test-trivial-hiding (lambda (x) (define-values (y) x) y)
                             (lambda (x) (letrec-values ([(y) x]) y)))
        (test-trivial-hiding (lambda (x) (begin (define-values (y) x)) y)
                             (lambda (x) (letrec-values ([(y) x]) y)))
        (test-trivial-hiding (lambda (x) (begin (define-values (y) x) y) x)
                             (lambda (x) (letrec-values ([(y) x]) y x)))
        (test-trivial-hiding (lambda (x) (id (define-values (y) x)) x)
                             (lambda (x) (letrec-values ([(y) x]) x)))
        (test-trivial-hiding (lambda (x) (id (begin (define-values (y) x) x)))
                             (lambda (x) (letrec-values ([(y) x]) x)))
        (test-trivial-hiding (lambda (x) (define-values (y) (id x)) y)
                             (lambda (x) (letrec-values ([(y) x]) y)))
        (test-trivial-hiding (lambda (x y) x (id y))
                             (lambda (x y) x y))
        (test-trivial-hiding (lambda (x) (define-values (y) (id x)) y)
                             (lambda (x) (letrec-values ([(y) x]) y))))
      #;
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
                             (lambda (x) (define-values (y) x) y))))
    (test-suite "Result tests for simple hiding"
      (test-suite "Atomic expressions"
        (test-simple-hiding/id *)
        (test-simple-hiding/id 1)
        (test-simple-hiding/id unbound-var))
      (test-suite "Basic expressions"
        (test-simple-hiding/id (if 1 2 3))
        (test-simple-hiding/id (with-continuation-mark 1 2 3))
        (test-simple-hiding/id (define-values (x) 1))
        (test-simple-hiding/id (define-syntaxes (x) 1)))
      (test-suite "Opaque macros"
        (test-simple-hiding/id (id '1))
        (test-simple-hiding/id (id 1))
        (test-simple-hiding/id (id (id '1)))
        ;; app is hidden:
        (test-simple-hiding/id (+ '1 '2)))
      (test-suite "Transparent macros"
        (test-simple-hiding (Tlist x)
                            (list x))
        (test-simple-hiding (Tid x) x)
        (test-simple-hiding (Tlist (id x))
                            (list (id x)))
        (test-simple-hiding (Tid (id x))
                            (id x))
        (test-simple-hiding (id (Tlist x))
                            (id (list x)))
        (test-simple-hiding (id (Tid x))
                            (id x)))
      (test-suite "Blocks"
        (test-simple-hiding/id (lambda (x y) x y))
        (test-simple-hiding (lambda (x y z) (begin x y) z)
                            (lambda (x y z) x y z))
        (test-simple-hiding/id (lambda (x y z) x (begin y z))) ;; expression begin!
        (test-simple-hiding (lambda (x) (define-values (y) x) y)
                            (lambda (x) (letrec-values ([(y) x]) y)))
        (test-simple-hiding (lambda (x) (begin (define-values (y) x)) y)
                            (lambda (x) (letrec-values ([(y) x]) y)))
        (test-simple-hiding (lambda (x) (begin (define-values (y) x) y) x)
                            (lambda (x) (letrec-values ([(y) x]) y x)))
        (test-simple-hiding (lambda (x) (id x))
                            (lambda (x) (id x)))
        (test-simple-hiding (lambda (x) (Tid x))
                            (lambda (x) x))
        (test-simple-hiding/id (lambda (x) (id (define-values (y) x)) x))
        (test-simple-hiding (lambda (x) (id (define-values (y) x)) (Tid x))
                            (lambda (x) (id (define-values (y) x)) x))
        (test-simple-hiding/id (lambda (x) (id (begin (define-values (y) x) x))))
        (test-simple-hiding (lambda (x) (begin (id (define-values (y) x)) y))
                            (lambda (x) (id (define-values (y) x)) y))
        (test-simple-hiding (lambda (x) (id (begin (Tid (define-values (y) x)))) (Tid y))
                            (lambda (x) (id (begin (define-values (y) x))) y))
        (test-simple-hiding (lambda (x) (id (begin (Tid (define-values (y) x)))) x (Tid y))
                            (lambda (x) (id (begin (define-values (y) x))) x y))
        (test-simple-hiding (lambda (x) (define-values (y) (id x)) y)
                            (lambda (x) (letrec-values ([(y) (id x)]) y)))
        (test-simple-hiding (lambda (x y) x (id y))
                            (lambda (x y) x (id y)))
        (test-simple-hiding (lambda (x y) x (Tid y))
                            (lambda (x y) x y))
        (test-simple-hiding (lambda (x) (id (define-values (y) x)) x (Tid y))
                            (lambda (x) (id (define-values (y) x)) x y))
        (test-simple-hiding/id (lambda (x) (id (define-values (y) (id x))) y))
        (test-simple-hiding (lambda (x) (id (define-values (y) (Tid x))) y)
                            (lambda (x) (id (define-values (y) x)) y)))
      (test-suite "Binding expressions"
        (test-simple-hiding/id (lambda (x) x))
        (test-simple-hiding/id (lambda (x) (id x))))
      (test-suite "Module declarations"
        (test-simple-hiding (module m mzscheme
                              (require 'helper)
                              (define x 1))
                            (module m mzscheme
                              (#%module-begin
                               (require 'helper)
                               (define x 1))))
        (test-simple-hiding (module m mzscheme
                              (require 'helper)
                              (define x (Tlist 1)))
                            (module m mzscheme
                              (#%module-begin
                               (require 'helper)
                               (define x (list 1)))))
        (test-simple-hiding (module m mzscheme
                              (#%plain-module-begin
                               (require 'helper)
                               (define x (Tlist 1))))
                            (module m mzscheme
                              (#%plain-module-begin
                               (require 'helper)
                               (define x (list 1)))))))))
