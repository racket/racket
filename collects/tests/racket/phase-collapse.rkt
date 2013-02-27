#lang racket/base
(require compiler/zo-parse)

(define (check-phase-collapsing is? form)
  (parameterize ([current-namespace (make-base-namespace)])
    (define o (open-output-bytes))
    (write (compile `(module m racket/kernel ,form)) o)
    (close-output-port o)
    (define i (open-input-bytes (get-output-bytes o)))
    (define e (zo-parse i))
    (unless (equal? is? (and (memq 'phase-collapsing (mod-flags (compilation-top-code e))) #t))
      (error 'phase-collapsing "failed: ~s ~s" is? form))))

(check-phase-collapsing #t '(define-values (x) 5))
(check-phase-collapsing #t '(define-values (x) '5))
(check-phase-collapsing #t '(define-values (x) (#%datum . 5)))
(check-phase-collapsing #t '(define-values (x) #t))
(check-phase-collapsing #t '(define-values (x) 'x))
(check-phase-collapsing #t '(define-values (x) "x"))
(check-phase-collapsing #t '(define-values (x) #"x"))
(check-phase-collapsing #t '(define-values (x) cons))
(check-phase-collapsing #t '(define-values (x) (cons 1 2)))
(check-phase-collapsing #t '(define-values (x) (list 1 2)))
(check-phase-collapsing #t '(define-values (x) (cons 1 '())))
(check-phase-collapsing #t '(#%require racket/tcp))
(check-phase-collapsing #t '(define-values (x) (lambda (x) x)))
(check-phase-collapsing #t '(define-values (x) (case-lambda [(x) x] [y y])))
(check-phase-collapsing #t '(define-values (struct: ? -ref) (make-struct-type-property 'p)))
(check-phase-collapsing #t '(define-values (struct: make- ? -ref -set!) (make-struct-type 's #f 0 0)))
(check-phase-collapsing #t '(define-values (struct: make- ? -ref -set!) (make-struct-type 's struct:exn 0 0)))
(check-phase-collapsing #t '(define-values (struct: make- ? -ref -set!) (make-struct-type 's struct:exn 1 0 #f (list (cons prop:procedure 0)))))
(check-phase-collapsing #t '(begin
                       (define-values (x) 5)
                       (define-values (y) 6)))

(check-phase-collapsing #f '(define-values (x) #(x)))
(check-phase-collapsing #f '(define-values (x) '(x)))
(check-phase-collapsing #f '(define-values (x) (lambda () (set! x 8))))
(check-phase-collapsing #f '(define-values (x) (quote-syntax x)))
(check-phase-collapsing #f '(define-values (x) (lambda () (quote-syntax x))))
(check-phase-collapsing #f '(define-values (x) (#%variable-reference)))
(check-phase-collapsing #f '(define-values (x) (lambda () (#%variable-reference))))
(check-phase-collapsing #f '(define-values (x) (lambda () (if #f (#%variable-reference) 10))))
(check-phase-collapsing #f '(define-values (x) (#%variable-reference x)))
(check-phase-collapsing #f '(#%require racket/base))

(check-phase-collapsing #t '(module* sub #f (vector 1 2 3)))
(check-phase-collapsing #t '(module* sub #f (#%variable-reference)))
(check-phase-collapsing #t '(module* sub racket/base (#%variable-reference)))
(check-phase-collapsing #t '(module sub racket/base (#%variable-reference)))

;; Check phase crossing via an exception:
(parameterize ([current-namespace (make-base-namespace)])
  (eval `(module m racket/kernel
           (#%provide s? make-s)
           (define-values (struct:s make-s s? s-ref s-set!) (make-struct-type 's #f 0 0))))
  (eval '(require 'm))
  (define s? (eval 's?))
  (eval '(require (for-syntax racket/base 'm)))
  (eval '(define-syntax (m stx) (raise (make-s))))
  (with-handlers ([s? void])
    (eval '(m)))
  (define (check-exn)
    (with-handlers ([s? void])
      (eval '(module n racket/base
               (require (for-syntax racket/base 'm))
               (begin-for-syntax
                (raise (make-s)))))))
  (check-exn)
  (define (check-attach namespace-attach-module)
    (define ns (make-base-namespace))
    (namespace-attach-module (current-namespace) ''m ns)
    (parameterize ([current-namespace ns])
      (check-exn)))
  (check-attach namespace-attach-module)
  (check-attach namespace-attach-module-declaration))

;; Check disallowing redeclaration:
(parameterize ([current-namespace (make-base-namespace)])
  (parameterize ([compile-enforce-module-constants #f])
    (eval `(module m racket/kernel
             (#%provide x)
             (define-values (x) 5)))
    (compile `(module m racket/kernel
                (#%provide x)
                (define-values (x) 6)))
    (unless (void?
             (with-handlers ([exn:fail? void])
               (eval `(module m racket/kernel
                        (#%provide x)
                        (define-values (x) 6)))
               'ok))
      (error 'phase-collapsing "redeclaration should have been disallowed"))))

(displayln "All tests passed.")

