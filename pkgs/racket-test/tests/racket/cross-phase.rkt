#lang racket/base

(require racket/string
         racket/exn)

(define (check-cross-phase is? form #:why-not-message [why-not-regexp #f])
  (parameterize ([current-namespace (make-base-namespace)])
    (define o (open-output-bytes))
    (define syntax-error
      (with-handlers ([exn:fail:syntax? (lambda (exn) exn)])
        (write (compile `(module m racket/kernel (#%declare #:cross-phase-persistent) ,form)) o)
        #f))
    (close-output-port o)
    (define i (open-input-bytes (get-output-bytes o)))
    (define e (parameterize ([read-accept-compiled #t])
                (read i)))
    (unless
        (if is?
            (and (not syntax-error)
                 (not (eof-object? e))
                 (module-compiled-cross-phase-persistent? e))
            (and syntax-error
                 (string-contains? (exn->string syntax-error) "cross-phase")
                 (or (not why-not-regexp) (regexp-match? why-not-regexp (exn->string syntax-error)))
                 (eof-object? e)))
      (error 'cross-phase "failed: ~s ~s" is? form))))

(check-cross-phase #t '(define-values (x) 5))
(check-cross-phase #t '(define-values (x) '5))
(check-cross-phase #t '(define-values (x) (#%datum . 5)))
(check-cross-phase #t '(define-values (x) #t))
(check-cross-phase #t '(define-values (x) 'x))
(check-cross-phase #t '(define-values (x) "x"))
(check-cross-phase #t '(define-values (x) #"x"))
(check-cross-phase #t '(define-values (x) cons))
(check-cross-phase #t '(define-values (x) (cons 1 2)))
(check-cross-phase #t '(define-values (x) (list 1 2)))
(check-cross-phase #t '(define-values (x) (#%app list 1 2)))
(check-cross-phase #t '(define-values (x) (cons 1 '())))
(check-cross-phase #t '(#%require racket/tcp))
(check-cross-phase #t '(define-values (x) (lambda (x) x)))
(check-cross-phase #t '(define-values (x) (case-lambda [(x) x] [y y])))
(check-cross-phase #t '(define-values (struct: ? -ref) (make-struct-type-property 'p)))
(check-cross-phase #t '(define-values (struct: make- ? -ref -set!) (make-struct-type 's #f 0 0)))
(check-cross-phase #t '(define-values (struct: make- ? -ref -set!) (make-struct-type 's struct:exn 0 0)))
(check-cross-phase #t '(define-values (struct: make- ? -ref -set!) (make-struct-type 's struct:exn 1 0 #f (list (cons prop:procedure 0)))))
(check-cross-phase #t '(begin
                         (define-values (x) 5)
                         (define-values (y) 6)))
(check-cross-phase #t '(define-values (x) (gensym)))
(check-cross-phase #t '(define-values (x) (gensym "s")))
(check-cross-phase #t '(define-values (x) (gensym '"s")))
(check-cross-phase #t '(define-values (x) (#%app gensym '"s")))
(check-cross-phase #t '(define-values (x) (string->uninterned-symbol "s")))
(check-cross-phase #t '(define-values (x) (string->uninterned-symbol '"s")))
(check-cross-phase #t '(define-values (x) (#%app string->uninterned-symbol '"s")))

(check-cross-phase #f '(define-values (x) #(x)))
(check-cross-phase #f '(define-values (x) '(x)))
(check-cross-phase #f '(define-values (x) (lambda () (set! x 8))))
(check-cross-phase #f '(define-values (x) (quote-syntax x)))
(check-cross-phase #f '(define-values (x) (lambda () (quote-syntax x))))
(check-cross-phase #f '(define-values (x) (#%variable-reference)))
(check-cross-phase #f '(define-values (x) (lambda () (#%variable-reference))))
(check-cross-phase #f '(define-values (x) (lambda () (if #f (#%variable-reference) 10))))
(check-cross-phase #f '(define-values (x) (lambda () (if #f 10 (#%variable-reference)))))
(check-cross-phase #f '(define-values (x) (lambda () (if (#%variable-reference) #f 10))))
(check-cross-phase #f '(define-values (x) (lambda () (with-continuation-mark (set! x x) 1 2))))
(check-cross-phase #f '(define-values (x) (lambda () (with-continuation-mark 1 (set! x x) 2))))
(check-cross-phase #f '(define-values (x) (lambda () (with-continuation-mark 1 2 (set! x x)))))
(check-cross-phase #f '(define-values (x) (lambda () (begin 1 2 (set! x x)))))
(check-cross-phase #f '(define-values (x) (lambda () (begin0 1 2 (set! x x)))))
(check-cross-phase #f '(define-values (x) (lambda () (let-values ([q (set! x x)]) q))))
(check-cross-phase #f '(define-values (x) (lambda () (let-values ([q 'ok]) (set! x x)))))
(check-cross-phase #f '(define-values (x) (lambda () (letrec-values ([q (set! x x)]) q))))
(check-cross-phase #f '(define-values (x) (lambda () (letrec-values ([q 'ok]) (set! x x)))))
(check-cross-phase #f '(define-values (x) (#%variable-reference x)))
(check-cross-phase #f '(#%require racket/base))
(check-cross-phase #f '(define-values (x) (gensym 1)))
(check-cross-phase #f '(define-values (x) (string->uninterned-symbol)))

(check-cross-phase #t '(module* sub #f (vector 1 2 3)))
(check-cross-phase #t '(module* sub #f (#%variable-reference)))
(check-cross-phase #t '(module* sub racket/base (#%variable-reference)))
(check-cross-phase #t '(module sub racket/base (#%variable-reference)))

;; Check phase crossing via an exception:
(parameterize ([current-namespace (make-base-namespace)])
  (eval `(module m racket/kernel
           (#%provide s? make-s)
           (#%declare #:cross-phase-persistent)
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
             (#%declare #:cross-phase-persistent)
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
      (error 'cross-phase "redeclaration should have been disallowed"))))

;; Check that `expand` allows cross-phase modules:
(parameterize ([current-namespace (make-base-namespace)])
  (void
   (expand (datum->syntax
            #f
            '(module m '#%kernel
               (#%declare #:cross-phase-persistent))))))

(displayln "All tests passed.")
