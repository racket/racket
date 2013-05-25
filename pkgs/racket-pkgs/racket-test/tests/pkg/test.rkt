#lang racket/base
(require (for-syntax racket/base
                     "util.rkt")
         "shelly.rkt"
         "util.rkt"
         racket/port
         (only-in pkg config))

;; By making these syntax-time includes, it made it so they would be
;; rebuilt and register as real dependencies.
(define-syntax (run-tests stx)
  (syntax-case stx ()
    [(_ f ...)
     (with-syntax
         ([(tests-f ...)
           (for/list ([f-stx (in-list (syntax->list #'(f ...)))])
             (define f (syntax->datum f-stx))
             `(file ,(format "tests-~a.rkt" f)))])
       (syntax/loc stx
         (run-tests*
          (list (let ()
                  (local-require (only-in tests-f run-pkg-tests))
                  run-pkg-tests)
                ...))))]))

(define (run-tests* l)
  (run-pkg-tests*
   (λ ()
     (shelly-case "All tests"
                  (for-each (λ (x) (x)) l)))))

(let ([v (getenv "PLT_PKG_NOSETUP")])
  (unless (and v (not (string=? v "")))
    (error "Set the PLT_PKG_NOSETUP environment variable before running these tests\n")))

(unless (equal? "user\n" (with-output-to-string
                           (lambda () (config #:installation #t "default-scope"))))
  (error "Run this test suite with `user' default package scope"))

(run-tests
 "name"
 "basic" "create" "install" "permissions"
 "conflicts" "checksums"
 "deps" "update"
 "remove"
 "locking"
 "overwrite"
 "config"
 ;; "network"
 ;; "planet"
 ;; "main-server"
 "update-deps"
 "update-auto"
 "versions"
 "platform"
 "raco"
 "binary"
 "catalogs")
