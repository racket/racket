#lang racket/base
(require (for-syntax racket/base
                     "util.rkt")
         "shelly.rkt"
         "util.rkt")

;; By making these syntax-time includes, it made it so they would be
;; rebuilt and register as real dependencies.
(define-syntax (run-tests stx)
  (syntax-case stx ()
    [(_ f ...)
     (with-syntax
         ([(tests-f ...)
           (for/list ([f-stx (in-list (syntax->list #'(f ...)))])
             (define f (syntax->datum f-stx))
             `(file ,(path->string (build-path test-directory (format "tests-~a.rkt" f)))))])
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

(run-tests
 "basic" "create" "install"
 "network" "conflicts" "checksums"
 "deps" "update" 
 "remove"
 "locking"
 "overwrite"
 "config"
 "planet"
 "update-deps"
 "update-auto"
 "raco"
 "main-server")
