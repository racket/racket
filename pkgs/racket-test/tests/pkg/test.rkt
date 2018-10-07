#lang racket/base
(require (for-syntax racket/base)
         "shelly.rkt"
         "util.rkt"
         racket/port)

;; By making these syntax-time includes, it made it so they would be
;; rebuilt and register as real dependencies.
(define-syntax (run-tests stx)
  (syntax-case stx ()
    [(_ f ...)
     (with-syntax
         ([(tests-f ...)
           (for/list ([f-stx (in-list (syntax->list #'(f ...)))])
             (define f (syntax->datum f-stx))
             (format "tests-~a.rkt" f))])
         (syntax/loc stx
           (let ([successful 0])
             (run-tests*
              (list (let ()
                      (local-require (only-in tests-f run-pkg-tests))
                      (λ ()
                        (printf "starting ~a\n" 'tests-f)
                        (run-pkg-tests)
                        (set! successful (add1 successful))))
                    ...))
             (unless (= successful (length '(f ...)))
               (exit 1)))))]))

(define (run-tests* l)
  (run-pkg-tests*
   (λ ()
     (shelly-case "All tests"
                  (for-each (λ (x) (x)) l)))))

(define (go)
  (run-tests
   "name"
   "basic" "create" "install" "permissions"
   "conflicts" "checksums"
   "deps" "update" "implies"
   "remove"
   "promote"
   "locking"
   "overwrite"
   "config"
   "clone"
   "catalog-links"

   "network"
   "planet"
   "main-server"

   "update-deps"
   "update-auto"
   "scope"
   "trash"
   "migrate"
   "versions"
   "platform"
   "raco"
   "binary"
   "catalogs"
   "failure"

   "api"))

(module+ test
  (module config info
    (define timeout 2400))
  (go))

(module+ main
  (require racket/cmdline)
  (define quiet? #f)
  (command-line
   #:once-each
   ["-q" "run quietly" (set! quiet? #t)]
   #:args ()
   (parameterize ([verbose? (not quiet?)]) (go))))
