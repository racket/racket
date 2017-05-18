#lang racket/base
(require rackunit)

;; Check that syntax/parse/pre has minimal *residual* dependencies.

(define (make-tracing-module-name-resolver omnr table)
  (case-lambda
    [(mod rel stx load?)
     (when load?
       (define res (omnr mod rel stx #f))
       (hash-set! table (resolved-module-path-name res) #t))
     (omnr mod rel stx load?)]
    [args
     (apply omnr args)]))

(define ns (make-base-namespace))
(define loaded (make-hash))

(define-check (check-no-dep mod)
  (define res ((current-module-name-resolver) mod #f #f #f))
  (define name (resolved-module-path-name res))
  (check-false (hash-ref loaded name #f)))

(parameterize ((current-module-name-resolver
                (make-tracing-module-name-resolver
                 (current-module-name-resolver)
                 loaded))
               (current-namespace ns))
  (namespace-require 'syntax/parse/pre)

  (test-case "no dep on racket/contract/base"
    (check-no-dep 'racket/contract/base))
  (test-case "no deps on impl modules"
    (check-no-dep 'syntax/parse/private/rep)
    (check-no-dep 'syntax/parse/private/parse))
  )

;; Note: this test fails if syntax/parse has out-of-date .zo files.
