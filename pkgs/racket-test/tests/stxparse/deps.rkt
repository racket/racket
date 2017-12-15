#lang racket/base
(require rackunit)

;; Check that syntax/parse/pre has minimal *residual* dependencies.

(define (make-tracing-module-name-resolver omnr table)
  (case-lambda
    [(mod rel stx load?)
     (when load?
       (define res (omnr mod rel stx #f))
       (let ([key (resolved-module-path-name res)])
         (hash-set! table key (cons rel (hash-ref table key null)))))
     (omnr mod rel stx load?)]
    [args
     (apply omnr args)]))

(define ns (make-base-namespace))
(define loaded (make-hash))

(define-check (check-no-dep mod)
  (define res ((current-module-name-resolver) mod #f #f #f))
  (define name (resolved-module-path-name res))
  (define (not-loaded? name) (not (hash-ref loaded name #f)))
  (check-pred not-loaded? name)
  (check-false (hash-ref loaded name #f)))

(parameterize ((current-module-name-resolver
                (make-tracing-module-name-resolver
                 (current-module-name-resolver)
                 loaded))
               (current-namespace ns))
  (namespace-require 'syntax/parse/pre)

  (when #f
    (printf "loaded dependencies:\n")
    (let ([mods (filter path? (hash-keys loaded))])
      (for ([mod (sort mods path<?)])
        (printf " ~s\n" mod))))

  (test-case "no dep on racket/contract/base"
    (check-no-dep 'racket/contract/base))
  (test-case "no deps on impl modules"
    (check-no-dep 'syntax/parse/private/rep)
    (check-no-dep 'syntax/parse/private/parse))
  )

;; Note: this test fails if syntax/parse has out-of-date .zo files.
