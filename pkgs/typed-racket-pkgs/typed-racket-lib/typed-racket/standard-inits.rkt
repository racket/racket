#lang racket/base

(require
 racket/base racket/lazy-require "env/env-req.rkt"
 "utils/timing.rkt" ;; only for timing/debugging
 )

(provide do-standard-inits)

(lazy-require
  [typed-racket/base-env/base-env ((init init-base-env))]
  [typed-racket/base-env/base-env-numeric ((init init-base-env-numeric))]
  [typed-racket/base-env/base-structs (initialize-structs)]
  [typed-racket/base-env/base-env-indexing (initialize-indexing)]
  [typed-racket/base-env/base-special-env (initialize-special)]
  [(submod typed-racket/base-env/base-types initialize) (initialize-type-names)])

(define initialized #f)
(define (do-standard-inits)
  (unless initialized
    (do-time "Starting initialization")
    (initialize-structs)
    (do-time "Finshed base-structs")
    (initialize-indexing)
    (do-time "Finshed base-env-indexing")
    (init-base-env)
    (do-time "Finshed base-env")
    (init-base-env-numeric)
    (do-time "Finshed base-env-numeric")
    (initialize-special)
    (do-time "Finished base-special-env")
    (initialize-type-names)
    (do-time "Finished base-types")
    (set! initialized #t))
  (do-requires))
