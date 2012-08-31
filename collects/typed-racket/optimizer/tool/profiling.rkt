#lang racket/base

(require profile/analyzer racket/gui/base)

(require "sandbox.rkt")

(provide generate-profile
         node-source node-line node-col node-pos node-span
         (all-from-out profile/analyzer))

(define ((mk accessor) node)
  (define src (node-src node))
  (and src (accessor src)))
(define node-source  (mk srcloc-source))
(define node-line    (mk srcloc-line))
(define node-col     (mk srcloc-column))
(define node-pos     (mk srcloc-position))
(define node-span    (mk srcloc-span))

(define compiled-module-name 'optimization-coach-compiled-module)

(define (generate-profile this source)
  (define res-mpi (make-resolved-module-path compiled-module-name))
  (define snapshots
    (run-inside-optimization-coach-sandbox
     this
     (lambda ()
       (parameterize ([current-module-declare-name res-mpi])
         (eval (let ([input (open-input-text-editor source)])
                 (port-count-lines! input)
                 (read-syntax res-mpi input)))
         ;; Require, to run the body, without actually adding anything to the
         ;; current namespace, in case the module calls `eval'.
         (eval '(require profile/sampler))
         (eval `(let ([sampler (create-sampler (current-thread) 0.05)])
                  (dynamic-require '',compiled-module-name #f)
                  (sampler 'stop)
                  (sampler 'get-snapshots)))))))
  (define (right-file? node)
    (define src (node-src node))
    (equal? (and src (srcloc-source src)) res-mpi))
  (define orig-profile (analyze-samples snapshots))
  (filter right-file? (profile-nodes orig-profile)))
