#lang racket/base

(require profile/analyzer racket/gui/base)

(require "report.rkt")

(provide generate-profile)

(define compiled-module-name 'optimization-coach-compiled-module)

(define (generate-profile this source)
  (define snapshots
    (run-inside-optimization-coach-sandbox
     this
     (lambda ()
       (parameterize ([current-module-declare-name
                       (make-resolved-module-path compiled-module-name)])
         (eval (let ([input (open-input-text-editor source)])
                 (port-count-lines! input)
                 (read-syntax #f input)))
         ;; Require, to run the body, without actually adding anything to the
         ;; current namespace, in case the module calls `eval'.
         (eval '(require profile/sampler))
         (eval `(let ([sampler (create-sampler (current-thread) 0.05)])
                  (dynamic-require '',compiled-module-name #f)
                  (sampler 'stop)
                  (sampler 'get-snapshots)))))))
  (for ([n (profile-nodes (analyze-samples snapshots))])
    (printf "~a -- ~a -- ~a -- ~a\n" (node-id n) (node-total n) (node-self n) (node-src n)))
  (profile-nodes (analyze-samples snapshots)))
