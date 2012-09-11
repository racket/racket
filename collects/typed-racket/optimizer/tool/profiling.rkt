#lang racket/base

(require profile/analyzer racket/gui/base)

(require "sandbox.rkt")

(provide generate-profile
         node-source node-line node-col node-pos node-span
         prune-profile
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


;; In some cases, we only want to consider "hot" functions for further
;; analysis. `prune-profile' prunes non-hot functions from the profile.
;; To determine what is hot, we pick, in order, the hottest functions
;; (by self time. total time could be used, but may not work as well)
;; until our picks cover `total-relative-time-cutoff' (e.g. half) of
;; the total running time.
(define total-relative-time-cutoff .95) ; picked arbitrarily, subject to tweaking
(define (prune-profile profile)
  (define total-time   (profile-total-time profile))
  (define target-time  (* total-time total-relative-time-cutoff))
  (define sorted-nodes (sort (profile-nodes profile) > #:key node-self))
  (define top-nodes
    (let loop ([nodes sorted-nodes] [res '()] [sum 0])
      ;; The last function we pick can go beyond the target.
      ;; O/w, if we had a single function, taking up 100% time, it would
      ;; be discarded.
      (cond [(or (null? nodes) (> sum target-time))
             res]
            [else
             (define h (car nodes))
             (loop (cdr nodes) (cons h res) (+ sum (node-self h)))])))
  top-nodes)
