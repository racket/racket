#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "known.rkt"
         "known-copy.rkt"
         "import.rkt"
         "mutated-state.rkt")

(provide authentic-valued?)

;; Check whether pre-schemifed `v` is known to produce a non-impersonator

(define (authentic-valued? v knowns prim-knowns imports mutated)
  (let authentic-valued? ([v v])
    (match v
      [`(quote ,v)
       (not (impersonator? v))]
      [`,_
       (define u-v (unwrap v))
       (cond
         [(symbol? u-v)
          (cond
            [(not (simple-mutated-state? (hash-ref mutated u-v #f)))
             #f]
            [(or (hash-ref prim-knowns u-v #f)
                 (hash-ref-either knowns imports u-v))
             => (lambda (k)
                  (or (known-authentic? k)
                      (known-procedure? k)
                      (and (known-literal? k)
                           (not (impersonator? (known-literal-value k))))
                      (and (known-copy? k)
                           (authentic-valued? (known-copy->local-id k u-v imports prim-knowns)))))]
            [else #f])]
         [else
          ;; Any literal allows as unquoted is authentic
          (not (pair? u-v))])])))
