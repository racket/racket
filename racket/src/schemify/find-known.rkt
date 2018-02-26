#lang racket/base
(require "wrap.rkt"
         "import.rkt"
         "known.rkt"
         "mutated-state.rkt")

(provide find-known)

(define (find-known key prim-knowns knowns imports mutated)
  (cond
    [(hash-ref prim-knowns key #f)
     => (lambda (k) k)]
    [(hash-ref-either knowns imports key)
     => (lambda (k)
          (and (simple-mutated-state? (hash-ref mutated key #f))
               (if (known-copy? k)
                   (find-known (unwrap (known-copy-id k)) prim-knowns knowns imports mutated)
                   k)))]
    [else #f]))
