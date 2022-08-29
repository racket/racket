#lang racket/base
(require "wrap.rkt"
         "known.rkt")

(provide known-copy->local-id)

;; can return #f
(define (known-copy->local-id k key imports prim-knowns)
  (define im (hash-ref imports key #f))
  (define id (known-copy-id k))
  (cond
    [(not im) id]
    [else
     ;; `(known-value-id k)` is a name from the imported module,
     ;; which might not exist (or might mean something else) in the
     ;; current module; if the know value seems worthwhile, we
     ;; could add imports here, but for now we substitute only primitives
     (cond
       [(hash-ref prim-knowns (unwrap id) #f) id]
       [else #f])]))
