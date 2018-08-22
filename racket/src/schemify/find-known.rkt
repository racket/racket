#lang racket/base
(require "wrap.rkt"
         "import.rkt"
         "known.rkt"
         "mutated-state.rkt")

(provide find-known+import
         find-known)

(define (find-known+import key prim-knowns knowns imports mutated)
  (cond
    [(hash-ref prim-knowns key #f)
     => (lambda (k) (values k #f))]
    [(hash-ref-either knowns imports key)
     => (lambda (k)
          (cond
            [(not (simple-mutated-state? (hash-ref mutated key #f)))
             (values #f #f)]
            [(known-copy? k)
             (find-known+import (unwrap (known-copy-id k)) prim-knowns knowns imports mutated)]
            [else (values k (hash-ref imports key #f))]))]
    [else (values #f #f)]))

(define (find-known key prim-knowns knowns imports mutated)
  (define-values (k im) (find-known+import key prim-knowns knowns imports mutated))
  k)
