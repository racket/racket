#lang racket/base
(require racket/include
         racket/fixnum
         racket/flonum
         racket/unsafe/ops
         racket/extflonum
         "../schemify/known.rkt")

(provide get-prim-knowns
         get-primitives
         check-known-values)

(define (get-prim-knowns)
  (let ([knowns (hasheq)])
    (define-syntax-rule (define-primitive-table id [prim known] ...)
      (begin (set! knowns (hash-set knowns 'prim known)) ...))
    (include "primitive/kernel.ss")
    (include "primitive/unsafe.ss")
    (include "primitive/flfxnum.ss")
    (include "primitive/paramz.ss")
    (include "primitive/extfl.ss")
    (include "primitive/network.ss")
    (include "primitive/futures.ss")
    (include "primitive/place.ss")
    (include "primitive/foreign.ss")
    (include "primitive/linklet.ss")
    (include "primitive/internal.ss")
    knowns))

(define (get-primitives)
  (let ([ns (make-base-namespace)])
    (namespace-attach-module (current-namespace) 'racket/fixnum ns)
    (namespace-require 'racket/fixnum ns)
    (namespace-attach-module (current-namespace) 'racket/flonum ns)
    (namespace-require 'racket/flonum ns)
    (namespace-attach-module (current-namespace) 'racket/unsafe/ops ns)
    (namespace-require 'racket/unsafe/ops ns)
    (define primitives (make-hasheq))
    (for ([s (in-list (namespace-mapped-symbols ns))])
      (define v (namespace-variable-value s
                                          #t
                                          (lambda () #f)
                                          ns))
      (when v
        (hash-set! primitives s v)))
    primitives))

;; Check known-value information
(define (check-known-values prim-knowns primitives)
  (for ([(name val) (in-hash primitives)])
    (define k (hash-ref prim-knowns name #f))
    (when k
      (when (known-procedure? k)
        (unless (= (known-procedure-arity-mask k)
                   (procedure-arity-mask val))
          (error 'convert
                 (string-append
                  "arity mask mismatch in \"primitive/...\"\n"
                  "  primitive: ~s\n"
                  "  specified: ~a\n"
                  "  actual: ~a")
                 name
                 (known-procedure-arity-mask k)
                 (procedure-arity-mask val)))))))

(module+ main
  (require racket/cmdline)

  (command-line
   #:args (dir)
   (printf "Checking primitive arity information\n")
   (parameterize ([current-directory dir])
     (define prim-knowns (get-prim-knowns))
     (define primitives (get-primitives))
     (check-known-values prim-knowns primitives))))

