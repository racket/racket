#lang racket/base

;; Utilities for defining static contracts that have internal structure but have no sub static
;; contracts. Example: (length/sc 1).

(require
  "structures.rkt"
  "constraints.rkt"
   racket/match
  (for-syntax
    racket/base
    syntax/parse))

(provide
  define-terminal-sc)

(begin-for-syntax
  (define-syntax-class kind-keyword
    [pattern #:flat #:with sym 'flat]
    [pattern #:chaperone #:with sym 'chaperone]
    [pattern #:impersonator #:with sym 'impersonator])

  (define-splicing-syntax-class printer
    [pattern (~seq #:printer (v p mode) body)
      #:with (methods ...) #'(#:methods gen:custom-write [(define (write-proc v p mode) body)])]
    [pattern (~seq)
      #:with (methods ...) #'()])

  )


(define-syntax (define-terminal-sc stx)
  (syntax-parse stx
    [(_ name:id (args:id ...) kind:kind-keyword p:printer body:expr)
     #'(struct name static-contract (args ...)
         #:transparent
         p.methods ...
         #:methods gen:sc
          [(define (sc-map v f) v)
           (define (sc-traverse v f) (void))
           (define (sc->contract v unused-f)
             (match-define (name args ...) v)
             body)
           (define (sc->constraints v f) (simple-contract-restrict 'kind.sym))
           (define (sc-terminal-kind v) 'kind.sym)])]))
