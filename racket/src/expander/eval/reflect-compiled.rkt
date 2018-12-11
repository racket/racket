#lang racket/base
(require "../compile/compiled-in-memory.rkt"
         "../host/linklet.rkt"
         "../compile/linklet.rkt"
         "../common/contract.rkt"
         "../namespace/provided.rkt"
         "../namespace/provide-for-api.rkt")

(provide compiled-expression?
         compiled-module-expression?

         compiled->linklet-directory-or-bundle
         normalize-to-linklet-directory)

(define (compiled-expression? c)
  (or (compiled-in-memory? c)
      (linklet-directory? c)
      (linklet-bundle? c)))

(define (compiled-module-expression? c)
  (define ld (compiled->linklet-directory-or-bundle c))
  (or (and (linklet-directory? ld)
           (let ([b (hash-ref (linklet-directory->hash ld) #f #f)])
             (and b (hash-ref (linklet-bundle->hash b) 'decl #f)))
           #t)
      (and (linklet-bundle? ld)
           (hash-ref (linklet-bundle->hash ld) 'decl #f)
           #t)))

;; ----------------------------------------

(define (compiled->linklet-directory-or-bundle c)
  (if (compiled-in-memory? c)
      (compiled-in-memory-linklet-directory c)
      c))

;; Normalize a compiled module that may have no submodules and is
;; represented directy by a linklet bundle to a representation that
;; uses a linklet directory
(define (normalize-to-linklet-directory c)
  (cond
   [(linklet-directory? (compiled->linklet-directory-or-bundle c))
    ;; already in linklet-directory form:
    c]
   [(linklet-bundle? c)
    (hash->linklet-directory (hasheq #f c))]
   [else
    (struct-copy compiled-in-memory c
                 [linklet-directory (normalize-to-linklet-directory
                                     (compiled-in-memory-linklet-directory c))])]))
