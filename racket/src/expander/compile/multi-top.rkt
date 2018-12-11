#lang racket/base
(require "compiled-in-memory.rkt"
         "multi-top-data.rkt"
         "linklet.rkt"
         "../host/linklet.rkt")

(provide compiled-tops->compiled-top
         compiled-top->compiled-tops)

;; Encode a sequence of compiled top-level forms by creating a linklet
;; directory using labels |0|, |1|, etc., to map to the given linklet
;; directories. Keep all the existing compile-in-memory records as
;; "pre" records, too.
;;
;; If `merge-serialization?` is true, then merge all serialized data
;; and generate a new serialization to be used across all top-level
;; forms in the sequence, so that sharing across the top-level forms
;; is preserved. (By doing that only on request for the very
;; top of a tree, we repeat work only twice and avoid non-linear
;; behavior.)
(define (compiled-tops->compiled-top all-cims
                                     #:to-correlated-linklet? [to-correlated-linklet? #f]
                                     #:merge-serialization? [merge-serialization? #f]
                                     #:namespace [ns #f]) ; need for `merge-serialization?`
  (define cims (remove-nontail-purely-functional all-cims))
  (cond
   [(= 1 (length cims))
    (car cims)]
   [else
    (define sequence-ht
      (for/hasheq ([cim (in-list cims)]
                   [i (in-naturals)])
        (values (string->symbol (number->string i))
                (compiled-in-memory-linklet-directory
                 cim))))
    (define ht (if merge-serialization?
                   (hash-set sequence-ht
                             'data
                             (hash->linklet-directory
                              (hasheq #f
                                      (hash->linklet-bundle
                                       (hasheq
                                        0
                                        (build-shared-data-linklet cims ns))))))
                   sequence-ht))
    (compiled-in-memory (hash->linklet-directory ht)
                        #f ; self
                        #f ; requires
                        #f ; provides
                        #hasheqv()
                        #f
                        #hasheqv()
                        #() ; mpis
                        #() ; syntax-literals
                        cims
                        null
                        #f
                        #f)]))

;; Decode a sequence of compiled top-level forms by unpacking the
;; linklet directory into a list of linklet directories
(define (compiled-top->compiled-tops ld)
  (define ht (linklet-directory->hash ld))
  (for*/list ([i (in-range (hash-count ht))]
              [top (in-value (hash-ref ht (string->symbol (number->string i)) #f))]
              #:when top)
    top))

;; ----------------------------------------

(define (remove-nontail-purely-functional cims)
  (let loop ([cims cims])
    (cond
     [(null? cims) null]
     [(null? (cdr cims)) cims]
     [(and (compiled-in-memory? (car cims))
           (compiled-in-memory-purely-functional? (car cims)))
      (loop (cdr cims))]
     [else
      (cons (car cims) (cdr cims))])))
