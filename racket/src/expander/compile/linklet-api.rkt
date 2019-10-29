#lang racket/base
(require (rename-in "linklet.rkt"
                    [linklet-directory? raw:linklet-directory?]
                    [linklet-directory->hash raw:linklet-directory->hash])
         "compiled-in-memory.rkt")

(provide linklet-directory?
         linklet-directory->hash
         (except-out (all-from-out "linklet.rkt")
                     raw:linklet-directory?
                     raw:linklet-directory->hash))

(define (linklet-directory? v)
  (or (raw:linklet-directory? v)
      (compiled-in-memory? v)))

(define (linklet-directory->hash ld)
  (raw:linklet-directory->hash (if (compiled-in-memory? ld)
                                   (compiled-in-memory-linklet-directory ld)
                                   ld)))
