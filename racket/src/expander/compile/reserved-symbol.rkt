#lang racket/base
(require "built-in-symbol.rkt")

;; Identifers used in the compiler's output; we make distinct names
;; for them once to avoid shadowing of other collisions
(provide phase-shift-id
         dest-phase-id
         ns-id
         self-id
         syntax-literals-id
         get-syntax-literal!-id
         bulk-binding-registry-id
         inspector-id
         deserialize-syntax-id
         deserialized-syntax-vector-id
         set-transformer!-id
         top-level-bind!-id
         top-level-require!-id
         mpi-vector-id)

(define phase-shift-id (make-built-in-symbol! 'phase))
(define dest-phase-id (make-built-in-symbol! 'dest-phase))
(define ns-id (make-built-in-symbol! 'namespace))
(define self-id (make-built-in-symbol! 'self))
(define syntax-literals-id (make-built-in-symbol! 'syntax-literals))
(define get-syntax-literal!-id (make-built-in-symbol! 'get-syntax-literal!))
(define bulk-binding-registry-id (make-built-in-symbol! 'bulk-binding-registry))
(define inspector-id (make-built-in-symbol! 'inspector))
(define deserialize-syntax-id (make-built-in-symbol! 'deserialize-syntax))
(define deserialized-syntax-vector-id (make-built-in-symbol! 'deserialized-syntax-vector))
(define set-transformer!-id (make-built-in-symbol! 'set-transformer!))
(define top-level-bind!-id (make-built-in-symbol! 'top-level-bind!))
(define top-level-require!-id (make-built-in-symbol! 'top-level-require!))
(define mpi-vector-id (make-built-in-symbol! 'mpi-vector))
