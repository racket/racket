#lang racket/base

;; Static contracts for common data types.
;; These are used during optimizations as simplifications.
;; Ex: (listof/sc any/sc) => list?/sc

(require "simple.rkt" "structural.rkt"
         (for-template racket/base racket/list racket/set racket/promise racket/mpair
                       racket/class racket/async-channel))
(provide (all-defined-out))

(define identifier?/sc (flat/sc #'identifier?))
(define box?/sc (flat/sc #'box?))
(define syntax?/sc (flat/sc #'syntax?))
(define promise?/sc (flat/sc #'promise?))

(define cons?/sc (flat/sc #'cons?))
(define list?/sc (flat/sc #'list?))

(define mpair?/sc (flat/sc #'mpair?))

(define set?/sc (flat/sc #'set?))
(define empty-set/sc (and/sc set?/sc (flat/sc #'set-empty?)))

(define vector?/sc (flat/sc #'vector?))

(define hash?/sc (flat/sc #'hash?))
(define empty-hash/sc (and/sc hash?/sc (flat/sc #'(λ (h) (zero? (hash-count h))))))

(define channel?/sc (flat/sc #'channel?))
(define async-channel?/sc (flat/sc #'async-channel?))
(define thread-cell?/sc (flat/sc #'thread-cell?))
(define prompt-tag?/sc (flat/sc #'continuation-prompt-tag?))
(define continuation-mark-key?/sc (flat/sc #'continuation-mark-key?))

(define class?/sc (flat/sc #'class?))
