#lang racket/base
(require "simple.rkt"
         (for-template racket/base racket/set racket/promise))
(provide (all-defined-out))

(define identifier?/sc (flat/sc #'identifier?))
(define list?/sc (flat/sc #'list?))
(define set?/sc (flat/sc #'set?))
(define box?/sc (flat/sc #'box?))
(define vector?/sc (flat/sc #'box?))
(define syntax?/sc (flat/sc #'syntax?))
(define promise?/sc (flat/sc #'promise?))
(define hash?/sc (flat/sc #'hash?))

