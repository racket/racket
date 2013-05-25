#lang scheme
(provide (all-defined-out))

(define init-allocator #f)
(define gc:deref #f)
(define gc:alloc-flat #f)
(define gc:cons #f)
(define gc:first #f)
(define gc:rest #f)
(define gc:set-first! #f)
(define gc:set-rest! #f)
(define gc:cons? #f)
(define gc:flat? #f)
