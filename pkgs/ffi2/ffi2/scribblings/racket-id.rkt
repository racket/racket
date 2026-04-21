#lang at-exp racket/base
(require (for-label racket/base
                    racket/contract/base)
         scribble/manual)

(provide racket-struct
         contract-arrow)

(define racket-struct @racket[struct_t])

(define contract-arrow @racket[->])
