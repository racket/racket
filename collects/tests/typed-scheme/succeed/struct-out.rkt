#lang typed-scheme
(require-typed-struct posn ([x : Number] [y : Number]) #:extra-constructor-name make-posn lang/posn)
(provide (struct-out posn))
