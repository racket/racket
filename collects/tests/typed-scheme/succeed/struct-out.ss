#lang typed-scheme
(require-typed-struct posn ([x : Number] [y : Number]) lang/posn)
(provide (struct-out posn))
