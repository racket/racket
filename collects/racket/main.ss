#lang racket/private
(require (except-in scheme struct struct/ctc)
         (only-in mzlib/unit struct~r/ctc)
         "private/struct.rkt")

(provide (all-from-out scheme) 
         (rename-out [struct~r/ctc struct/ctc])
         struct)
