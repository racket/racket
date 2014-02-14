#lang typed/racket/base

(require "../common/types.rkt")

(require/typed/provide
 plot/private/contracted/kde
 [kde  (case-> ((Listof Real) Real
                              -> (Values mapped-function (Option Real) (Option Real)))
               ((Listof Real) Real (U (Listof Real) #f)
                              -> (Values mapped-function (Option Real) (Option Real))))]
 )
