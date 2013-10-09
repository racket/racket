#lang typed/racket/base

(require "../common/types.rkt")

(require/typed/provide
 plot/private/contracted/kde
 [kde  ((Listof Real) Real -> (Values mapped-function (Option Real) (Option Real)))]
 )
