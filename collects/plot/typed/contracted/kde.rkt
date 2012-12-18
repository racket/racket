#lang typed/racket/base

(require "../common/types.rkt")

(require/typed/provide
 plot/contracted/kde
 [kde  ((Listof Real) Real -> (Values mapped-function (Option Real) (Option Real)))]
 )
