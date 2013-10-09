#lang typed/racket/base

;; This is very incomplete, but exports enough that typed/plot/utils at least exports typed versions
;; all of the *documented* functions in plot/utils

(require "../common/types.rkt")

(require/typed/provide
 plot/private/contracted/sample
 [linear-seq (Real Real Integer [#:start? Boolean] [#:end? Boolean] -> (Listof Real))]
 
 [linear-seq* ((Listof Real) Integer [#:start? Boolean] [#:end? Boolean] -> (Listof Real))]
 
 [nonlinear-seq (Real Real Integer Axis-Transform 
                      [#:start? Boolean] [#:end? Boolean] -> (Listof Real))])
