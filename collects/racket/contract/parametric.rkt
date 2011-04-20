#lang racket/base
(require "private/exists.rkt"
         "private/parametric.rkt")
(provide (all-from-out "private/parametric.rkt")
         (except-out (all-from-out "private/exists.rkt")
                     ∀∃?))
