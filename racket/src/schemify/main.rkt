#lang racket/base
(require "schemify.rkt"
         "known.rkt"
         "lift.rkt"
         "jitify.rkt"
         "xify.rkt"
         "fasl-literal.rkt"
         "interpret.rkt"
         "size.rkt"
         "fasl.rkt")

(provide schemify-linklet
         schemify-body
         
         (all-from-out "known.rkt")

         lift-in-schemified-linklet
         lift-in-schemified-body

         jitify-schemified-linklet

         xify

         fasl-literal?
         fasl-literals
         unfasl-literals/lazy
         force-unfasl-literals

         interpreter-link!
         interpretable-jitified-linklet
         interpret-linklet

         linklet-bigger-than?

         ->fasl
         fasl->)
