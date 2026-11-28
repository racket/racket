#lang compiler/demod
"force.rkt"
#:include (#:dir ".")
;; Pruning keeps unexported definitions, like `force`, internal
;; to the demodularized module
#:prune-definitions
