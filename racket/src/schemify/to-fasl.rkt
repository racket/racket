#lang racket/base

(provide (struct-out to-fasl))

(struct to-fasl (vb    ; box containing byte string as marshaled or other as unmarshaled
                 lifts ; vector of external lifts
                 wrt)  ; directory for unmarshaling
  #:mutable)
