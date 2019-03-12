#lang racket/base

(provide (struct-out to-fasl))

(struct to-fasl (vb   ; box containing byte string as marhsaled or other as unmarshaled
                 wrt) ; directory for unmarshaling
  #:mutable)
