#lang racket/base
(require racket/promise
         "redemod/amalgam.rkt")

;; Demodularizing this program merges an already-demodularized
;; module, "redemod/amalgam.rkt"
(force (delay (printf "promise\n")))
(use-force (lambda () 'thunk))
