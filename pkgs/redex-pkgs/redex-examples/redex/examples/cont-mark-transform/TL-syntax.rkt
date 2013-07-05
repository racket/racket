#lang racket

(require redex)
(provide TL)

(define-language TL
  (e a
     (a ... e)
     (letrec ([σ v] ...) e)
     (w-c-m a a e)
     (c-c-m [a ...])
     (match a l ...)
     (abort e))
  
  (l [(K x ...) e])
  (a (λ (x ...) e)
     σ
     x
     (K a ...))
  (w v
     x)
  (v (λ (x ...) e)
     (K v ...)
     σ)
  
  (x variable-not-otherwise-mentioned)
  (σ (ref variable))
  
  (Σ ∅
     (Σ [σ ↦ v]))
  
  (K string))



