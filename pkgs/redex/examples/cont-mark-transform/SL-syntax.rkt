#lang racket

(require "TL-syntax.rkt"
         redex)
(provide SL no-dup-keys)

(define-extended-language SL TL
  (e ....
     (call/cc w))
  (v ....
     (κ (hide-hole E)))
  (a ....
     (κ (hide-hole E)))
  
  (D (w-c-m v v D)
     hole
     (v ... D))
  (E (side-condition D_1 (term (no-dup-keys D_1 ()))))
  
  ; used in the marks collapsing rule (6)
  (C (w-c-m v v C)
     hole)
  
  ; used in translation
  (r (a ...)
     (letrec ([σ w] ...) e)
     (w-c-m a a a)
     (c-c-m [a ...])
     (match a l ...)
     (abort e)
     (call/cc e))
  (T (w-c-m a a T)
     hole
     (a ... T))
  (k v
     #f))

(define-metafunction SL
  [(no-dup-keys hole (v ...)) 
   #t]
  [(no-dup-keys (w-c-m v_i v D) (v_0 ... v_i v_i+1 ...))
   #f]
  [(no-dup-keys (w-c-m v_i v D) (v_0 ...))
   (no-dup-keys D (v_i v_0 ...))]
  [(no-dup-keys (v ... D) (v_0 ...))
   (no-dup-keys D ())])
