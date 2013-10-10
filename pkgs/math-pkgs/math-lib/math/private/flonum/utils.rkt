#lang racket/base

(provide near-pow2
         near-pow2/div)

(module untyped-defs racket/base
  (require racket/flonum)
  
  (provide (all-defined-out))
  
  (define-syntax-rule (near-pow2 a)
    (flexpt 2.0 (flmax -1023.0 (flmin 1023.0 (flround (fl/ (fllog (flabs a)) (fllog 2.0)))))))
  
  (define-syntax-rule (near-pow2/div a b)
    ;; Clamping both values makes this work properly when a or b is infinite or zero
    (let ([ea  (flmax -511.0 (flmin 511.0 (fl/ (fllog (flabs a)) (fllog 2.0))))]
          [eb  (flmax -511.0 (flmin 511.0 (fl/ (fllog (flabs b)) (fllog 2.0))))])
      (flexpt 2.0 (flround (fl* 0.5 (fl+ ea eb))))))
  
  )  ; module

(require (submod "." untyped-defs))
