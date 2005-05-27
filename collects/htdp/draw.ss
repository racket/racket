#cs(module draw mzscheme
  (require (lib "big-draw.ss" "htdp")
           (lib "draw-sig.ss" "htdp")
           (lib "unitsig.ss"))

     (define-syntax (draw s)
       (syntax-case s (produce)
         [(_ stmt ... produce exp) (syntax (begin (and stmt ...) exp))]
         [(_ stmt ... produce) 
          (raise-syntax-error #f "produce must be followed by an expression" s)]
         [(_ stmt ... produce exp exp2)
          (raise-syntax-error #f "produce must be followed by exactly one expression" s)]
         [(_ stmt ... produce exp exp2 exp3)
          (raise-syntax-error #f "produce must be followed by exactly one expression" s)]
         [(_ stmt ...)
          (raise-syntax-error #f "use drawing instructions between _draw_ and _produce_ and an expression behind produce" s)]
         ))
     
     (provide 
      draw ;; (draw <expression> ... produce <expression>)
      )

     (provide-signature-elements draw^))
