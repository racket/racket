(module arithmetic mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss")
  
  (define lang
    (language 
     (e (binop e e)
        (sqrt e)
        number)
     (binop +
            -
            *
            /)
     
     (e-ctxt (binop v e-ctxt)
             (binop e-ctxt e)
             (sqrt e-ctxt)
             hole)
     (v number)))
  
  (define-syntax (--> stx)
    (syntax-case stx ()
      [(_ op from to)
       (syntax (reduction/name op
                               lang
                               (in-hole e-ctxt_1 from)
                               (term (in-hole e-ctxt_1 ,to))))]))
  
  (define reductions
    (list
     (--> "add"
          (+ number_1 number_2)
          (+ (term number_1) (term number_2)))
     (--> "subtract"
          (- number_1 number_2)
          (- (term number_1) (term number_2)))
     (--> "multiply"
          (* number_1 number_2)
          (* (term number_1) (term number_2)))
     (--> "divide"
          (/ number_1 number_2)
          (/ (term number_1) (term number_2)))
     (--> "sqrt"
          (sqrt number_1)
          (sqrt (term number_1)))))
     
  (traces lang reductions '(- (* (sqrt 36) (/ 1 2)) (+ 1 2))))