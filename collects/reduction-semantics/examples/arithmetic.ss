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
     
     (e-ctxt (binop e e-ctxt)
             (binop e-ctxt e)
             (sqrt e-ctxt)
             hole)
     (v number)))
  
  (define reductions
    (list
     (reduction lang
                (in-hole e-ctxt_1
                         (+ number_1 number_2))
                (plug (term e-ctxt_1) (+ (term number_1) (term number_2))))
     (reduction/context lang
                        e-ctxt
                        (- number_1 number_2)
                        (- (term number_1) (term number_2)))
     (reduction/context lang
                        e-ctxt
                        (* number_1 number_2)
                        (* (term number_1) (term number_2)))
     (reduction/context lang
                        e-ctxt
                        (/ number_1 number_2)
                        (/ (term number_1) (term number_2)))
     (reduction/context lang
                        e-ctxt
                        (sqrt number_1)
                        (sqrt (term number_1)))))
     
  (traces lang reductions '(- (* (sqrt 36) (/ 1 2)) (+ 1 2))))