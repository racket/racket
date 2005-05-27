(module control mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss"
           "../subst.ss")
  
  (reduction-steps-cutoff 100)
  
  (define lang
    (language (e (e e)
                 (\# e)
                 (f e)
                 variable
                 (+ e e)
                 v)
              (c (v c)
                 (c e)
                 (\# c)
                 (f c)
                 (+ v c)
                 (+ c e)
                 hole)
              (c# (v c#)
                  (c# e)
                  (f c#)
                  (+ v c#)
                  (+ c# e)
                  hole)
              (v (lambda (variable) e)
                 number)))
  
  (define reductions
    (list
     (reduction lang
                (in-hole c_1 (\# (in-hole c#_1 (f v_1))))
                (term-let ([x (variable-not-in (term c#) 'x)])
                  (term
                   (in-hole c_1
                            (v_1 (lambda (x) (in-hole c#_1 x)))))))
     (reduction lang
                (in-hole c#_1 (f v_1))
                (term-let ([x (variable-not-in (term c#_1) 'x)])
                  (term (v_1 (lambda (x) (in-hole c#_1 x))))))
     (reduction/context lang
                        c
                        ((lambda (variable_x) e_body) v_arg)
                        (lc-subst (term variable_x) (term v_arg) (term e_body)))
     (reduction/context lang
                        c
                        (+ number_1 number_2)
                        (+ (term number_1) (term number_2)))))
  
  (define lc-subst
    (subst
     [`(\# ,e)
      (all-vars '())
      (build (lambda (vars body) `(\# ,body)))
      (subterm '() e)]
     [`(f ,e)
      (all-vars '())
      (build (lambda (vars body) `(f ,body)))
      (subterm '() e)]
     [`(+ ,e1 ,e2)
      (all-vars '())
      (build (lambda (vars e1 e2) `(+ ,e1 ,e2)))
      (subterm '() e1)
      (subterm '() e2)]
     [(? symbol?) (variable)]
     [(? number?) (constant)]
     [`(lambda (,x) ,b)
      (all-vars (list x))
      (build (lambda (vars body) `(lambda (,(car vars)) ,body)))
      (subterm (list x) b)]
     [`(,f ,x)
      (all-vars '())
      (build (lambda (vars f x) `(,f ,x)))
      (subterm '() f)
      (subterm '() x)]))
      
  (traces/multiple lang reductions
                (list '(+ 1 (+ 2 (f (lambda (d) (d (d 0))))))
                      '(+ 1 (\# (+ 2 (f (lambda (d) (d (d 0))))))))))