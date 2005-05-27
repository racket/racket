(module omega mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss"
           "../subst.ss")
  
  (reduction-steps-cutoff 10)
  
  (define lang
    (language (e (e e)
                 (abort e)
                 x
                 v)
              (c (v c)
                 (c e)
                 hole)
              (v call/cc
                 number
                 (lambda (x) e))
              
              (x (variable-except lambda call/cc abort))
              ))
  
  (define reductions
    (list
     (reduction lang
                (in-hole c_1 (call/cc v_arg))
                (term-let ([v (variable-not-in (term c_1) 'x)])
                  (term (in-hole c_1 (v_arg (lambda (v) (abort (in-hole c_1 v))))))))
     (reduction lang
                (in-hole c (abort e_1))
                (term e_1))
     
     (reduction/context lang
                        c
                        ((lambda (variable_x) e_body) v_arg)
                        (lc-subst (term variable_x) (term v_arg) (term e_body)))))
  
  (define lc-subst
    (plt-subst
     ['abort (constant)]
     ['call/cc (constant)]
     [(? symbol?) (variable)]
     [(? number?) (constant)]
     [`(lambda (,x) ,b)
      (all-vars (list x))
      (build (lambda (vars body) `(lambda (,(car vars)) ,body)))
      (subterm (list x) b)]
     [`(call/cc ,v)
      (all-vars '())
      (build (lambda (vars arg) `(call/cc ,arg)))
      (subterm '() v)]
     [`(,f ,x)
      (all-vars '())
      (build (lambda (vars f x) `(,f ,x)))
      (subterm '() f)
      (subterm '() x)]))
      
  
  ;(traces lang reductions '((lambda (x) (x x)) (lambda (x) (x x))))
  
  ;(traces lang reductions '((call/cc call/cc) (call/cc call/cc)))
  (traces lang reductions '((lambda (x) ((call/cc call/cc) x)) (call/cc call/cc)))
  
  )
