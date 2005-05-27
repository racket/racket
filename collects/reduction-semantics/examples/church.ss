(module church mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss"
           "../subst.ss")
  
  (reduction-steps-cutoff 100)
  
  (define lang
    (language (e (lambda (x) e)
                 (let (x e) e)
                 (app e e)
                 (+ e e)
                 number
                 x)
              (e-ctxt (lambda (x) e-ctxt)
                      a-ctxt)
              (a-ctxt (let (x a-ctxt) e)
                      (app a-ctxt e)
                      (app x a-ctxt)
                      hole)
              (v (lambda (x) e)
                 x)
              (x variable)))
  
  (define reductions
    (list
     (reduction/context lang
                        e-ctxt
                        (app (lambda (x_1) e_body) e_arg)
                        (ch-subst (term x_1) (term e_arg) (term e_body)))
     (reduction/context lang
                        e-ctxt
                        (let (x_1 v_1) e_1)
                        (ch-subst (term x_1) (term v_1) (term e_1)))))
  
  (define ch-subst
    (subst
     [`(let (,x ,v) ,b)
      (all-vars (list x))
      (build (lambda (vars v b) `(let (,(car vars) ,v) ,b)))
      (subterm '() v)
      (subterm (list x) b)]
     [`(app ,f ,x)
      (all-vars '())
      (build (lambda (vars f x) `(app ,f ,x)))
      (subterm '() f)
      (subterm '() x)]
     [`(lambda (,x) ,b)
      (all-vars (list x))
      (build (lambda (vars body) `(lambda (,(car vars)) ,body)))
      (subterm (list x) b)]
     [(? number?) (constant)]
     [(? symbol?) (variable)]))
      
  
  (traces lang reductions
       '(let (plus (lambda (m) 
                     (lambda (n) 
                       (lambda (s)
                         (lambda (z)
                           (app (app m s) (app (app n s) z)))))))
          (let (two (lambda (s) (lambda (z) (app s (app s z)))))
            (app (app plus two) two)))))