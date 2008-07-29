(module eta mzscheme
  (require (planet robby/redex:5/reduction-semantics)
           (planet robby/redex:5/gui)
           (planet robby/redex:5/subst))
  
  (reduction-steps-cutoff 100)
  
  (define-language lang
    (e (e e)
       x
       (+ e e)
       v)
    (c (v c)
       (c e)
       (+ v c)
       (+ c e)
       hole)
    (v (lambda (x) e)
       number)
    (x (variable-except lambda +)))
  
  (define reductions
    (reduction-relation
     lang
     (c=> ((lambda (variable_x) e_body) v_arg)
          ,(lc-subst (term variable_x) (term v_arg) (term e_body)))
     (c=> (+ number_1 number_2)
          ,(+ (term number_1) (term number_2)))
     (c=> (side-condition (lambda (variable_x) (e_fun variable_x))
                          (equal? (term e_fun) (lc-subst (term variable_x) 1234 (term e_fun))))
          e_fun)
     
     (--> (in-hole c (number_n v_arg))
          ,(format "procedure application: expected procedure, given: ~a; arguments were: ~a" 
                   (term number_n)
                   (term v_arg)))
     (--> (in-hole c (+ (name non-num (lambda (variable) e)) (name arg2 v)))
          ,(format "+: expects type <number> as 1st argument, given: ~s; other arguments were: ~s"
                   (term non-num) (term arg2)))
     (--> (in-hole c (+ (name arg1 v) (name non-num (lambda (variable) e))))
          ,(format "+: expects type <number> as 2nd argument, given: ~s; other arguments were: ~s"
                   (term arg1) (term non-num)))
     
     where
     
     [(c=> x y) (--> (in-hole c_1 x) (in-hole c_1 y))]))
  
  (define lc-subst
    (subst
     [(? symbol?) (variable)]
     [(? number?) (constant)]
     [`(lambda (,x) ,b)
      (all-vars (list x))
      (build (lambda (vars body) `(lambda (,(car vars)) ,body)))
      (subterm (list x) b)]
     [`(+ ,n2 ,n1)
      (all-vars '())
      (build (lambda (vars n1 n2) `(+ ,n1 ,n1)))
      (subterm '() n1)
      (subterm '() n2)]
     [`(,f ,x)
      (all-vars '())
      (build (lambda (vars f x) `(,f ,x)))
      (subterm '() f)
      (subterm '() x)]))
      
  (traces lang reductions '(+ (lambda (x) ((+ 1 2) x)) 1)))