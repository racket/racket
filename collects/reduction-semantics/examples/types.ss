(module types mzscheme
  (require (lib "reduction-semantics.ss" "reduction-semantics")
           (lib "gui.ss" "reduction-semantics")
           (lib "subst.ss" "reduction-semantics"))
  
  (reduction-steps-cutoff 10)
  
  (define lang
    (language (e (e e)
                 x
                 number
                 (lambda (x t) e)
                 (if e e e)
                 (= e e)
		 (-> e e)
                 num
                 bool)
              (c (t c)
                 (c e)
                 (-> t c)
                 (-> c e)
                 (= t c)
                 (= c e)
                 (if c e e)
                 (if t c e)
                 (if t t c)
                 hole)
	      (x (variable-except lambda -> if =))
              (t num bool (-> t t))))
  
  (define-syntax (r--> stx)
    (syntax-case stx ()
      [(_ i r) (syntax (reduction/context lang c i r))]))

  (define-syntax (e--> stx)
    (syntax-case stx ()
      [(_ i msg) (syntax (reduction lang (in-hole c i) msg))]))
	 
  (define reductions
    (list
     (r--> number 
           'num)
     
     (r--> (lambda (x_1 t_1) e_body)
           (term (-> t_1 ,(lc-subst (term x_1) 
                                    (term t_1)
                                    (term e_body)))))
     
     (r--> ((-> t_1 t_2) t_1)
           (term t_2))
     
     (e--> (side-condition ((-> t_1 t) t_2)
                           (not (equal? (term t_1) (term t_2))))
           (format "app: domain error ~s and ~s" (term t_1) (term t_2)))
     
     (e--> (num t_1)
           (format "app: non function error ~s" (term t_1)))
     
     (r--> (if bool t_1 t_1)
           (term t_1))
     (e--> (side-condition (if bool t_1 t_2)
                           (not (equal? (term t_1) (term t_2))))
           (format "if: then and else clause mismatch ~s and ~s" (term t_1) (term t_2)))
     (e--> (side-condition (if t_1 t t)
                           (not (equal? (term t_1) 'bool)))
           (format "if: test not boolean ~s" (term t_1)))
     
     (r--> (= num num) 'bool)
     (e--> (side-condition (= t_1 t_2)
                           (or (not (equal? (term t_1) 'num))
                               (not (equal? (term t_2) 'num))))
           (format "=: not comparing numbers ~s and ~s" (term t_1) (term t_2)))))
  
  (define lc-subst
    (subst
     [(? symbol?) (variable)]
     [(? number?) (constant)]
     [`(lambda (,x ,t) ,b)
      (all-vars (list x))
      (build (lambda (vars body) `(lambda (,(car vars) ,t) ,body)))
      (subterm (list x) b)]
     [`(,f ,@(xs ...))
      (all-vars '())
      (build (lambda (vars f . xs) `(,f ,@xs)))
      (subterm '() f)
      (subterms '() xs)]))
  
  ; (define theterm '((lambda (x num) (lambda (y num) (if (= x y) 0 x))) 1))
  (define theterm '((lambda (x num) (lambda (y num) (if (= x y) 0 (lambda (x num) x)))) 1))
  (traces lang reductions theterm)
  )
