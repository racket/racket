(module threads mzscheme
  (require (planet robby/redex:5/reduction-semantics)
           (planet robby/redex:5/subst)
           (planet robby/redex:5/gui)
           (lib "plt-match.ss"))
  
  (reduction-steps-cutoff 100)
  
  (define-language threads
    (p ((store (x v) ...) (threads e ...)))
    (e (set! x e)
       (let ((x e)) e)
       (e e)
       x
       v
       (+ e e))
    (v (lambda (x) e)
       number)
    (x variable)
    (pc ((store (x v) ...) tc))
    (tc (threads e ... ec e ...))
    (ec (ec e) (v ec) (set! variable ec) (let ((x ec)) e) (+ ec e) (+ v ec) hole))
  
  (define reductions
    (reduction-relation
     threads
     (--> (in-hole pc_1 (+ number_1 number_2))
          (in-hole pc_1 ,(+ (term number_1) (term number_2)))
          sum)
     
     (--> ((store
            (name befores (x v)) ...
            (x_i v_i)
            (name afters (x v)) ...)
           (in-hole tc_1 x_i))
          ((store 
            befores ... 
            (x_i v_i)
            afters ...)
           (in-hole tc_1 v_i))
          deref)
     
     (--> ((store (x_1 v_1) ... (x_i v) (x_2 v_2) ...)
           (in-hole tc_1 (set! x_i v_new)))
          ((store (x_1 v_1) ... (x_i v_new) (x_2 v_2) ...)
           (in-hole tc_1 v_new))
          set!)
     
     (--> (in-hole pc_1 ((lambda (x_1) e_1) v_1))
          (in-hole pc_1 ,(substitute (term x_1) (term v_1) (term e_1)))
          app)
     
     (--> ((store (name the-store any) ...)
           (in-hole tc_1 (let ((x_1 v_1)) e_1)))
          (term-let ((new-x (variable-not-in (term (the-store ...)) (term x_1))))
                    (term 
                     ((store the-store ... (new-x v_1))
                      (in-hole tc_1 ,(substitute (term x_1) (term new-x) (term e_1))))))
          let)))
  
  (define substitute
    (plt-subst
     [(? symbol?) (variable)]
     [(? number?) (constant)]
     [`(lambda (,x) ,b)
      (all-vars (list x))
      (build (lambda (vars body) `(lambda (,(car vars)) ,body)))
      (subterm (list x) b)]
     [`(set! ,x ,e)
      (all-vars '())
      (build (lambda (vars name body) `(set! ,name ,body)))
      (subterm '() x)
      (subterm '() e)]
     [`(let ((,x ,e1)) ,e2)
      (all-vars (list x))
      (build (lambda (vars letval body) `(let ((,(car vars) ,letval)) ,body)))
      (subterm '() e1)
      (subterm (list x) e2)]
     [`(+ ,e1 ,e2)
      (all-vars '())
      (build (lambda (vars e1 e2) `(+ ,e1 ,e2)))
      (subterm '() e1)
      (subterm '() e2)]
     [`(,f ,x)
      (all-vars '())
      (build (lambda (vars f x) `(,f ,x)))
      (subterm '() f)
      (subterm '() x)]))
  
  (define (run es) (traces threads reductions `((store) (threads ,@es))))
  (provide run)
  
  (define (count x) 
    (match x
      [`(set! ,x ,e) (+ 1 (count e))]
      [(? symbol?) 1]
      [(? number?) 0]
      [`(+ ,e1 ,e2) (+ 1 (count e1) (count e2))]))
  
  ;; use a pretty-printer that just summaizes the terms, showing the depth of each thread.
  (traces threads reductions
          '((store (x 1))
            (threads
             (set! x (+ x -1))
             (set! x (+ x 1))))
          
          (lambda (exp)
            (match exp
              [`((store (x ,x)) (threads ,t1 ,t2))
                (format "~a ~a ~a" x (count t1) (count t2))])))

  (parameterize ([initial-char-width 16])
    (stepper threads reductions '((store) (threads 
                                           (+ 1 1)
                                           (+ 1 1)
                                           (+ 1 1)))))
  )
