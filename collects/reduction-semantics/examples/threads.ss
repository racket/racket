(module threads mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss"
           "../subst.ss"
           (lib "plt-match.ss"))
  
  (reduction-steps-cutoff 100)
  
  (define threads
    (language
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
     (ec (ec e) (v ec) (set! variable ec) (let ((x ec)) e) (+ ec e) (+ v ec) hole)))
  
  (define reductions
    (list
     
     ; sum
     (reduction threads
                (in-hole pc_1 (+ number_1 number_2))
                (plug (term pc_1) 
                      (+ (term number_1) (term number_2))))
     
     ; deref
     (reduction threads
                ((store
                  (name befores (x v)) ...
                  (x_i v_i)
                  (name afters (x v)) ...)
                 (in-hole tc_1 x_i))
                (term
                 ((store 
                   befores ... 
                   (x_i v_i)
                   afters ...)
                  ,(plug (term tc_1) (term v_i)))))
     ; set!
     (reduction threads
                ((store (name befores (variable v)) ...
                        (x_i v)
                        (name afters (variable v)) ...)
                 (in-hole tc_1 (set! x_i v_new)))
                (term 
                 ((store 
                   befores ... 
                   (x_i v_new)
                   afters ...)
                  ,(plug (term tc_1)
                         (term v_new)))))
     ; beta
     (reduction threads
                (in-hole pc_1 ((lambda (x_1) e_1) v_1))
                (plug (term pc_1) 
                      (substitute (term x_1) (term v_1) (term e_1))))
     
     ; let
     (reduction threads
                ((store (name the-store any) ...)
                 (in-hole tc_1 (let ((x_1 v_1)) e_1)))
                (let ((new-x (variable-not-in (term (the-store ...)) (term x_1))))
                  (term 
                   ((store the-store ... (,new-x v_1))
                    ,(plug (term tc_1) 
                           (substitute (term x_1) new-x (term e_1)))))))))
  
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
  
  (traces threads reductions
       '((store (x 1))
         (threads
          (set! x (+ x -1))
          (set! x (+ x 1))))
       
      (lambda (exp)
        (match exp
          [`((store (x ,x)) (threads ,t1 ,t2))
           (format "~a ~a ~a" x (count t1) (count t2))])))

  (parameterize ([initial-char-width 12])
    (traces threads reductions '((store) (threads (+ 1 1) (+ 1 1)))))
  )
