(module letrec mzscheme
  (require (planet robby/redex:5/reduction-semantics)
           (planet robby/redex:5/gui)
           (planet robby/redex:5/subst)
           (lib "list.ss"))
  
  (reduction-steps-cutoff 20)
  
  (define-language lang
    (p ((store (x v) ...) e))
    (e (set! x e)
       (let ((x e)) e)
       (letrec ((x e)) e)
       (begin e e ...)
       (e e)
       x
       v)
    (v (lambda (x) e)
       number)
    (x variable)
    (pc ((store (x v) ...) ec))
    (ec (ec e)
        (v ec)
        (set! variable ec)
        (let ((x ec)) e)
        (begin ec e e ...)
        hole))
  
  (define substitute
    (subst
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
     [`(letrec ((,x ,e1)) ,e2)
       (all-vars (list x))
       (build (lambda (vars letval body) `(letrec ((,(car vars) ,letval)) ,body)))
       (subterm (list x) e1)
       (subterm (list x) e2)]
     [`(begin ,@(es ...))
       (all-vars (list))
       (build (lambda (vars . rest) `(begin ,@rest)))
       (subterms '() es)]
     [`(,f ,x)
       (all-vars '())
       (build (lambda (vars f x) `(,f ,x)))
       (subterm '() f)
       (subterm '() x)]))
  
  ;; collect : term -> term
  ;; performs a garbage collection on the term `p'
  (define (collect p)
    (define (find-unused vars p)
      (filter (λ (var) (unused? var p))
              vars))
    
    (define (unused? var p)
      (let ([rhss (map cadr (cdar p))]
            [body (cadr p)])
        (and (not (free-in? var body))
             (andmap (λ (rhs) (not (free-in? var rhs)))
                     rhss))))
    
    (define (free-in? var body)
      (not (equal? (substitute var (gensym) body)
                   body)))
    
    (define (remove-unused vars p)
      `((store ,@(filter (λ (binding) (not (memq (car binding) vars)))
                         (cdar p)))
        ,(cadr p)))
    
    (let* ([vars (map car (cdar p))]
           [unused (find-unused vars p)])
      (cond
        [(null? unused) p]
        [else
         (collect (remove-unused unused p))])))
  
  (define reductions
    (reduction-relation
     lang
     (==> (in-hole pc_1 (begin v e_1 e_2 ...))
          (in-hole pc_1 (begin e_1 e_2 ...))
          begin\ many)
     
     (==> (in-hole pc_1 (begin e_1))
          (in-hole pc_1 e_1)
          begin\ one)
     
     (==> ((store (x_before v_before) ...
             (x_i v_i)
             (x_after v_after) ...)
           (in-hole ec_1 x_i))
          ((store 
            (x_before v_before) ... 
            (x_i v_i)
            (x_after v_after) ...)
           (in-hole ec_1 v_i))
          deref)
     
     (==> ((store (x_before v_before) ...
                  (x_i v)
                  (x_after v_after) ...)
           (in-hole ec_1 (set! x_i v_new)))
          ((store (x_before v_before) ... 
            (x_i v_new)
            (x_after v_after) ...)
           (in-hole ec_1 v_new))
          set!)     
     
     (==> (in-hole pc_1 ((lambda (x_1) e_1) v_1))
          (in-hole pc_1
                   ,(substitute (term x_1) (term v_1) (term e_1)))
          βv)
     
     (==> ((store (name the-store any) ...)
           (in-hole ec_1 (let ((x_1 v_1)) e_1)))
          ,(let ((new-x (variable-not-in (term (the-store ...)) (term x_1))))
             (term
              ((store the-store ... (,new-x v_1))
               (in-hole ec_1 
                        ,(substitute (term x_1) new-x (term e_1))))))
          let)
     
     (==> (in-hole pc_1 (letrec ((x_1 e_1)) e_2))
          (in-hole pc_1 (let ((x_1 0)) (begin (set! x_1 e_1) e_2)))
          letrec)
     
     where
     [(==> a b) (--> a ,(collect (term b)))]))
  
  (define (run e) (traces lang reductions `((store) ,e)))
  
  (run '(letrec ((f (lambda (x)
                      (letrec ((y (f 1))) 
                        2))))
          (f 3)))
  
  (run '(letrec ((f (lambda (x)
                      (letrec ((y 1))
                        (f 1)))))
          (f 3))))
