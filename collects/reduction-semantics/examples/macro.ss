(module macro mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss")
  
  (define lang
    (language 
     (e (lambda (variable) e)
        (app e e)
        number
        variable)
     (e-ctxt (lambda (variable) e-ctxt)
             (app e-ctxt any)
             (app e e-ctxt)
             hole)))
  
  (define macros '(or let if true false id))
  
  (define-syntax (--> stx)
    (syntax-case stx ()
      [(_ frm to)
       (syntax (reduction/context lang e-ctxt frm to))]))
  
  (define reductions
    (list
     (--> (id (name e any))
          (term e))
     (--> (side-condition ((name e1 any) (name e2 any))
                          (not (memq (term e1) macros)))
          (term (app e1 e2)))
     (--> (or (name e1 any) (name e2 any))
          (let ([var (variable-not-in (list (term e1) (term e2)) 'x)])
            (term (let (,var e1) (if ,var ,var e2)))))
     (--> (let ((name var variable) (name rhs any)) 
            (name body any))
          (term ((lambda (var) body) rhs)))
     (--> (if (name test any)
              (name thn any)
              (name els any))
          (term ((test thn) els)))
     (--> (true)
          (term (lambda (x) (lambda (y) x))))
     (--> (false)
          (term (lambda (x) (lambda (y) y))))))
  
  (traces lang reductions '((id id) 5))
  (traces lang reductions '(id 5))
  (traces lang reductions '(or (false) (true))))
