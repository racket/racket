#|

This is an adaptation of Cormac Flanagan's future semantics
to a scheme where each term only has a single hole, but
there are multiple decompositions for each term.

|#

(module future mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss"
           "../subst.ss")
  
  (define lang
    (language
     (state (flet (variable state) state)
            m
            error)
     (m (let (variable (future m)) m)
        (let (variable (car v)) m)
        (let (variable (cdr v)) m)
        (let (variable (if v m m)) m)
        (let (variable (apply v v)) m)
        (let (variable v) m)
        v)
     (v number
        variable
        (cons v v)
        (lambda (variable) m))
     
     (e-state (flet (variable e-state) state)
              (flet (variable state) e-state)
              e)
     (e hole
        (let (variable e) m)
        (let (variable (future e)) m))))
  
  (define reductions
    (list
     (reduction lang
                (in-hole (name e e-state)
                         (let (variable_1 v_val)
                           m_exp))
                (plug (term e)
                      (future-subst (term variable_1) (term v_val) (term m_exp))))
     (reduction lang
                (in-hole (name e e-state)
                         (let (variable_1 (car (cons v_val v)))
                           m_exp))
                (plug (term e) (future-subst (term variable_1)
                                             (term v_val) 
                                             (term m_exp))))
     (reduction lang
                (in-hole (name e e-state)
                         (let (variable_1 (cdr (cons v v_val)))
                           m_exp))
                (plug (term e) (future-subst (term variable_1)
                                             (term v_val)
                                             (term m_exp))))
     (reduction lang
                (in-hole (name e e-state)
                         (let (variable_1 (if true m_then m))
                           m_exp))
                (plug (term e) (term (let (variable_1 m_then) m_exp))))
     (reduction lang
                (in-hole (name e e-state)
                         (let (variable_1 (if false m m_else))
                           m_exp))
                (plug (term e) (term (let (variable_1 m_else) m_exp))))
     (reduction lang
                (in-hole (name e e-state)
                         (let (variable_1 
                               (apply (lambda (variable_formal) m_body)
                                      v_actual))
                           m_exp))
                (plug 
                 (term e)
                 (term (let (variable_1 ,(future-subst (term variable_formal) (term v_actual) (term m_body)))
                         m_exp))))
     (reduction lang
                (in-hole (name e e-state)
                         (let (variable_x (future m_1)) m_2))
                (let ([p (variable-not-in (list (term e) (term m_1) (term m_2)) 'p)])
                  (term (flet (,p m_1) (let (variable_x ,p) m_2)))))
     (reduction lang
                (flet (variable_p v_1) state_body)
                (future-subst (term variable_p) (term v_1) (term state_body)))
     (reduction lang
                (flet (variable_2 (flet (variable_1 state_1) state_2))
                      state_3)
                (term (flet (variable_1 state_1) (flet (variable_2 state_2) state_3))))))
  
  (define future-subst
    (subst
     [`(let (,a-var ,rhs-exp) ,body-exp)
      (all-vars (list a-var))
      (build (lambda (vars rhs-exp body-exp) `(let (,(car vars) ,rhs-exp) ,body-exp)))
      (subterm '() rhs-exp)
      (subterm (list a-var) body-exp)]
     [`(lambda (,a-var) ,exp)
      (all-vars (list a-var))
      (build (lambda (vars body) `(lambda (,(car vars)) ,body)))
      (subterm (list a-var) exp)]
     [(? number?) (constant)]
     [(? symbol?) (variable)]
     [`(cons ,hd ,tl)
      (all-vars '())
      (build (lambda (vars hd tl) `(cons ,hd ,tl)))
      (subterm '() hd)
      (subterm '() tl)]))
  
  (define (copy-sexp x) (if (pair? x) (cons (copy-sexp (car x)) (copy-sexp (cdr x))) x))
  
  '(traces lang reductions '(let (x (future (let (y (cons 1 2))
                                           (let (z (car y))
                                             z))))
                           (let (p (cons 3 4))
                             (let (q (car p))
                               (cons x q)))))
  
  (traces lang reductions '(let (x (future (let (y 1)
                                          y)))
                          x)))