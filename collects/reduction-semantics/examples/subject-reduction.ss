(module subject-reduction mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss"
           "../subst.ss"
           (lib "plt-match.ss"))
  
  (reduction-steps-cutoff 10)
  
  (define lang
    (language (e (e e)
                 (abort e)
                 x
                 v)
              (x (variable-except lambda call/cc abort))
              (c (v c)
                 (c e)
                 hole)
              (v call/cc
                 number
                 (lambda (x t) e))
              (t num
                 (t -> t))))
  
  (define reductions
    (list
     (reduction lang
                (in-hole c_1 (call/cc v_arg))
                (let ([v (variable-not-in (term c_1) 'x)])
                  (plug 
                   (term c_1) 
                   (term (v_arg (lambda (,v) 
                                  (abort ,(plug (term c_1) v))))))))
     (reduction lang
                (in-hole c (abort e_1))
                (term e_1))
     (reduction/context lang
                        c
                        ((lambda (x_format t_1) e_body) v_actual)
                        ;(lc-subst x_format v_actual e_body)
                        (lc-subst (term x_format) (term e_body) (term v_actual))
                        )))
  
  (define lc-subst
    (plt-subst
     [(? symbol?) (variable)]
     [(? number?) (constant)]
     [`(lambda (,x ,t) ,b)
      (all-vars (list x))
      (build (lambda (vars body) `(lambda (,(car vars) ,t) ,body)))
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
      
  (define (type-check term)
    (let/ec k
      (let loop ([term term]
                 [env '()])
        (match term
          [(? symbol?) 
            (let ([l (assoc term env)])
              (if l
                  (cdr l)
                  (k #f)))]
          [(? number?) 'num]
          [`(lambda (,x ,t) ,b)
            (let ([body (loop b (cons (cons x t) env))])
              `(,t -> ,body))]
          [`(,e1 ,e2)
            (let ([t1 (loop e1 env)]
                  [t2 (loop e2 env)])
              (match t1
                [`(,td -> ,tr)
                  (if (equal? td t2)
                      tr
                      (k #f))]
                [else (k #f)]))]))))
      
  (define (pred term1)
    (let ([t1 (type-check term1)])
      (lambda (term2)
        (and t1
             (equal? (type-check term2) t1)))))
  
  (define (show term)
    (traces/pred lang reductions (list term) (pred term)))
  
  ;(show '((lambda (x num) x) 1))
  (show '((lambda (x (num -> num)) 1) ((lambda (x (num -> num)) x) (lambda (x num) x))))
  )
