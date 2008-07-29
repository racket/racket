(module iswim mzscheme
  (require (planet robby/redex:5/reduction-semantics)
           (planet robby/redex:5/subst)
           (lib "contract.ss"))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Expression grammar:
  
  (define-language iswim-grammar
    (M (M M)
       (o1 M)
       (o2 M M)
       V
       ("letcc" X M)
       ("cc" M M))
    (V X
       ("lam" variable M)
       b
       ("[" M "]"))
    (X variable)
    (b number)
    (o1 "add1" "sub1" "iszero")
    (o2 "+" "-" "*" "^")
    (on o1 o2)
    
    ;; Evaluation contexts:
    (E hole
       (E M)
       (V E)
       (o1 E)
       (o2 E M)
       (o2 V E)
       ("cc" E M)
       ("cc" V E))
    
    ;; Continuations (CK machine):
    (k "mt"
       ("fun" V k)
       ("arg" M k)
       ("narg" (V ... on) (M ...) k))
    
    ;; Environments and closures (CEK):
    (env ((X = vcl) ...))
    (cl (M : env))
    (vcl (V- : env))
    
    ;; Values that are not variables:
    (V- ("lam" variable M)
        b)
    
    ;; Continuations with closures (CEK):
    (k- "mt"
        ("fun" vcl k-)
        ("arg" cl k-)
        ("narg" (vcl ... on) (cl ...) k-)))

  (define M? (redex-match iswim-grammar M))
  (define V? (redex-match iswim-grammar V))
  (define o1? (redex-match iswim-grammar o1))
  (define o2? (redex-match iswim-grammar o2))
  (define on? (redex-match iswim-grammar on))
  (define k? (redex-match iswim-grammar k))

  (define env? (redex-match iswim-grammar env))
  (define cl? (redex-match iswim-grammar cl))
  (define vcl? (redex-match iswim-grammar vcl))
  (define k-? (redex-match iswim-grammar k-))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Substitution:
  
  ;; The subst form makes implemention of capture-avoiding
  ;; easier. We just have to describe how variables bind
  ;; in our language's forms.
  
  (define iswim-subst/backwards
    (subst
     [(? symbol?) (variable)]
     [(? number?) (constant)]
     [`("lam" ,X ,M)
      (all-vars (list X))
      (build (lambda (X-list M) `("lam" ,(car X-list) ,M)))
      (subterm (list X) M)]
     [`(,(and o (or "add1" "sub1" "iszero")) ,M1)
      (all-vars '())
      (build (lambda (vars M1) `(,o ,M1)))
      (subterm '() M1)]
     [`(,(and o (or "+" "-" "*" "^")) ,M1 ,M2)
      (all-vars '())
      (build (lambda (vars M1 M2) `(,o ,M1 ,M2)))
      (subterm '() M1)
      (subterm '() M2)]
     [`(,M1 ,M2)
      (all-vars '())
      (build (lambda (empty-list M1 M2) `(,M1 ,M2)))
      (subterm '() M1)
      (subterm '() M2)]
     [`("letcc" ,X ,M)
      (all-vars (list X))
      (build (lambda (X-list M) `("letcc" ,(car X-list) ,M)))
      (subterm (list X) M)]
     [`("cc" ,M1 ,M2)
      (all-vars '())
      (build (lambda (vars M1 M2) `("cc" ,M1 ,M2)))
      (subterm '() M1)
      (subterm '() M2)]
     [`("[" ,E "]")
      (all-vars '())
      (build (lambda (vars) `("[" ,E "]")))]))
     

  ;; the argument order for the subst-generated function
  ;; doesn't match the order in the notes:
  (define (iswim-subst M Xr Mr)
    (iswim-subst/backwards Xr Mr M))

  (define empty-env '())

  ;; Environment lookup
  (define (env-lookup env X)
    (let ([m (assq X env)])
      (and m (caddr m))))

  ;; Environment extension
  (define (env-extend env X vcl)
    (cons (list X '= vcl) env))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Reductions:
  
  ;; beta_v reduction
  (define beta_v
    (reduction-relation
     iswim-grammar
     (--> (("lam" X_1 M_1) V_1)
          ,(iswim-subst (term M_1) (term X_1) (term V_1)))))
  
  (define delta
    (reduction-relation
     iswim-grammar
     (--> ("add1" b_1) ,(add1 (term b_1)))
     (--> ("sub1" b_1) ,(sub1 (term b_1)))
     (--> ("iszero" b_1)
          ,(if (zero? (term b_1)) 
               (term ("lam" x ("lam" y x)))
               (term ("lam" x ("lam" y y)))))
     (--> ("+" b_1 b_2) ,(+ (term b_1) (term b_2)))
     (--> ("-" b_1 b_2) ,(- (term b_1) (term b_2)))
     (--> ("*" b_1 b_2) ,(* (term b_1) (term b_2)))
     (--> ("^" b_1 b_2) ,(expt (term b_1) (term b_2)))))
  
  ;; ->v
  (define ->v (compatible-closure (union-reduction-relations beta_v delta)
                                  iswim-grammar 
                                  M))
  
  ;; :->v
  (define :->v (context-closure (union-reduction-relations beta_v delta)
                                iswim-grammar
                                E))

  ;; :->v+letcc
  (define :->v+letcc 
    (union-reduction-relations
     :->v
     (reduction-relation
      iswim-grammar
      
      ;; letcc rule:
      (--> (in-hole E_1 ("letcc" X_1 M_1))
           (in-hole E_1 ,(iswim-subst (term M_1)
                                      (term X_1)
                                      `("[" (in-hole E_1 ||) "]"))))
      
      ;; cc rule:
      (--> (in-hole E ("cc" ("[" (in-hole E_2 ||) "]") V_1))
           (in-hole E_2 V_1)))))
			
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Helpers:
  
  (define (delta*n on Vs)
    (let ([l (apply-reduction-relation delta `(,on ,@Vs))])
      (if (null? l)
	  #f
	  (car l))))

  (define (delta*1 o1 V)
    (delta*n o1 (list V)))

  (define (delta*2 o2 V1 V2)
    (delta*n o2 (list V1 V2)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Abbreviations:
  
  (define (if0 test then else)
    (let ([X (variable-not-in `(,then ,else) 'X)])
      `(((("iszero" ,test) ("lam" ,X ,then)) ("lam" ,X ,else)) 77)))
  
  (define true '("lam" x ("lam" y x)))
  (define false '("lam" x ("lam" y y)))
  (define boolean-not `("lam" x ((x ,false) ,true)))
  
  (define mkpair '("lam" x ("lam" y ("lam" s ((s x) y)))))
  (define fst '("lam" p (p ("lam" x ("lam" y x)))))
  (define snd '("lam" p (p ("lam" x ("lam" y y)))))
  
  (define Y_v '("lam" f ("lam" x
			 ((("lam" g (f ("lam" x ((g g) x))))
			   ("lam" g (f ("lam" x ((g g) x)))))
			  x))))
  
  (define mksum `("lam" s
		  ("lam" x 
		   ,(if0 'x 0 '("+" x (s ("sub1" x)))))))
  (define sum `(,Y_v ,mksum))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Exports:
  
  (provide iswim-grammar)
  
  (provide/contract (M? (any/c . -> . any/c))
		    (V? (any/c . -> . any/c))
		    (o1? (any/c . -> . any/c))
		    (o2? (any/c . -> . any/c))
		    (on? (any/c . -> . any/c))
		    (k? (any/c . -> . any/c))
		    (env? (any/c . -> . any/c))
		    (cl? (any/c . -> . any/c))
		    (vcl? (any/c . -> . any/c))
		    (iswim-subst (M? symbol? M? . -> . M?))
		    (env-lookup (env? symbol? . -> . (union false/c vcl?)))
		    (env-extend (env? symbol? vcl? . -> . env?))
		    (empty-env env?)
		    (beta_v reduction-relation?)
		    (delta reduction-relation?)
		    (delta*1 (o1? V?  . -> . (union false/c V?)))
		    (delta*2 (o2? V? V? . -> .  (union false/c V?)))
		    (delta*n (on? (listof V?) . -> .  (union false/c V?)))
		    (->v reduction-relation?)
		    (:->v reduction-relation?)
		    (:->v+letcc reduction-relation?)
		    (if0 (M? M? M? . -> . M?))
		    (true M?)
		    (false M?)
		    (boolean-not M?)
		    (mkpair M?)
		    (fst M?)
		    (snd M?)
		    (Y_v M?)
		    (sum M?)))
