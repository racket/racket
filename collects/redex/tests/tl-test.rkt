(module tl-test scheme
  (require "../reduction-semantics.ss"
           "test-util.ss"
           (only-in "../private/matcher.ss" make-bindings make-bind)
           scheme/match
           "../private/struct.ss")
  
  (reset-count)

  (parameterize ([current-namespace syn-err-test-namespace])
    (eval (quote-syntax
           (define-language grammar
             (M (M M)
                number)
             (E hole
                (E M)
                (number E))
             (X (number any)
                (any number))
             (Q (Q ...)
                variable)
             (UN (add1 UN)
                 zero)))))
  
;                                                          
;                                                          
;    ;;                                                    
;     ;                                                    
;     ;     ;;;  ;; ;;    ;; ;;;;  ;;   ;;;    ;; ;;  ;;;  
;     ;    ;   ;  ;;  ;  ;  ;;  ;   ;  ;   ;  ;  ;;  ;   ; 
;     ;     ;;;;  ;   ;  ;   ;  ;   ;   ;;;;  ;   ;  ;;;;; 
;     ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;     
;     ;    ;   ;  ;   ;  ;   ;  ;  ;;  ;   ;  ;   ;  ;     
;   ;;;;;   ;;;;;;;; ;;;  ;;;;   ;; ;;  ;;;;;  ;;;;   ;;;; 
;                            ;                    ;        
;                         ;;;                  ;;;         
;                                                          
;                                                          

  
  (define-language empty-language)
  
  (define-language grammar
    (M (M M)
       number)
    (E hole
       (E M)
       (number E))
    (X (number any)
       (any number))
    (Q (Q ...)
       variable)
    (UN (add1 UN)
        zero))
  
  (test (pair? (redex-match grammar M '(1 1))) #t)
  (test (pair? (redex-match grammar M '(1 1 1))) #f)
  (test (pair? (redex-match grammar
                            (side-condition (M_1 M_2) (equal? (term M_1) (term M_2)))
                            '(1 1)))
        #t)
  (test (pair? (redex-match grammar
                           (side-condition (M_1 M_2) (equal? (term M_1) (term M_2))) 
                           '(1 2)))
        #f)
  
  (test (pair? ((redex-match grammar M) '(1 1)))
        #t)
  
  (test (pair? (redex-match grammar (name not-an-nt_subscript 1) 1)) #t)

  ;; next 3: test naming of subscript-less non-terminals
  (test (pair? (redex-match grammar (M M) (term (1 1)))) #t)
  (test (pair? (redex-match grammar (M M) (term (1 2)))) #f)
  (test (pair? (redex-match grammar (M_1 M_2) (term (1 2)))) #t)
  
  (define-language base-grammar
    (q 1)
    (e (+ e e) number)
    (x (variable-except +)))
  
  (define-extended-language extended-grammar
    base-grammar 
    (e .... (* e e))
    (x (variable-except + *))
    (r 2))
  
  (test (pair? (redex-match extended-grammar e '(+ 1 1))) #t)
  (test (pair? (redex-match extended-grammar e '(* 2 2))) #t)
  (test (pair? (redex-match extended-grammar r '2)) #t)
  (test (pair? (redex-match extended-grammar q '1)) #t)
  (test (pair? (redex-match extended-grammar x '*)) #f)
  (test (pair? (redex-match extended-grammar x '+)) #f)
  (test (pair? (redex-match extended-grammar e '....)) #f)
  
  ;; make sure that `language' with a four period ellipses signals an error
  (test (regexp-match #rx"[.][.][.][.]" (with-handlers ([exn? exn-message]) 
                                          (let ()
                                            (define-language x (e ....))
                                            12)))
        '("...."))
  

  
  ;; test multiple variable non-terminals
  (let ()
    (define-language lang
      ((l m) (l m) x)
      (x variable-not-otherwise-mentioned))
    (test (pair? (redex-match lang m (term x)))
          #t))
  
  ;; test multiple variable non-terminals
  (let ()
    (define-language lang
      ((l m) (l m) x)
      (x variable-not-otherwise-mentioned))
    (test (pair? (redex-match lang l (term x)))
          #t))
  
  (let ()
    (define-language lang
      ((x y) 1 2 3))
    (define-extended-language lang2 lang
      (x .... 4))
    (test (pair? (redex-match lang2 x 4)) #t)
    (test (pair? (redex-match lang2 y 4)) #t)
    (test (pair? (redex-match lang2 x 1)) #t)
    (test (pair? (redex-match lang2 y 2)) #t))
  
  ;; test that the variable "e" is not bound in the right-hand side of a side-condition
  ;; this one signaled an error at some point
  (let ()
    (define-language bad
      (e 2 (side-condition (e) #t)))
    (test (pair? (redex-match bad e '(2)))
          #t))

  ;; test that the variable "e" is not bound in the right-hand side of a side-condition
  ;; this one tests to make sure it really isn't bound
  (let ([x #f])
    (define-language bad
      (e 2 (side-condition (e) (set! x (term e)))))
    (redex-match bad e '(2))
    (test x 'e))
  
  ;; test multiple variable non-terminals being extended
  (let ()
    (define-language lang
      ((x y) 1 2 3))
    (define-extended-language lang2 lang
      (x .... 4))
    (test (pair? (redex-match lang2 x 4)) #t)
    (test (pair? (redex-match lang2 y 4)) #t)
    (test (pair? (redex-match lang2 x 1)) #t)
    (test (pair? (redex-match lang2 y 2)) #t))
  
  ;; test multiple variable non-terminals in an extended language
  (let ()
    (define-language lang
      ((x y) 1 2 3))
    (define-extended-language lang2 lang
      ((z w) 5 6 7))
    (test (pair? (redex-match lang2 z 5)) #t)
    (test (pair? (redex-match lang2 w 6)) #t))
  
  ;; test cases that ensure that extending any one of a
  ;; multiply defined non-terminal gets extended properly
  (let ()
    (define-language iswim
      ((V U W) AA))

    (define-extended-language iswim-cont
      iswim
      (W .... QQ))

    (test (pair? (redex-match iswim-cont U (term QQ)))
          #t))
  
  (let ()
    (define-language iswim
      ((V U W) AA))

    (define-extended-language iswim-cont
      iswim
      (W .... QQ))

    (test (pair? (redex-match iswim-cont V (term QQ)))
          #t)
    (test (pair? (redex-match iswim-cont U (term QQ)))
          #t)
    (test (pair? (redex-match iswim-cont W (term QQ)))
          #t))
  
  (let ()
    (define-language iswim
      ((V U W) AA))
    
    (define-extended-language iswim-cont
      iswim
      (V .... QQ))
    
    (test (pair? (redex-match iswim-cont V (term QQ)))
          #t)
    (test (pair? (redex-match iswim-cont U (term QQ)))
          #t)
    (test (pair? (redex-match iswim-cont W (term QQ)))
          #t))
  
  (let ()
    (define-language okay
      [(X Y) z])
    
    (define-extended-language replace-both
      okay
      [(X Y) q])
    
    ;; this test ran into an infinite loop in an earlier version of redex.
    (test (redex-match replace-both X (term explode)) #f))
  
  (test (with-handlers ([exn? exn-message])
          (let () 
            (define-language main
              [(X Y) z])
            (define-extended-language new
              main
              [(X Y Z) q])
            (void)))
        "extend-language: new language extends old non-terminal X and also adds new shortcut Z")
  
  (test (with-handlers ([exn? exn-message])
          (let () 
            (define-language main
              [(X Y) z]
              [(P Q) w])
            (define-extended-language new
              main
              [(X P) q])
            (void)))
        "extend-language: new language does not have the same non-terminal aliases as the old, non-terminal P was not in the same group as X in the old language")
  
  ;; underscores in literals
  (let ()
    (define-language L
      (x (variable-except a_b))
      (y (variable-prefix a_b)))
    (test (pair? (redex-match L x (term a_c))) #t)
    (test (pair? (redex-match L y (term a_bc))) #t))
  
  ; underscores allowed on built-in non-terminals and names bound
  (let ([m (redex-match 
            grammar 
            (any_1 number_1 natural_1 integer_1
                   real_1 string_1 variable_1
                   variable-not-otherwise-mentioned_1)
            '(1 2 3 4 5 "s" s t))])
    (test (if m
              (map bind-exp
                   (sort (match-bindings (car m))
                         string<=?
                         #:key (compose symbol->string bind-name)))
              '())
          '(1 4 3 2 5 "s" t s)))
  
  ;; test caching
  (let ()
    (define match? #t)
    
    (define-language lang
      (x (side-condition any match?)))
    
    (test (pair? (redex-match lang x 1)) #t)
    (set! match? #f)
    (test (pair? (redex-match lang x 1)) #t)
    (parameterize ([caching-enabled? #f])
      (test (pair? (redex-match lang x 1)) #f)))
  
  
  (let ()
    (define sc-eval-count 0)
    (define-language lang
      (x (side-condition any (begin (set! sc-eval-count (+ sc-eval-count 1))
                                    #t))))
    
    (redex-match lang x 1)
    (redex-match lang x 1)
    (parameterize ([caching-enabled? #f])
      (redex-match lang x 1))
    (test sc-eval-count 2))
  
  (let ()
    (define rhs-eval-count 0)
    (define-metafunction empty-language
      [(f any) ,(begin (set! rhs-eval-count (+ rhs-eval-count 1))
                       1)])
    
    (term (f 1))
    (term (f 1))
    (parameterize ([caching-enabled? #f])
      (term (f 1)))
    (test rhs-eval-count 2))
  
;                                                                                             
;                                                                                             
;                                 ;;;                                ;                        
;                  ;             ;                           ;                                
;  ;;; ;    ;;;   ;;;;;   ;;;   ;;;;; ;;  ;; ;; ;;    ;;;;  ;;;;;  ;;;     ;;;  ;; ;;    ;;;; 
;   ; ; ;  ;   ;   ;     ;   ;   ;     ;   ;  ;;  ;  ;   ;   ;       ;    ;   ;  ;;  ;  ;   ; 
;   ; ; ;  ;;;;;   ;      ;;;;   ;     ;   ;  ;   ;  ;       ;       ;    ;   ;  ;   ;   ;;;  
;   ; ; ;  ;       ;     ;   ;   ;     ;   ;  ;   ;  ;       ;       ;    ;   ;  ;   ;      ; 
;   ; ; ;  ;       ;   ; ;   ;   ;     ;  ;;  ;   ;  ;   ;   ;   ;   ;    ;   ;  ;   ;  ;   ; 
;  ;;;;;;;  ;;;;    ;;;   ;;;;; ;;;;;   ;; ;;;;; ;;;  ;;;     ;;;  ;;;;;   ;;;  ;;; ;;; ;;;;  
;                                                                                             
;                                                                                             
;                                                                                             
;                                                                                             

  
  (define-metafunction grammar
    [(f (side-condition (number_1 number_2)
                        (< (term number_1)
                           (term number_2))))
     x]
    [(f (number 1)) y]
    [(f (number_1 2)) ,(+ (term number_1) 2)]
    [(f (4 4)) q]
    [(f (4 4)) r])

  (define-metafunction grammar
    [(g X) x])
  
  (test (term (f (1 17))) 'x)
  (test (term (f (11 1))) 'y)
  (test (term (f (11 2))) 13)
  
  
  ;; match two clauess => take first one
  (test (term (f (4 4))) 'q)
  
  ;; match one clause two ways => error
  (let ()
    (define-metafunction empty-language
      [(ll (number_1 ... number_2 ...)) (number_1 ...)])
    (test (with-handlers ((exn? (λ (x) 'exn-raised))) 
            (term (ll ()))
            'no-exn)
          'no-exn)
    (test (with-handlers ((exn? (λ (x) 'exn-raised))) 
            (term (ll (4 4)))
            'no-exn)
          'exn-raised))
  
  ;; match no ways => error
  (test (with-handlers ((exn? (λ (x) 'exn-raised))) (term (f mis-match)) 'no-exn)
        'exn-raised)

  (define-metafunction grammar
    [(h (M_1 M_2)) ((h M_2) (h M_1))]
    [(h number_1) ,(+ (term number_1) 1)])
  
  (test (term (h ((1 2) 3)))
        (term (4 (3 2))))
  
  (define-metafunction grammar
    [(h2 (Q_1 ...)) ((h2 Q_1) ...)]
    [(h2 variable) z])
  
  (test (term (h2 ((x y) a b c)))
        (term ((z z) z z z)))
  
  (let ()
    (define-metafunction empty-language
      [(f (1)) 1]
      [(f (2)) 2]
      [(f 3) 3])
    (test (in-domain? (f 1)) #f)
    (test (in-domain? (f (1))) #t)
    (test (in-domain? (f ((1)))) #f)
    (test (in-domain? (f 3)) #t)
    (test (in-domain? (f 4)) #f))
  
  (let ()
    (define-metafunction empty-language
      f : number -> number
      [(f 1) 1])
    (test (in-domain? (f 1)) #t)
    (test (in-domain? (f 2)) #t)
    (test (in-domain? (f x)) #f))
  
  (let ()
    (define-metafunction empty-language
      [(f x) x])
    (test 
     (term-let ((y 'x))
               (in-domain? (f y)))
     #t)
    (test 
     (term-let ((y 'z))
               (in-domain? (f y)))
     #f))
  
  ;; mutually recursive metafunctions
  (define-metafunction grammar
    [(odd zero) #f]
    [(odd (add1 UN_1)) (even UN_1)])
  
  (define-metafunction grammar
    [(even zero) #t]
    [(even (add1 UN_1)) (odd UN_1)])
  
  (test (term (odd (add1 (add1 (add1 (add1 zero))))))
        (term #f))
    
  (let ()
    (define-metafunction empty-language
      [(pRe xxx) 1])
    
    (define-metafunction empty-language
      [(Merge-Exns any_1) any_1])
    
    (test (term (pRe (Merge-Exns xxx)))
          1))
  
  (let ()
    (define-metafunction empty-language
      [(f (x)) ,(term-let ([var-should-be-lookedup 'y]) (term (f var-should-be-lookedup)))]
      [(f y) y]
      [(f var-should-be-lookedup) var-should-be-lookedup]) ;; taking this case is bad!
    
    (test (term (f (x))) (term y)))
  
  (let ()
    (define-metafunction empty-language
      [(f (x)) (x ,@(term-let ([var-should-be-lookedup 'y]) (term (f var-should-be-lookedup))) x)]
      [(f y) (y)]
      [(f var-should-be-lookedup) (var-should-be-lookedup)]) ;; taking this case is bad!
    
    (test (term (f (x))) (term (x y x))))
  
  (let ()
    (define-metafunction empty-language
      [(f (any_1 any_2))
       case1
       (side-condition (not (equal? (term any_1) (term any_2))))
       (side-condition (not (equal? (term any_1) 'x)))]
      [(f (any_1 any_2))
       case2
       (side-condition (not (equal? (term any_1) (term any_2))))]
      [(f (any_1 any_2))
       case3])
    (test (term (f (q r))) (term case1))
    (test (term (f (x y))) (term case2))
    (test (term (f (x x))) (term case3)))

  (let ()
    (define-metafunction empty-language
      [(f (n number)) (n number)]
      [(f (a any)) (a any)]
      [(f (v variable)) (v variable)]
      [(f (s string)) (s string)])
    (test (term (f (n 1))) (term (n 1)))
    (test (term (f (a (#f "x" whatever)))) (term (a (#f "x" whatever))))
    (test (term (f (v x))) (term (v x)))
    (test (term (f (s "x"))) (term (s "x"))))
  
  ;; test ..._1 patterns
  (let ()
    (define-metafunction empty-language
      [(zip ((variable_id ..._1) (number_val ..._1)))
       ((variable_id number_val) ...)])
    
    (test (term (zip ((a b) (1 2)))) (term ((a 1) (b 2)))))
  
  (let ()
    (define-metafunction empty-language
      [(f any_1 any_2 any_3) (any_3 any_2 any_1)])
    (test (term (f 1 2 3)) 
          (term (3 2 1))))
  
  (let ()
    (define-metafunction empty-language
      [(f (any_1 any_2 any_3)) 3])
    (define-metafunction/extension f empty-language
      [(g (any_1 any_2)) 2])
    (define-metafunction/extension g empty-language
      [(h (any_1)) 1])
    (test (term (h (1))) 1)
    (test (term (h (1 2))) 2)
    (test (term (h (1 2 3))) 3))
  
  (let ()
    (define-metafunction empty-language
      [(f any_1 any_2 any_3) 3])
    (define-metafunction/extension f empty-language
      [(g any_1 any_2) 2])
    (define-metafunction/extension g empty-language
      [(h any_1) 1])
    (test (term (h 1)) 1)
    (test (term (h 1 2)) 2)
    (test (term (h 1 2 3)) 3))
  
  (let ()
    (define-metafunction empty-language
      [(f number_1 number_2) (f number_1)])
    (define-metafunction/extension f empty-language
      [(g number_1) number_1])
    (define-metafunction empty-language
      [(h number_1 number_2) (h number_1)]
      [(h number_1) number_1])
    (test (term (g 11 17)) 11)
    (test (term (h 11 17)) 11))
  
  (let ()
    (define-metafunction empty-language
      [(f (number_1 number_2))
       number_3
       (where number_3 ,(+ (term number_1) (term number_2)))])
    (test (term (f (11 17))) 28))
  
  (let ()
    (define-metafunction empty-language
      [(f variable) 
       (any_x any_x)
       (where any_x (variable variable))])
    (test (term (f z)) 
          (term ((z z) (z z)))))
  
  (let ()
    (define-metafunction empty-language
      [(f number_1)
       number_1
       (where number_2 ,(add1 (term number_1)))
       (where number_3 ,(add1 (term number_2)))
       (side-condition (and (number? (term number_3))
                            (= (term number_3) 4)))]
      [(f any) 0])
    (test (term (f 2)) 2))
  
  (let ()
    (define-language x-lang
      (x variable))
    (define-metafunction x-lang
      f : x x -> x
      [(f x_1 x_2) x_1])
    (test (term (f p q)) (term p))
    (test (in-domain? (f p q)) #t))
  
  (let ()
    (define-metafunction empty-language
      [(err number_1 ... number_2 ...) (number_1 ...)])
    (test (term (err)) (term ()))
    (test (with-handlers ((exn:fail:redex? (λ (x) 'right-exn))
                          ((λ (x) #t) (λ (x) 'wrong-exn)))
            (term (err 1 2))
            'no-exn)
          'right-exn))
  
  (let ()
    (define-metafunction empty-language
      err : number ... -> number
      [(err number ...) 1])
    (test (with-handlers ((exn:fail:redex? (λ (x) 'right-exn))
                          ((λ (x) #t) (λ (x) 'wrong-exn)))
            (term (err #f #t))
            'no-exn)
          'right-exn))
  
  (let ()
    (define-metafunction empty-language
      err : number ... -> number
      [(err number ...) #f])
    (test (with-handlers ((exn:fail:redex? (λ (x) 'right-exn))
                          ((λ (x) #t) (λ (x) 'wrong-exn)))
            (term (err 1 2))
            'no-exn)
          'right-exn))
  
  (let ()
    (define-metafunction empty-language
      err : number ... -> (number number)
      [(err number ...) (number ...)])
    (test (with-handlers ((exn:fail:redex? (λ (x) 'right-exn))
                          ((λ (x) #t) (λ (x) 'wrong-exn)))
            (term (err 1 2))
            'no-exn)
          'no-exn)
    (test (with-handlers ((exn:fail:redex? (λ (x) 'right-exn))
                          ((λ (x) #t) (λ (x) 'wrong-exn)))
            (term (err 1 1))
            'no-exn)
          'no-exn))
    
  (let ()
    ;; test that 'where' clauses can contain recursive calls.
    (define-metafunction empty-language
      [(f (any)) 
       x
       (where x (f any))]
      [(f any) any])
    (test (term (f ((((x))))))
          (term x)))
  
  (let ()
    (define-language lamv
      (z variable hole))

    (define-metafunction lamv
      foo : z  -> any
      [(foo hole) dontcare]
      [(foo variable) docare])

    (test (term (foo hole))
          (term dontcare))
    (test (term (foo y))
          (term docare)))
  
  (let ()
    (define f-called? #f)
    (define-metafunction empty-language
      f : (side-condition any_1 (begin (set! f-called? #t) #t)) -> any
      [(f any_1) any_1])
    (test (term (f 1)) 1)
    (test f-called? #t))
    
  (let ()
    (define g-called? #f)
    (define-metafunction empty-language
      g : any -> (side-condition any_1 (begin (set! g-called? #t) #t))
      [(g any_1) any_1])
    (test (term (g 1)) 1)
    (test g-called? #t))
  
  ;; test that tracing works properly
  ;; note that caching comes into play here (which is why we don't see the recursive calls)
  (let ()
    (define-metafunction empty-language
      [(f 0) 0]
      [(f number) (f ,(- (term number) 1))])
    
    (let ([sp (open-output-string)])
      (parameterize ([current-output-port sp])
        (term (f 1)))
      (test (get-output-string sp) ""))
    
    (let ([sp (open-output-string)])
      (parameterize ([current-output-port sp]
                     [current-traced-metafunctions 'all]
                     [print-as-expression #f])
        (term (f 1)))
      (test (get-output-string sp) ">(f 1)\n<0\n"))
    
    (let ([sp (open-output-string)])
      (parameterize ([current-output-port sp]
                     [current-traced-metafunctions '(f)]
                     [print-as-expression #f])
        (term (f 1)))
      (test (get-output-string sp) ">(f 1)\n<0\n")))
  
  (let ()
    (define-language var-lang [(x y z w) variable])
    
    ;; this should produce the second case, 
    ;; since the where clause (should) fail to match 
    ;; in the first case.
    (define-metafunction var-lang
      [(f x)
       first-case
       (where (AnotherAtom y) (Atom x))]
      [(f x)
       second-case])
    
    (test (term (f a)) (term second-case)))
  
  (let ()
    
    ;; this is an ambiguous function 
    ;; and should signal an error if it is ever called
    ;; with multiple different arguments, but if the
    ;; arguments are all the same, it will return
    ;; the same result for any parse, and thus should be allowed.
    (define-metafunction empty-language
      [(f any_x ... any_y any_z ...)
       any_y])
    
    (test (term (f 1 1 1 1 1)) (term 1)))
  
  (let ()
    (define-metafunction empty-language
      [(ex variable_x) 
       variable_x
       (where quote variable_x)])
    
    (test (term (ex quote)) (term quote)))
  
  (let ()
    (define-metafunction empty-language
      [(f any ...)
       (any ...)
       (where variable_1 x)
       (side-condition #f)
       (where (number ...) y)]
      [(f any ...)
       12345])
    
    (test (term (f 8)) 12345))
  
  
  (let ()
    (test-syn-err
     (define-metafunction grammar
       [(f x)])
     #rx"expected a pattern and a right-hand side"))
  
;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                                                                                 
;       ;;           ;;; ;;                                   ;;           ;;  ;;                 
;       ;;          ;;;  ;;                                   ;;          ;;;  ;;                 
;    ;;;;;   ;;;;  ;;;;; ;;  ;; ;;;   ;;;;       ;;;;  ;;;;   ;;   ;;;;  ;;;;; ;;   ;;;;   ;; ;;; 
;   ;;;;;;  ;;  ;; ;;;;  ;;  ;;;;;;  ;;  ;;      ;;;; ;;  ;;  ;;  ;;  ;;  ;;;; ;;  ;;;;;;  ;;;;;; 
;  ;;;  ;; ;;;;;;;; ;;   ;;  ;;  ;; ;;;;;;;;;;;; ;;  ;;;;;;;; ;;    ;;;;  ;;;  ;; ;;;  ;;; ;;  ;; 
;  ;;;  ;; ;;;      ;;   ;;  ;;  ;; ;;;     ;;;; ;;  ;;;      ;;  ;;; ;;  ;;;  ;; ;;;  ;;; ;;  ;; 
;   ;;;;;;  ;;; ;;  ;;   ;;  ;;  ;;  ;;; ;;      ;;   ;;; ;;  ;; ;;;  ;;  ;;;; ;;  ;;;;;;  ;;  ;; 
;    ;;;;;   ;;;;   ;;   ;;  ;;  ;;   ;;;;       ;;    ;;;;   ;;  ;;;;;;   ;;; ;;   ;;;;   ;;  ;; 
;                                                                                                 
;                                                                                                 
;                                                                                                 

  
  (let ()
    (define-relation empty-language
      [(<: any any) #t])
    
    (test (term (<: 1 1)) #t)
    (test (term (<: 1 2)) #f))
  
  (let ()
    (define-relation empty-language
      [(<: number_1 number_2) ,(< (term number_1) (term number_2))]
      [(<: number_1 number_1) #t])
    
    (test (term (<: 1 2)) #t)
    (test (term (<: 1 1)) #t)
    (test (term (<: 2 1)) #f))
  
  (let ()
    (define-relation empty-language
      [(<: number_1 ... number_2 number_3 ... number_2 number_4 ...) #t])
    
    (test (term (<: 1 2 3 4)) #f)
    (test (term (<: 1 1 2 3 4)) #t)
    (test (term (<: 1 2 1 3 4)) #t)
    (test (term (<: 1 2 3 1 4)) #t)
    (test (term (<: 1 2 3 4 1)) #t))
  
  (let ()
    (define-relation empty-language
      [(<: number_1 number_1)])
    (test (term (<: 1 1)) #t)
    (test (term (<: 1 2)) #f))
  
  (let ()
    (define-relation empty-language
      [(<: number_1 number_2 number_3)
       ,(= (term number_1) (term number_2))
       ,(= (term number_2) (term number_3))])
    (test (term (<: 1 2 3)) #f)
    (test (term (<: 1 1 2)) #f)
    (test (term (<: 1 2 2)) #f)
    (test (term (<: 1 1 1)) #t))
  
 
;                    ;;                         ;                                        ;;                    ;                 
;                     ;                 ;                                                 ;            ;                         
;   ;; ;;   ;;;    ;; ; ;;  ;;   ;;;;  ;;;;;  ;;;     ;;;  ;; ;;          ;; ;;   ;;;     ;     ;;;   ;;;;;  ;;;     ;;;  ;; ;;  
;    ;;    ;   ;  ;  ;;  ;   ;  ;   ;   ;       ;    ;   ;  ;;  ;          ;;    ;   ;    ;    ;   ;   ;       ;    ;   ;  ;;  ; 
;    ;     ;;;;;  ;   ;  ;   ;  ;       ;       ;    ;   ;  ;   ;  ;;;;;   ;     ;;;;;    ;     ;;;;   ;       ;    ;   ;  ;   ; 
;    ;     ;      ;   ;  ;   ;  ;       ;       ;    ;   ;  ;   ;          ;     ;        ;    ;   ;   ;       ;    ;   ;  ;   ; 
;    ;     ;      ;   ;  ;  ;;  ;   ;   ;   ;   ;    ;   ;  ;   ;          ;     ;        ;    ;   ;   ;   ;   ;    ;   ;  ;   ; 
;   ;;;;;   ;;;;   ;;;;;  ;; ;;  ;;;     ;;;  ;;;;;   ;;;  ;;; ;;;        ;;;;;   ;;;;  ;;;;;   ;;;;;   ;;;  ;;;;;   ;;;  ;;; ;;;
;                                                                                                                                
;                                                                                                                                
;                                                                                                                                
;                                                                                                                                

  
  
  (test (apply-reduction-relation
         (reduction-relation 
          grammar
          (--> (in-hole E_1 (number_1 number_2))
               (in-hole E_1 ,(* (term number_1) (term number_2)))))
         '((2 3) (4 5)))
        (list '(6 (4 5))))

  (test (apply-reduction-relation
         (reduction-relation 
          grammar
          (~~> (number_1 number_2)
               ,(* (term number_1) (term number_2)))
          with
          [(--> (in-hole E_1 a) (in-hole E_1 b)) (~~> a b)])
         '((2 3) (4 5)))
        (list '(6 (4 5))))
  
  (test (apply-reduction-relation
         (reduction-relation 
          grammar
          (==> (number_1 number_2)
               ,(* (term number_1) (term number_2)))
          with
          [(--> (M_1 a) (M_1 b)) (~~> a b)]
          [(~~> (M_1 a) (M_1 b)) (==> a b)])
         '((1 2) ((2 3) (4 5))))
        (list '((1 2) ((2 3) 20))))
  
  (test (apply-reduction-relation
         (reduction-relation 
          grammar
          (~~> (number_1 number_2)
               ,(* (term number_1) (term number_2)))
          (==> (number_1 number_2)
               ,(* (term number_1) (term number_2)))
          with
          [(--> (M_1 a) (M_1 b)) (~~> a b)]
          [(--> (a M_1) (b M_1)) (==> a b)])
         '((2 3) (4 5)))
        (list '(6 (4 5))
              '((2 3) 20)))
  
  (test (apply-reduction-relation
         (reduction-relation 
          grammar
          (--> (M_1 (number_1 number_2))
               (M_1 ,(* (term number_1) (term number_2))))
          (==> (number_1 number_2)
               ,(* (term number_1) (term number_2)))
          with
          [(--> (a M_1) (b M_1)) (==> a b)])
         '((2 3) (4 5)))
        (list '((2 3) 20)
              '(6 (4 5))))
  
  ; shortcuts like this fail if compilation fails to preserve
  ; lexical context for side-conditions expressions.
  (test (let ([x #t])
          (apply-reduction-relation
           (reduction-relation
            grammar
            (==> variable variable)
            with
            [(--> (a (side-condition number x)) b)
             (==> a b)])
           '(x 4)))
        '(x))
  
  (test (apply-reduction-relation/tag-with-names
         (reduction-relation 
          grammar
          (--> (number_1 number_2) 
               ,(* (term number_1) (term number_2))
               mul))
         '(4 5))
        (list (list "mul" 20)))
  
  (test (apply-reduction-relation/tag-with-names
         (reduction-relation 
          grammar
          (--> (number_1 number_2) 
               ,(* (term number_1) (term number_2))
               "mul"))
         '(4 5))
        (list (list "mul" 20)))
  
  (test (apply-reduction-relation/tag-with-names
         (reduction-relation 
          grammar
          (--> (number_1 number_2) 
               ,(* (term number_1) (term number_2))))
         '(4 5))
        (list (list #f 20)))
  
  (test (apply-reduction-relation/tag-with-names
         (reduction-relation 
          grammar
          (==> (number_1 number_2) 
               ,(* (term number_1) (term number_2))
               mult)
          with
          [(--> (M_1 a) (M_1 b)) (==> a b)])
         '((2 3) (4 5)))
        (list (list "mult" '((2 3) 20))))
  
  (test (apply-reduction-relation
         (union-reduction-relations
          (reduction-relation empty-language
                              (--> x a)
                              (--> x b))
          (reduction-relation empty-language
                              (--> x c)
                              (--> x d)))
         'x)
        (list 'a 'b 'c 'd))
  
  (test (apply-reduction-relation
         (union-reduction-relations
          (reduction-relation empty-language (--> x a))
          (reduction-relation empty-language (--> x b))
          (reduction-relation empty-language (--> x c))
          (reduction-relation empty-language (--> x d)))
         'x)
        (list 'a 'b 'c 'd))
  
  (test (apply-reduction-relation
         (reduction-relation 
          empty-language
          (--> (number_1 number_2) 
               number_2
               (side-condition (< (term number_1) (term number_2))))
          (--> (number_1 number_2) 
               number_1
               (side-condition (< (term number_2) (term number_1)))))
         '(1 2))
        (list 2))
  
  (test (apply-reduction-relation
         (reduction-relation 
          empty-language
          (--> x #f))
         (term x))
        (list #f))
  
  (define-language x-language
    (x variable))
  
  (test (apply-reduction-relation
         (reduction-relation 
          x-language
          (--> x (x x)))
         'y)
        (list '(y y)))
  
  (test (apply-reduction-relation
         (reduction-relation 
          x-language
          (--> (x ...) ((x ...))))
         '(p q r))
        (list '((p q r))))

  #;
  (test (apply-reduction-relation
         (reduction-relation 
          empty-language
          #:main-arrow :->
          (:-> 1 2))
         1)
        '(2))
  
  (test (apply-reduction-relation
         (reduction-relation 
          empty-language
          #:domain number
          (--> 1 2))
         1)
        '(2))
  
  
  (test (let ([red
               (reduction-relation 
                empty-language
                #:domain number
                (--> 1 2))])
          (with-handlers ((exn? exn-message))
            (apply-reduction-relation red 'x)
            'no-exception-raised))
        "reduction-relation: relation not defined for x")

  (test (let ([red
               (reduction-relation 
                empty-language
                #:domain number
                (--> 1 x))])
          (with-handlers ((exn? exn-message))
            (apply-reduction-relation red 1)
            'no-exception-raised))
        "reduction-relation: relation reduced to x via rule #0 (counting from 0), which is outside its domain")

  (let* ([red1
          (reduction-relation 
           empty-language
           #:domain (side-condition number_1 (even? (term number_1)))
           (--> number number))]
         [red2
          (reduction-relation 
           empty-language
           #:domain (side-condition number_1 (odd? (term number_1)))
           (--> number number))]
         [red-c
          (union-reduction-relations red1 red2)])
    
    ;; ensure first branch of 'union' is checked  
    (test (with-handlers ((exn? exn-message))
            (apply-reduction-relation red-c 1)
            'no-exception-raised)
          "reduction-relation: relation not defined for 1")

    ;; ensure second branch of 'union' is checked
    (test (with-handlers ((exn? exn-message))
            (apply-reduction-relation red-c 2)
            'no-exception-raised)
          "reduction-relation: relation not defined for 2"))

  (let ()
    (define-language l1
      (D 0 1 2))
    (define r1
      (reduction-relation 
       l1
       #:domain D
       (--> D D)))
    (define-language l2
      (D 0 1 2 3))
    (define r2
      (extend-reduction-relation r1 l2))
    
    ;; test that the domain is re-interpreted for the extended reduction-relation
    (test (apply-reduction-relation r2 3)
          '(3)))
  
  (let ()
    (define-language l1
      (D 0 1 2))
    (define r1
      (reduction-relation 
       l1
       #:domain (D D)
       (--> (D_1 D_2) (D_2 D_1))))
    
    ;; test that duplicated identifiers in the domain contract do not have to be equal
    (test (apply-reduction-relation r1 (term (1 2)))
          (list (term (2 1)))))
  
  ;;test that #:arrow keyword works
  (test (apply-reduction-relation 
         (reduction-relation 
          empty-language
          #:arrow :->
          (:-> 1 2))
         1)
        '(2))

  (let ()
    (define-language n-lang
      [n number])
    (test (apply-reduction-relation
           (reduction-relation n-lang [--> any ,(length (redex-match n-lang n 1))])
           11)
          '(1)))
  
  (test-syn-err (reduction-relation 
                 grammar
                 (~~> (number_1 number_2)
                      ,(* (term number_1) (term number_2)))
                 with
                 [(--> (M a) (M b)) (~~> a b)]
                 [(~~> (M a) (M b)) (==> a b)])
                #rx"no rules")
  
  (test-syn-err (reduction-relation 
                 grammar
                 (~~> (number_1 number_2)
                      ,(* (term number_1) (term number_2)))
                 with
                 [(--> (M a) (M b)) (~~> a b)]
                 [(~~> (M a) (M b)) (==> a b)])
                #rx"no rules")
  
  (test-syn-err (reduction-relation grammar)
                #rx"no rules use -->")
  
  (test-syn-err (reduction-relation 
                 grammar
                 (~~> (number_1 number_2)
                      ,(* (term number_1) (term number_2))))
                #rx"~~> relation is not defined")
  
  (test-syn-err (reduction-relation 
                 grammar
                 (--> (number_1 number_2) 
                      ,(* (term number_1) (term number_2))
                      mult)
                 (--> (number_1 number_2) 
                      ,(* (term number_1) (term number_2))
                      mult))
                #rx"same name on multiple rules"
                2)
  
  (test-syn-err (reduction-relation 
                 grammar
                 (--> 1 2)
                 (==> 3 4))
                #rx"==> relation is not defined")
  
  (test-syn-err  (reduction-relation 
                  grammar
                  (--> 1 2)
                  (==> 3 4)
                  with
                  [(~> a b) (==> a b)])
                 #rx"~> relation is not defined")
  
  (test-syn-err  (reduction-relation
                  grammar
                  (==> 1 2)
                  with
                  [(--> a b)
                   (==> a (+ 3 b))])
                 #rx"expected identifier")
  
  (test-syn-err  (reduction-relation
                  grammar
                  (==> 1 2)
                  with
                  [(--> a b)
                   (==> (+ 3 a) b)])
                 #rx"expected identifier")
  
  (test-syn-err (define-language bad-lang1 (e name)) #rx"name")
  (test-syn-err (define-language bad-lang2 (name x)) #rx"name")
  (test-syn-err (define-language bad-lang3 (x_y x)) #rx"cannot have _")
  (test-syn-err (define-language bad-lang4 (a 1 2) (b)) #rx"no productions")
  (test-syn-err (let ()
                  (define-language good-lang (a 1 2))
                  (define-extended-language bad-lang5 good-lang (a) (b 2)))
                #rx"no productions")
  
  (test-syn-err (redex-match grammar m_1) #rx"before underscore")
  (test-syn-err (redex-match grammar (variable-except a 2 c)) #rx"expected an identifier")
  (test-syn-err (redex-match grammar (variable-prefix 7)) #rx"expected an identifier")
  (test-syn-err (redex-match grammar (cross 7)) #rx"expected an identifier")
  
  ;; expect union with duplicate names to fail
  (test (with-handlers ((exn? (λ (x) 'passed)))
          (union-reduction-relations
           (reduction-relation 
            grammar
            (--> (number_1 number_2) 
                 ,(* (term number_1) (term number_2))
                 mult))
           (reduction-relation 
            grammar
            (--> (number_1 number_2) 
                 ,(* (term number_1) (term number_2))
                 mult)))
          'failed)
        'passed)
  
  (test (with-handlers ((exn? (λ (x) 'passed)))
          (union-reduction-relations
           (union-reduction-relations
            (reduction-relation 
             grammar
             (--> (number_1 number_2) 
                  ,(* (term number_1) (term number_2))
                  mult))
            (reduction-relation 
             grammar
             (--> (number_1 number_2) 
                  ,(* (term number_1) (term number_2))
                  mult3)))
           
           (union-reduction-relations
            (reduction-relation 
             grammar
             (--> (number_1 number_2) 
                  ,(* (term number_1) (term number_2))
                  mult))
            (reduction-relation 
             grammar
             (--> (number_1 number_2) 
                  ,(* (term number_1) (term number_2))
                  mult2))))
          'passed)
        'passed)
  
  ;; sorting in this test case is so that the results come out in a predictable manner.
  (test (sort
         (apply-reduction-relation
          (compatible-closure 
           (reduction-relation 
            grammar
            (--> (number_1 number_2) 
                 ,(* (term number_1) (term number_2))
                 mult))
           grammar
           M)
          '((2 3) (4 5)))
         (λ (x y) (string<=? (format "~s" x) (format "~s" y))))
        (list '((2 3) 20)
              '(6 (4 5))))
  
  (test (apply-reduction-relation
         (compatible-closure 
          (reduction-relation 
           grammar
           (--> (number_1 number_2) 
                ,(* (term number_1) (term number_2))
                mult))
          grammar
          M)
         '(4 2))
        (list '8))
  
  (test (apply-reduction-relation
         (context-closure 
          (context-closure 
           (reduction-relation grammar (--> 1 2))
           grammar
           (y hole))
          grammar
          (x hole))
         '(x (y 1)))
        (list '(x (y 2))))

  (test (apply-reduction-relation
         (reduction-relation 
          grammar
          (--> (variable_1 variable_2) 
               (variable_1 variable_2 x)
               mul
               (fresh x)))
         '(x x1))
        (list '(x x1 x2)))
  
  (test (apply-reduction-relation
         (reduction-relation 
          grammar
          (~~> number 
               x
               (fresh x))
          with 
          [(--> (variable_1 variable_2 a) (variable_1 variable_2 b)) (~~> a b)])
         '(x x1 2))
        (list '(x x1 x2)))
  
  (test (apply-reduction-relation
         (reduction-relation 
          x-language
          (--> (x_1 ...)
               (x ...)
               (fresh ((x ...) (x_1 ...)))))
         '(x y x1))
        (list '(x2 x3 x4)))

  (test (apply-reduction-relation
         (reduction-relation
          empty-language
          (--> (variable_1 ...)
               (x ... variable_1 ...)
               (fresh ((x ...) (variable_1 ...) (variable_1 ...)))))
         '(x y z))
        (list '(x1 y1 z1 x y z)))

  (test (apply-reduction-relation 
         (reduction-relation 
          empty-language
          (--> any (any_y x)
               (where any_y x)
               (fresh x)))
         (term junk))
        (list '(x x1)))
  
  (test (apply-reduction-relation 
         (reduction-relation 
          empty-language
          (--> (variable ...) (variable_0 ... variable_1 ...)
               (fresh ((variable_0 ...) (variable ...)))
               (fresh ((variable_1 ...) (variable ...)))))
         (term (x y)))
        (list '(variable_0 variable_1 variable_2 variable_3)))

  
  ;; test that redex match can be used in a side-condition 
  ;; with the same language that is used to define the 
  ;; reduction relation.
  (test (apply-reduction-relation
         (reduction-relation
          empty-language
          (--> any_1 3
               (side-condition (redex-match empty-language (any_1 any_2) (term any_1)))))
         '(a b))
        '(3))
  
  (test (apply-reduction-relation
         (reduction-relation
          empty-language
          (--> variable_1
               (x variable_1)
               (fresh (x variable_1))))
         'q)
        (list '(q1 q)))
  
  (test (apply-reduction-relation
         (extend-reduction-relation (reduction-relation empty-language (--> 1 2))
                                    empty-language
                                    (--> 1 3))
         1)
        '(3 2))
  
  (let ()
    (define-language e1
      (e 1))
    (define-language e2
      (e 2))
    (define red1 (reduction-relation e1 (--> e (e e))))
    (define red2 (extend-reduction-relation red1 e2 (--> ignoreme ignoreme)))
    (test (apply-reduction-relation red1 1) '((1 1)))
    (test (apply-reduction-relation red1 2) '())
    (test (apply-reduction-relation red2 1) '())
    (test (apply-reduction-relation red2 2) '((2 2))))
  
  (let ()
    (define red1 (reduction-relation empty-language
                                     (--> a (a a) 
                                          a)
                                     (--> b (b b) 
                                          b)
                                     (--> q x)))
    (define red2 (extend-reduction-relation red1
                                            empty-language
                                            (--> a (c c)
                                                 a)
                                            (--> q z)))
    (test (apply-reduction-relation red1 (term a)) (list (term (a a))))
    (test (apply-reduction-relation red1 (term b)) (list (term (b b))))
    (test (apply-reduction-relation red1 (term q)) (list (term x)))
    (test (apply-reduction-relation red2 (term a)) (list (term (c c))))
    (test (apply-reduction-relation red2 (term b)) (list (term (b b))))
    (test (apply-reduction-relation red2 (term q)) (list (term z) (term x))))
  
  (let ()
    (define red1 
      (reduction-relation
       empty-language
       (==> a (a a) 
            a)
       (==> b (b b) 
            b)
       (==> q w)
       with
       [(--> (X a) (X b)) (==> a b)]))
    
    (define red2 
      (extend-reduction-relation
       red1
       empty-language
       (==> a (c c)
            a)
       (==> q z)
       with
       [(--> (X a) (X b)) (==> a b)]))
    
    (test (apply-reduction-relation red1 (term (X a))) (list (term (X (a a)))))
    (test (apply-reduction-relation red1 (term (X b))) (list (term (X (b b)))))
    (test (apply-reduction-relation red1 (term (X q))) (list (term (X w))))
    (test (apply-reduction-relation red2 (term (X a))) (list (term (X (c c)))))
    (test (apply-reduction-relation red2 (term (X b))) (list (term (X (b b)))))
    (test (apply-reduction-relation red2 (term (X q))) (list (term (X z)) 
                                                             (term (X w)))))
  
  (test (reduction-relation->rule-names
         (reduction-relation
          empty-language
          (--> x y a)))
        '(a))
  
  (test (reduction-relation->rule-names
         (reduction-relation
          empty-language
          (--> x y a)
          (--> y z b)
          (--> z w c)))
        '(a b c))
  
  (test (reduction-relation->rule-names
         (reduction-relation
          empty-language
          (--> x y a)
          (--> y z b)
          (--> z w c)
          (--> p q z)
          (--> q r y)
          (--> r p x)))
        '(a b c z y x))
  
  (test (reduction-relation->rule-names
         (extend-reduction-relation
          (reduction-relation
           empty-language
           (--> x y a)
           (--> y z b)
           (--> z w c))
          empty-language
          (--> p q z)
          (--> q r y)
          (--> r p x)))
        '(a b c z y x))
  
    (test (reduction-relation->rule-names
           (union-reduction-relations
            (reduction-relation
             empty-language
             (--> x y a)
             (--> y z b)
             (--> z w c))
            (reduction-relation
             empty-language
             (--> p q z)
             (--> q r y)
             (--> r p x))))
        '(a b c z y x))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; examples from doc.txt
  ;;
  
  (define-language lc-lang
    (e (e e ...)
       x
       v)
    (c (v ... c e ...)
       hole)
    (v (lambda (x ...) e))
    (x variable-not-otherwise-mentioned))
  
  (test (let ([m (redex-match lc-lang e (term (lambda (x) x)))])
          (and m (length m)))
        1)
  
  (define-extended-language qabc-lang lc-lang (q a b c))
  
  (test (redex-match qabc-lang
                     e
                     (term (lambda (a) a)))
        #f)
  
  (test (let ([m (redex-match qabc-lang
                              e
                              (term (lambda (z) z)))])
          (and m (length m)))
        1)
  
  (require (lib "list.ss"))
  (let ()
    (define-metafunction lc-lang
      free-vars : e -> (x ...)
      [(free-vars (e_1 e_2 ...))
       (∪ (free-vars e_1) (free-vars e_2) ...)]
      [(free-vars x) (x)]
      [(free-vars (lambda (x ...) e))
       (- (free-vars e) (x ...))])
    
    (define-metafunction lc-lang
      ∪ : (x ...) ... -> (x ...)
      [(∪ (x_1 ...) (x_2 ...) (x_3 ...) ...)
       (∪ (x_1 ... x_2 ...) (x_3 ...) ...)]
      [(∪ (x_1 ...))
       (x_1 ...)]
      [(∪) ()])
    
    (define-metafunction lc-lang
      - : (x ...) (x ...) -> (x ...)
      [(- (x ...) ()) (x ...)]
      [(- (x_1 ... x_2 x_3 ...) (x_2 x_4 ...))
       (- (x_1 ... x_3 ...) (x_2 x_4 ...))
       (side-condition (not (memq (term x_2) (term (x_3 ...)))))]
      [(- (x_1 ...) (x_2 x_3 ...))
       (- (x_1 ...) (x_3 ...))])
    
    (test (term (∪)) (term ()))
    (test (term (∪ (x y) (z a) (b c))) (term (x y z a b c)))
    
    (test (term (- (x y) ())) (term (x y)))
    (test (term (- (x y) (x))) (term (y)))
    (test (term (- (y x) (x))) (term (y)))
    (test (term (- (x x x x x) (x))) (term ()))
    
    (test (term (free-vars (lambda (x) (x y)))) (list 'y))
    (test (term (free-vars (a (b (c (d e)))))) (term (a b c d e))))

  (test (variable-not-in (term (x y z)) 'x)
        (term x1))
  
  (test (variable-not-in (term (y z)) 'x)
        (term x))
  (test (variable-not-in (term (x x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)) 'x)
        (term x11))
  (test (variable-not-in (term (x x11)) 'x)
        (term x1))
  (test (variable-not-in (term (x x1 x2 x3)) 'x1)
        (term x4))
  (test (variable-not-in (term (x x1 x1 x2 x2)) 'x)
        (term x3))
  
  (test (variables-not-in (term (x y z)) '(x))
        '(x1))
  (test (variables-not-in (term (x2 y z)) '(x x x))
        '(x x1 x3))
  
  (test ((term-match/single empty-language
                            [(variable_x variable_y)
                             (cons (term variable_x)
                                   (term variable_y))])
         '(x y))
        '(x . y))
  
  (test ((term-match/single empty-language
                            [(side-condition (variable_x variable_y)
                                             (eq? (term variable_x) 'x))
                             (cons (term variable_x)
                                   (term variable_y))])
         '(x y))
        '(x . y))
  

  (test (with-handlers ((exn:fail:redex? (λ (x) 'right-exn))
                        ((λ (x) #t) (λ (x) 'wrong-exn)))
          ((term-match/single empty-language
                              [(number_1 ... number_2 ...) 1])
           '(1 2 3))
          'no-exn)
        'right-exn)
  
  (test (with-handlers ((exn:fail:redex? (λ (x) 'right-exn))
                        ((λ (x) #t) (λ (x) 'wrong-exn)))
          ((term-match/single empty-language
                              [(number_1 ... number_2 ...) 1])
           'x)
          'no-exn)
        'right-exn)
  
  (test ((term-match empty-language
                     [(number_1 ... number_2 ...) 1])
         'x)
        '())
                             
  (define-language x-is-1-language
    [x 1])
  
  (test ((term-match/single x-is-1-language
                            [(x x)
                             1])
         '(1 1))
        1)
  
  (test (call-with-values
         (λ () 
           ((term-match/single empty-language
                               [() (values 1 2)])
            '()))
         list)
        '(1 2))
  
  (test (let ([x 0])
          (cons ((term-match empty-language
                             [(any_a ... number_1 any_b ...)
                              (begin (set! x (+ x 1))
                                     (term number_1))])
                 '(1 2 3))
                x))
        '((3 2 1) . 3))
  
  (test ((term-match empty-language
                     [number_1
                      (term number_1)]
                     [number_1
                      (term number_1)])
         '1)
        '(1 1))
  
  (test (apply-reduction-relation
         (reduction-relation
          x-language
          (--> (x_one x_!_one x_!_one x_!_one)
               (x_one x_!_one)))
         (term (a a b c)))
        (list (term (a x_!_one))))
  
  ;; tests `where' clauses in reduction relation
  (test (apply-reduction-relation
         (reduction-relation empty-language
                             (--> number_1 
                                  any_y
                                  (where any_y ,(+ 1 (term number_1)))))
         3)
        '(4))
  
  ;; tests `where' clauses scoping
  (test (let ([x 5])
          (apply-reduction-relation
           (reduction-relation empty-language
                               (--> any 
                                    any_z
                                    (where any_y ,x)
                                    (where any_x 2)
                                    (where any_z ,(+ (term any_y) (term any_x)))))
           'whatever))
        '(7))
  
  ;; tests `where' clauses bind in side-conditions
  (test (let ([x 'unk])
          (apply-reduction-relation
           (reduction-relation empty-language
                               (--> any 
                                    the-result
                                    (where any_y any)
                                    (side-condition (eq? (term any_y) 'whatever))))
           'whatever))
        '(the-result))
  
  ;; test that where clauses bind in side-conditions that follow
  (let ([save1 #f]
        [save2 #f])
    (term-let ([any_y (term outer-y)])
              (test (begin (apply-reduction-relation
                            (reduction-relation empty-language
                                                (--> number_1 
                                                     any_y
                                                     (side-condition (set! save1 (term any_y)))
                                                     (where any_y inner-y)
                                                     (side-condition (set! save2 (term any_y)))))
                            3)
                           (list save1 save2))
                    (list 'outer-y 'inner-y))))
  
  (test (apply-reduction-relation
         (reduction-relation empty-language
                             (--> any 
                                  any_y
                                  (fresh x)
                                  (where any_y x)))
         'x)
        '(x1))

  (let ()
    ;; tests where's ability to have redex patterns, not just syntax-case patterns
    (define-language var-lang [(x y z w) variable])
    
    (define red
      (reduction-relation
       var-lang
       (--> (x ...)
            (y ... z ...)
            (where (y ... w z ...) (x ...)))))
    
    (test (apply-reduction-relation red (term (a b c)))
          (list (term (a b)) (term (a c)) (term (b c)))))
  
  
  (let ([r (reduction-relation
            grammar
            (->1 1 2)
            (->2 3 4)
            (->4 5 6)
            with
            [(--> (side-condition (a number) (even? (term number))) b)
             (->1 a b)]
            [(--> (X 
                   (number number)
                   (X_1 X_1)
                   (M_!_1 M_!_1)
                   (1 ..._1 1 ..._1)
                   (1 ..._!_1 1 ..._!_1))
                  b)
             (->2 X b)]
            [(--> (a 1) b)
             (->3 a b)]
            [(->3 (a 2) b)
             (->4 a b)])])
    
    ; test that names are properly bound for side-conditions in shortcuts
    (let* ([lhs ((rewrite-proc-lhs (first (reduction-relation-make-procs r))) grammar)]
           [proc (third lhs)]
           [name (cadadr lhs)]
           [bind (λ (n) (make-bindings (list (make-bind name n))))])
      (test (and (proc (bind 4)) (not (proc (bind 3)))) #t))
    
    ; test binder renaming
    (let ([sym-mtch? (λ (rx) (λ (s) (and (symbol? s) (regexp-match? rx (symbol->string s)))))])
      (match (rewrite-proc-lhs (second (reduction-relation-make-procs r)))
        [`(3
           (,(and n1 (? (sym-mtch? #px"^number_\\d+$"))) ,n1)
           (,(and n2 (? (sym-mtch? #px"^X_1\\d+$"))) ,n2)
           (,(and m1 (? (sym-mtch? #px"^M_!_1\\d+$"))) ,m1)
           (1 ,(and ...1 (? (sym-mtch? #px"^\\.\\.\\._1\\d+$"))) 1 ,...1)
           (1 ,(and ...!1 (? (sym-mtch? #px"^\\.\\.\\._!_1\\d+$"))) 1 ,...!1))
         #t]
        [else #f]))
    
    ; test shortcut in terms of shortcut
    (test (match ((rewrite-proc-lhs (third (reduction-relation-make-procs r))) grammar)
            [`(((side-condition 5 ,(? procedure?) ,_) 2) 1) #t]
            [else #f])
          #t))
  
  (let ([< (λ (c d) (string<? (car c) (car d)))])
    
    (let* ([R (reduction-relation
               empty-language
               (--> number (q ,(add1 (term number)))
                    (side-condition (odd? (term number)))
                    side-condition)
               (--> 1 4 plain)
               (==> 2 t
                    shortcut)
               with
               [(--> (q a) b)
                (==> a b)])]
           [c (make-coverage R)])
      (parameterize ([relation-coverage (list c)])
        (apply-reduction-relation R 4)
        (test (sort (covered-cases c) <)
              '(("plain" . 0) ("shortcut" . 0) ("side-condition" . 0)))
        
        (apply-reduction-relation R 3)
        (test (sort (covered-cases c) <)
              '(("plain" . 0) ("shortcut" . 0) ("side-condition" . 1)))
        
        (apply-reduction-relation* R 1)
        (test (sort (covered-cases c) <)
              '(("plain" . 1) ("shortcut" . 1) ("side-condition" . 2)))))
    
    (let* ([S (reduction-relation
               empty-language
               (--> 1 1 uno))]
           [S+ (extend-reduction-relation
                S empty-language
                (--> 2 2 dos))])
      (let ([c (make-coverage S+)])
        (parameterize ([relation-coverage (list c)])
          (apply-reduction-relation S (term 1))
          (test (sort (covered-cases c) <)
                '(("dos" . 0) ("uno" . 1)))))
      (let ([c (make-coverage S)])
        (parameterize ([relation-coverage (list c)])
          (apply-reduction-relation S+ (term 1))
          (test (sort (covered-cases c) <)
                '(("uno" . 1))))))
    
    (let* ([T (reduction-relation empty-language (--> any any))]
           [c (make-coverage T)])
      (parameterize ([relation-coverage (list c)])
        (apply-reduction-relation T (term q))
        (test (and (regexp-match #px"tl-test.(?:.+):\\d+:\\d+" (caar (covered-cases c))) #t)
              #t))))
  
  (let* ([R (reduction-relation
             empty-language
             (--> any any id))]
         [c (make-coverage R)]
         [c* (make-coverage R)])
    (parameterize ([relation-coverage (list c c*)])
      (apply-reduction-relation R 4)
      (test (covered-cases c) '(("id" . 1)))
      (test (covered-cases c*) '(("id" . 1)))))
  
  (let* ([< (λ (c d) 
              (let ([line-no (compose
                              string->number
                              second
                              (curry regexp-match #px".*:(\\d+):\\d+"))])
                (< (line-no (car c)) (line-no (car d)))))]
         [src-ok? (curry regexp-match? #px"tl-test.(?:.+):\\d+:\\d+")]
         [sorted-counts (λ (cc) (map cdr (sort (covered-cases cc) <)))])
    (define-metafunction empty-language
      [(f 1) 1]
      [(f 2) 2])
    (define-metafunction/extension f empty-language
      [(g 3) 3])
    (define-relation empty-language
      [(R number)
       ,(even? (term number))]
      [(R number)
       ,(= 3 (term number))])
    
    (let ([fc (make-coverage f)]
          [rc (make-coverage (reduction-relation empty-language (--> any any)))])
      (parameterize ([relation-coverage (list rc fc)])
        (term (f 2))
        (test (andmap (compose src-ok? car) (covered-cases fc))
              #t)
        (test (sorted-counts fc) '(0 1))
        
        (term (f 1))
        (term (f 1))
        (test (sorted-counts fc) '(2 1))))
    
    (let ([c (make-coverage f)])
      (parameterize ([relation-coverage (list c)])
        (term (g 1))
        (test (sorted-counts c) '(1 0))))
    (let ([c (make-coverage g)])
      (parameterize ([relation-coverage (list c)])
        (term (f 1))
        (test (sorted-counts c) '(1 0 0))))
    
    (let ([c (make-coverage R)])
      (parameterize ([relation-coverage (list c)])
        (term (R 2))
        (term (R 3))
        (term (R 5))
        (test (sorted-counts c) '(1 1))))
    
    (let ([c (make-coverage f)]
          [c* (make-coverage f)])
      (parameterize ([relation-coverage (list c* c)])
        (term (f 1))
        (test (sorted-counts c) '(1 0))
        (test (sorted-counts c*) '(1 0)))))

;                                                                       
;                                                                       
;                                                                       
;                                                                       
;                                                 ;;;                   
;   ;;                ;;           ;             ;; ;;                  
;   ;;                ;;            ;            ;; ;;                  
;  ;;;;;  ;;;    ;;; ;;;;;           ;;           ;;;      ;;;;   ;;;;  
;   ;;   ;; ;;  ;; ;  ;;  ;;;;;;;;     ;;        ;;;      ;; ;   ;;  ;; 
;   ;;   ;;;;;  ;;;   ;;              ;;;       ;;  ;  ;; ;;     ;;  ;; 
;   ;;   ;;       ;;  ;;            ;;;         ;;  ;;;;  ;;     ;;  ;; 
;   ;;   ;;  ;  ; ;;  ;;           ;;           ;;   ;;   ;; ;   ;;  ;; 
;   ;;;;  ;;;   ;;;   ;;;;         ;             ;;;;; ;;  ;;;;   ;;;;  
;                                                                       
;                                                                       
;                                                                       

  
  (define-syntax-rule 
    (capture-output arg1 args ...)
    (let ([p (open-output-string)])
      (parameterize ([current-output-port p]
                     [current-error-port p])
        arg1 args ...)
      (get-output-string p)))
  
  (let ()
    (define red (reduction-relation empty-language (--> 1 2)))
    (test (capture-output (test-->> red 1 2) (test-results))
          "One test passed.\n")
    (test (capture-output (test-->> red 2 3) (test-results))
          #rx"FAILED tl-test.(?:.+):[0-9.]+\nexpected: 3\n  actual: 2\n1 test failed \\(out of 1 total\\).\n"))
    
  (let ()
    (define red-share (reduction-relation 
                       empty-language
                       (--> a b)
                       (--> a c)
                       (--> c d)
                       (--> b d)))
    (test (capture-output (test-->> red-share (term a) (term d)) (test-results))
          "One test passed.\n"))
  
  (let ()
    (define red-cycle (reduction-relation 
                       empty-language
                       (--> a a)))
    (test (capture-output (test-->> red-cycle #:cycles-ok (term a)) (test-results))
          "One test passed.\n")
    (test (capture-output (test-->> red-cycle (term a)) (test-results))
          #rx"FAILED tl-test.(?:.+):[0-9.]+\nfound a cycle in the reduction graph\n1 test failed \\(out of 1 total\\).\n"))
  
  (let ()
    (define-metafunction empty-language [(f any) ((any))])
    (test (capture-output (test-equal (term (f 1)) (term ((1))))
                          (test-results))
          "One test passed.\n"))
  
  (let ()
    (test (capture-output (test-predicate odd? 1)
                          (test-results))
          "One test passed.\n"))
  
  (let ()
    (define red (reduction-relation empty-language (--> any (any))))
    (test (capture-output (test--> red (term (1 2 3)) (term ((1 2 3)))) (test-results))
          "One test passed.\n"))
  
  (let ()
    (define red (reduction-relation empty-language 
                                    (--> any (any))
                                    (--> (any) any)))
    (test (capture-output (test--> red (term (x)) (term ((x))) (term x)) (test-results))
          "One test passed.\n")
    (test (capture-output (test--> red (term (x)) (term x) (term ((x)))) (test-results))
          "One test passed.\n"))
  
  (let ()
    (define-language L
      (i integer))
    
    (define R
      (reduction-relation
       L
       (--> i i)
       (--> i ,(add1 (term i)))))
    
    (define (mod2=? i j)
      (= (modulo i 2) (modulo j 2)))
    
    (test (capture-output (test--> R #:equiv mod2=? 7 1 0) (test-results))
          "One test passed.\n")
    (test (capture-output (test--> R #:equiv mod2=? 7 1) (test-results))
          #rx"FAILED tl-test.(?:.+):[0-9.]+\nexpected: 1\n  actual: 8\n  actual: 7\n1 test failed \\(out of 1 total\\).\n"))
  
  (let-syntax ([test-bad-equiv-arg
                (λ (stx)
                  (syntax-case stx ()
                    [(_ test-form)
                     #'(test (with-handlers ([exn:fail:contract? exn-message])
                               (test-form (reduction-relation empty-language (--> any any))
                                          #:equiv 1 2)
                               "no error raised")
                             #rx"expected argument of type")]))])
    (test-bad-equiv-arg test-->)
    (test-bad-equiv-arg test-->>))

  (print-tests-passed 'tl-test.ss))
