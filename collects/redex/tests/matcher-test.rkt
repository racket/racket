(module matcher-test mzscheme
  (require "../private/matcher.ss"
           (only "test-util.ss" equal/bindings?)
           (lib "list.ss"))
  
  (error-print-width 500)
  
  (define (make-test-mtch a b c) (make-mtch a (build-flat-context b) c))
  
  (define (test)
    (print-struct #t)
    (test-empty 'any 1 (list (make-test-mtch (make-bindings (list (make-bind 'any 1))) 1 none)))
    (test-empty 'any 'true (list (make-test-mtch (make-bindings (list (make-bind 'any 'true))) 'true none)))
    (test-empty 'any "a" (list (make-test-mtch (make-bindings (list (make-bind 'any "a"))) "a" none)))
    (test-empty 'any '(a b) (list (make-test-mtch (make-bindings (list (make-bind 'any '(a b)))) '(a b) none)))
    (test-empty 'any #t (list (make-test-mtch (make-bindings (list (make-bind 'any #t))) #t none)))
    (test-empty 1 1 (list (make-test-mtch (make-bindings null) 1 none)))
    (test-empty 1 '() #f)
    (test-empty 99999999999999999999999999999999999999999999999
                99999999999999999999999999999999999999999999999
                (list (make-test-mtch (make-bindings null) 
                                 99999999999999999999999999999999999999999999999
                                 none)))
    (test-empty 99999999999999999999999999999999999999999999999
                '()
                #f)
    (test-empty 'x 'x (list (make-test-mtch (make-bindings null) 'x none)))
    (test-empty 'x '() #f)
    (test-empty 1 2 #f)
    (test-empty "a" "b" #f)
    (test-empty "a" '(x) #f)
    (test-empty "a" '() #f)
    (test-empty "a" "a" (list (make-test-mtch (make-bindings null) "a" none)))
    (test-empty 'number 1 (list (make-test-mtch (make-bindings (list (make-bind 'number 1))) 1 none)))
    (test-empty 'number 'x #f)
    (test-empty 'number '() #f)
    (test-empty 'natural 1 (list (make-test-mtch (make-bindings (list (make-bind 'natural 1))) 1 none)))
    (test-empty 'natural 'x #f)
    (test-empty 'natural '() #f)
    (test-empty 'natural -1 #f)
    (test-empty 'natural 1.0 #f)
    (test-empty 'integer -1 (list (make-test-mtch (make-bindings (list (make-bind 'integer -1))) -1 none)))
    (test-empty 'integer 'x #f)
    (test-empty 'integer '() #f)
    (test-empty 'integer 1.0 #f)
    (test-empty 'real 1.1 (list (make-test-mtch (make-bindings (list (make-bind 'real 1.1))) 1.1 none)))
    (test-empty 'real 'x #f)
    (test-empty 'real '() #f)
    (test-empty 'real 2+3i #f)
    (test-empty 'string "a" (list (make-test-mtch (make-bindings (list (make-bind 'string "a"))) "a" none)))
    (test-empty 'string 1 #f)
    (test-empty 'string '() #f)
    (test-empty 'variable 'x (list (make-test-mtch (make-bindings (list (make-bind 'variable 'x))) 'x none)))
    (test-empty 'variable 1 #f)
    (test-empty '(variable-except x) 1 #f)
    (test-empty '(variable-except x) 'x #f)
    (test-empty '(variable-except x) 'y (list (make-test-mtch (make-bindings null) 'y none)))
    (test-lang 'x 'y (list (make-mtch (make-bindings (list (make-bind 'x 'y))) 'y none))
               (list (make-nt 'x (list (make-rhs '(variable-except x))))))
    (test-empty '(variable-prefix x:) 'x: (list (make-test-mtch (make-bindings null) 'x: none)))
    (test-empty '(variable-prefix x:) 'x:x (list (make-test-mtch (make-bindings null) 'x:x none)))
    (test-empty '(variable-prefix x:) ': #f)
    (test-empty '(variable-prefix x:) '() #f)
    
    (test-empty 'hole 1 #f)
    (test-empty `hole
                the-hole
                (list (make-test-mtch (make-bindings (list)) the-hole none)))
    (test-empty '(in-hole (hole 2) 1)
                '(1 2)
                (list (make-test-mtch (make-bindings (list)) `(1 2) none)))
    
    (test-empty '(in-hole (name E_1 ((hide-hole hole) hole)) x)
                `(,the-hole x)
                (list (make-test-mtch (make-bindings (list (make-bind 'E_1 `(,the-not-hole ,the-hole)))) 
                                      `(,the-hole x)
                                      none)))
    

    
    (test-empty '(name x number) 1 (list (make-test-mtch (make-bindings (list (make-bind 'x 1) (make-bind 'number 1))) 1 none)))
    (test-empty 'number_x 1 (list (make-test-mtch (make-bindings (list (make-bind 'number_x 1))) 1 none)))
    (test-empty 'string_y "b" (list (make-test-mtch (make-bindings (list (make-bind 'string_y "b"))) "b" none)))
    (test-empty 'any_z '(a b) (list (make-test-mtch (make-bindings (list (make-bind 'any_z '(a b)))) '(a b) none)))
    
    (test-empty '(name x_!_1 number) 1 (list (make-test-mtch (make-bindings (list (make-bind 'number 1))) 1 none)))
    (test-empty '((name x_!_1 number) (name x_!_1 number)) '(1 1) #f)
    (test-empty '((name x_!_1 number_a) (name x_!_1 number_b)) '(1 2) 
                (list (make-test-mtch (make-bindings (list (make-bind 'number_a 1)
                                                           (make-bind 'number_b 2)))
                                      '(1 2) 
                                      none)))
    (test-empty '(number_!_1 number_!_1) '(1 1) #f)
    (test-empty '(number_!_1 number_!_1) '(1 2) (list (make-test-mtch (make-bindings (list)) '(1 2) none)))
    (test-empty '(number_!_1 ...) '(1 2) (list (make-test-mtch (make-bindings (list)) '(1 2) none)))
    (test-empty '(number_!_1 ...) '(1 2 3 4 5) (list (make-test-mtch (make-bindings (list)) '(1 2 3 4 5) none)))
    (test-empty '(number_!_1 ...) '(1 2 3 1 5) (list (make-test-mtch (make-bindings (list)) '(1 2 3 1 5) none)))
    (test-empty '((number_!_1 ...) (number_!_1 ...)) 
                '((1 2 3 1 5) (1 2 3 1 5))
                #f)
    (test-empty '((number_!_1 ...) (number_!_1 ...)) 
                '((17 2 3 1 5) (1 2 3 1 5))
                (list (make-test-mtch (make-bindings (list)) '((17 2 3 1 5) (1 2 3 1 5)) none)))
    (test-empty '((number_!_1 number_!_1) ... number_!_1 ...) '((1 1) (2 2) 1 3) #f)
    (test-empty '((number_!_1 number_!_1) ... number_!_1 ...) '((1 1) (2 3) 1 2) #f)
    (test-empty '((number_!_1 number_!_1) ... number_!_1 ...)
                '((1 1) (2 3) 1 4)
                (list (make-test-mtch (make-bindings (list)) '((1 1) (2 3) 1 4) none)))
    
    (test-ellipses '(a) '(a))
    (test-ellipses '(a ...) `(,(make-repeat 'a '() #f #f)))
    (test-ellipses '((a ...) ...) `(,(make-repeat '(a ...) '() #f #f)))
    (test-ellipses '(a ... b c ...) `(,(make-repeat 'a '() #f #f) b ,(make-repeat 'c '() #f #f)))
    (test-ellipses '((name x a) ...) `(,(make-repeat '(name x a) (list (make-bind 'x '())) #f #f))) 
    (test-ellipses '((name x (a ...)) ...)
                   `(,(make-repeat '(name x (a ...)) (list (make-bind 'x '())) #f #f)))
    (test-ellipses '(((name x a) ...) ...)
                   `(,(make-repeat '((name x a) ...) (list (make-bind 'x '())) #f #f)))
    (test-ellipses '((1 (name x a)) ...)
                   `(,(make-repeat '(1 (name x a)) (list (make-bind 'x '())) #f #f)))
    (test-ellipses '((any (name x a)) ...)
                   `(,(make-repeat '(any (name x a)) (list (make-bind 'any '())
                                                           (make-bind 'x '())) 
                                   #f #f)))
    (test-ellipses '((number (name x a)) ...)
                   `(,(make-repeat '(number (name x a)) (list (make-bind 'number '())
                                                              (make-bind 'x '())) 
                                   #f #f)))
    (test-ellipses '((variable (name x a)) ...)
                   `(,(make-repeat '(variable (name x a)) (list (make-bind 'variable '())
                                                                (make-bind 'x '()))
                                   #f #f)))
    (test-ellipses '(((name x a) (name y b)) ...)
                   `(,(make-repeat '((name x a) (name y b)) (list (make-bind 'x '()) (make-bind 'y '())) #f #f)))
    (test-ellipses '((name x (name y b)) ...)
                   `(,(make-repeat '(name x (name y b)) (list (make-bind 'x '()) (make-bind 'y '())) #f #f)))
    (test-ellipses '((in-hole (name x a) (name y b)) ...)
                   `(,(make-repeat '(in-hole (name x a) (name y b)) 
                                   (list (make-bind 'y '()) (make-bind 'x '())) #f #f)))
    
    (test-ellipses '(a ..._1)
                   `(,(make-repeat 'a (list) '..._1 #f)))
    (test-ellipses '(a ..._!_1)
                   `(,(make-repeat 'a (list) '..._!_1 #t)))
    
    (test-empty '() '() (list (make-test-mtch (make-bindings null) '() none)))
    (test-empty '(a) '(a) (list (make-test-mtch (make-bindings null) '(a) none)))
    (test-empty '(a) '(b) #f)
    (test-empty '(a b) '(a b) (list (make-test-mtch (make-bindings null) '(a b) none)))
    (test-empty '(a b) '(a c) #f)
    (test-empty '() 1 #f)
    (test-empty '(#f x) '(#f x) (list (make-test-mtch (make-bindings null) '(#f x) none)))
    (test-empty '(#f (name y any)) '(#f) #f)
    (test-empty '(in-hole (z hole) a) '(z a) (list (make-test-mtch (make-bindings (list)) '(z a) none)))
    (test-empty '(in-hole (z hole) (in-hole (x hole) a)) 
                '(z (x a))
                (list (make-test-mtch (make-bindings (list)) '(z (x a)) none)))
    
    (run-test/cmp 'in-hole-zero-holes 
                  (with-handlers ([exn:fail? (λ (e) (regexp-match #rx"zero holes" (exn-message e)))])
                    (test-empty '(in-hole (1 2) 2) '(1 2) 'never-gets-here)
                    'should-have-raised-an-exception)
                  '("zero holes")
                  equal?)
                
    
    (test-empty '(in-hole (in-hole (x hole) hole) y)
                '(x y)
                (list (make-test-mtch (make-bindings (list)) '(x y) none)))
    
    (test-empty '(number number) '(1 1) (list (make-test-mtch (make-bindings (list (make-bind 'number 1))) '(1 1) none)))
    (test-empty '((name x number) (name x number)) '(1 1) (list (make-test-mtch (make-bindings (list (make-bind 'x 1) (make-bind 'number 1))) '(1 1) none)))
    (test-empty '((name x number_q) (name x number_r)) '(1 1) (list (make-test-mtch (make-bindings (list (make-bind 'x 1) 
                                                                                                         (make-bind 'number_q 1)
                                                                                                         (make-bind 'number_r 1)))
                                                                                    '(1 1)
                                                                                    none)))
    (test-empty '(number number) '(1 2) #f)
    (test-empty '((name x number) (name x number)) '(1 2) #f)
    (test-empty '((name x number_q) (name x number_r)) '(1 2) #f)
    
    (test-empty '(a ...) '() (list (make-test-mtch (make-bindings empty) '() none)))
    (test-empty '(a ...) '(a) (list (make-test-mtch (make-bindings empty) '(a) none)))
    (test-empty '(a ...) '(a a) (list (make-test-mtch (make-bindings empty) '(a a) none)))
    (test-empty '((name x a) ...) '() (list (make-test-mtch (make-bindings (list (make-bind 'x '()))) '() none)))
    (test-empty '((name x a) ...) '(a) (list (make-test-mtch (make-bindings (list (make-bind 'x '(a)))) '(a) none)))
    (test-empty '((name x a) ...) '(a a) (list (make-test-mtch (make-bindings (list (make-bind 'x '(a a)))) '(a a) none)))
    (test-empty '(hole ...) '() (list (make-test-mtch (make-bindings empty) '() none)))
    
    (test-empty '(b ... a ...) '() (list (make-test-mtch (make-bindings empty) '() none)))
    (test-empty '(b ... a ...) '(a) (list (make-test-mtch (make-bindings empty) '(a) none)))
    (test-empty '(b ... a ...) '(b) (list (make-test-mtch (make-bindings empty) '(b) none)))
    (test-empty '(b ... a ...) '(b a) (list (make-test-mtch (make-bindings empty) '(b a) none)))
    (test-empty '(b ... a ...) '(b b a a) (list (make-test-mtch (make-bindings empty) '(b b a a) none)))
    (test-empty '(b ... a ...) '(a a) (list (make-test-mtch (make-bindings empty) '(a a) none)))
    (test-empty '(b ... a ...) '(b b) (list (make-test-mtch (make-bindings empty) '(b b) none)))
    
    (test-empty '(a ..._1 a ..._2) 
                '(a) 
                (list (make-test-mtch (make-bindings (list (make-bind '..._1 1) (make-bind '..._2 0))) '(a) none)
                      (make-test-mtch (make-bindings (list (make-bind '..._1 0) (make-bind '..._2 1))) '(a) none)))
    (test-empty '(a ..._1 a ..._1) '(a) #f)
    (test-empty '(a ..._1 a ..._1)
                '(a a) 
                (list (make-test-mtch (make-bindings (list (make-bind '..._1 1))) '(a a) none)))

    (test-empty '((name x a) ..._!_1 (name y a) ..._!_1) 
                '(a a) 
                (list (make-test-mtch (make-bindings (list (make-bind 'x '()) (make-bind 'y '(a a)))) '(a a) none)
                      (make-test-mtch (make-bindings (list (make-bind 'x '(a a)) (make-bind 'y '()))) '(a a) none)))
    
    (test-empty '((name y b) ... (name x a) ...) '() 
                (list (make-test-mtch (make-bindings (list (make-bind 'x '())
                                                      (make-bind 'y '())))
                                 '()
                                 none)))
    (test-empty '((name y b) ... (name x a) ...) '(a)
                (list (make-test-mtch (make-bindings (list (make-bind 'x '(a))
                                                      (make-bind 'y '())))
                                 '(a)
                                 none)))
    (test-empty '((name y b) ... (name x a) ...) '(b) 
                (list (make-test-mtch (make-bindings (list (make-bind 'x '())
                                                      (make-bind 'y '(b))))
                                 '(b)
                                 none)))
    (test-empty '((name y b) ... (name x a) ...) '(b b a a) 
                (list (make-test-mtch (make-bindings (list (make-bind 'x '(a a))
                                                      (make-bind 'y '(b b))))
                                 '(b b a a)
                                 none)))
    (test-empty '((name y a) ... (name x a) ...) '(a) 
                (list (make-test-mtch (make-bindings (list (make-bind 'x '())
                                                      (make-bind 'y '(a))))
                                 '(a)
                                 none)
                      (make-test-mtch (make-bindings (list (make-bind 'x '(a))
                                                      (make-bind 'y '())))
                                 '(a)
                                 none)))
    (test-empty '((name y a) ... (name x a) ...) '(a a) 
                (list (make-test-mtch (make-bindings (list (make-bind 'x '())
                                                      (make-bind 'y '(a a))))
                                 '(a a)
                                 none)
                      (make-test-mtch (make-bindings (list (make-bind 'x '(a))
                                                      (make-bind 'y '(a))))
                                 '(a a)
                                 none)
                      (make-test-mtch (make-bindings (list (make-bind 'x '(a a))
                                                      (make-bind 'y '())))
                                 '(a a)
                                 none)))
    
    (test-ab '(bb_y ... aa_x ...) '() 
             (list (make-test-mtch (make-bindings (list (make-bind 'aa_x '())
                                                        (make-bind 'bb_y '())))
                                   '()
                                   none)))
    (test-ab '(bb_y ... aa_x ...) '(a)
             (list (make-test-mtch (make-bindings (list (make-bind 'aa_x '(a))
                                                   (make-bind 'bb_y '())))
                              '(a) 
                              none)))
    (test-ab '(bb_y ... aa_x ...) '(b) 
             (list (make-test-mtch (make-bindings (list (make-bind 'aa_x '())
                                                   (make-bind 'bb_y '(b))))
                              '(b)
                              none)))
    (test-ab '(bb_y ... aa_x ...) '(b b a a) 
             (list (make-test-mtch (make-bindings (list (make-bind 'aa_x '(a a))
                                                   (make-bind 'bb_y '(b b))))
                              '(b b a a)
                              none)))
    (test-ab '(aa_y ... aa_x ...) '(a) 
             (list (make-test-mtch (make-bindings (list (make-bind 'aa_x '())
                                                   (make-bind 'aa_y '(a))))
                              '(a)
                              none)
                   (make-test-mtch (make-bindings (list (make-bind 'aa_x '(a))
                                                   (make-bind 'aa_y '())))
                              '(a)
                              none)))
    (test-ab '(aa_y ... aa_x ...) '(a a) 
             (list (make-test-mtch (make-bindings (list (make-bind 'aa_x '())
                                                   (make-bind 'aa_y '(a a))))
                              '(a a)
                              none)
                   (make-test-mtch (make-bindings (list (make-bind 'aa_x '(a))
                                                   (make-bind 'aa_y '(a))))
                              '(a a)
                              none)
                   (make-test-mtch (make-bindings (list (make-bind 'aa_x '(a a))
                                                   (make-bind 'aa_y '())))
                              '(a a)
                              none)))
    
    (test-empty '((name x number) ...) '(1 2) (list (make-test-mtch (make-bindings (list (make-bind 'x '(1 2)) (make-bind 'number '(1 2)))) '(1 2) none)))
    
    (test-empty '(a ...) '(b) #f)
    (test-empty '(a ... b ...) '(c) #f)
    (test-empty '(a ... b) '(b c) #f)
    (test-empty '(a ... b) '(a b c) #f)
    
    (test-empty '((name x any) 
                  ((name x number) ...))
                '((1 1) (1 1))
                (list (make-test-mtch (make-bindings (list (make-bind 'x '(1 1))
                                                           (make-bind 'any '(1 1))
                                                           (make-bind 'number '(1 1))))
                                      '((1 1) (1 1)) 
                                      none)))
    
    (test-empty '((variable_1 variable_1) ...)
                '((x y))
                #f)
    
    
    (test-empty '(number ...) '()
                (list (make-test-mtch (make-bindings (list (make-bind 'number '()))) '() none)))
    (test-ab '(aa ...) '()
             (list (make-test-mtch (make-bindings (list (make-bind 'aa '()))) '() none)))
    
    
    ;; testing block-in-hole
    (test-empty '(hide-hole a) 'b #f)
    (test-empty '(hide-hole a) 'a (list (make-test-mtch (make-bindings '()) 'a none)))
    (test-empty '(hide-hole a) '(block-in-hole a) #f)
    (test-empty '(in-hole (x (hide-hole hole)) 1) '(x 1) #f)
    (test-empty '(in-hole (x hole) 1) '(x 1) (list (make-test-mtch (make-bindings '()) '(x 1) none)))
    (test-empty '(in-hole ((hole #f) (hide-hole hole)) junk)
                '(junk junk2)
                #f)
    
    (test-xab 'lsts '() (list (make-test-mtch (make-bindings (list (make-bind 'lsts '()))) '() none)))
    (test-xab 'lsts '(x) (list (make-test-mtch (make-bindings (list (make-bind 'lsts '(x)))) '(x) none)))
    (test-xab 'lsts 'x (list (make-test-mtch (make-bindings (list (make-bind 'lsts 'x))) 'x none)))
    (test-xab 'lsts #f (list (make-test-mtch (make-bindings (list (make-bind 'lsts #f))) #f none)))
    (test-xab 'split-out '1 (list (make-test-mtch (make-bindings (list (make-bind 'split-out 1))) '1 none)))

    (test-xab 'exp 1 (list (make-test-mtch (make-bindings (list (make-bind 'exp 1))) 1 none)))
    (test-xab 'exp '(+ 1 2) (list (make-test-mtch (make-bindings (list (make-bind 'exp '(+ 1 2)))) '(+ 1 2) none)))
    (test-xab '(in-hole ctxt any)
              '1
              (list (make-test-mtch (make-bindings (list (make-bind 'ctxt the-hole) (make-bind 'any 1))) 1 none)))
    (test-xab '(in-hole ctxt (name x any))
              '1
              (list (make-test-mtch (make-bindings (list (make-bind 'ctxt the-hole) (make-bind 'x 1) (make-bind 'any 1))) 1 none)))
    (test-xab '(in-hole (name c ctxt) (name x any))
              '(+ 1 2)
              (list (make-test-mtch (make-bindings (list (make-bind 'ctxt (build-context the-hole))
                                                         (make-bind 'c (build-context the-hole))
                                                         (make-bind 'x '(+ 1 2))
                                                         (make-bind 'any '(+ 1 2))))
                                    '(+ 1 2) none)
                    (make-test-mtch (make-bindings (list (make-bind 'ctxt (build-context `(+ ,the-hole 2)))
                                                         (make-bind 'c (build-context `(+ ,the-hole 2)))
                                                         (make-bind 'x 1)
                                                         (make-bind 'any 1)))
                                    '(+ 1 2) none)
                    (make-test-mtch (make-bindings (list (make-bind 'ctxt (build-context `(+ 1 ,the-hole)))
                                                         (make-bind 'c (build-context `(+ 1 ,the-hole)))
                                                         (make-bind 'x 2)
                                                         (make-bind 'any 2))) 
                                    '(+ 1 2) none)))
    (test-xab '(in-hole (name c ctxt) (name i (+ number_1 number_2)))
              '(+ (+ 1 2) (+ 3 4))
              (list (make-test-mtch 
                     (make-bindings (list (make-bind 'i '(+ 1 2))
                                          (make-bind 'number_1 1)
                                          (make-bind 'number_2 2)
                                          (make-bind 'ctxt (build-context `(+ ,the-hole (+ 3 4))))
                                          (make-bind 'c (build-context `(+ ,the-hole (+ 3 4))))))
                     '(+ (+ 1 2) (+ 3 4))
                     none)
                    (make-test-mtch (make-bindings (list (make-bind 'i '(+ 3 4)) 
                                                         (make-bind 'number_1 3)
                                                         (make-bind 'number_2 4)
                                                         (make-bind 'ctxt `(+ (+ 1 2) ,the-hole))
                                                         (make-bind 'c `(+ (+ 1 2) ,the-hole))))
                               '(+ (+ 1 2) (+ 3 4))
                               none)))
    
    (test-empty '(in-hole ((z hole)) (name x any))
                '((z a))
                (list (make-test-mtch (make-bindings (list (make-bind 'x 'a) (make-bind 'any 'a))) '((z a)) none)))
    (test-empty '(in-hole (name c (z ... hole z ...)) any)
                '(z z)
                (list 
                 (make-test-mtch (make-bindings (list (make-bind 'c `(z ,the-hole)) (make-bind 'any 'z))) '(z z) none)
                 (make-test-mtch (make-bindings (list (make-bind 'c `(,the-hole z)) (make-bind 'any 'z))) '(z z) none)))
    (test-empty '(in-hole (name c (z ... hole z ...)) any)
                '(z z z)
                (list 
                 (make-test-mtch (make-bindings (list (make-bind 'c `(z z ,the-hole)) (make-bind 'any 'z))) '(z z z) none)
                 (make-test-mtch (make-bindings (list (make-bind 'c `(z ,the-hole z)) (make-bind 'any 'z))) '(z z z) none)
                 (make-test-mtch (make-bindings (list (make-bind 'c `(,the-hole z z)) (make-bind 'any 'z))) '(z z z) none)))
    
    (test-empty '(z (in-hole (name c (z hole)) a))
                '(z (z a))
                (list 
                 (make-test-mtch (make-bindings (list (make-bind 'c `(z ,the-hole))))
                            '(z (z a))
                            none)))
    
    (test-empty '(a (in-hole (name c1 (b (in-hole (name c2 (c hole)) d) hole)) e))
                '(a (b (c d) e))
                (list 
                 (make-test-mtch (make-bindings (list (make-bind 'c2 `(c ,the-hole))
                                                 (make-bind 'c1 `(b (c d) ,the-hole))))
                            '(a (b (c d) e))
                            none)))

    (test-empty '(in-hole (in-hole hole hole) a)
                'a
                (list (make-test-mtch (make-bindings (list)) 'a none)))
    
    (test-empty '(a (b (in-hole (name c1 (in-hole (name c2 (c hole)) (d hole))) e)))
                '(a (b (c (d e))))
                (list 
                 (make-test-mtch (make-bindings (list (make-bind 'c1 `(c (d ,the-hole)))
                                                 (make-bind 'c2 `(c ,the-hole))))
                            '(a (b (c (d e))))
                            none)))
    
    (test-empty `(+ 1 (side-condition any ,(lambda (bindings) #t) #t))
                '(+ 1 b)
                (list (make-test-mtch (make-bindings (list (make-bind 'any 'b))) '(+ 1 b) none)))
    (test-empty `(+ 1 (side-condition any ,(lambda (bindings) #f) #f))
                '(+ 1 b)
                #f)
    
    (test-empty `(+ 1 (side-condition b ,(lambda (bindings) #t) #t))
                '(+ 1 b)
                (list (make-test-mtch (make-bindings '()) '(+ 1 b) none)))
    (test-empty `(+ 1 (side-condition a ,(lambda (bindings) #t)) #t)
                '(+ 1 b)
                #f)

    (test-empty `(side-condition (name x any) ,(lambda (bindings) (eq? (lookup-binding bindings 'x) 'a)) (eq? (term x) 'a))
                'a
                (list 
                 (make-test-mtch (make-bindings (list (make-bind 'x 'a)
                                                      (make-bind 'any 'a)))
                            'a
                            none)))

    (test-empty `(+ 1 (side-condition (name x any) ,(lambda (bindings) (eq? (lookup-binding bindings 'x) 'a)) (eq? (term x) 'a)))
                '(+ 1 a)
                (list 
                 (make-test-mtch (make-bindings (list (make-bind 'x 'a)
                                                      (make-bind 'any 'a)))
                            '(+ 1 a)
                            none)))

    (test-empty `(side-condition (name x any) ,(lambda (bindings) (eq? (lookup-binding bindings 'x) 'a)) (eq? (term x) 'a))
                'b
                #f)
    
    (test-empty `(+ 1 (side-condition (name x any) ,(lambda (bindings) (eq? (lookup-binding bindings 'x) 'a)) (eq? (term x) 'a)))
                '(+ 1 b)
                #f)
    
    (test-empty `(side-condition ((any_1 ..._a) (any_2 ..._a))
                                 ,(lambda (bindings) (error 'should-not-be-called))
                                 (error 'should-not-be-called))
                '((1 2 3) (4 5))
                #f)
    
    (test-xab 'exp_1
              '(+ 1 2)
              (list (make-test-mtch (make-bindings (list (make-bind 'exp_1 '(+ 1 2)))) '(+ 1 2) none)))
    (test-xab '(exp_1 exp_2)
              '((+ 1 2) (+ 3 4))
              (list (make-test-mtch (make-bindings (list (make-bind 'exp_1 '(+ 1 2)) (make-bind 'exp_2 '(+ 3 4))))
                               '((+ 1 2) (+ 3 4))
                               none)))
    (test-xab '(exp_1 exp_1)
              '((+ 1 2) (+ 3 4))
              #f)
    (test-xab 'nesting-names
              'b
              (list (make-test-mtch (make-bindings (list (make-bind 'nesting-names 'b))) 'b none)))
    (test-xab 'nesting-names
              '(a b)
              (list (make-test-mtch (make-bindings (list (make-bind 'nesting-names '(a b)))) '(a b) none)))
    (test-xab 'nesting-names
              '(a (a b))
              (list (make-test-mtch (make-bindings (list (make-bind 'nesting-names '(a (a b))))) '(a (a b)) none)))
    (test-xab '((name x a) nesting-names)
              '(a (a (a b)))
              (list (make-test-mtch (make-bindings (list (make-bind 'x 'a)
                                                         (make-bind 'nesting-names '(a (a b)))))
                                    '(a (a (a b))) none)))
    (test-xab 'nesting-names
              '(a (a (a (a b))))
              (list (make-test-mtch (make-bindings (list (make-bind 'nesting-names '(a (a (a (a b)))))))
                                    '(a (a (a (a b)))) none)))
    
    (test-xab 'same-in-nt
              '(x x)
              (list (make-test-mtch (make-bindings (list (make-bind 'same-in-nt '(x x)))) '(x x) none)))
    (test-xab 'same-in-nt
              '(x y)
              #f)
    
    (test-xab '(in-hole (cross forever-list) 1)
              '(a b c)
              #f)
    
    (test-xab '(in-hole (cross forever-list) 1)
              '(1 x x)
              (list (make-test-mtch (make-bindings '()) '(1 x x) none)))
    
    (test-xab '(in-hole (cross forever-list) 1)
              '(x 1 x)
              (list (make-test-mtch (make-bindings '()) '(x 1 x) none)))
    
    
    (test-xab '(in-hole (cross simple) g)
              'g
              (list (make-mtch (make-bindings (list)) 'g none)))
    
    (test-xab 'var '+ #f)
    (test-xab 'var 'anunusedvariable (list (make-mtch (make-bindings (list (make-bind 'var 'anunusedvariable))) 'anunusedvariable none)))
    (test-xab 'var 'exp (list (make-mtch (make-bindings (list (make-bind 'var 'exp))) 'exp none)))
    (test-xab 'var 'exp_x (list (make-mtch (make-bindings (list (make-bind 'var 'exp_x))) 'exp_x none)))
    
    (test-xab 'underscore '(+ 1 2) (list (make-mtch (make-bindings (list (make-bind 'underscore '(+ 1 2)))) '(+ 1 2) none)))
    (test-xab 'underscore '2 (list (make-mtch (make-bindings (list (make-bind 'underscore 2))) 2 none)))
    
    (run-test
     'compatible-context-language1
     (build-compatible-context-language
      (mk-hasheq '((exp . ()) (ctxt . ())))
      (list (make-nt 'exp
                     (list (make-rhs '(+ exp exp))
                           (make-rhs 'number)))
            (make-nt 'ctxt
                     (list (make-rhs '(+ ctxt exp))
                           (make-rhs '(+ exp ctxt))
                           (make-rhs 'hole)))))
     (list
      (make-nt 'ctxt-ctxt
               (list (make-rhs 'hole)
                     (make-rhs `(+ (cross ctxt-ctxt) exp))
                     (make-rhs `(+ ctxt (cross ctxt-exp)))
                     (make-rhs `(+ (cross ctxt-exp) ctxt))
                     (make-rhs `(+ exp (cross ctxt-ctxt)))))
      (make-nt 'ctxt-exp
               (list (make-rhs `(+ (cross ctxt-exp) exp))
                     (make-rhs `(+ exp (cross ctxt-exp)))))
      (make-nt 'exp-ctxt
               (list (make-rhs `(+ (cross exp-ctxt) exp))
                     (make-rhs `(+ ctxt (cross exp-exp)))
                     (make-rhs `(+ (cross exp-exp) ctxt))
                     (make-rhs `(+ exp (cross exp-ctxt)))))
      (make-nt 'exp-exp 
               (list (make-rhs 'hole) 
                     (make-rhs `(+ (cross exp-exp) exp)) 
                     (make-rhs `(+ exp (cross exp-exp)))))))
    
    (run-test
     'compatible-context-language2
     (build-compatible-context-language
      (mk-hasheq '((m . ()) (v . ())))
      (list (make-nt 'm (list (make-rhs '(m m)) (make-rhs '(+ m m)) (make-rhs 'v)))
            (make-nt 'v (list (make-rhs 'number) (make-rhs '(lambda (x) m))))))
     (list
      (make-nt 'v-v (list (make-rhs 'hole) (make-rhs (list 'lambda (list 'x) (list 'cross 'v-m)))))
      (make-nt 'v-m
               (list
                (make-rhs (list (list 'cross 'v-m) 'm))
                (make-rhs (list 'm (list 'cross 'v-m)))
                (make-rhs (list '+ (list 'cross 'v-m) 'm))
                (make-rhs (list '+ 'm (list 'cross 'v-m)))
                (make-rhs (list 'cross 'v-v))))
      (make-nt 'm-v (list (make-rhs (list 'lambda (list 'x) (list 'cross 'm-m)))))
      (make-nt 'm-m
               (list
                (make-rhs 'hole)
                (make-rhs (list (list 'cross 'm-m) 'm))
                (make-rhs (list 'm (list 'cross 'm-m)))
                (make-rhs (list '+ (list 'cross 'm-m) 'm))
                (make-rhs (list '+ 'm (list 'cross 'm-m)))
                (make-rhs (list 'cross 'm-v))))))
    
    (run-test
     'compatible-context-language3
     (build-compatible-context-language
      (mk-hasheq '((m . ()) (seven . ())))
      (list (make-nt 'm (list (make-rhs '(m seven m)) (make-rhs 'number)))
            (make-nt 'seven (list (make-rhs 7)))))
     `(,(make-nt
         'm-m
         `(,(make-rhs 'hole) ,(make-rhs `((cross m-m) seven m)) ,(make-rhs `(m seven (cross m-m)))))
       ,(make-nt
         'seven-m
         `(,(make-rhs `((cross seven-m) seven m)) ,(make-rhs `(m (cross seven-seven) m)) ,(make-rhs `(m seven (cross seven-m)))))
       ,(make-nt 'seven-seven `(,(make-rhs 'hole)))))
    
    (run-test
     'compatible-context-language4
     (build-compatible-context-language
      (mk-hasheq '((a . ()) (b . ()) (c . ())))
      (list (make-nt 'a (list (make-rhs 'b)))
            (make-nt 'b (list (make-rhs 'c)))
            (make-nt 'c (list (make-rhs 3)))))
     (list (make-nt 'c-c (list (make-rhs 'hole)))
           (make-nt 'c-b (list (make-rhs '(cross c-c))))
           (make-nt 'c-a (list (make-rhs '(cross c-b))))
           (make-nt 'b-b (list (make-rhs 'hole)))
           (make-nt 'b-a (list (make-rhs '(cross b-b))))
           (make-nt 'a-a (list (make-rhs 'hole)))))
    
    #;
    (test-xab '(in-hole (cross exp) (+ number number))
              '(+ (+ 1 2) 3)
              (list (make-bindings (list (make-bind 'hole (make-hole-binding (list '+ 1 2) (list 'cdr 'car) #f))))))
    
    (run-test/cmp 'split-underscore1 (split-underscore 'a_1) 'a eq?)
    (run-test/cmp 'split-underscore2 (split-underscore 'a_!_1) 'a eq?)
    (run-test/cmp 'split-underscore3 
                  (with-handlers ([exn:fail? (λ (e) (cadr (regexp-match #rx"^([^:]+):" (exn-message e))))]) 
                    (split-underscore 'a_b_1))
                  "compile-pattern"
                  equal?)
    
    (test-ellipsis-binding '((number_1 number_2) ...) '() '((1 2)))
    (test-ellipsis-binding '((name x number_1) ...) '() '(1 2))
    (test-ellipsis-binding '(((number_1 ...) (number_2 ...)) ...) '() '(((1) (2))))
    (test-ellipsis-binding '(number ... variable) '() '(1 x))
    (test-ellipsis-binding '((in-hole H_1 number_1) ...) '((H hole)) '(1 2))
    
    (cond
      [(= failures 0)
       (printf "matcher-test.ss: all ~a tests passed.\n" test-count)]
      [else
       (printf "matcher-test.ss: ~a test~a failed.\n" 
               failures
               (if (= failures 1)
                   ""
                   "s"))]))

  ;; mk-hasheq : (listof (cons sym any)) -> hash-table
  ;; builds a hash table that has the bindings in assoc-list
  (define (mk-hasheq assoc-list)
    (let ([ht (make-hash-table)])
      (for-each
       (lambda (a)
         (hash-table-put! ht (car a) (cdr a)))
       assoc-list)
      ht))
  
  ;; test-empty : sexp[pattern] sexp[term] answer -> void
  ;; returns #t if pat matching exp with the empty language produces ans.
  (define (test-empty pat exp ans)
    (run-match-test
     `(match-pattern (compile-pattern (compile-language 'pict-stuff-not-used '() '()) ',pat #t) ',exp)
     (match-pattern 
      (compile-pattern (compile-language 'pict-stuff-not-used '() '()) pat #t)
      exp)
     ans))
  
  ;; make-nt-map : (listof nt) -> (listof (listof symbol))
  (define (make-nt-map nts)
    (map (λ (x) (list (nt-name x))) nts))
  
  ;; test-lang : sexp[pattern] sexp[term] answer (list/c nt) -> void
  ;; returns #t if pat matching exp with the language defined by the given nts
  (define (test-lang pat exp ans nts)
    (let ([nt-map (make-nt-map nts)])
      (run-match-test
       `(match-pattern (compile-pattern (compile-language 'pict-stuff-not-used ',nts ',nt-map) ',pat #t) ',exp)
       (match-pattern 
        (compile-pattern (compile-language 'pict-stuff-not-used nts nt-map) pat #t)
        exp)
       ans)))
  
  (define xab-lang #f)
  ;; test-xab : sexp[pattern] sexp[term] answer -> void
  ;; returns #t if pat matching exp with a simple language produces ans.
  (define (test-xab pat exp ans)
    (unless xab-lang
      (let ([nts
             (list (make-nt 'exp
                            (list (make-rhs '(+ exp exp))
                                  (make-rhs 'number)))
                   (make-nt 'ctxt
                            (list (make-rhs '(+ ctxt exp))
                                  (make-rhs '(+ exp ctxt))
                                  (make-rhs 'hole)))
                   
                   (make-nt 'ec-one
                            (list (make-rhs '(+ (hole xx) exp))
                                  (make-rhs '(+ exp (hole xx)))))
                   
                   (make-nt 'same-in-nt (list (make-rhs '((name x any) (name x any)))))
                   
                   (make-nt 'forever-list (list (make-rhs '(forever-list forever-list ...))
                                                (make-rhs 'x)))
                   
                   (make-nt 'lsts
                            (list (make-rhs '())
                                  (make-rhs '(x))
                                  (make-rhs 'x)
                                  (make-rhs '#f)))
                   (make-nt 'split-out
                            (list (make-rhs 'split-out2)))
                   (make-nt 'split-out2
                            (list (make-rhs 'number)))
                   
                   (make-nt 'simple (list (make-rhs 'simple-rhs)))
                   
                   (make-nt 'nesting-names
                            (list (make-rhs '(a (name x nesting-names)))
                                  (make-rhs 'b)))
                   (make-nt 'var (list (make-rhs `variable-not-otherwise-mentioned)))
                   
                   (make-nt 'underscore (list (make-rhs 'exp_1)))
                   )])
      (set! xab-lang
            (compile-language 'pict-stuff-not-used
                              nts
                              (map (λ (x) (list (nt-name x))) nts)))))
    (run-match-test
     `(match-pattern (compile-pattern xab-lang ',pat #t) ',exp)
     (match-pattern (compile-pattern xab-lang pat #t) exp)
     ans))
  
  (define ab-lang #f)
  ;; test-xab : sexp[pattern] sexp[term] answer -> void
  ;; returns #t if pat matching exp with a simple language produces ans.
  (define (test-ab pat exp ans)
    (unless ab-lang
      (set! ab-lang
            (compile-language 
             'pict-stuff-not-used
             (list (make-nt 'aa
                            (list (make-rhs 'a)))
                   (make-nt 'bb
                            (list (make-rhs 'b))))
             '((aa) (bb)))))
    (run-match-test
     `(match-pattern (compile-pattern ab-lang ',pat #t) ',exp)
     (match-pattern (compile-pattern ab-lang pat #t) exp)
     ans))
  
  ;; test-ellipses : sexp sexp -> void
  (define (test-ellipses pat expected)
    (run-test
     `(rewrite-ellipses test-suite:non-underscore-binder? ',pat (lambda (x) (values x #f)))
     (let-values ([(compiled-pattern has-hole?) (rewrite-ellipses test-suite:non-underscore-binder? pat (lambda (x) (values x #f)))])
       (cons compiled-pattern has-hole?))
     (cons expected #f)))
  
  (define (test-suite:non-underscore-binder? x)
    (memq x '(number any variable string)))
  
  ;; test-ellipsis-binding: sexp sexp sexp -> boolean
  ;; Checks that `extract-empty-bindings' produces bindings in the same order
  ;; as the matcher, as required by `collapse-single-multiples'
  (define (test-ellipsis-binding pat nts/sexp exp)
    (define (binding-names bindings)
      (map (λ (b)
             (cond [(bind? b) (bind-name b)]
                   [(mismatch-bind? b) (mismatch-bind-name b)]))
           bindings))
    (run-test
     `(test-ellipsis-binding ,pat)
     (binding-names
      (bindings-table-unchecked
       (mtch-bindings
        (car 
         ((compiled-pattern-cp
           (let ([nts (map (λ (nt-def) (nt (car nt-def) (map rhs (cdr nt-def)))) nts/sexp)])
             (compile-pattern (compile-language 'pict-stuff-not-used nts (make-nt-map nts)) pat #t)))
          exp
          #t)))))
     (binding-names (extract-empty-bindings test-suite:non-underscore-binder? pat))))
  
  ;; run-test/cmp : sexp any any (any any -> boolean)
  ;; compares ans with expected. If failure,
  ;; prints info about the test and increments failures
  (define failures 0)
  (define test-count 0)
  (define (run-test/cmp symbolic ans expected cmp?)
    (set! test-count (+ test-count 1))
    (cond
      [(cmp? ans expected)
       '(printf "passed: ~s\n" symbolic)]
      [else 
       (set! failures (+ failures 1))
       (fprintf (current-error-port)
                "    test: ~s\nexpected: ~e\n     got: ~e\n"
                symbolic expected ans)]))
  
  (define (run-test symbolic ans expected) (run-test/cmp symbolic ans expected equal/bindings?))
  
  ;; run-match-test : sexp got expected
  ;;   expects both ans and expected to be lists or both to be #f and
  ;;   compares them using a set-like equality if they are lists
  (define (run-match-test symbolic ans expected)
    (run-test/cmp
     symbolic ans expected
     (λ (xs ys)
       (cond
         [(and (not xs) (not ys)) #t]
         [(and (list? xs)
               (list? ys))
          (and (andmap (λ (x) (memf (λ (y) (equal/bindings? x y)) ys)) xs)
               (andmap (λ (y) (memf (λ (x) (equal/bindings? x y)) xs)) ys)
               (= (length xs) (length ys)))]
         [else #f]))))
  
  (define (build-context c)
    (let loop ([c c])
      (cond
        [(eq? c the-hole) the-hole]
        [(pair? c) (build-cons-context (loop (car c)) (loop (cdr c)))]
        [(or (null? c)
             (number? c)
             (symbol? c))
         (build-flat-context c)]
        [else (error 'build-context "unknown ~s" c)])))
  
  (test))
