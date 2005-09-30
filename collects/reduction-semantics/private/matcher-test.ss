(module matcher-test mzscheme
  (require "matcher.ss"
           (lib "list.ss"))
  
  (define (make-test-mtch a b c) (make-mtch a (build-flat-context b) c))
  
  (define (test)
    (print-struct #t)
    (test-empty 'any 1 (list (make-test-mtch (make-bindings null) 1 none)))
    (test-empty 'any 'true (list (make-test-mtch (make-bindings null) 'true none)))
    (test-empty 'any "a" (list (make-test-mtch (make-bindings null) "a" none)))
    (test-empty 'any '(a b) (list (make-test-mtch (make-bindings null) '(a b) none)))
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
    (test-empty 'number 1 (list (make-test-mtch (make-bindings null) 1 none)))
    (test-empty 'number 'x #f)
    (test-empty 'number '() #f)
    (test-empty 'string "a" (list (make-test-mtch (make-bindings null) "a" none)))
    (test-empty 'string 1 #f)
    (test-empty 'string '() #f)
    (test-empty 'variable 'x (list (make-test-mtch (make-bindings null) 'x none)))
    (test-empty 'variable 1 #f)
    (test-empty '(variable-except x) 1 #f)
    (test-empty '(variable-except x) 'x #f)
    (test-empty '(variable-except x) 'y (list (make-test-mtch (make-bindings null) 'y none)))
    (test-empty 'hole 1 #f)
    (test-empty '(hole hole-name) 1 #f)
    (test-empty '(name x number) 1 (list (make-test-mtch (make-bindings (list (make-rib 'x 1))) 1 none)))
    (test-empty 'number_x 1 (list (make-test-mtch (make-bindings (list (make-rib 'number_x 1))) 1 none)))
    (test-empty 'string_y "b" (list (make-test-mtch (make-bindings (list (make-rib 'string_y "b"))) "b" none)))
    (test-empty 'any_z '(a b) (list (make-test-mtch (make-bindings (list (make-rib 'any_z '(a b)))) '(a b) none)))
    
    (test-ellipses '(a) '(a))
    (test-ellipses '(a ...) `(,(make-repeat 'a '())))
    (test-ellipses '((a ...) ...) `(,(make-repeat '(a ...) '())))
    (test-ellipses '(a ... b c ...) `(,(make-repeat 'a '()) b ,(make-repeat 'c '())))
    (test-ellipses '((name x a) ...) `(,(make-repeat '(name x a) (list (make-rib 'x '()))))) 
    (test-ellipses '((name x (a ...)) ...)
                   `(,(make-repeat '(name x (a ...)) (list (make-rib 'x '())))))
    (test-ellipses '(((name x a) ...) ...)
                   `(,(make-repeat '((name x a) ...) (list (make-rib 'x '())))))
    (test-ellipses '((1 (name x a)) ...)
                   `(,(make-repeat '(1 (name x a)) (list (make-rib 'x '())))))
    (test-ellipses '((any (name x a)) ...)
                   `(,(make-repeat '(any (name x a)) (list (make-rib 'x '())))))
    (test-ellipses '((number (name x a)) ...)
                   `(,(make-repeat '(number (name x a)) (list (make-rib 'x '())))))
    (test-ellipses '((variable (name x a)) ...)
                   `(,(make-repeat '(variable (name x a)) (list (make-rib 'x '())))))
    (test-ellipses '(((name x a) (name y b)) ...)
                   `(,(make-repeat '((name x a) (name y b)) (list (make-rib 'y '()) (make-rib 'x '())))))
    (test-ellipses '((name x (name y b)) ...)
                   `(,(make-repeat '(name x (name y b)) (list (make-rib 'y '()) (make-rib 'x '())))))
    (test-ellipses '((in-hole (name x a) (name y b)) ...)
                   `(,(make-repeat '(in-hole (name x a) (name y b)) 
                                   (list (make-rib 'x '()) (make-rib 'y '())))))
    
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
    
    (test-empty '(in-named-hole h1 (z (hole h1)) a) 
                '(z a)
                (list (make-test-mtch (make-bindings (list)) '(z a) none)))
    
    (test-empty '(in-named-hole h1 (z (hole h1)) a) '(z a) (list (make-test-mtch (make-bindings (list)) '(z a) none)))
    (test-empty '(in-named-hole c (any (hole c)) y)
                '(x y)
                (list (make-test-mtch (make-bindings (list)) '(x y) none)))
    (test-empty '(in-named-hole a (in-named-hole b (x (hole b)) (hole a)) y)
                '(x y)
                (list (make-test-mtch (make-bindings (list)) '(x y) none)))
    (test-empty '(in-hole (in-hole (x hole) hole) y)
                '(x y)
                (list (make-test-mtch (make-bindings (list)) '(x y) none)))
    
    (test-empty '((name x number) (name x number)) '(1 1) (list (make-test-mtch (make-bindings (list (make-rib 'x 1))) '(1 1) none)))
    (test-empty '((name x number) (name x number)) '(1 2) #f)
    
    (test-empty '(a ...) '() (list (make-test-mtch (make-bindings empty) '() none)))
    (test-empty '(a ...) '(a) (list (make-test-mtch (make-bindings empty) '(a) none)))
    (test-empty '(a ...) '(a a) (list (make-test-mtch (make-bindings empty) '(a a) none)))
    (test-empty '((name x a) ...) '() (list (make-test-mtch (make-bindings (list (make-rib 'x '()))) '() none)))
    (test-empty '((name x a) ...) '(a) (list (make-test-mtch (make-bindings (list (make-rib 'x '(a)))) '(a) none)))
    (test-empty '((name x a) ...) '(a a) (list (make-test-mtch (make-bindings (list (make-rib 'x '(a a)))) '(a a) none)))
    
    (test-empty '(b ... a ...) '() (list (make-test-mtch (make-bindings empty) '() none)))
    (test-empty '(b ... a ...) '(a) (list (make-test-mtch (make-bindings empty) '(a) none)))
    (test-empty '(b ... a ...) '(b) (list (make-test-mtch (make-bindings empty) '(b) none)))
    (test-empty '(b ... a ...) '(b a) (list (make-test-mtch (make-bindings empty) '(b a) none)))
    (test-empty '(b ... a ...) '(b b a a) (list (make-test-mtch (make-bindings empty) '(b b a a) none)))
    (test-empty '(b ... a ...) '(a a) (list (make-test-mtch (make-bindings empty) '(a a) none)))
    (test-empty '(b ... a ...) '(b b) (list (make-test-mtch (make-bindings empty) '(b b) none)))
    
    (test-empty '((name y b) ... (name x a) ...) '() 
                (list (make-test-mtch (make-bindings (list (make-rib 'x '())
                                                      (make-rib 'y '())))
                                 '()
                                 none)))
    (test-empty '((name y b) ... (name x a) ...) '(a)
                (list (make-test-mtch (make-bindings (list (make-rib 'x '(a))
                                                      (make-rib 'y '())))
                                 '(a)
                                 none)))
    (test-empty '((name y b) ... (name x a) ...) '(b) 
                (list (make-test-mtch (make-bindings (list (make-rib 'x '())
                                                      (make-rib 'y '(b))))
                                 '(b)
                                 none)))
    (test-empty '((name y b) ... (name x a) ...) '(b b a a) 
                (list (make-test-mtch (make-bindings (list (make-rib 'x '(a a))
                                                      (make-rib 'y '(b b))))
                                 '(b b a a)
                                 none)))
    (test-empty '((name y a) ... (name x a) ...) '(a) 
                (list (make-test-mtch (make-bindings (list (make-rib 'x '())
                                                      (make-rib 'y '(a))))
                                 '(a)
                                 none)
                      (make-test-mtch (make-bindings (list (make-rib 'x '(a))
                                                      (make-rib 'y '())))
                                 '(a)
                                 none)))
    (test-empty '((name y a) ... (name x a) ...) '(a a) 
                (list (make-test-mtch (make-bindings (list (make-rib 'x '())
                                                      (make-rib 'y '(a a))))
                                 '(a a)
                                 none)
                      (make-test-mtch (make-bindings (list (make-rib 'x '(a))
                                                      (make-rib 'y '(a))))
                                 '(a a)
                                 none)
                      (make-test-mtch (make-bindings (list (make-rib 'x '(a a))
                                                      (make-rib 'y '())))
                                 '(a a)
                                 none)))

    (test-ab '(bb_y ... aa_x ...) '() 
             (list (make-test-mtch (make-bindings (list (make-rib 'aa_x '())
                                                   (make-rib 'bb_y '())))
                              '()
                              none)))
    (test-ab '(bb_y ... aa_x ...) '(a)
             (list (make-test-mtch (make-bindings (list (make-rib 'aa_x '(a))
                                                   (make-rib 'bb_y '())))
                              '(a) 
                              none)))
    (test-ab '(bb_y ... aa_x ...) '(b) 
             (list (make-test-mtch (make-bindings (list (make-rib 'aa_x '())
                                                   (make-rib 'bb_y '(b))))
                              '(b)
                              none)))
    (test-ab '(bb_y ... aa_x ...) '(b b a a) 
             (list (make-test-mtch (make-bindings (list (make-rib 'aa_x '(a a))
                                                   (make-rib 'bb_y '(b b))))
                              '(b b a a)
                              none)))
    (test-ab '(aa_y ... aa_x ...) '(a) 
             (list (make-test-mtch (make-bindings (list (make-rib 'aa_x '())
                                                   (make-rib 'aa_y '(a))))
                              '(a)
                              none)
                   (make-test-mtch (make-bindings (list (make-rib 'aa_x '(a))
                                                   (make-rib 'aa_y '())))
                              '(a)
                              none)))
    (test-ab '(aa_y ... aa_x ...) '(a a) 
             (list (make-test-mtch (make-bindings (list (make-rib 'aa_x '())
                                                   (make-rib 'aa_y '(a a))))
                              '(a a)
                              none)
                   (make-test-mtch (make-bindings (list (make-rib 'aa_x '(a))
                                                   (make-rib 'aa_y '(a))))
                              '(a a)
                              none)
                   (make-test-mtch (make-bindings (list (make-rib 'aa_x '(a a))
                                                   (make-rib 'aa_y '())))
                              '(a a)
                              none)))

    (test-empty '((name x number) ...) '(1 2) (list (make-test-mtch (make-bindings (list (make-rib 'x '(1 2)))) '(1 2) none)))
    
    (test-empty '(a ...) '(b) #f)
    (test-empty '(a ... b ...) '(c) #f)
    (test-empty '(a ... b) '(b c) #f)
    (test-empty '(a ... b) '(a b c) #f)
    
    (test-xab 'lsts '() (list (make-test-mtch (make-bindings null) '() none)))
    (test-xab 'lsts '(x) (list (make-test-mtch (make-bindings null) '(x) none)))
    (test-xab 'lsts 'x (list (make-test-mtch (make-bindings null) 'x none)))
    (test-xab 'lsts #f (list (make-test-mtch (make-bindings null) #f none)))
    (test-xab 'split-out '1 (list (make-test-mtch (make-bindings null) '1 none)))

    (test-xab 'exp 1 (list (make-test-mtch (make-bindings null) 1 none)))
    (test-xab 'exp '(+ 1 2) (list (make-test-mtch (make-bindings null) '(+ 1 2) none)))
    (test-xab '(in-hole ctxt any)
              '1
              (list (make-test-mtch (make-bindings (list)) 1 none)))
    (test-xab '(in-hole ctxt (name x any))
              '1
              (list (make-test-mtch (make-bindings (list (make-rib 'x 1))) 1 none)))
    (test-xab '(in-hole (name c ctxt) (name x any))
              '(+ 1 2)
              (list (make-test-mtch (make-bindings (list (make-rib 'c (build-context hole)) (make-rib 'x '(+ 1 2)))) '(+ 1 2) none)
                    (make-test-mtch (make-bindings (list (make-rib 'c (build-context `(+ ,hole 2)))
                                                         (make-rib 'x 1)))
                                    '(+ 1 2) none)
                    (make-test-mtch (make-bindings (list (make-rib 'c (build-context `(+ 1 ,hole)))
                                                         (make-rib 'x 2))) '(+ 1 2) none)))
    (test-xab '(in-hole (name c ctxt) (name i (+ number number)))
              '(+ (+ 1 2) (+ 3 4))
              (list (make-test-mtch 
                     (make-bindings (list (make-rib 'i '(+ 1 2)) (make-rib 'c (build-context `(+ ,hole (+ 3 4))))))
                     '(+ (+ 1 2) (+ 3 4))
                     none)
                    (make-test-mtch (make-bindings (list (make-rib 'i '(+ 3 4)) (make-rib 'c `(+ (+ 1 2) ,hole))))
                               '(+ (+ 1 2) (+ 3 4))
                               none)))
    
    (test-empty '(in-hole ((z hole)) (name x any))
                '((z a))
                (list (make-test-mtch (make-bindings (list (make-rib 'x 'a))) '((z a)) none)))
    (test-empty '(in-hole (name c (z ... hole z ...)) any)
                '(z z)
                (list 
                 (make-test-mtch (make-bindings (list (make-rib 'c `(z ,hole)))) '(z z) none)
                 (make-test-mtch (make-bindings (list (make-rib 'c `(,hole z)))) '(z z) none)))
    (test-empty '(in-hole (name c (z ... hole z ...)) any)
                '(z z z)
                (list 
                 (make-test-mtch (make-bindings (list (make-rib 'c `(z z ,hole)))) '(z z z) none)
                 (make-test-mtch (make-bindings (list (make-rib 'c `(z ,hole z)))) '(z z z) none)
                 (make-test-mtch (make-bindings (list (make-rib 'c `(,hole z z)))) '(z z z) none)))
    
    (test-empty '(z (in-hole (name c (z hole)) a))
                '(z (z a))
                (list 
                 (make-test-mtch (make-bindings (list (make-rib 'c `(z ,hole))))
                            '(z (z a))
                            none)))
    
    (test-empty '(a (in-hole (name c1 (b (in-hole (name c2 (c hole)) d) hole)) e))
                '(a (b (c d) e))
                (list 
                 (make-test-mtch (make-bindings (list (make-rib 'c2 `(c ,hole))
                                                 (make-rib 'c1 `(b (c d) ,hole))))
                            '(a (b (c d) e))
                            none)))

    (test-empty '(in-hole (in-hole hole hole) a)
                'a
                (list (make-test-mtch (make-bindings (list)) 'a none)))
    
    (test-empty '(a (b (in-hole (name c1 (in-hole (name c2 (c hole)) (d hole))) e)))
                '(a (b (c (d e))))
                (list 
                 (make-test-mtch (make-bindings (list (make-rib 'c1 `(c (d ,hole)))
                                                 (make-rib 'c2 `(c ,hole))))
                            '(a (b (c (d e))))
                            none)))
    
    (test-empty `(+ 1 (side-condition any ,(lambda (bindings) #t)))
                '(+ 1 b)
                (list (make-test-mtch (make-bindings '()) '(+ 1 b) none)))
    (test-empty `(+ 1 (side-condition any ,(lambda (bindings) #f)))
                '(+ 1 b)
                #f)
    
    (test-empty `(+ 1 (side-condition b ,(lambda (bindings) #t)))
                '(+ 1 b)
                (list (make-test-mtch (make-bindings '()) '(+ 1 b) none)))
    (test-empty `(+ 1 (side-condition a ,(lambda (bindings) #t)))
                '(+ 1 b)
                #f)

    (test-empty `(side-condition (name x any) ,(lambda (bindings) (eq? (lookup-binding bindings 'x) 'a)))
                'a
                (list 
                 (make-test-mtch (make-bindings (list (make-rib 'x 'a)))
                            'a
                            none)))

    (test-empty `(+ 1 (side-condition (name x any) ,(lambda (bindings) (eq? (lookup-binding bindings 'x) 'a))))
                '(+ 1 a)
                (list 
                 (make-test-mtch (make-bindings (list (make-rib 'x 'a)))
                            '(+ 1 a)
                            none)))

    (test-empty `(side-condition (name x any) ,(lambda (bindings) (eq? (lookup-binding bindings 'x) 'a)))
                'b
                #f)
    
    (test-empty `(+ 1 (side-condition (name x any) ,(lambda (bindings) (eq? (lookup-binding bindings 'x) 'a))))
                '(+ 1 b)
                #f)
    
    (test-xab 'exp_1
              '(+ 1 2)
              (list (make-test-mtch (make-bindings (list (make-rib 'exp_1 '(+ 1 2)))) '(+ 1 2) none)))
    (test-xab '(exp_1 exp_2)
              '((+ 1 2) (+ 3 4))
              (list (make-test-mtch (make-bindings (list (make-rib 'exp_1 '(+ 1 2)) (make-rib 'exp_2 '(+ 3 4))))
                               '((+ 1 2) (+ 3 4))
                               none)))
    (test-xab '(exp_1 exp_1)
              '((+ 1 2) (+ 3 4))
              #f)
    (test-xab 'nesting-names
              'b
              (list (make-test-mtch (make-bindings (list)) 'b none)))
    (test-xab 'nesting-names
              '(a b)
              (list (make-test-mtch (make-bindings (list)) '(a b) none)))
    (test-xab 'nesting-names
              '(a (a b))
              (list (make-test-mtch (make-bindings (list)) '(a (a b)) none)))
    (test-xab '((name x a) nesting-names)
              '(a (a (a b)))
              (list (make-test-mtch (make-bindings (list (make-rib 'x 'a))) '(a (a (a b))) none)))
    (test-xab 'nesting-names
              '(a (a (a (a b))))
              (list (make-test-mtch (make-bindings (list)) '(a (a (a (a b)))) none)))
    
    (test-xab 'same-in-nt
              '(x x)
              (list (make-test-mtch (make-bindings (list)) '(x x) none)))
    (test-xab 'same-in-nt
              '(x y)
              #f)
     
    #;
    (test-xab '(in-hole ec-multi (+ number number))
              '(+ 1 2)
              (list (make-bindings (list (make-rib 'hole (make-hole-binding '(+ 1 2) '() #f))))))
    
    #;
    (test-xab '(in-hole ec-multi (+ number number))
              '(+ 1 (+ 5 6))
              (list (make-bindings (list (make-rib 'hole (make-hole-binding '(+ 5 6) '(cdr cdr car) #f))))))
    
    #;
    (test-xab '(in-hole ec-multi (+ number number))
              '(+ (+ (+ 1 2) 3) 4)
              (list (make-bindings (list (make-rib 'hole (make-hole-binding '(+ 1 2) '(cdr car cdr car) #f))))))
    
    #;
    (test-xab '(in-hole ec-multi (+ number number))
              '(+ (+ 3 (+ 1 2)) 4)
              (list (make-bindings (list (make-rib 'hole (make-hole-binding '(+ 1 2) '(cdr car cdr cdr car) #f))))))
    
    #;
    (test-xab '(in-hole ec-multi (+ number number))
              '(+ (+ (+ 1 2) (+ 3 4)) (+ 5 6))
              (list (make-bindings (list (make-rib 'hole (make-hole-binding '(+ 5 6) '(cdr cdr car) #f))))
                    (make-bindings (list (make-rib 'hole (make-hole-binding '(+ 1 2) '(cdr car cdr car) #f))))
                    (make-bindings (list (make-rib 'hole (make-hole-binding '(+ 3 4) '(cdr car cdr cdr car) #f))))))
    
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
      (make-nt 'exp-exp 
               (list (make-rhs 'hole) 
                     (make-rhs `(+ (cross exp-exp) exp)) 
                     (make-rhs `(+ exp (cross exp-exp)))))
      (make-nt 'exp-ctxt
               (list (make-rhs `(+ (cross exp-ctxt) exp))
                     (make-rhs `(+ ctxt (cross exp-exp)))
                     (make-rhs `(+ (cross exp-exp) ctxt))
                     (make-rhs `(+ exp (cross exp-ctxt)))))
      (make-nt 'ctxt-exp
               (list (make-rhs `(+ (cross ctxt-exp) exp))
                     (make-rhs `(+ exp (cross ctxt-exp)))))
      (make-nt 'ctxt-ctxt
               (list (make-rhs 'hole)
                     (make-rhs `(+ (cross ctxt-ctxt) exp))
                     (make-rhs `(+ ctxt (cross ctxt-exp)))
                     (make-rhs `(+ (cross ctxt-exp) ctxt))
                     (make-rhs `(+ exp (cross ctxt-ctxt)))))))
    
    (run-test
     'compatible-context-language2
     (build-compatible-context-language
      (mk-hasheq '((m . ()) (v . ())))
      (list (make-nt 'm (list (make-rhs '(m m)) (make-rhs '(+ m m)) (make-rhs 'v)))
            (make-nt 'v (list (make-rhs 'number) (make-rhs '(lambda (x) m))))))
     (list
      (make-nt 'm-m
               (list
                (make-rhs 'hole)
                (make-rhs (list (list 'cross 'm-m) 'm))
                (make-rhs (list 'm (list 'cross 'm-m)))
                (make-rhs (list '+ (list 'cross 'm-m) 'm))
                (make-rhs (list '+ 'm (list 'cross 'm-m)))
                (make-rhs (list 'cross 'm-v))))
      (make-nt 'm-v (list (make-rhs (list 'lambda (list 'x) (list 'cross 'm-m)))))
      (make-nt 'v-m
               (list
                (make-rhs (list (list 'cross 'v-m) 'm))
                (make-rhs (list 'm (list 'cross 'v-m)))
                (make-rhs (list '+ (list 'cross 'v-m) 'm))
                (make-rhs (list '+ 'm (list 'cross 'v-m)))
                (make-rhs (list 'cross 'v-v))))
      (make-nt 'v-v (list (make-rhs 'hole) (make-rhs (list 'lambda (list 'x) (list 'cross 'v-m)))))))
    
    (run-test
     'compatible-context-language3
     (build-compatible-context-language
      (mk-hasheq '((m . ()) (seven . ())))
      (list (make-nt 'm (list (make-rhs '(m seven m)) (make-rhs 'number)))
            (make-nt 'seven (list (make-rhs 7)))))
     `(,(make-nt
         'm-m
         `(,(make-rhs 'hole) ,(make-rhs `((cross m-m) seven m)) ,(make-rhs `(m (cross m-seven) m)) ,(make-rhs `(m seven (cross m-m)))))
       ,(make-nt 'm-seven `())
       ,(make-nt
         'seven-m
         `(,(make-rhs `((cross seven-m) seven m)) ,(make-rhs `(m (cross seven-seven) m)) ,(make-rhs `(m seven (cross seven-m)))))
       ,(make-nt 'seven-seven `(,(make-rhs 'hole)))))
    
    #;
    (test-xab '(in-hole (cross exp) (+ number number))
              '(+ (+ 1 2) 3)
              (list (make-bindings (list (make-rib 'hole (make-hole-binding (list '+ 1 2) (list 'cdr 'car) #f))))))
    
    (unless failure?
      (fprintf (current-error-port) "All ~a tests passed.\n" test-count)))

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
     `(match-pattern (compile-pattern (compile-language '()) ',pat) ',exp)
     (match-pattern 
      (compile-pattern (compile-language '()) pat)
      exp)
     ans))
  
  (define xab-lang #f)
  ;; test-xab : sexp[pattern] sexp[term] answer -> void
  ;; returns #t if pat matching exp with a simple language produces ans.
  (define (test-xab pat exp ans)
    (unless xab-lang
      (set! xab-lang
            (compile-language (list (make-nt 'exp
                                             (list (make-rhs '(+ exp exp))
                                                   (make-rhs 'number)))
                                    (make-nt 'ctxt
                                             (list (make-rhs '(+ ctxt exp))
                                                   (make-rhs '(+ exp ctxt))
                                                   (make-rhs 'hole)))
                                    
                                    (make-nt 'ec-multi
                                             (list (make-rhs 'hole)
                                                   (make-rhs '(in-named-hole xx ec-one ec-multi))))
                                    (make-nt 'ec-one
                                             (list (make-rhs '(+ (hole xx) exp))
                                                   (make-rhs '(+ exp (hole xx)))))

                                    (make-nt 'same-in-nt (list (make-rhs '((name x any) (name x any)))))
                                    
                                    (make-nt 'lsts
                                             (list (make-rhs '())
                                                   (make-rhs '(x))
                                                   (make-rhs 'x)
                                                   (make-rhs '#f)))
                                    (make-nt 'split-out
                                             (list (make-rhs 'split-out2)))
                                    (make-nt 'split-out2
                                             (list (make-rhs 'number)))
                                    
                                    (make-nt 'nesting-names
                                             (list (make-rhs '(a (name x nesting-names)))
                                                   (make-rhs 'b)))))))
    (run-match-test
     `(match-pattern (compile-pattern xab-lang ',pat) ',exp)
     (match-pattern (compile-pattern xab-lang pat) exp)
     ans))
  
  (define ab-lang #f)
  ;; test-xab : sexp[pattern] sexp[term] answer -> void
  ;; returns #t if pat matching exp with a simple language produces ans.
  (define (test-ab pat exp ans)
    (unless ab-lang
      (set! ab-lang
            (compile-language (list (make-nt 'aa
                                             (list (make-rhs 'a)))
                                    (make-nt 'bb
                                             (list (make-rhs 'b)))))))
    (run-match-test
     `(match-pattern (compile-pattern ab-lang ',pat) ',exp)
     (match-pattern (compile-pattern ab-lang pat) exp)
     ans))
  
  ;; test-ellipses : sexp sexp -> void
  (define (test-ellipses pat expected)
    (run-test
     `(rewrite-ellipses ',pat (lambda (x) (values x #f)))
     (let-values ([(compiled-pattern has-hole?) (rewrite-ellipses pat (lambda (x) (values x #f)))])
       (cons compiled-pattern has-hole?))
     (cons expected #f)))
  
  ;; run-test/cmp : sexp any any (any any -> boolean)
  ;; compares ans with expected. If failure,
  ;; prints info about the test and sets `failure?' to #t.
  (define failure? #f)
  (define test-count 0)
  (define (run-test/cmp symbolic ans expected cmp?)
    (set! test-count (+ test-count 1))
    (cond
      [(cmp? ans expected)
       '(printf "passed: ~s\n" symbolic)]
      [else 
       (set! failure? #t)
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
               (andmap (λ (y) (memf (λ (x) (equal/bindings? x y)) xs)) ys))]
         [else #f]))))
  
  ;; equal/bindings? : any any -> boolean
  ;; compares two sexps (with embedded bindings) for equality.
  ;; uses an order-insensitive comparison for the bindings
  (define (equal/bindings? fst snd)
    (let loop ([fst fst]
               [snd snd])
      (cond
        [(pair? fst)
         (and (pair? snd) 
              (loop (car fst) (car snd))
              (loop (cdr fst) (cdr snd)))]
        [(mtch? fst)
         (and (mtch? snd)
              (loop (mtch-bindings fst)
                    (mtch-bindings snd))
              (let ([g1 (gensym 'run-match-test-sym)])
                (equal? (plug (mtch-context fst) g1)
                        (plug (mtch-context snd) g1)))
              (equal? (mtch-hole fst)
                      (mtch-hole snd)))]
        [(bindings? fst)
         (and (bindings? snd)
              (let ([fst-table (bindings-table fst)]
                    [snd-table (bindings-table snd)])
                (and (= (length fst-table)
                        (length snd-table))
                     (andmap
                      loop
                      (quicksort fst-table rib-lt)
                      (quicksort snd-table rib-lt)))))]
        [(and (rib? fst)
              (rib? snd)
              (context? (rib-exp fst))
              (context? (rib-exp snd)))
         (and (equal? (rib-name fst) (rib-name snd))
              (let ([g (gensym 'run-match-test-sym2)])
                (equal? (plug (rib-exp fst) g)
                        (plug (rib-exp snd) g))))]
        [else (equal? fst snd)])))
  
  (define (build-context c)
    (let loop ([c c])
      (cond
        [(eq? c hole) hole]
        [(pair? c) (build-cons-context (loop (car c)) (loop (cdr c)))]
        [(or (null? c)
             (number? c)
             (symbol? c))
         (build-flat-context c)]
        [else (error 'build-context "unknown ~s" c)])))
  
  ;; rib-lt : rib rib -> boolean
  (define (rib-lt r1 r2) (string<=? (format "~s" (rib-name r1))
                                    (format "~s" (rib-name r2))))
  
  (test))
