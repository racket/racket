#lang racket/base

(require redex/private/matcher
         redex/private/lang-struct
         (only-in "test-util.rkt" equal/bindings?)
         (for-syntax racket/base)
         racket/list)

(error-print-width 500)
  
(define (make-test-mtch a b c) (make-mtch a (build-flat-context b) c))
  
(define (test)
  (let-syntax ([this-line (λ (stx) (datum->syntax #'here (syntax-line stx)))])
    (print-struct #t)
    (test-empty '(name any any) 1 (list (make-test-mtch (make-bindings (list (make-bind 'any 1))) 1 none)))
    (test-empty '(name any any) 'true (list (make-test-mtch (make-bindings (list (make-bind 'any 'true))) 'true none)))
    (test-empty '(name any any) "a" (list (make-test-mtch (make-bindings (list (make-bind 'any "a"))) "a" none)))
    (test-empty '(name any any) '(a b) (list (make-test-mtch (make-bindings (list (make-bind 'any '(a b)))) '(a b) none)))
    (test-empty '(name any any) #t (list (make-test-mtch (make-bindings (list (make-bind 'any #t))) #t none)))
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
    (test-empty #s(x 1) #s(x 1) (list (make-test-mtch (make-bindings null) #s(x 1) none)))
    (test-empty #s(x 1) #s(x 2) #f)
    (test-empty '(name number number) 1 (list (make-test-mtch (make-bindings (list (make-bind 'number 1))) 1 none)))
    (test-empty 'number 'x #f)
    (test-empty 'number '() #f)
    (test-empty '(name natural natural) 1 (list (make-test-mtch (make-bindings (list (make-bind 'natural 1))) 1 none)))
    (test-empty 'natural 'x #f)
    (test-empty 'natural '() #f)
    (test-empty 'natural -1 #f)
    (test-empty 'natural 1.0 #f)
    (test-empty '(name integer integer) -1 (list (make-test-mtch (make-bindings (list (make-bind 'integer -1))) -1 none)))
    (test-empty 'integer 'x #f)
    (test-empty 'integer '() #f)
    (test-empty 'integer 1.0 #f)
    (test-empty '(name real real) 1.1 (list (make-test-mtch (make-bindings (list (make-bind 'real 1.1))) 1.1 none)))
    (test-empty 'real 'x #f)
    (test-empty 'real '() #f)
    (test-empty 'real 2+3i #f)
    (test-empty 'boolean #t (list (make-test-mtch (make-bindings (list)) #t none)))
    (test-empty 'boolean #f (list (make-test-mtch (make-bindings (list)) #f none)))
    (test-empty 'boolean 'x #f)
    (test-empty '(name string string) "a" (list (make-test-mtch (make-bindings (list (make-bind 'string "a"))) "a" none)))
    (test-empty 'string 1 #f)
    (test-empty 'string '() #f)
    (test-empty '(name variable variable) 'x (list (make-test-mtch (make-bindings (list (make-bind 'variable 'x))) 'x none)))
    (test-empty 'variable 1 #f)
    (test-empty '(variable-except x) 1 #f)
    (test-empty '(variable-except x) 'x #f)
    (test-empty '(variable-except x) 'y (list (make-test-mtch (make-bindings null) 'y none)))
    (test-lang (this-line)
               '(name x (nt x))
               'y
               (list (make-mtch (make-bindings (list (make-bind 'x 'y))) 'y none))
               (list (make-nt 'x (list (make-rhs '(variable-except x))))))
    (test-empty '(variable-prefix x:) 'x: (list (make-test-mtch (make-bindings null) 'x: none)))
    (test-empty '(variable-prefix x:) 'x:x (list (make-test-mtch (make-bindings null) 'x:x none)))
    (test-empty '(variable-prefix x:) ': #f)
    (test-empty '(variable-prefix x:) '() #f)
    
    (test-empty 'hole 1 #f)
    (test-empty 'hole
                the-hole
                (list (make-test-mtch (make-bindings (list)) the-hole none)))
    (test-empty '(in-hole (list hole 2) 1)
                '(1 2)
                (list (make-test-mtch (make-bindings (list)) `(1 2) none)))
    
    (test-empty '(in-hole (name E_1 (list (hide-hole hole) hole)) x)
                `(,the-hole x)
                (list (make-test-mtch (make-bindings (list (make-bind 'E_1 `(,the-not-hole ,the-hole)))) 
                                      `(,the-hole x)
                                      none)))
    
    (test-empty '(name x (name number number)) 1 (list (make-test-mtch (make-bindings (list (make-bind 'x 1) (make-bind 'number 1))) 1 none)))
    (test-empty '(name number_x number) 1 (list (make-test-mtch (make-bindings (list (make-bind 'number_x 1))) 1 none)))
    (test-empty '(name string_y string) "b" (list (make-test-mtch (make-bindings (list (make-bind 'string_y "b"))) "b" none)))
    (test-empty '(name any_z any) '(a b) (list (make-test-mtch (make-bindings (list (make-bind 'any_z '(a b)))) '(a b) none)))
    
    (test-empty '(mismatch-name x_!_1 (name number number)) 1 (list (make-test-mtch (make-bindings (list (make-bind 'number 1))) 1 none)))
    (test-empty '(list (mismatch-name x_!_1 (name number number)) (mismatch-name x_!_1 number)) '(1 1) #f)
    (test-empty '(list (mismatch-name x_!_1 (name number_a number)) (mismatch-name x_!_1 (name number_b number))) '(1 2) 
                (list (make-test-mtch (make-bindings (list (make-bind 'number_a 1)
                                                           (make-bind 'number_b 2)))
                                      '(1 2) 
                                      none)))
    (test-empty '(list (mismatch-name number_!_1 number) (mismatch-name number_!_1 number)) '(1 1) #f)
    (test-empty '(list (mismatch-name number_!_1 number) (mismatch-name number_!_1 number)) '(1 2) (list (make-test-mtch (make-bindings (list)) '(1 2) none)))
    (test-empty '(list (repeat (mismatch-name number_!_1 number) #f #f)) '(1 2) (list (make-test-mtch (make-bindings (list)) '(1 2) none)))
    (test-empty '(list (repeat (mismatch-name number_!_1 number) #f #f)) '(1 2 3 4 5) (list (make-test-mtch (make-bindings (list)) '(1 2 3 4 5) none)))
    (test-empty '(list (repeat (mismatch-name number_!_1 number) #f #f)) '(1 2 3 1 5) (list (make-test-mtch (make-bindings (list)) '(1 2 3 1 5) none)))
    (test-empty '(list (list (repeat (mismatch-name number_!_1 number) #f #f)) (list (repeat number_!_1 #f #f))) 
                '((1 2 3 1 5) (1 2 3 1 5))
                #f)
    (test-empty '(list (list (repeat (mismatch-name number_!_1 number) #f #f))
                       (list (repeat (mismatch-name number_!_1 number) #f #f))) 
                '((17 2 3 1 5) (1 2 3 1 5))
                (list (make-test-mtch (make-bindings (list)) '((17 2 3 1 5) (1 2 3 1 5)) none)))
    (test-empty '(list (repeat (list (mismatch-name number_!_1 number) (mismatch-name number_!_1 number)) #f #f) 
                       (repeat (mismatch-name number_!_1 number) #f #f)) 
                '((1 1) (2 2) 1 3)
                #f)
    (test-empty '(list (repeat (list (mismatch-name number_!_1 number) (mismatch-name number_!_1 number)) #f #f)
                       (repeat (mismatch-name number_!_1 number) #f #f))
                '((1 1) (2 3) 1 2)
                #f)
    (test-empty '(list (repeat (list (mismatch-name number_!_1 number) (mismatch-name number_!_1 number)) #f #f)
                       (repeat (mismatch-name number_!_1 number) #f #f))
                '((1 1) (2 3) 1 4)
                (list (make-test-mtch (make-bindings (list)) '((1 1) (2 3) 1 4) none)))
    
    (test-empty '(list (repeat (name x_1 1) ..._1 ..._!_1)
                       (repeat (name x_1 1) ..._1 #f)
                       (repeat (name x_2 2) ..._2 ..._!_1)
                       (repeat (name x_2 2) ..._2 #f))
                '(1 1 2 2)
                #f)
    (test-empty '(list (repeat (name x_1 1) ..._1 ..._!_1)
                       (repeat (name x_1 1) ..._1 #f)
                       (repeat (name x_2 2) ..._2 ..._!_1)
                       (repeat (name x_2 2) ..._2 #f))
                '(1 1 2 2 2)
                #f)
    (test-empty '(list (repeat (name x_1 1) ..._1 ..._!_1)
                       (repeat (name x_1 1) ..._1 #f)
                       (repeat (name x_2 2) ..._2 ..._!_1)
                       (repeat (name x_2 2) ..._2 #f))
                '(1 1 1 2 2)
                #f)
    (test-empty '(list (repeat (name x_1 1) ..._1 ..._!_1)
                       (repeat (name x_1 1) ..._1 #f)
                       (repeat (name x_2 2) ..._2 ..._!_1)
                       (repeat (name x_2 2) ..._2 #f))
                '(1 1 1 1 2 2)
                (list (make-mtch (make-bindings (list (make-bind 'x_1 '(1 1))
                                                      (make-bind 'x_2 '(2))
                                                      (make-bind '..._2 1)
                                                      (make-bind '..._1 2)))
                                 '(1 1 1 1 2 2)
                                 none)))
    
    (test-ellipses '(a) '(a))
    (test-ellipses '((repeat a #f #f)) `(,(make-repeat 'a '() #f #f)))
    (test-ellipses '((repeat (list (repeat a #f #f)) #f #f)) `(,(make-repeat '(list (repeat a #f #f)) '() #f #f)))
    (test-ellipses '((repeat a #f #f) b (repeat c #f #f)) `(,(make-repeat 'a '() #f #f) b ,(make-repeat 'c '() #f #f)))
    (test-ellipses '((repeat (name x a) #f #f)) `(,(make-repeat '(name x a) (list (make-bind 'x '())) #f #f))) 
    (test-ellipses '((repeat (name x (list (repeat a #f #f))) #f #f))
                   `(,(make-repeat '(name x (list (repeat a #f #f))) (list (make-bind 'x '())) #f #f)))
    (test-ellipses '((repeat (list (repeat (name x a) #f #f)) #f #f))
                   `(,(make-repeat '(list (repeat (name x a) #f #f)) (list (make-bind 'x '())) #f #f)))
    (test-ellipses '((repeat (list 1 (name x a)) #f #f))
                   `(,(make-repeat '(list 1 (name x a)) (list (make-bind 'x '())) #f #f)))
    (test-ellipses '((repeat (list (name any any) (name x a)) #f #f))
                   `(,(make-repeat '(list (name any any) (name x a)) 
                                        (list (make-bind 'any '())
                                              (make-bind 'x '())) 
                                        #f #f)))
    (test-ellipses '((repeat (list (name number number) (name x a)) #f #f))
                   `(,(make-repeat '(list (name number number) (name x a)) 
                                   (list (make-bind 'number '())
                                         (make-bind 'x '()))
                                   #f #f)))
    (test-ellipses '((repeat (list (name variable variable) (name x a)) #f #f))
                   `(,(make-repeat '(list (name variable variable) (name x a))
                                        (list (make-bind 'variable '())
                                              (make-bind 'x '()))
                                        #f #f)))
    (test-ellipses '((repeat (list (name x a) (name y b)) #f #f))
                   `(,(make-repeat '(list (name x a) (name y b)) (list (make-bind 'x '()) (make-bind 'y '())) #f #f)))
    
    (test-ellipses '((repeat (name x (name y b)) #f #f))
                   `(,(make-repeat '(name x (name y b)) (list (make-bind 'x '()) (make-bind 'y '())) #f #f)))
    (test-ellipses '((repeat (in-hole (name x a) (name y b)) #f #f))
                   `(,(make-repeat '(in-hole (name x a) (name y b)) 
                                   (list (make-bind 'y '()) (make-bind 'x '())) 
                                   #f #f)))
    
    (test-ellipses '((repeat a ..._1 #f))
                   `(,(make-repeat 'a (list) '..._1 #f)))
    (test-ellipses '((repeat a #f ..._!_1))
                   `(,(make-repeat 'a (list) #f '..._!_1)))
    
    (test-empty '(list) '() (list (make-test-mtch (make-bindings null) '() none)))
    (test-empty '(list a) '(a) (list (make-test-mtch (make-bindings null) '(a) none)))
    (test-empty '(list a) '(b) #f)
    (test-empty '(list a b) '(a b) (list (make-test-mtch (make-bindings null) '(a b) none)))
    (test-empty '(list a b) '(a c) #f)
    (test-empty '(list) 1 #f)
    (test-empty '(list #f x) '(#f x) (list (make-test-mtch (make-bindings null) '(#f x) none)))
    (test-empty '(list #f (name y any)) '(#f) #f)
    (test-empty '(in-hole (list z hole) a) '(z a) (list (make-test-mtch (make-bindings (list)) '(z a) none)))
    (test-empty '(in-hole (list z hole) (in-hole (list x hole) a)) 
                '(z (x a))
                (list (make-test-mtch (make-bindings (list)) '(z (x a)) none)))
    
    (run-test/cmp (this-line)
                  'in-hole-zero-holes 
                  (with-handlers ([exn:fail? (λ (e) (regexp-match #rx"no hole" (exn-message e)))])
                    (test-empty '(in-hole (list 1 2) 2) '(1 2) 'never-gets-here)
                    'should-have-raised-an-exception)
                  '("no hole")
                  equal?)
                
    
    (test-empty '(in-hole (in-hole (list x hole) hole) y)
                '(x y)
                (list (make-test-mtch (make-bindings (list)) '(x y) none)))
    
    (test-empty '(list (name number number) (name number number)) '(1 1) (list (make-test-mtch (make-bindings (list (make-bind 'number 1))) '(1 1) none)))
    (test-empty '(list (name x (name number number)) (name x (name number number)))
                '(1 1)
                (list (make-test-mtch (make-bindings (list (make-bind 'x 1) (make-bind 'number 1))) '(1 1) none)))
    (test-empty '(list (name x (name number_q number)) (name x (name number_r number)))
                '(1 1)
                (list (make-test-mtch (make-bindings (list (make-bind 'x 1) 
                                                           (make-bind 'number_q 1)
                                                           (make-bind 'number_r 1)))
                                      '(1 1)
                                      none)))
    (test-empty '(list (name number number) (name number number)) '(1 2) #f)
    (test-empty '(list (name x number) (name x number)) '(1 2) #f)
    (test-empty '(list (name x number_q) (name x number_r)) '(1 2) #f)
    
    (test-empty '(list (repeat a #f #f)) '() (list (make-test-mtch (make-bindings empty) '() none)))
    (test-empty '(list (repeat a #f #f)) '(a) (list (make-test-mtch (make-bindings empty) '(a) none)))
    (test-empty '(list (repeat a #f #f)) '(a a) (list (make-test-mtch (make-bindings empty) '(a a) none)))
    (test-empty '(list (repeat (name x a) #f #f)) '() (list (make-test-mtch (make-bindings (list (make-bind 'x '()))) '() none)))
    (test-empty '(list (repeat (name x a) #f #f)) '(a) (list (make-test-mtch (make-bindings (list (make-bind 'x '(a)))) '(a) none)))
    (test-empty '(list (repeat (name x a) #f #f)) '(a a) (list (make-test-mtch (make-bindings (list (make-bind 'x '(a a)))) '(a a) none)))
    (test-empty '(list (repeat hole #f #f)) '() (list (make-test-mtch (make-bindings empty) '() none)))
    
    (test-empty '(list (repeat b #f #f) (repeat a #f #f)) '() (list (make-test-mtch (make-bindings empty) '() none)))
    (test-empty '(list (repeat b #f #f) (repeat a #f #f)) '(a) (list (make-test-mtch (make-bindings empty) '(a) none)))
    (test-empty '(list (repeat b #f #f) (repeat a #f #f)) '(b) (list (make-test-mtch (make-bindings empty) '(b) none)))
    (test-empty '(list (repeat b #f #f) (repeat a #f #f)) '(b a) (list (make-test-mtch (make-bindings empty) '(b a) none)))
    (test-empty '(list (repeat b #f #f) (repeat a #f #f)) '(b b a a) (list (make-test-mtch (make-bindings empty) '(b b a a) none)))
    (test-empty '(list (repeat b #f #f) (repeat a #f #f)) '(a a) (list (make-test-mtch (make-bindings empty) '(a a) none)))
    (test-empty '(list (repeat b #f #f) (repeat a #f #f)) '(b b) (list (make-test-mtch (make-bindings empty) '(b b) none)))
    
    (test-empty '(list (repeat a ..._1 #f) (repeat a ..._2 #f)) 
                '(a) 
                (list (make-test-mtch (make-bindings (list (make-bind '..._1 1) (make-bind '..._2 0))) '(a) none)
                      (make-test-mtch (make-bindings (list (make-bind '..._1 0) (make-bind '..._2 1))) '(a) none)))
    (test-empty '(list (repeat a ..._1 #f) (repeat a ..._1 #f)) '(a) #f)
    (test-empty '(list (repeat a ..._1 #f) (repeat a ..._1 #f))
                '(a a) 
                (list (make-test-mtch (make-bindings (list (make-bind '..._1 1))) '(a a) none)))
    
    (test-empty '(list (repeat (list (repeat a ..._1 #f) (repeat a ..._1 #f)) #f #f))
                '((a a a a)) 
                (list (make-test-mtch (make-bindings (list (make-bind '..._1 '(2)))) '((a a a a)) none)))
    (test-empty '(list (repeat (list (repeat a #f ..._!_1) (repeat a #f ..._!_1)) #f #f))
                '((a a a a)) 
                (list (make-test-mtch (make-bindings '()) '((a a a a)) none)
                      (make-test-mtch (make-bindings '()) '((a a a a)) none)
                      (make-test-mtch (make-bindings '()) '((a a a a)) none)
                      (make-test-mtch (make-bindings '()) '((a a a a)) none)))

    (test-empty '(list (repeat (name x a) #f ..._!_1) (repeat (name y a) #f ..._!_1)) 
                '(a a) 
                (list (make-test-mtch (make-bindings (list (make-bind 'x '()) (make-bind 'y '(a a)))) '(a a) none)
                      (make-test-mtch (make-bindings (list (make-bind 'x '(a a)) (make-bind 'y '()))) '(a a) none)))
    
    (test-empty '(list (repeat (name y b) #f #f) (repeat (name x a) #f #f)) '() 
                (list (make-test-mtch (make-bindings (list (make-bind 'x '())
                                                      (make-bind 'y '())))
                                 '()
                                 none)))
    (test-empty '(list (repeat (name y b) #f #f) (repeat (name x a) #f #f)) '(a)
                (list (make-test-mtch (make-bindings (list (make-bind 'x '(a))
                                                      (make-bind 'y '())))
                                 '(a)
                                 none)))
    (test-empty '(list (repeat (name y b) #f #f) (repeat (name x a) #f #f)) '(b) 
                (list (make-test-mtch (make-bindings (list (make-bind 'x '())
                                                      (make-bind 'y '(b))))
                                 '(b)
                                 none)))
    (test-empty '(list (repeat (name y b) #f #f) (repeat (name x a) #f #f)) '(b b a a) 
                (list (make-test-mtch (make-bindings (list (make-bind 'x '(a a))
                                                      (make-bind 'y '(b b))))
                                 '(b b a a)
                                 none)))
    (test-empty '(list (repeat (name y a) #f #f) (repeat (name x a) #f #f)) '(a) 
                (list (make-test-mtch (make-bindings (list (make-bind 'x '())
                                                      (make-bind 'y '(a))))
                                 '(a)
                                 none)
                      (make-test-mtch (make-bindings (list (make-bind 'x '(a))
                                                      (make-bind 'y '())))
                                 '(a)
                                 none)))
    (test-empty '(list (repeat (name y a) #f #f) (repeat (name x a) #f #f)) '(a a) 
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
    
    (test-ab (this-line)
             '(list (repeat (name bb_y (nt bb)) #f #f) (repeat (name aa_x (nt aa)) #f #f))
             '() 
             (list (make-test-mtch (make-bindings (list (make-bind 'aa_x '())
                                                        (make-bind 'bb_y '())))
                                   '()
                                   none)))
    (test-ab (this-line)
             '(list (repeat (name bb_y (nt bb)) #f #f) (repeat (name aa_x (nt aa)) #f #f))
             '(a)
             (list (make-test-mtch (make-bindings (list (make-bind 'aa_x '(a))
                                                   (make-bind 'bb_y '())))
                              '(a) 
                              none)))
    (test-ab (this-line)
             '(list (repeat (name bb_y (nt bb)) #f #f) (repeat (name aa_x (nt aa)) #f #f))
             '(b) 
             (list (make-test-mtch (make-bindings (list (make-bind 'aa_x '())
                                                   (make-bind 'bb_y '(b))))
                              '(b)
                              none)))
    (test-ab (this-line)
             '(list (repeat (name bb_y (nt bb)) #f #f) (repeat (name aa_x (nt aa)) #f #f))
             '(b b a a) 
             (list (make-test-mtch (make-bindings (list (make-bind 'aa_x '(a a))
                                                   (make-bind 'bb_y '(b b))))
                              '(b b a a)
                              none)))
    (test-ab (this-line)
             '(list (repeat (name aa_y (nt aa)) #f #f) (repeat (name aa_x (nt aa)) #f #f))
             '(a) 
             (list (make-test-mtch (make-bindings (list (make-bind 'aa_x '())
                                                   (make-bind 'aa_y '(a))))
                              '(a)
                              none)
                   (make-test-mtch (make-bindings (list (make-bind 'aa_x '(a))
                                                   (make-bind 'aa_y '())))
                              '(a)
                              none)))
    (test-ab (this-line)
             '(list (repeat (name aa_y (nt aa)) #f #f) (repeat (name aa_x (nt aa)) #f #f))
             '(a a) 
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
    
    (test-empty '(list (repeat (name x (name number number)) #f #f))
                '(1 2)
                (list (make-test-mtch (make-bindings (list (make-bind 'x '(1 2)) (make-bind 'number '(1 2)))) '(1 2) none)))
    
    (test-empty '(list (repeat a #f #f)) '(b) #f)
    (test-empty '(list (repeat a #f #f) (repeat b #f #f)) '(c) #f)
    (test-empty '(list (repeat a #f #f) b) '(b c) #f)
    (test-empty '(list (repeat a #f #f) b) '(a b c) #f)
    
    (test-lang (this-line)
               '(list (name n (nt n)) (repeat (name n (nt n)) #f #f))
               '((1 1) 1 1)
               (list (make-mtch (make-bindings (list (make-bind 'n '(1 1)))) '((1 1) 1 1) none))
               (list (make-nt 'n (list (make-rhs 'any) (make-rhs 'number)))))
    (test-lang (this-line)
               '(list (name n (nt n)) (list (repeat (name n (nt n)) #f #f)))
               '((1 1) (1 1))
               (list (make-mtch (make-bindings (list (make-bind 'n '(1 1)))) '((1 1) (1 1)) none))
               (list (make-nt 'n (list (make-rhs 'any) (make-rhs 'number)))))
    (test-empty '(list (name x (name any any)) 
                       (list (repeat (name x (name number number)) #f #f)))
                '((1 1) (1 1))
                (list (make-test-mtch (make-bindings (list (make-bind 'x '(1 1))
                                                           (make-bind 'any '(1 1))
                                                           (make-bind 'number '(1 1))))
                                      '((1 1) (1 1)) 
                                      none)))
    
    (test-empty '(list (repeat (list variable_1 variable_1) #f #f))
                '((x y))
                #f)
    
    (test-empty '(list (repeat (name number number) #f #f)) '()
                (list (make-test-mtch (make-bindings (list (make-bind 'number '()))) '() none)))
    (test-ab (this-line)
             '(list (repeat (name aa aa) #f #f))
             '()
             (list (make-test-mtch (make-bindings (list (make-bind 'aa '()))) '() none)))
    
    
    ;; testing block-in-hole
    (test-empty '(hide-hole a) 'b #f)
    (test-empty '(hide-hole a) 'a (list (make-test-mtch (make-bindings '()) 'a none)))
    (test-empty '(hide-hole a) '(block-in-hole a) #f)
    (test-empty '(in-hole (list x hole) 1) '(x 1) (list (make-test-mtch (make-bindings '()) '(x 1) none)))
    (test-empty '(in-hole (list hole (hide-hole hole)) junk)
                '(junk junk2)
                #f)
    
    (test-xab '(name lsts (nt lsts)) '() (list (make-test-mtch (make-bindings (list (make-bind 'lsts '()))) '() none)))
    (test-xab '(name lsts (nt lsts)) '(x) (list (make-test-mtch (make-bindings (list (make-bind 'lsts '(x)))) '(x) none)))
    (test-xab '(name lsts (nt lsts)) 'x (list (make-test-mtch (make-bindings (list (make-bind 'lsts 'x))) 'x none)))
    (test-xab '(name lsts (nt lsts)) #f (list (make-test-mtch (make-bindings (list (make-bind 'lsts #f))) #f none)))
    (test-xab '(name split-out (nt split-out)) '1 (list (make-test-mtch (make-bindings (list (make-bind 'split-out 1))) '1 none)))

    (test-xab '(name exp (nt exp)) 1 (list (make-test-mtch (make-bindings (list (make-bind 'exp 1))) 1 none)))
    (test-xab '(name exp (nt exp)) '(+ 1 2) (list (make-test-mtch (make-bindings (list (make-bind 'exp '(+ 1 2)))) '(+ 1 2) none)))
    (test-xab '(in-hole (name ctxt (nt ctxt)) (name any any))
              '1
              (list (make-test-mtch (make-bindings (list (make-bind 'ctxt the-hole) (make-bind 'any 1))) 1 none)))
    (test-xab '(in-hole (name ctxt (nt ctxt)) (name x (name any any)))
              '1
              (list (make-test-mtch (make-bindings (list (make-bind 'ctxt the-hole) (make-bind 'x 1) (make-bind 'any 1))) 1 none)))
    (test-xab '(in-hole (name c (name ctxt (nt ctxt))) (name x (name any any)))
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
    (test-xab '(in-hole (name c (name ctxt (nt ctxt))) (name i (list + (name number_1 number) (name number_2 number))))
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
    
    (test-empty '(in-hole (list (list z hole)) (name x (name any any)))
                '((z a))
                (list (make-test-mtch (make-bindings (list (make-bind 'x 'a) (make-bind 'any 'a))) '((z a)) none)))
    (test-empty '(in-hole (name c (list (repeat z #f #f) hole (repeat z #f #f))) (name any any))
                '(z z)
                (list 
                 (make-test-mtch (make-bindings (list (make-bind 'c `(z ,the-hole)) (make-bind 'any 'z))) '(z z) none)
                 (make-test-mtch (make-bindings (list (make-bind 'c `(,the-hole z)) (make-bind 'any 'z))) '(z z) none)))
    (test-empty '(in-hole (name c (list (repeat z #f #f) hole (repeat z #f #f))) (name any any))
                '(z z z)
                (list 
                 (make-test-mtch (make-bindings (list (make-bind 'c `(z z ,the-hole)) (make-bind 'any 'z))) '(z z z) none)
                 (make-test-mtch (make-bindings (list (make-bind 'c `(z ,the-hole z)) (make-bind 'any 'z))) '(z z z) none)
                 (make-test-mtch (make-bindings (list (make-bind 'c `(,the-hole z z)) (make-bind 'any 'z))) '(z z z) none)))
    
    (test-empty '(list z (in-hole (name c (list z hole)) a))
                '(z (z a))
                (list 
                 (make-test-mtch (make-bindings (list (make-bind 'c `(z ,the-hole))))
                            '(z (z a))
                            none)))
    
    (test-empty '(list a (in-hole (name c1 (list b (in-hole (name c2 (list c hole)) d) hole)) e))
                '(a (b (c d) e))
                (list 
                 (make-test-mtch (make-bindings (list (make-bind 'c2 `(c ,the-hole))
                                                 (make-bind 'c1 `(b (c d) ,the-hole))))
                            '(a (b (c d) e))
                            none)))

    (test-empty '(in-hole (in-hole hole hole) a)
                'a
                (list (make-test-mtch (make-bindings (list)) 'a none)))
    
    (test-empty '(list a (list b (in-hole (name c1 (in-hole (name c2 (list c hole)) (list d hole))) e)))
                '(a (b (c (d e))))
                (list 
                 (make-test-mtch (make-bindings (list (make-bind 'c1 `(c (d ,the-hole)))
                                                 (make-bind 'c2 `(c ,the-hole))))
                            '(a (b (c (d e))))
                            none)))
    
    (test-empty `(list + 1 (side-condition (name any any) ,(lambda (bindings) #t) #t))
                '(+ 1 b)
                (list (make-test-mtch (make-bindings (list (make-bind 'any 'b))) '(+ 1 b) none)))
    (test-empty `(list + 1 (side-condition (name any any) ,(lambda (bindings) #f) #f))
                '(+ 1 b)
                #f)
    
    (test-empty `(list + 1 (side-condition b ,(lambda (bindings) #t) #t))
                '(+ 1 b)
                (list (make-test-mtch (make-bindings '()) '(+ 1 b) none)))
    (test-empty `(list + 1 (side-condition a ,(lambda (bindings) #t) #t) #t)
                '(+ 1 b)
                #f)

    (test-empty `(side-condition (name x (name any any)) ,(lambda (bindings) (eq? (lookup-binding bindings 'x) 'a)) side-condition-srcloc)
                'a
                (list 
                 (make-test-mtch (make-bindings (list (make-bind 'x 'a)
                                                      (make-bind 'any 'a)))
                            'a
                            none)))

    (test-empty `(list + 1 (side-condition (name x (name any any))
                                           ,(lambda (bindings) (eq? (lookup-binding bindings 'x) 'a)) side-condition-srcloc))
                '(+ 1 a)
                (list 
                 (make-test-mtch (make-bindings (list (make-bind 'x 'a)
                                                      (make-bind 'any 'a)))
                            '(+ 1 a)
                            none)))

    (test-empty `(side-condition (name x any) ,(lambda (bindings) (eq? (lookup-binding bindings 'x) 'a)) (eq? (term x) 'a))
                'b
                #f)
    
    (test-empty `(list + 1 (side-condition (name x any) ,(lambda (bindings) (eq? (lookup-binding bindings 'x) 'a)) (eq? (term x) 'a)))
                '(+ 1 b)
                #f)
    
    (test-empty `(side-condition (list (list (repeat any_1 ..._a #f)) (list (repeat any_2 ..._a #f)))
                                 ,(lambda (bindings) (error 'should-not-be-called))
                                 (error 'should-not-be-called))
                '((1 2 3) (4 5))
                #f)
    
    (test-xab '(name exp_1 (nt exp))
              '(+ 1 2)
              (list (make-test-mtch (make-bindings (list (make-bind 'exp_1 '(+ 1 2)))) '(+ 1 2) none)))
    (test-xab '(list (name exp_1 (nt exp)) (name exp_2 (nt exp)))
              '((+ 1 2) (+ 3 4))
              (list (make-test-mtch (make-bindings (list (make-bind 'exp_1 '(+ 1 2)) (make-bind 'exp_2 '(+ 3 4))))
                               '((+ 1 2) (+ 3 4))
                               none)))
    (test-xab '(list (name exp_1 (nt exp)) (name exp_1 (nt exp)))
              '((+ 1 2) (+ 3 4))
              #f)
    (test-xab '(name nesting-names (nt nesting-names))
              'b
              (list (make-test-mtch (make-bindings (list (make-bind 'nesting-names 'b))) 'b none)))
    (test-xab '(name nesting-names (nt nesting-names))
              '(a b)
              (list (make-test-mtch (make-bindings (list (make-bind 'nesting-names '(a b)))) '(a b) none)))
    (test-xab '(name nesting-names (nt nesting-names))
              '(a (a b))
              (list (make-test-mtch (make-bindings (list (make-bind 'nesting-names '(a (a b))))) '(a (a b)) none)))
    (test-xab '(list (name x a) (name nesting-names (nt nesting-names)))
              '(a (a (a b)))
              (list (make-test-mtch (make-bindings (list (make-bind 'x 'a)
                                                         (make-bind 'nesting-names '(a (a b)))))
                                    '(a (a (a b))) none)))
    (test-xab '(name nesting-names (nt nesting-names))
              '(a (a (a (a b))))
              (list (make-test-mtch (make-bindings (list (make-bind 'nesting-names '(a (a (a (a b)))))))
                                    '(a (a (a (a b)))) none)))
    
    (test-xab '(name same-in-nt (nt same-in-nt))
              '(x x)
              (list (make-test-mtch (make-bindings (list (make-bind 'same-in-nt '(x x)))) '(x x) none)))
    (test-xab '(name same-in-nt (nt same-in-nt))
              '(x y)
              #f)
    
    (test-xab '(in-hole (cross forever-list-forever-list) 1)
              '(a b c)
              #f)
    
    (test-xab '(in-hole (cross forever-list-forever-list) 1)
              '(1 x x)
              (list (make-test-mtch (make-bindings '()) '(1 x x) none)))
    
    (test-xab '(in-hole (cross forever-list-forever-list) 1)
              '(x 1 x)
              (list (make-test-mtch (make-bindings '()) '(x 1 x) none)))
    
    
    (test-xab '(in-hole (cross simple-simple) g)
              'g
              (list (make-mtch (make-bindings (list)) 'g none)))
    
    (test-xab '(name var (nt var)) '+ #f)
    (test-xab '(name var (nt var)) 'anunusedvariable (list (make-mtch (make-bindings (list (make-bind 'var 'anunusedvariable))) 'anunusedvariable none)))
    (test-xab '(name var (nt var)) 'exp (list (make-mtch (make-bindings (list (make-bind 'var 'exp))) 'exp none)))
    (test-xab '(name var (nt var)) 'exp_x (list (make-mtch (make-bindings (list (make-bind 'var 'exp_x))) 'exp_x none)))
    
    (test-xab '(name underscore (nt underscore)) '(+ 1 2) (list (make-mtch (make-bindings (list (make-bind 'underscore '(+ 1 2)))) '(+ 1 2) none)))
    (test-xab '(name underscore (nt underscore)) '2 (list (make-mtch (make-bindings (list (make-bind 'underscore 2))) 2 none)))
  
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; tests to check on the name/non-name optimization likely
    ;; many of these are duplicates of the ones above, but it 
    ;; is hard to know which are and which aren't.
    ;;
    
    (test-xab '(name x (name y 1)) 1 (list (make-mtch (make-bindings (list (make-bind 'x 1) (make-bind 'y 1))) 1 none)))
    (test-xab '(list (mismatch-name y_!_1 1)
                     (mismatch-name y_!_1 2))
              '(1 2)
              (list (make-mtch (make-bindings '()) '(1 2) none)))
    (test-xab '(list (mismatch-name x_!_1 (name a 1))
                     (mismatch-name x_!_1 (name b 2)))
              '(1 2)
              (list (make-mtch (make-bindings (list (make-bind 'a 1) (make-bind 'b 2))) '(1 2) none)))
    (test-xab '(in-hole (name x hole) (name y 1)) 1 (list (make-mtch (make-bindings (list (make-bind 'x the-hole) (make-bind 'y 1)))
                                                                     1
                                                                     none)))
    (test-xab '(in-hole (name x hole) 1) 1 (list (make-mtch (make-bindings (list (make-bind 'x the-hole)))
                                                            1
                                                            none)))
    (test-xab '(in-hole hole (name y 1)) 1 (list (make-mtch (make-bindings (list (make-bind 'y 1)))
                                                            1
                                                            none)))
    (test-xab '(in-hole hole 1) 1 (list (make-mtch (make-bindings '()) 1 none)))
    (test-xab '(hide-hole (list hole 1)) `(,the-hole 1) (list (make-mtch (make-bindings '()) `(,the-hole 1) none)))
    (test-xab '(hide-hole (list 2 1)) `(2 1) (list (make-mtch (make-bindings '()) `(2 1) none)))
    (test-xab '(hide-hole (list (name z 2) 1)) `(2 1) (list (make-mtch (make-bindings (list (make-bind 'z 2))) `(2 1) none)))
    (test-xab `(side-condition (name x 1) ,(λ (bindings) (equal? bindings (make-bindings (list (make-bind 'x 1))))) 'srcloc)
              1
              (list (make-mtch (make-bindings (list (make-bind 'x 1))) 1 none)))
    (test-xab `(side-condition 2 ,(λ (bindings) (equal? bindings (make-bindings '()))) 'srcloc)
              2
              (list (make-mtch (make-bindings '()) 2 none)))
    (test-xab '(list (repeat (name x 1) #f #f))
              '(1 1 1)
              (list (make-mtch (make-bindings (list (make-bind 'x '(1 1 1)))) '(1 1 1) none)))
    (test-xab '(list (repeat 1 ..._1 #f))
              '(1 1 1)
              (list (make-mtch (make-bindings (list (make-bind '..._1 3))) '(1 1 1) none)))
    (test-xab '(list (repeat 1 #f ..._!_1)
                     (repeat 2 #f ..._!_1))
              '(1 1 2)
              (list (make-mtch (make-bindings '()) '(1 1 2) none)))
    (test-xab '(list (repeat 1 #f #f))
              '(1 1 1)
              (list (make-mtch (make-bindings '()) '(1 1 1) none)))
    
    (test-xab '(in-hole (name hh-D (nt hh-D)) whatever)
              `(,the-hole whatever)
              (list (make-mtch (make-bindings (list (make-bind 'hh-D (list the-hole the-not-hole))))
                               `(,the-hole whatever)
                               none)))

    ;;
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    
    (run-test
     (this-line)
     'compatible-context-language1
     (build-compatible-context-language
      (mk-hasheq '((exp . ()) (ctxt . ())))
      (list (make-nt 'exp
                     (list (make-rhs '(list + (nt exp) (nt exp)))
                           (make-rhs 'number)))
            (make-nt 'ctxt
                     (list (make-rhs '(list + (nt ctxt) (nt exp)))
                           (make-rhs '(list + (nt exp) (nt ctxt)))
                           (make-rhs 'hole)))))
     (list
      (make-nt 'ctxt-ctxt
               (list (make-rhs 'hole)
                     (make-rhs `(list (hide-hole +) (cross ctxt-ctxt) (hide-hole (nt exp))))
                     (make-rhs `(list (hide-hole +) (hide-hole (nt ctxt)) (cross ctxt-exp)))
                     (make-rhs `(list (hide-hole +) (cross ctxt-exp) (hide-hole (nt ctxt))))
                     (make-rhs `(list (hide-hole +) (hide-hole (nt exp)) (cross ctxt-ctxt)))))
      (make-nt 'ctxt-exp
               (list (make-rhs `(list (hide-hole +) (cross ctxt-exp) (hide-hole (nt exp))))
                     (make-rhs `(list (hide-hole +) (hide-hole (nt exp)) (cross ctxt-exp)))))
      (make-nt 'exp-ctxt
               (list (make-rhs `(list (hide-hole +) (cross exp-ctxt) (hide-hole (nt exp))))
                     (make-rhs `(list (hide-hole +) (hide-hole (nt ctxt)) (cross exp-exp)))
                     (make-rhs `(list (hide-hole +) (cross exp-exp) (hide-hole (nt ctxt))))
                     (make-rhs `(list (hide-hole +) (hide-hole (nt exp)) (cross exp-ctxt)))))
      (make-nt 'exp-exp 
               (list (make-rhs 'hole) 
                     (make-rhs `(list (hide-hole +) (cross exp-exp) (hide-hole (nt exp)))) 
                     (make-rhs `(list (hide-hole +) (hide-hole (nt exp)) (cross exp-exp)))))))
    
    (run-test
     (this-line)
     'compatible-context-language2
     (build-compatible-context-language
      (mk-hasheq '((m . ()) (v . ())))
      (list (make-nt 'm (list (make-rhs '(list (nt m) (nt m))) (make-rhs '(list + (nt m) (nt m))) (make-rhs '(nt v))))
            (make-nt 'v (list (make-rhs 'number) (make-rhs '(list lambda (list x) (nt m)))))))
     (list
      (make-nt 'v-v (list (make-rhs 'hole) (make-rhs '(list (hide-hole lambda) (hide-hole (list x)) (cross v-m)))))
      (make-nt 'v-m
               (list
                (make-rhs '(list (cross v-m) (hide-hole (nt m))))
                (make-rhs '(list (hide-hole (nt m)) (cross v-m)))
                (make-rhs '(list (hide-hole +) (cross v-m) (hide-hole (nt m))))
                (make-rhs '(list (hide-hole +) (hide-hole (nt m)) (cross v-m)))
                (make-rhs '(cross v-v))))
      (make-nt 'm-v (list (make-rhs '(list (hide-hole lambda) (hide-hole (list x)) (cross m-m)))))
      (make-nt 'm-m
               (list
                (make-rhs 'hole)
                (make-rhs '(list (cross m-m) (hide-hole (nt m))))
                (make-rhs '(list (hide-hole (nt m)) (cross m-m)))
                (make-rhs '(list (hide-hole +) (cross m-m) (hide-hole (nt m))))
                (make-rhs '(list (hide-hole +) (hide-hole (nt m)) (cross m-m)))
                (make-rhs '(cross m-v))))))
    
    (run-test
     (this-line)
     'compatible-context-language3
     (build-compatible-context-language
      (mk-hasheq '((m . ()) (seven . ())))
      (list (make-nt 'm (list (make-rhs '(list (nt m) (nt seven) (nt m)))
                              (make-rhs 'number)))
            (make-nt 'seven (list (make-rhs 7)))))
     `(,(make-nt
         'm-m
         `(,(make-rhs 'hole) 
           ,(make-rhs `(list (cross m-m) (hide-hole (nt seven)) (hide-hole (nt m))))
           ,(make-rhs `(list (hide-hole (nt m)) (hide-hole (nt seven)) (cross m-m)))))
       ,(make-nt
         'seven-m
         `(,(make-rhs `(list (cross seven-m) (hide-hole (nt seven)) (hide-hole (nt m))))
           ,(make-rhs `(list (hide-hole (nt m)) (cross seven-seven) (hide-hole (nt m))))
           ,(make-rhs `(list (hide-hole (nt m)) (hide-hole (nt seven)) (cross seven-m)))))
       ,(make-nt 'seven-seven `(,(make-rhs 'hole)))))
    
    (run-test
     (this-line)
     'compatible-context-language4
     (build-compatible-context-language
      (mk-hasheq '((a . ()) (b . ()) (c . ())))
      (list (make-nt 'a (list (make-rhs '(nt b))))
            (make-nt 'b (list (make-rhs '(nt c))))
            (make-nt 'c (list (make-rhs 3)))))
     (list (make-nt 'c-c (list (make-rhs 'hole)))
           (make-nt 'c-b (list (make-rhs '(cross c-c))))
           (make-nt 'c-a (list (make-rhs '(cross c-b))))
           (make-nt 'b-b (list (make-rhs 'hole)))
           (make-nt 'b-a (list (make-rhs '(cross b-b))))
           (make-nt 'a-a (list (make-rhs 'hole)))))
    
    (run-test
     (this-line)
     'compatible-context-language5
     (build-compatible-context-language
      (mk-hasheq '((a . ()) (b . ()) (c . ())))
      (list (make-nt 'a (list (make-rhs '1) (make-rhs '2) (make-rhs '3)))
            (make-nt 'b (list (make-rhs '(nt a))
                              (make-rhs '(list (name a_1 (nt a)) (mismatch-name b_!_1 (nt b))))))))
      (list (make-nt 'a-a (list (make-rhs 'hole)))
            (make-nt 'a-b (list (make-rhs '(cross a-a))
                                (make-rhs '(list (name a_1 (cross a-a)) (hide-hole (mismatch-name b_!_1 (nt b)))))
                                (make-rhs '(list (hide-hole (name a_1 (nt a))) (mismatch-name b_!_1 (cross a-b)))))) 
            (make-nt 'b-b (list (make-rhs 'hole) 
                                (make-rhs '(list (hide-hole (name a_1 (nt a))) (mismatch-name b_!_1 (cross b-b))))))))
    
    (test-ellipsis-binding '(list (repeat (list (name number_1 number) (name number_2 number)) #f #f)) '() '((1 2)))
    (test-ellipsis-binding '(list (repeat (name x (name number_1 number)) #f #f)) '() '(1 2))
    (test-ellipsis-binding '(list (repeat (list (list (repeat (name number_1 number) #f #f)) 
                                                (list (repeat (name number_2 number) #f #f)))
                                          #f
                                          #f))
                           '()
                           '(((1) (2))))
    (test-ellipsis-binding '(list (repeat number #f #f) variable) '() '(1 x))
    (test-ellipsis-binding '(list (repeat (in-hole (name H_1 (nt H)) (name number_1 number)) #f #f)) '((H hole)) '(1 2))
    
    (cond
      [(= failures 0)
       (printf "matcher-test.rkt: all ~a tests passed.\n" test-count)]
      [else
       (eprintf "matcher-test.rkt: ~a test~a failed.\n" 
                failures
                (if (= failures 1)
                    ""
                    "s"))])))

  ;; mk-hasheq : (listof (cons sym any)) -> hash
  ;; builds a hash table that has the bindings in assoc-list
  (define (mk-hasheq assoc-list)
    (let ([ht (make-hash)])
      (for-each
       (lambda (a)
         (hash-set! ht (car a) (cdr a)))
       assoc-list)
      ht))
  
  ;; test-empty : sexp[pattern] sexp[term] answer -> void
  ;; returns #t if pat matching exp with the empty language produces ans.
  (define-syntax (test-empty stx)
    (syntax-case stx ()
      [(_ . args)
       (with-syntax ([line (syntax-line stx)])
         #'(test-empty/proc line . args))]))
  
  (define (test-empty/proc line pat exp ans)
    (run-match-test
     line
     `(match-pattern (compile-pattern (compile-language 'pict-stuff-not-used '() (hash)) ',pat #t) ',exp)
     (match-pattern 
      (compile-pattern (compile-language 'pict-stuff-not-used '() (hash)) pat #t)
      exp)
     ans))
  
  ;; test-lang : sexp[pattern] sexp[term] answer (list/c nt) -> void
  ;; returns #t if pat matching exp with the language defined by the given nts
  (define (test-lang line pat exp ans nts)
    (let ([nt-map (mk-uf-sets (map (λ (x) (list (nt-name x)))
                                   nts))])
      (run-match-test
       line
       `(match-pattern (compile-pattern (compile-language 'pict-stuff-not-used ',nts ,nt-map) ',pat #t) ',exp)
       (match-pattern 
        (compile-pattern (compile-language 'pict-stuff-not-used nts nt-map) pat #t)
        exp)
       ans)))
  
  (define xab-lang #f)
  ;; test-xab : sexp[pattern] sexp[term] answer -> void
  ;; returns #t if pat matching exp with a simple language produces ans.
  
  (define-syntax (test-xab stx)
    (syntax-case stx ()
      [(_ . args)
       (with-syntax ([line (syntax-line stx)])
         #'(test-xab/proc line . args))]))
  
  (define (test-xab/proc line pat exp ans)
    (unless xab-lang
      (let ([nts
             (list (make-nt 'exp
                            (list (make-rhs '(list + (nt exp) (nt exp)))
                                  (make-rhs 'number)))
                   (make-nt 'ctxt
                            (list (make-rhs '(list + (nt ctxt) (nt exp)))
                                  (make-rhs '(list + (nt exp) (nt ctxt)))
                                  (make-rhs 'hole)))
                   
                   (make-nt 'ec-one
                            (list (make-rhs '(list + hole (nt exp)))
                                  (make-rhs '(list + (nt exp) hole))))
                   
                   (make-nt 'same-in-nt (list (make-rhs '(list (name x any) (name x any)))))
                   
                   (make-nt 'forever-list (list (make-rhs '(list (nt forever-list) (repeat (nt forever-list) #f #f)))
                                                (make-rhs 'x)))
                   
                   (make-nt 'lsts
                            (list (make-rhs '(list))
                                  (make-rhs '(list x))
                                  (make-rhs 'x)
                                  (make-rhs '#f)))
                   (make-nt 'split-out
                            (list (make-rhs '(nt split-out2))))
                   (make-nt 'split-out2
                            (list (make-rhs 'number)))
                   
                   (make-nt 'simple (list (make-rhs 'simple-rhs)))
                   
                   (make-nt 'nesting-names
                            (list (make-rhs '(list a (name x (nt nesting-names))))
                                  (make-rhs 'b)))
                   (make-nt 'var (list (make-rhs `variable-not-otherwise-mentioned)))
                   
                   (make-nt 'underscore (list (make-rhs '(name exp_1 (nt exp)))))

                   (make-nt 'hh-v (list (make-rhs '(hide-hole (nt hh-D)))))
                   (make-nt 'hh-D (list (make-rhs 'hole) (make-rhs '(list (nt hh-v) (nt hh-D)))))
                   
                   )])
      (set! xab-lang
            (compile-language 'pict-stuff-not-used
                              nts
                              (mk-uf-sets (map (λ (x) (list (nt-name x))) nts))))))
    (run-match-test
     line
     `(match-pattern (compile-pattern xab-lang ',pat #t) ',exp)
     (match-pattern (compile-pattern xab-lang pat #t) exp)
     ans))
  
  (define ab-lang #f)
  ;; test-ab : sexp[pattern] sexp[term] answer -> void
  ;; returns #t if pat matching exp with a simple language produces ans.
  (define (test-ab line pat exp ans)
    (unless ab-lang
      (set! ab-lang
            (compile-language 
             'pict-stuff-not-used
             (list (make-nt 'aa
                            (list (make-rhs 'a)))
                   (make-nt 'bb
                            (list (make-rhs 'b))))
             (mk-uf-sets '((aa) (bb))))))
    (run-match-test
     line
     `(match-pattern (compile-pattern ab-lang ',pat #t) ',exp)
     (match-pattern (compile-pattern ab-lang pat #t) exp)
     ans))
  
  ;; test-ellipses : sexp sexp -> void
  (define-syntax (test-ellipses stx)
    (syntax-case stx ()
      [(_ . args)
       (with-syntax ([line (syntax-line stx)])
         #'(test-ellipses/proc line . args))]))
  
  ;; pats : (listof pat)
  (define (test-ellipses/proc line pats expected)
    (run-test
     line
     `(rewrite-ellipses ',pats (lambda (x) (values x #f #f)))
     (let-values ([(compiled-pattern has-hole? has-hide-hole? names) (rewrite-ellipses pats (lambda (x) (values x #f #f '())))])
       compiled-pattern)
     expected))
  
  ;; test-ellipsis-binding: sexp sexp sexp -> boolean
  ;; Checks that `extract-empty-bindings' produces bindings in the same order
  ;; as the matcher, as required by `collapse-single-multiples'
  (define-syntax (test-ellipsis-binding stx)
    (syntax-case stx ()
      [(_ . args)
       (with-syntax ([line (syntax-line stx)])
         #'(test-ellipsis-binding/proc line . args))]))
  
  (define (test-ellipsis-binding/proc line pat nts/sexp exp)
    (define (binding-names bindings)
      (map (λ (b)
             (cond [(bind? b) (bind-name b)]
                   [(mismatch-bind? b) (mismatch-bind-name b)]))
           bindings))
    (run-test
     line
     `(test-ellipsis-binding ,pat)
     (let ([mtch ((compiled-pattern-cp
                   (let ([nts (map (λ (nt-def) (nt (car nt-def) (map rhs (cdr nt-def)))) nts/sexp)])
                     (compile-pattern (compile-language 'pict-stuff-not-used nts 
                                                        (mk-uf-sets (map (λ (x) (list (nt-name x))) nts)))
                                      pat #t)))
                  exp
                  #t)])
       (if mtch
           (binding-names
            (bindings-table-unchecked
             (mtch-bindings
              (car mtch))))
           'failed-to-match))
     (binding-names (extract-empty-bindings pat))))
  
  ;; run-test/cmp : sexp any any (any any -> boolean)
  ;; compares ans with expected. If failure,
  ;; prints info about the test and increments failures
  (define failures 0)
  (define test-count 0)
  (define (run-test/cmp line symbolic ans expected cmp?)
    (set! test-count (+ test-count 1))
    (cond
      [(cmp? ans expected)
       '(printf "passed: line ~a\n" line)]
      [else 
       (set! failures (+ failures 1))
       (eprintf "    test on line ~a\n   input: ~s\nexpected: ~s\n     got: ~s\n"
                line symbolic expected ans)]))
  
  (define (run-test line symbolic ans expected) (run-test/cmp line symbolic ans expected equal/bindings?))
  
  ;; run-match-test : sexp got expected
  ;;   expects both ans and expected to be lists or both to be #f and
  ;;   compares them using a set-like equality if they are lists
  (define (run-match-test line symbolic ans expected)
    (with-handlers ((exn:fail? (λ (x)
                                 (eprintf "exception raised while running test on line ~a\n" line)
                                 (raise x))))
      (run-test/cmp
       line
       symbolic ans expected
       (λ (xs ys)
         (cond
           [(and (not xs) (not ys)) #t]
           [(and (list? xs)
                 (list? ys))
            (and (andmap (λ (x) (memf (λ (y) (equal/bindings? x y)) ys)) xs)
                 (andmap (λ (y) (memf (λ (x) (equal/bindings? x y)) xs)) ys)
                 (= (length xs) (length ys)))]
           [else #f])))))
  
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
  
  (test)
