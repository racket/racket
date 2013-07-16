#!r6rs

(library (tests r6rs syntax-case)
  (export run-syntax-case-tests)
  (import (for (rnrs) run expand)
          (rename (only (rnrs base) cons) (cons kons)) ; for free-identifier=?
          (tests r6rs test))

  (define (unwrap s)
    (cond
     [(pair? s) (cons (unwrap (car s)) (unwrap (cdr s)))]
     [(vector? s) (list->vector (map unwrap (vector->list s)))]
     [(null? s) s]
     [(number? s) s]
     [(string? s) s]
     [(boolean? s) s]
     [else (syntax->datum s)]))

  ;; ----------------------------------------
  
  (define p (cons 4 5))
  (define-syntax p.car
    (lambda (x)
      (syntax-case x ()
        [(_ . rest) #'((car p) . rest)]
        [_  #'(car p)])))

  ;; Different frmo the report to avoid set-car!
  (define p2 (cons 4 5))
  (define-syntax p2.car
    (make-variable-transformer
     (lambda (x)
       (syntax-case x (set!)
         [(set! _ e) #'(set! p2 (cons e (cdr p2)))]
         [(_ . rest) #'((car p2) . rest)]
         [_  #'(car p2)]))))
  
  (define-syntax rec
    (lambda (x)
      (syntax-case x ()
        [(_ x e)
         (identifier? #'x)
         #'(letrec ([x e]) x)])))

  (define-syntax loop
    (lambda (x)
      (syntax-case x ()
        [(k e ...)
         (with-syntax
             ([break (datum->syntax #'k 'break)])
           #'(call-with-current-continuation
              (lambda (break)
                (let f () e ... (f)))))])))

  ;; ----------------------------------------
  
  (define (run-syntax-case-tests)

    (test p.car 4)
    ;; (test/exn (set! p.car 15) &syntax) ; not a runtime exception

    (set! p2.car 15)
    (test p2.car 15)
    (test p2 '(15 . 5))
    
    (test (map (rec fact
                    (lambda (n)
                      (if (= n 0)                 
                          1
                          (* n (fact (- n 1))))))
               '(1 2 3 4 5))
          '(1 2 6 24 120))
    
    ; (test/exn (rec 5 (lambda (x) x)) &syntax) ; not a runtime exception
    
    (test
     (let ([fred 17])
       (define-syntax a
         (lambda (x)
           (syntax-case x ()
             [(_ id) #'(b id fred)])))
       (define-syntax b
         (lambda (x)
           (syntax-case x ()
             [(_ id1 id2)
              #`(list
                   #,(free-identifier=? #'id1 #'id2)
                   #,(bound-identifier=? #'id1 #'id2))])))
       (a fred)) 
     '(#t #f))

    ; (test/exn (let ([a 3] [a 4]) (+ a a)) &syntax)

    (test (let-syntax
              ([dolet (lambda (x)
                        (syntax-case x ()
                          [(_ b)
                           #'(let ([a 3] [b 4]) (+ a b))]))])
            (dolet a)) 
          7)

    ;; check that it's ok as an expression:
    (test 6
          (let-syntax ([foo
                        (syntax-rules ()
                          [(_)
                           (let-syntax ([bar
                                         (syntax-rules ()
                                           [(_) 5])])
                             (bar))])])
            (+ 1 (foo))))

    #;
    (test/exn (let ([else #f])
                (case 0 [else (write "oops")])) 
              &syntax)

    (test (let ((n 3) (ls '()))
            (loop
             (if (= n 0) (break ls))
             (set! ls (cons 'a ls))
             (set! n (- n 1)))) 
          '(a a a))

    ;; ----------------------------------------

    (test (syntax-case #'1 () [1 'one])  'one)
    (test (syntax-case #'(1) () [(1) 'one])  'one)
    (test (syntax-case '(1) () [(x) #'x]) 1)
    (test (syntax-case #'(1) () [(x) (syntax->datum #'x)]) 1)
    (test (syntax-case '("a") () [(x) #'x]) "a")
    (test (syntax-case #'("a") () [(x) (syntax->datum #'x)]) "a")
    (test (syntax-case '(1 #f "s" #vu8(9) #(5 7)) () 
            [(x ...) #'(x ...)]) 
          '(1 #f "s" #vu8(9) #(5 7)))
    (test (syntax-case #'(1 #f "s" #vu8(9) #(5 7)) () 
            [(x ...) (map syntax->datum #'(x ...))]) 
          '(1 #f "s" #vu8(9) #(5 7)))
    (test (syntax-case '(1 2 3 4) () [(x y . z) #'z]) '(3 4))
    (test (syntax-case #'(a b c d) () [(x y . z) (syntax->datum #'z)]) 
          '(c d))
    (test (syntax-case #'(nonesuch 12) (nonesuch) 
            [(nonesuch x) (syntax->datum #'x)])
          12)
    (test (syntax-case #'(different 12) (nonesuch) 
            [(nonesuch x) #'x]
            [_ 'other])
          'other)
    (test (syntax-case '(1 2 3 4) ()
            [(1 x ...) #'(x ...)])
          '(2 3 4))
    (test (syntax-case '(1 2 3 4) ()
            [(1 x ... 3 4) #'(x ...)])
          '(2))
    (test (syntax-case '(1 2 3 4) ()
            [(1 x ... 2 3 4) #'(x ...)])
          '())
    (test (syntax-case '(1 2 3 4) ()
            [(1 x ... . y) #'y])
          '())
    (test (syntax-case '(1 2 3 4 . 5) ()
            [(1 x ... . y) #'y])
          '5)
    (test (syntax-case '(1 2 3 4 . 5) ()
            [(1 x ... 4 . y) #'y])
          '5)
    (test (syntax-case '(1 2 3 4 . 5) ()
            [(1 x ... 5 . y) #'y]
            [_ 'no])
          'no)
    (test (syntax-case '#(1 2 3 4) ()
            [#(1 x y 4) (car #'(x y))])
          '2)
    (test (syntax-case '#(1 2 3 4) ()
            [#(1 x y 4) (cadr #'(x y))])
          '3)
    (test (syntax-case '#(1 2 3 4) ()
            [#(1 x y 4) (syntax->datum (cddr #'(x y)))])
          '())
    (test (syntax-case '#(1 2 3 4) ()
            [#(1 2 3 4) 'match])
          'match)
    (test (syntax-case '#(1 2 3 4) ()
            [#(1 x y 4) #'y])
          '3)
    (test (syntax-case '#(1 2 3 4) ()
            [#(1 x ...) #'(x ...)])
          '(2 3 4))
    (test (syntax-case '#(1 2 3 4) ()
            [#(1 x ... 4) #'(x ...)])
          '(2 3))
    (test (syntax-case '#(1 2 3 4) ()
            [#(1 x ... 2 3 4) #'(x ...)])
          '())
    (test (syntax-case #'() ()
            [(x ...)
             (let ([v #'#(x ...)])
               (list (syntax->datum v) (vector? v)))])
          '(#() #t))
    (test (syntax-case #'(1) ()
            [(_) (syntax->datum #'_)])
          '_)
    (test (syntax-case '((a) (b c)) ()
            [((x ...) ...)
             #'(x ... ...)])
          '(a b c))
    (test (syntax-case #'((a) (b c)) ()
            [((x ...) ...)
             (map syntax->datum #'(x ... ...))])
          '(a b c))

    (test (syntax-case #'(... x) ()
            [a (syntax->datum #'a)])
          'x)
    (test (syntax-case #'(... ...) ()
            [a (syntax->datum #'a)])
          '...)
    (test (syntax-case #'(... (other ...)) ()
            [a (syntax->datum #'a)])
          '(other ...))
    (test (syntax-case #'(1 2 3) ()
            [(a ...) (syntax->datum #'((a (... ...)) ...))])
          '((1 ...) (2 ...) (3 ...)))
    (test (syntax-case #'(1 2 3) ()
            [(a b c) (syntax->datum #'(... (a ...)))])
          '(1 ...))
    (test (syntax-case #'(1 2 3) ()
            [(a b c) (syntax->datum #'(... (... (a) b)))])
          '(... (1) 2))

    (test (identifier? 'x) #f)
    (test (identifier? #'x) #t)
    (test (bound-identifier=? #'x #'x) #t)
    (test (bound-identifier=? #'x #'y) #f)
    (test (bound-identifier=? #'cons #'kons) #f)
    (test (free-identifier=? #'x #'x) #t)
    (test (free-identifier=? #'x #'y) #f)
    ;; (test (free-identifier=? #'cons #'kons) #t) ;; see PLT bug report #10210

    (test (syntax->datum #'1) 1)
    (test (syntax->datum #'a) 'a)
    (test (syntax->datum #'(a b)) '(a b))
    (test (syntax->datum #'(a . b)) '(a . b))

    (test (syntax->datum '1) 1)
    (test (syntax->datum '(1 . 2)) '(1 . 2))
    (test (syntax->datum '(1 2)) '(1 2))
    (test (syntax->datum (cons #'a #'b)) '(a . b))
    (test (syntax->datum (vector #'a #'b)) '#(a b))
    (test (syntax->datum '#(1 2)) '#(1 2))

    (test (syntax->datum (datum->syntax #'x 1)) 1)
    (test (syntax->datum (datum->syntax #'x 'a)) 'a)
    (test (syntax->datum (datum->syntax #'x '(a b))) '(a b))
    (test (syntax->datum (datum->syntax #'x '(a . b))) '(a . b))

    (test (number? (car (syntax->datum (datum->syntax #'x (list 1))))) #t)

    (test (map identifier? (generate-temporaries '(1 2 3))) '(#t #t #t))
    (test (map identifier? (generate-temporaries #'(1 2 3))) '(#t #t #t))
    (test (map identifier? (generate-temporaries (cons 1 #'(2 3)))) '(#t #t #t))

    (test (cadr (with-syntax ([x 1]
                              [y 2])
                  #'(x y)))
          2)

    (test (syntax->datum #`(1 2 3)) '(1 2 3))
    (test (syntax->datum #`1) 1)
    
    ;; Check wrapping:
    (test (let ([v #`(1 #,(+ 1 1) 3)])
            (list (pair? v)
                  (syntax->datum (car v))
                  (cadr v)
                  (syntax->datum (cddr v))))
          '(#t 1 2 (3)))
    (test (let ([v #`(1 #,@(list (+ 1 1)) 3)])
            (list (pair? v)
                  (syntax->datum (car v))
                  (cadr v)
                  (syntax->datum (cddr v))))
          '(#t 1 2 (3)))
    (test (let ([v #`(1 #,@(list (+ 1 1) (- 8 1)) 3)])
            (list (pair? v)
                  (syntax->datum (car v))
                  (cadr v)
                  (caddr v)
                  (syntax->datum (cdddr v))))
          '(#t 1 2 7 (3)))
    (test (syntax-case '(1 2 3) ()
            [(x ...) #`(x ...)])
          '(1 2 3))

    (test (unwrap
           #`(1 2 (unsyntax 3 4 5) 6))
          '(1 2 3 4 5 6))
    (test (unwrap
           #`(1 2 (unsyntax-splicing '(3 4) '(5)) 6))
          '(1 2 3 4 5 6))

    (test (unwrap
           #`#(1 2 (unsyntax-splicing '(3 4) '(5)) 6))
          '#(1 2 3 4 5 6))
    (test (unwrap
           #`(1 #`(#,(+ 3 4) #,#,(+ 1 1))))
          '(1 #`(#,(+ 3 4) #,2)))

    (test (unwrap
           (syntax-case #'(weird-letrec ([x 1][y 7]) x) ()
             [(_ ([v e] ...) . b)
              #'(let ()
                  (define v) ...
                  . b)]))
          '(let () (define x) (define y) x))

    (test/exn (syntax-violation #f "bad" 7) &syntax)
    (test/exn (syntax-violation 'form "bad" 7) &syntax)
    (test/exn (syntax-violation #f "bad" #'7) &syntax)
    (test/exn (syntax-violation #f "bad" #'7 8) &syntax)
    (test/exn (syntax-violation #f "bad" #'7 #'8) &syntax)
    (test/exn (syntax-violation #f "bad" 7 #'8) &syntax)
    (test/exn (syntax-violation 'form "bad" #'7 #'8) &syntax)
    (test/exn (syntax-violation 'form "bad" 7 #'8) &syntax)
    (test/exn (syntax-violation 'form "bad" #'7 8) &syntax)
    (test/exn (syntax-violation 'form "bad" 7 8) &syntax)
    (test/exn (syntax-violation "form" "bad" 7) &syntax)
    (test/exn (syntax-violation "form" "bad" 7 8) &syntax)

    (test (condition-message
           (guard (v [#t v])
                  (syntax-violation 'apple "bad" 'worm)))
          "bad")
    (test (condition-who
           (guard (v [#t v])
                  (syntax-violation 'apple "bad" 'worm)))
          'apple)
    (test (condition-who
           (guard (v [#t v])
                  (syntax-violation "apple" "bad" 'worm)))
          "apple")
    (test (who-condition?
           (guard (v [#t v])
                  (syntax-violation #f "bad" 'worm)))
          #f)
    (test (condition-who
           (guard (v [#t v])
                  (syntax-violation #f "bad" #'worm)))
          'worm)
    (test (syntax-violation-form
           (guard (v [#t v])
                  (syntax-violation 'apple "bad" '(worm))))
          '(worm))
    (test (syntax-violation-subform
           (guard (v [#t v])
                  (syntax-violation 'apple "bad" '(worm))))
          #f)
    (test (syntax-violation-subform
           (guard (v [#t v])
                  (syntax-violation 'apple "bad" '(worm) '((another)))))
          '((another)))


    ;;
    ))

