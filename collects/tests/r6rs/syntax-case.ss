#!r6rs

(library (tests r6rs syntax-case)
  (export run-syntax-case-tests)
  (import (for (rnrs) run expand)
          (tests r6rs test))

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
    
    ;;
    ))

