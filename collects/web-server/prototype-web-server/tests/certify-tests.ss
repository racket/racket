(module certify-tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           "util.ss")
  (provide certify-suite)
    
  (define certify-suite
    (test-suite
     "Test the certification process"
     
     (test-suite
      "Splicing tests"
            
      (test-case
       "quasi-quote with splicing: need to recertify context for qq-append"
       (let-values ([(test-m01.1)
                     (make-module-eval
                      (module m01.1 (lib "lang.ss" "web-server" "prototype-web-server")
                        (provide start)
                        (define (start initial)
                          `(,@(list 1 2 initial)))))])         
         (check equal? (list 1 2 3) (test-m01.1 '(dispatch-start start 3)))
         (check equal? (list 1 2 'foo) (test-m01.1 '(dispatch-start start 'foo)))))
      
      (test-case
       "recertify context test (1)"
       (let-values ([(test-m01.2)
                     (make-module-eval
                      (module m01.1 (lib "lang.ss" "web-server" "prototype-web-server")
                        (provide start)
                        (define (start initial)
                          `(foo ,@(list 1 2 3)))))])
         (check-true #t)))
      
      (test-case
       "recertify context test (2)"
       (let-values ([(test-m01.3)
                     (make-module-eval  
                      (module m01.3 (lib "lang.ss" "web-server" "prototype-web-server")
                        (provide start)
                        (define (start n)
                          `(n ,@(list 1 2 3)))))])
         (check-true #t)))
      
      (test-case
       "recertify context test (3)"
       (let-values ([(test-m01.4)
                     (make-module-eval
                      (module m1 (lib "lang.ss" "web-server" "prototype-web-server")
                        (provide start)
                        (define (start initial)
                          (define (bar n)
                            `(n ,@(list 1 2 3)))
                          (bar 7))))])
         (check-true #t)))))))