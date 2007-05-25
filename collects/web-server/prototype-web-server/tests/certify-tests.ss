(module certify-tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
           "language-tester.ss")
  (provide certify-suite)
    
  (define certify-suite
    (make-test-suite
     "Test the certification process"
     
     (make-test-suite
      "Splicing tests"
            
      (make-test-case
       "quasi-quote with splicing: need to recertify context for qq-append"
       (let-values ([(go test-m01.1)
                     (make-module-eval
                      (module m01.1 "../lang.ss"
                        (provide start)
                        (define (start initial)
                          `(,@(list 1 2 initial)))))])         
         (go)
         (assert equal? (list 1 2 3) (test-m01.1 '(dispatch-start 3)))
         (assert equal? (list 1 2 'foo) (test-m01.1 '(dispatch-start 'foo)))))
      
      (make-test-case
       "recertify context test (1)"
       (let-values ([(go test-m01.2)
                     (make-module-eval
                      (module m01.1 "../lang.ss"
                        (provide start)
                        (define (start initial)
                          `(foo ,@(list 1 2 3)))))])
         (go)
         (assert-true #t)))
      
      (make-test-case
       "recertify context test (2)"
       (let-values ([(go test-m01.3)
                     (make-module-eval  
                      (module m01.3 "../lang.ss"
                        (provide start)
                        (define (start n)
                          `(n ,@(list 1 2 3)))))])
         (go)
         (assert-true #t)))
      
      (make-test-case
       "recertify context test (3)"
       (let-values ([(go test-m01.4)
                     (make-module-eval
                      (module m1 "../lang.ss"
                        (provide start)
                        (define (start initial)
                          (define (bar n)
                            `(n ,@(list 1 2 3)))
                          (bar 7))))])
         (go)
         (assert-true #t)))))))