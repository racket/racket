(module persistent-interaction-tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
           "language-tester.ss")
  (provide persistent-interaction-suite)
  
  (define (catch-unsafe-context-exn thunk)
    (with-handlers ([void
                     (lambda (the-exn)
                       (or
                        (and
                         (regexp-match ".*Attempt to capture a continuation from within an unsafe context$"
                                       (exn-message the-exn))
                         #t)
                        (raise the-exn)))])
      (and (thunk) #f)))
  
  (define persistent-interaction-suite
    (make-test-suite
     "Test the persistent interaction language"
     
     ;; ****************************************
     ;; ****************************************
     ;; BASIC TESTS  
     (make-test-suite
      "Basic Tests"
      
      (make-test-case
       "Function application with single argument in tail position"
       (let ([test-m00.4
              (make-module-eval
               (module m00.4 "../persistent-interaction.ss"
                 (define (id x) x)
                 
                 (let ([f (let ([m 7]) m)])
                   (+ f (start-interaction id)))))])
         
         (assert = 8 (test-m00.4 '(dispatch-start 1)))))
      
      (make-test-case
       "start-interaction in argument position of a function call"
       (let ([test-m00.3
              (make-module-eval
               (module m00.3 "../persistent-interaction.ss"
                 (define (id x) x)
                 (define (foo x) 'foo)
                 (foo (start-interaction id))))])
         
         (assert eqv? 'foo (test-m00.3 '(dispatch-start 7)))))
      
      (make-test-case
       "identity interaction, dispatch-start called multiple times"
       (let ([test-m00
              (make-module-eval
               (module m00 "../persistent-interaction.ss"
                 (define (id x) x)
                 (id (start-interaction id))))])
         
         
         (assert = 7 (test-m00 '(dispatch-start 7)))
         (assert eqv? 'foo (test-m00 '(dispatch-start 'foo)))))
      
      (make-test-case
       "start-interaction in argument position of a primitive"
       (let ([test-m00.1
              (make-module-eval
               (module m00.1 "../persistent-interaction.ss"
                 (define (id x) x)
                 (+ 1 (start-interaction id))))])
         
         (assert = 2 (test-m00.1 '(dispatch-start 1)))))
      
      (make-test-case
       "dispatch-start called multiple times for s-i in non-trivial context"
       (let ([test-m00.2
              (make-module-eval
               (module m00.2 "../persistent-interaction.ss"
                 (define (id x) x)
                 (+ (+ 1 1) (start-interaction id))))])
         
         (assert = 14 (test-m00.2 '(dispatch-start 12)))
         (assert = 20 (test-m00.2 '(dispatch-start 18)))))
      
      
      (make-test-case
       "start-interaction in third position"
       (let ([test-m01
              (make-module-eval
               (module m01 "../persistent-interaction.ss"
                 (define (id x) x)
                 (+ (* 1 2) (* 3 4) (start-interaction id))))])
         
         (assert = 14 (test-m01 '(dispatch-start 0)))
         (assert = 20 (test-m01 '(dispatch-start 6)))))
      
      (make-test-case
       "quasi-quote with splicing: need to recertify context for qq-append"
       (let ([test-m01.1
              (make-module-eval
               (module m01.1 "../persistent-interaction.ss"
                 (define (id x) x)
                 `(,@(list 1 2 (start-interaction id)))))])
         
         (assert equal? (list 1 2 3) (test-m01.1 '(dispatch-start 3)))
         (assert equal? (list 1 2 'foo) (test-m01.1 '(dispatch-start 'foo)))))
      
      (make-test-case
       "recertify context test (1)"
       (let ([test-m01.2
              (make-module-eval
               (module m01.1 "../persistent-interaction.ss"
                 `(foo ,@(list 1 2 3))))])
         (assert-true #t)))
      
      (make-test-case
       "recertify context test (2)"
       (let ([test-m01.3
              (make-module-eval  
               (module m01.3 "../persistent-interaction.ss"
                 (lambda (n)
                   `(n ,@(list 1 2 3)))))])
         (assert-true #t)))
      
      (make-test-case
       "recertify context test (3)"
       (let ([test-m01.4
              (make-module-eval
               (module m1 "../persistent-interaction.ss"
                 (define (bar n)
                   `(n ,@(list 1 2 3)))
                 (bar 7)))])
         (assert-true #t)))
      
      ;; start-interaction may be called mutitple times
      ;; each call overwrites the previous interaction
      ;; continuation with the latest one.
      (make-test-case
       "start-interaction called twice, dispatch-start will invoke different continuations"
       (let ([test-m02
              (make-module-eval
               (module m02 "../persistent-interaction.ss"
                 (define (id x) x)
                 (+ (start-interaction id)
                    (start-interaction id))))])
         
         (assert-true (void? (test-m02 '(dispatch-start 1))))
         (assert = 3 (test-m02 '(dispatch-start 2)))
         (assert = 0 (test-m02 '(dispatch-start -1))))))
     
     
     
     ;; ****************************************
     ;; ****************************************
     ;; TESTS INVOLVING CALL/CC
     (make-test-suite
      "Tests Involving call/cc"
      
      (make-test-case
       "continuation invoked in non-trivial context from within proc"
       (let ([test-m03
              (make-module-eval
               (module m03 "../persistent-interaction.ss"
                 (define (f x)
                   (let/cc k
                     (+ 2 4 (k 3) 6 8)))
                 (f (start-interaction (lambda (x) x)))))])
         
         (assert = 3 (test-m03 '(dispatch-start 'foo)))
         (assert = 3 (test-m03 '(dispatch-start 7)))))
      
      ;; in the following test, if you modify
      ;; resume to print the "stack" you will
      ;; see that this is not tail recursive
      (make-test-case
       "non-tail-recursive 'escaping' continuation"
       (let ([test-m04
              (make-module-eval
               (module m04 "../persistent-interaction.ss"
                 (define (mult ln)
                   (let/cc k
                     (cond
                       [(null? ln) 1]
                       [(zero? (car ln)) (k 0)]
                       [else
                        (* (car ln)
                           (mult (cdr ln)))])))
                 
                 (mult (start-interaction (lambda (x) x)))))])
         
         (assert = 0 (test-m04 '(dispatch-start (list 1 2 3 4 5 6 7 0 8 9))))
         (assert = 120 (test-m04 '(dispatch-start (list 1 2 3 4 5))))))
      
      ;; this version captures the continuation
      ;; outside the recursion and should be tail
      ;; recursive. A "stack trace" reveals this
      ;; as expected.
      (make-test-case
       "tail-recursive escaping continuation"
       (let ([test-m05
              (make-module-eval
               (module m05 "../persistent-interaction.ss"
                 (provide mult)
                 
                 (define (mult ln)
                   (let/cc escape
                     (mult/escape escape ln)))
                 
                 (define (mult/escape escape ln)
                   (cond
                     [(null? ln) 1]
                     [(zero? (car ln)) (escape 0)]
                     [else
                      (* (car ln)
                         (mult/escape escape (cdr ln)))]))
                 
                 (mult (start-interaction (lambda (x) x)))))])
         
         (assert = 0 (test-m05 '(dispatch-start (list 1 2 3 0 4 5 6))))
         (assert = 120 (test-m05 '(dispatch-start (list 1 2 3 4 5)))))))
     
     ;; ****************************************
     ;; ****************************************
     ;; TESTS INVOLVING send/suspend
     (make-test-suite
      "Tests Involving send/suspend"
      
      (make-test-case
       "curried add with send/suspend"
       (let ([table-01-eval
              (make-module-eval
               (module table01 mzscheme
                 (provide store-k
                          lookup-k)
                 
                 (define the-table (make-hash-table))
                 
                 (define (store-k k)
                   (let ([key (string->symbol (symbol->string (gensym 'key)))])
                     (hash-table-put! the-table key k)
                     key))
                 (define (lookup-k key-pair)
                   (hash-table-get the-table (car key-pair) (lambda () #f)))))])
         
         (table-01-eval
          '(module m06 "../persistent-interaction.ss"
             (require table01)
             
             (define (gn which)
               (cadr
                (send/suspend
                 (lambda (k)
                   (let ([ignore (printf "Please send the ~a number.~n" which)])
                     (store-k k))))))
             
             (let ([ignore (start-interaction lookup-k)])
               (let ([result (+ (gn "first") (gn "second"))])
                 (let ([ignore (printf "The answer is: ~s~n" result)])
                   result)))))
         
         (table-01-eval '(require m06))
         
         (let* ([first-key (table-01-eval '(dispatch-start 'foo))]
                [second-key (table-01-eval `(dispatch '(,first-key 1)))]
                [third-key (table-01-eval `(dispatch '(,first-key -7)))])
           
           
           (assert = 3 (table-01-eval `(dispatch '(,second-key 2))))
           (assert = 4 (table-01-eval `(dispatch '(,second-key 3))))
           (assert-true (zero? (table-01-eval `(dispatch '(,second-key -1)))))
           (assert = -7 (table-01-eval `(dispatch '(,third-key 0))))
           (assert-true (zero? (table-01-eval `(dispatch '(,third-key 7))))))))
      
      (make-test-case
       "curried with send/suspend and serializaztion"
       
       (let ([test-m06.1
              (make-module-eval
               (module m06.1 (lib "persistent-interaction.ss" "web-server" "prototype-web-server")
                 (define (id x) x)
                 
                 (define (gn which)
                   (cadr
                    (send/suspend
                     (lambda (k)
                       (let ([ignore (printf "Please send the ~a number.~n" which)])
                         k)))))
                 
                 (let ([ignore (start-interaction car)])
                   (let ([result (+ (gn "first") (gn "second"))])
                     (let ([ignore (printf "The answer is: ~s~n" result)])
                       result)))))])         
         
         (let* ([first-key (test-m06.1 '(dispatch-start 'foo))]
                [second-key (test-m06.1 `(dispatch (list (deserialize (serialize ,first-key)) 1)))]
                [third-key (test-m06.1 `(dispatch (list (deserialize (serialize ,first-key)) -7)))])
           (values
            (assert = 3 (test-m06.1 `(dispatch (list ,second-key 2))))
            (assert = 4 (test-m06.1 `(dispatch (list ,second-key 3))))
            (assert-true (zero? (test-m06.1 `(dispatch (list ,second-key -1)))))
            (assert = -7 (test-m06.1 `(dispatch (list ,third-key 0))))
            (assert-true (zero? (test-m06.1 `(dispatch (list ,third-key 7)))))))))
      
      
      )
     
     ;; ****************************************
     ;; ****************************************
     ;; TESTS INVOLVING LETREC
     (make-test-suite
      "Tests Involving letrec"
      
      (make-test-case
       "mutually recursive even? and odd?"
       (let ([test-m07
              (make-module-eval
               (module m07 "../persistent-interaction.ss"
                 (define (id x) x)
                 
                 (letrec ([even? (lambda (n)
                                   (or (zero? n)
                                       (odd? (sub1 n))))]
                          [odd? (lambda (n)
                                  (and (not (zero? n))
                                       (even? (sub1 n))))])
                   (even? (start-interaction id)))))])
         
         (assert-true (test-m07 '(dispatch-start 0)))
         (assert-true (test-m07 '(dispatch-start 16)))
         (assert-false (test-m07 '(dispatch-start 1)))
         (assert-false (test-m07 '(dispatch-start 7)))))
      
      (make-test-case
       "send/suspend on rhs of letrec binding forms"
       (let ([test-m08
              (make-module-eval
               (module m08 "../persistent-interaction.ss"
                 (define (id x) x)
                 
                 (define (gn which)
                   (cadr
                    (send/suspend
                     (lambda (k)
                       (let ([ignore (printf "Please send the ~a number.~n" which)])
                         k)))))
                 
                 (let ([ignore (start-interaction car)])
                   (letrec ([f (let ([n (gn "first")])
                                 (lambda (m) (+ n m)))]
                            [g (let ([n (gn "second")])
                                 (lambda (m) (+ n (f m))))])
                     (let ([result (g (gn "third"))])
                       (let ([ignore (printf "The answer is: ~s~n" result)])
                         result))))))])
         (let* ([k0 (test-m08 '(serialize (dispatch-start 'foo)))]
                [k1 (test-m08 `(serialize (dispatch (list (deserialize ',k0) 1))))]
                [k2 (test-m08 `(serialize (dispatch (list (deserialize ',k1) 2))))])
           (assert = 6 (test-m08 `(dispatch (list (deserialize ',k2) 3))))
           (assert = 9 (test-m08 `(dispatch (list (deserialize ',k2) 6))))
           (let* ([k1.1 (test-m08 `(serialize (dispatch (list (deserialize ',k0) -1))))]
                  [k2.1 (test-m08 `(serialize (dispatch (list (deserialize ',k1.1) -2))))])
             (assert-true (zero? (test-m08 `(dispatch (list (deserialize ',k2.1) 3)))))
             (assert = 6 (test-m08 `(dispatch (list (deserialize ',k2) 3))))))))
      )
     
     ;; ****************************************
     ;; ****************************************
     ;; TEST UNSAFE CONTEXT CONDITION
     (make-test-suite
      "Unsafe Context Condition Tests"
      
      (make-test-case
       "simple attempt to capture a continuation from an unsafe context"
       
       (let ([nta-eval
              (make-module-eval
               (module nta mzscheme
                 (provide non-tail-apply)
                 
                 (define (non-tail-apply f . args)
                   (let ([result (apply f args)])
                     (printf "result = ~s~n" result)
                     result))))])
         (nta-eval '(module m09 "../persistent-interaction.ss"
                      (require nta)
                      (define (id x) x)
                      
                      (let ([ignore (start-interaction id)])
                        (non-tail-apply (lambda (x) (let/cc k (k x))) 7))))
         
         (nta-eval '(require m09))
         
         (assert-true (catch-unsafe-context-exn
                       (lambda () (nta-eval '(dispatch-start 'foo)))))))
            
      (make-test-case
       "sanity-check: capture continuation from safe version of context"
       
       (let ([m10-eval
              (make-module-eval
               (module m10 "../persistent-interaction.ss"
                 (define (id x) x)
  
                 (define (nta f arg)
                   (let ([result (f arg)])
                     (printf "result = ~s~n" result)
                     result))
  
                 (let ([ignore (start-interaction id)])
                   (nta (lambda (x) (let/cc k (k x))) 7))))])
         
         (assert = 7 (m10-eval '(dispatch-start 'foo)))))
      
      (make-test-case
       "attempt continuation capture from standard call to map"
       
       (let ([m11-eval
              (make-module-eval
               (module m11 "../persistent-interaction.ss"
                 (define (id x) x)
                 
                 (let ([ignore (start-interaction id)])
                   (map
                    (lambda (x) (let/cc k k))
                    (list 1 2 3)))))])

         (assert-true (catch-unsafe-context-exn
                       (lambda () (m11-eval '(dispatch-start 'foo)))))))
      
      ;; if the continuation-capture is attempted in tail position then we
      ;; should be just fine.
      (make-test-case
       "continuation capture from tail position of untranslated procedure"
       
       (let ([ta-eval
              (make-module-eval
               (module ta mzscheme
                 (provide tail-apply)
                 
                 (define (tail-apply f . args)
                   (apply f args))))])
         
         (ta-eval '(module m12 "../persistent-interaction.ss"
                     (require ta)
                     (define (id x) x)

                     (+ (start-interaction id) 
                        (tail-apply (lambda (x) (let/cc k (k x))) 1))))
         
         (ta-eval '(require m12))
         
         (assert = 2 (ta-eval '(dispatch-start 1)))))
      
      (make-test-case
       "attempt send/suspend from standard call to map"
       
       (let ([m13-eval
              (make-module-eval
               (module m11 "../persistent-interaction.ss"
                 (define (id x) x)
                 
                 (let ([ignore (start-interaction car)])
                   (map
                    (lambda (n) (send/suspend
                                 (lambda (k)
                                   (let ([ignore (printf "n = ~s~n" n)])
                                     k))))
                    (list 1 2 3)))))])

         (assert-true (catch-unsafe-context-exn
                       (lambda () (m13-eval '(dispatch-start 'foo)))))))
      
      (make-test-case
       "attempt send/suspend from tail position of untranslated procedure"
       
       (let ([ta-eval
              (make-module-eval
               (module ta mzscheme
                 (provide tail-apply)
               
                 (define (tail-apply f . args)
                   (apply f args))))])
         
         (ta-eval '(module m14 "../persistent-interaction.ss"
                     (require ta)
                     
                     (let ([ignore (start-interaction car)])
                       (+ 1 (tail-apply
                             (lambda (n)
                               (cadr
                                (send/suspend
                                 (lambda (k)
                                   (let ([ignore (printf "n = ~s~n" n)])
                                     k))))) 7)))))
         (ta-eval '(require m14))
         
         (let ([k0 (ta-eval '(dispatch-start 'foo))])
           (assert = 3 (ta-eval `(dispatch (list ,k0 2))))
           (assert = 0 (ta-eval `(dispatch (list ,k0 -1)))))))
               
       
      
      ))))