(module persistent-interaction-tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
           "language-tester.ss")
  (provide persistent-interaction-suite)
  
  (define (catch-unsafe-context-exn thunk)
    (with-handlers ([void
                     (lambda (the-exn)
                       (or
                        (and
                         (regexp-match ".*Attempt to capture a continuation from within an unsafe context"
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
       (let-values ([(go test-m00.4)
              (make-module-eval
               (module m00.4 "../lang.ss"
                 (provide start)
                 (define (start initial)
                   (let ([f (let ([m 7]) m)])
                     (+ f initial)))))])
         (go)
         (assert = 8 (test-m00.4 '(dispatch-start 1)))))
      
      (make-test-case
       "start-interaction in argument position of a function call"
       (let-values ([(go test-m00.3)
              (make-module-eval
               (module m00.3 "../lang.ss"
                 (define (foo x) 'foo)
                 (provide start)
                 (define (start initial)
                   (foo initial))))])
         (go)
         (assert eqv? 'foo (test-m00.3 '(dispatch-start 7)))))
      
      (make-test-case
       "identity interaction, dispatch-start called multiple times"
       (let-values ([(go test-m00)
              (make-module-eval
               (module m00 "../lang.ss"
                 (define (id x) x)
                 (provide start)
                 (define (start initial)
                   (id initial))))])
         (go)
         (assert = 7 (test-m00 '(dispatch-start 7)))
         (assert eqv? 'foo (test-m00 '(dispatch-start 'foo)))))
      
      (make-test-case
       "start-interaction in argument position of a primitive"
       (let-values ([(go test-m00.1)
              (make-module-eval
               (module m00.1 "../lang.ss"
                 (provide start)
                 (define (start initial)
                   (+ 1 initial))))])         
         (go)
         (assert = 2 (test-m00.1 '(dispatch-start 1)))))
      
      (make-test-case
       "dispatch-start called multiple times for s-i in non-trivial context"
       (let-values ([(go test-m00.2)
              (make-module-eval
               (module m00.2 "../lang.ss"
                 (provide start)
                 (define (start initial)
                   (+ (+ 1 1) initial))))])         
         (go)
         (assert = 14 (test-m00.2 '(dispatch-start 12)))
         (assert = 20 (test-m00.2 '(dispatch-start 18)))))
      
      (make-test-case
       "start-interaction in third position"
       (let-values ([(go test-m01)
              (make-module-eval
               (module m01 "../lang.ss"
                 (provide start)
                 (define (start initial)
                   (+ (* 1 2) (* 3 4) initial))))])         
         (go)
         (assert = 14 (test-m01 '(dispatch-start 0)))
         (assert = 20 (test-m01 '(dispatch-start 6)))))
      
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
                 `(foo ,@(list 1 2 3))))])
         (go)
         (assert-true #t)))
      
      (make-test-case
       "recertify context test (2)"
       (let-values ([(go test-m01.3)
              (make-module-eval  
               (module m01.3 "../lang.ss"
                 (lambda (n)
                   `(n ,@(list 1 2 3)))))])
         (go)
         (assert-true #t)))
      
      (make-test-case
       "recertify context test (3)"
       (let-values ([(go test-m01.4)
              (make-module-eval
               (module m1 "../lang.ss"
                 (define (bar n)
                   `(n ,@(list 1 2 3)))
                 (bar 7)))])
         (go)
         (assert-true #t)))
      
      ;; start-interaction may be called mutitple times
      ;; each call overwrites the previous interaction
      ;; continuation with the latest one.
      ; XXX We have taken this power away.
      #;(make-test-case
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
       (let-values ([(go test-m03)
              (make-module-eval
               (module m03 "../lang.ss"
                 (provide start)
                 (define (start x)
                   (let/cc k
                     (+ 2 4 (k 3) 6 8)))))])         
         (go)
         (assert = 3 (test-m03 '(dispatch-start 'foo)))
         (assert = 3 (test-m03 '(dispatch-start 7)))))
      
      ;; in the following test, if you modify
      ;; resume to print the "stack" you will
      ;; see that this is not tail recursive
      (make-test-case
       "non-tail-recursive 'escaping' continuation"
       (let-values ([(go test-m04)
              (make-module-eval
               (module m04 "../lang.ss"
                 (provide start)
                 (define (start ln)
                   (let/cc k
                     (cond
                       [(null? ln) 1]
                       [(zero? (car ln)) (k 0)]
                       [else
                        (* (car ln)
                           (start (cdr ln)))])))))])
         (go)
         (assert = 0 (test-m04 '(dispatch-start (list 1 2 3 4 5 6 7 0 8 9))))
         (assert = 120 (test-m04 '(dispatch-start (list 1 2 3 4 5))))))
      
      ;; this version captures the continuation
      ;; outside the recursion and should be tail
      ;; recursive. A "stack trace" reveals this
      ;; as expected.
      (make-test-case
       "tail-recursive escaping continuation"
       (let-values ([(go test-m05)
              (make-module-eval
               (module m05 "../lang.ss"
                 (provide start)                 
                 
                 (define (start ln)
                   (let/cc escape
                     (mult/escape escape ln)))
                 
                 (define (mult/escape escape ln)
                   (cond
                     [(null? ln) 1]
                     [(zero? (car ln)) (escape 0)]
                     [else
                      (* (car ln)
                         (mult/escape escape (cdr ln)))]))))])     
         (go)
         (assert = 0 (test-m05 '(dispatch-start (list 1 2 3 0 4 5 6))))
         (assert = 120 (test-m05 '(dispatch-start (list 1 2 3 4 5)))))))
     
     ;; ****************************************
     ;; ****************************************
     ;; TESTS INVOLVING send/suspend
     (make-test-suite
      "Tests Involving send/suspend"
      
      ; XXX This doesn't work, because we don't allow a different dispatcher
      #;(make-test-case
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
          '(module m06 "../lang.ss"
             (require table01)
             (provide start)
             
             (define (gn which)
               (cadr
                (send/suspend
                 (lambda (k)
                   (let ([ignore (printf "Please send the ~a number.~n" which)])
                     (store-k k))))))
             
             (define (start ignore)
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
       
       (let-values ([(go test-m06.1)
              (make-module-eval
               (module m06.1 (lib "lang.ss" "web-server" "prototype-web-server")
                 (provide start)
                 (define (gn which)
                   (cadr
                    (send/suspend
                     (lambda (k)
                       (let ([ignore (printf "Please send the ~a number.~n" which)])
                         k)))))
                 
                 (define (start ignore)
                   (let ([result (+ (gn "first") (gn "second"))])
                     (let ([ignore (printf "The answer is: ~s~n" result)])
                       result)))))])
         (go)
         (let* ([first-key (test-m06.1 '(dispatch-start 'foo))]
                [second-key (test-m06.1 `(dispatch (list (deserialize (serialize ,first-key)) 1)))]
                [third-key (test-m06.1 `(dispatch (list (deserialize (serialize ,first-key)) -7)))])
           (values
            (assert = 3 (test-m06.1 `(dispatch (list ,second-key 2))))
            (assert = 4 (test-m06.1 `(dispatch (list ,second-key 3))))
            (assert-true (zero? (test-m06.1 `(dispatch (list ,second-key -1)))))
            (assert = -7 (test-m06.1 `(dispatch (list ,third-key 0))))
            (assert-true (zero? (test-m06.1 `(dispatch (list ,third-key 7))))))))))
     
     ;; ****************************************
     ;; ****************************************
     ;; TESTS INVOLVING LETREC
     (make-test-suite
      "Tests Involving letrec"
      
      (make-test-case
       "mutually recursive even? and odd?"
       (let-values ([(go test-m07)
              (make-module-eval
               (module m07 "../lang.ss"
                 (provide start)
                 (define (start initial)
                   (letrec ([even? (lambda (n)
                                     (or (zero? n)
                                         (odd? (sub1 n))))]
                            [odd? (lambda (n)
                                    (and (not (zero? n))
                                         (even? (sub1 n))))])
                     (even? initial)))))])         
         (go)
         (assert-true (test-m07 '(dispatch-start 0)))
         (assert-true (test-m07 '(dispatch-start 16)))
         (assert-false (test-m07 '(dispatch-start 1)))
         (assert-false (test-m07 '(dispatch-start 7)))))
      
      (make-test-case
       "send/suspend on rhs of letrec binding forms"
       (let-values ([(go test-m08)
              (make-module-eval
               (module m08 "../lang.ss"
                 (provide start)
                 (define (gn which)
                   (cadr
                    (send/suspend
                     (lambda (k)
                       (let ([ignore (printf "Please send the ~a number.~n" which)])
                         k)))))
                 
                 (define (start ignore)
                   (letrec ([f (let ([n (gn "first")])
                                 (lambda (m) (+ n m)))]
                            [g (let ([n (gn "second")])
                                 (lambda (m) (+ n (f m))))])
                     (let ([result (g (gn "third"))])
                       (let ([ignore (printf "The answer is: ~s~n" result)])
                         result))))))])
         (go)
         (let* ([k0 (test-m08 '(serialize (dispatch-start 'foo)))]
                [k1 (test-m08 `(serialize (dispatch (list (deserialize ',k0) 1))))]
                [k2 (test-m08 `(serialize (dispatch (list (deserialize ',k1) 2))))])
           (assert = 6 (test-m08 `(dispatch (list (deserialize ',k2) 3))))
           (assert = 9 (test-m08 `(dispatch (list (deserialize ',k2) 6))))
           (let* ([k1.1 (test-m08 `(serialize (dispatch (list (deserialize ',k0) -1))))]
                  [k2.1 (test-m08 `(serialize (dispatch (list (deserialize ',k1.1) -2))))])
             (assert-true (zero? (test-m08 `(dispatch (list (deserialize ',k2.1) 3)))))
             (assert = 6 (test-m08 `(dispatch (list (deserialize ',k2) 3)))))))))
     
     ;; ****************************************
     ;; ****************************************
     ;; TEST UNSAFE CONTEXT CONDITION
     (make-test-suite
      "Unsafe Context Condition Tests"
      
      ; XXX Bizarre
      #;(make-test-case
       "simple attempt to capture a continuation from an unsafe context"
       
       (let-values ([(go nta-eval)
              (make-module-eval
               (module nta mzscheme
                 (provide non-tail-apply)
                 
                 (define (non-tail-apply f . args)
                   (let ([result (apply f args)])
                     (printf "result = ~s~n" result)
                     result))))])
         (nta-eval '(module m09 "../lang.ss"
                      (require nta)
                      (provide start)
                      (define (start ignore)
                        (non-tail-apply (lambda (x) (let/cc k (k x))) 7))))
         
         (nta-eval '(require m09))
         
         (assert-true (catch-unsafe-context-exn
                       (lambda () (nta-eval '(dispatch-start 'foo)))))))
      
      (make-test-case
       "sanity-check: capture continuation from safe version of context"
       
       (let-values ([(go m10-eval)
              (make-module-eval
               (module m10 "../lang.ss"
                 (provide start)
                 (define (nta f arg)
                   (let ([result (f arg)])
                     (printf "result = ~s~n" result)
                     result))
                 (define (start ignore)
                   (nta (lambda (x) (let/cc k (k x))) 7))))])         
         (go)
         (assert = 7 (m10-eval '(dispatch-start 'foo)))))
      
      (make-test-case
       "attempt continuation capture from standard call to map"
       
       (let-values ([(go m11-eval)
              (make-module-eval
               (module m11 "../lang.ss"
                 (provide start)
                 (define (start ignore)
                   (map
                    (lambda (x) (let/cc k k))
                    (list 1 2 3)))))])         
         (go)
         (assert-true (catch-unsafe-context-exn
                       (lambda () (m11-eval '(dispatch-start 'foo)))))))
      
      ;; if the continuation-capture is attempted in tail position then we
      ;; should be just fine.
      ; XXX Weird
      #;(make-test-case
       "continuation capture from tail position of untranslated procedure"
       
       (let ([ta-eval
              (make-module-eval
               (module ta mzscheme
                 (provide tail-apply)
                 
                 (define (tail-apply f . args)
                   (apply f args))))])
         
         (ta-eval '(module m12 "../lang.ss"
                     (require ta)
                     (provide start)
                     (define (start initial)
                       (+ initial 
                          (tail-apply (lambda (x) (let/cc k (k x))) 1)))))
         
         (ta-eval '(require m12))
         
         (assert = 2 (ta-eval '(dispatch-start 1)))))
      
      (make-test-case
       "attempt send/suspend from standard call to map"
       
       (let-values ([(go m13-eval)
              (make-module-eval
               (module m11 "../lang.ss"
                 (provide start)
                 (define (start initial)
                   (map
                    (lambda (n) (send/suspend
                                 (lambda (k)
                                   (let ([ignore (printf "n = ~s~n" n)])
                                     k))))
                    (list 1 2 3)))))])
         (go)
         (assert-true (catch-unsafe-context-exn
                       (lambda () (m13-eval '(dispatch-start 'foo)))))))
      
      ; XXX Weird
      #;(make-test-case
       "attempt send/suspend from tail position of untranslated procedure"
       
       (let-values ([(go ta-eval)
              (make-module-eval
               (module ta mzscheme
                 (provide tail-apply)
                 
                 (define (tail-apply f . args)
                   (apply f args))))])
         
         (ta-eval '(module m14 "../lang.ss"
                     (require ta)
                     (provide start)
                     (define (start ignore)
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
           (assert = 0 (ta-eval `(dispatch (list ,k0 -1)))))))))))