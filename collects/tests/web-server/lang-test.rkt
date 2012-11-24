#lang racket/base
(require rackunit
         "util.rkt")
(provide lang-tests)

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

(define the-dispatch
  `(lambda (k*v)
     (lambda (k*v)
       ((car k*v) k*v))))

(define lang-tests
  (test-suite
   "Web Language Servlets"
   
   ;; ****************************************
   ;; ****************************************
   ;; BASIC TESTS  
   (test-suite
    "Basic Tests"
    
    (test-case
     "Function application with single argument in tail position"
     (let-values ([(test-m00.4)
                   (make-module-eval
                    (module m00.4 web-server/lang
                      (provide start)
                      (define (start initial)
                        (let ([f (let ([m 7]) m)])
                          (+ f initial)))))])
       (check = 8 (test-m00.4 '(dispatch-start start 1)))))

    (test-case
     "set!"
     (let-values ([(test-m00.4)
                   (make-module-eval
                    (module m00.4 web-server/lang
                            (provide start)
                            (define x 1)
                            (define (start initial)
                              (set! x (add1 x))
                              x)))])
       (check = 2 (test-m00.4 '(dispatch-start start #f)))
       (check = 3 (test-m00.4 '(dispatch-start start #f)))))
        
    (test-case
     "Embedded Definitions"
     (let-values ([(test-m00.4)
                   (make-module-eval
                    (module m00.4 web-server/lang
                      (provide start)
                      (define (start initial)
                        (define m 7)
                        (define f m)
                        (+ f initial))))])
       (check = 8 (test-m00.4 '(dispatch-start start 1)))))
    
    (test-case
     "Embedded Definitions + Intermixed expressions"
     (let-values ([(test-m00.4)
                   (make-module-eval
                    (module m00.4 web-server/lang
                      (provide start)
                      (define (start initial)
                        (define m 7)
                        (+ m 13)
                        (define f m)
                        (+ f initial))))])
       (check = 8 (test-m00.4 '(dispatch-start start 1)))))
    
    (test-case
     "start-interaction in argument position of a function call"
     (let-values ([(test-m00.3)
                   (make-module-eval
                    (module m00.3 web-server/lang
                      (define (foo x) 'foo)
                      (provide start)
                      (define (start initial)
                        (foo initial))))])
       (check eqv? 'foo (test-m00.3 '(dispatch-start start 7)))))
    
    (test-case
     "identity interaction, dispatch-start called multiple times"
     (let-values ([(test-m00)
                   (make-module-eval
                    (module m00 web-server/lang
                      (define (id x) x)
                      (provide start)
                      (define (start initial)
                        (id initial))))])
       (check = 7 (test-m00 '(dispatch-start start 7)))
       (check eqv? 'foo (test-m00 '(dispatch-start start 'foo)))))
    
    (test-case
     "start-interaction in argument position of a primitive"
     (let-values ([(test-m00.1)
                   (make-module-eval
                    (module m00.1 web-server/lang
                      (provide start)
                      (define (start initial)
                        (+ 1 initial))))])         
       (check = 2 (test-m00.1 '(dispatch-start start 1)))))
    
    (test-case
     "dispatch-start called multiple times for s-i in non-trivial context"
     (let-values ([(test-m00.2)
                   (make-module-eval
                    (module m00.2 web-server/lang
                      (provide start)
                      (define (start initial)
                        (+ (+ 1 1) initial))))])         
       (check = 14 (test-m00.2 '(dispatch-start start 12)))
       (check = 20 (test-m00.2 '(dispatch-start start 18)))))
    
    (test-case
     "start-interaction in third position"
     (let-values ([(test-m01)
                   (make-module-eval
                    (module m01 web-server/lang
                      (provide start)
                      (define (start initial)
                        (+ (* 1 2) (* 3 4) initial))))])         
       (check = 14 (test-m01 '(dispatch-start start 0)))
       (check = 20 (test-m01 '(dispatch-start start 6))))))          
   
   (test-suite
    "Tests involving multiple values"
    (test-case
     "begin with intermediate multiple values"
     (let-values ([(test)
                   (make-module-eval
                    (module m03 web-server/lang
                      (provide start)
                      (define (start x)
                        (begin (printf "Before\n")
                               (values 1 x)
                               (printf "After\n")
                               x))))])
       (check = 3 (test `(dispatch-start start 3)))))
    
    (test-case
     "begin0 with intermediate multiple values"
     (let-values ([(test)
                   (make-module-eval
                    (module m03 web-server/lang
                      (provide start)
                      (define (start x)
                        (begin0 x
                                (printf "Before\n")
                                (values 1 x)
                                (printf "After\n")))))])
       (check = 3 (test `(dispatch-start start 3)))))
    
    (test-case
     "begin0 with multiple values"
     (let-values ([(test)
                   (make-module-eval
                    (module m03 web-server/lang
                      (provide start)
                      (define (start x)
                        (let-values ([(_ ans)
                                      (begin0 (values 1 x) 
                                              (printf "Before\n")
                                              x
                                              (printf "After\n"))])
                          ans))))])
       (check = 3 (test `(dispatch-start start 3))))))
   
   (test-suite
    "Tests Involving call/cc"
    
    (test-case
     "continuation invoked in non-trivial context from within proc"
     (let-values ([(test-m03)
                   (make-module-eval
                    (module m03 web-server/lang
                      (provide start)
                      (define (start x)
                        (let/cc k
                          (+ 2 4 (k 3) 6 8)))))])         
       (check = 3 (test-m03 '(dispatch-start start 'foo)))
       (check = 3 (test-m03 '(dispatch-start start 7)))))
    
    ;; in the following test, if you modify
    ;; resume to print the "stack" you will
    ;; see that this is not tail recursive
    (test-case
     "non-tail-recursive 'escaping' continuation"
     (let-values ([(test-m04)
                   (make-module-eval
                    (module m04 web-server/lang
                      (provide start)
                      (define (start ln)
                        (let/cc k
                          (cond
                            [(null? ln) 1]
                            [(zero? (car ln)) (k 0)]
                            [else
                             (* (car ln)
                                (start (cdr ln)))])))))])
       (check = 0 (test-m04 '(dispatch-start start (list 1 2 3 4 5 6 7 0 8 9))))
       (check = 120 (test-m04 '(dispatch-start start (list 1 2 3 4 5))))))
    
    ;; this version captures the continuation
    ;; outside the recursion and should be tail
    ;; recursive. A "stack trace" reveals this
    ;; as expected.
    (test-case
     "tail-recursive escaping continuation"
     (let-values ([(test-m05)
                   (make-module-eval
                    (module m05 web-server/lang
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
       (check = 0 (test-m05 '(dispatch-start start (list 1 2 3 0 4 5 6))))
       (check = 120 (test-m05 '(dispatch-start start (list 1 2 3 4 5)))))))
   
   ;; ****************************************
   ;; ****************************************
   ;; TESTS INVOLVING call-with-serializable-current-continuation
   (test-suite
    "Tests Involving call-with-serializable-current-continuation"
    
    (test-case
     "curried add with call-with-serializable-current-continuation"
     (let ([table-01-eval
            (make-module-eval
             (module table01 racket
               (provide store-k
                        lookup-k)
               
               (define the-table (make-hash))
               
               (define (store-k k)
                 (let ([key (string->symbol (symbol->string (gensym 'key)))])
                   (hash-set! the-table key k)
                   key))
               (define (lookup-k key-pair)
                 (hash-ref the-table (car key-pair) (lambda () #f)))))])         
       (table-01-eval
        '(module m06 web-server/lang
           (require 'table01)
           (provide start)
           
           (define (gn which)
             (cadr
              (call-with-serializable-current-continuation
               (lambda (k)
                 (let ([ignore (printf "Please send the ~a number.\n" which)])
                   (store-k k))))))
           
           (define (start ignore)
             (let ([result (+ (gn "first") (gn "second"))])
               (let ([ignore (printf "The answer is: ~s\n" result)])
                 result)))))         
       (table-01-eval '(require 'm06))         
       (let* ([first-key (table-01-eval '(dispatch-start start 'foo))]
              [second-key (table-01-eval `(dispatch lookup-k '(,first-key 1)))]
              [third-key (table-01-eval `(dispatch lookup-k '(,first-key -7)))])
         #;(printf "~S\n" (list first-key second-key third-key))
         (check = 3 (table-01-eval `(dispatch lookup-k '(,second-key 2))))
         (check = 4 (table-01-eval `(dispatch lookup-k '(,second-key 3))))
         (check-true (zero? (table-01-eval `(dispatch lookup-k '(,second-key -1)))))
         (check = -7 (table-01-eval `(dispatch lookup-k '(,third-key 0))))
         (check-true (zero? (table-01-eval `(dispatch lookup-k '(,third-key 7))))))))
    
    (test-case
     "curried with call-with-serializable-current-continuation and serializaztion"
     
     (let-values ([(test-m06.1)
                   (make-module-eval
                    (module m06.1 web-server/lang
                      (provide start)
                      (define (gn which)
                        (cadr
                         (call-with-serializable-current-continuation
                          (lambda (k)
                            (let ([ignore (printf "Please send the ~a number.\n" which)])
                              k)))))
                      
                      (define (start ignore)
                        (let ([result (+ (gn "first") (gn "second"))])
                          (let ([ignore (printf "The answer is: ~s\n" result)])
                            result)))))])
       (let* ([first-key (test-m06.1 '(dispatch-start start 'foo))]
              [second-key (test-m06.1 `(dispatch ,the-dispatch (list (deserialize (serialize ,first-key)) 1)))]
              [third-key (test-m06.1 `(dispatch ,the-dispatch (list (deserialize (serialize ,first-key)) -7)))])
         (check = 3 (test-m06.1 `(call-with-web-prompt (lambda () (dispatch ,the-dispatch (list ,second-key 2))))))
         (check = 4 (test-m06.1 `(dispatch ,the-dispatch (list ,second-key 3))))
         (check-true (zero? (test-m06.1 `(dispatch ,the-dispatch (list ,second-key -1)))))
         (check = -7 (test-m06.1 `(dispatch ,the-dispatch (list ,third-key 0))))
         (check-true (zero? (test-m06.1 `(dispatch ,the-dispatch (list ,third-key 7))))))))
    
    (test-case
     "curried with call-with-serializable-current-continuation and serializaztion (keyword args)"
     
     (let-values ([(test-m06.2)
                   (make-module-eval
                    (module m06.2 web-server/lang
                      (provide start)
                      (define (gn #:page which)
                        (cadr
                         (call-with-serializable-current-continuation
                          (lambda (k)
                            (let ([ignore (printf "Please send the ~a number.\n" which)])
                              k)))))
                      
                      (define (start ignore)
                        (let ([result (+ (gn #:page "first") (gn #:page "second"))])
                          (let ([ignore (printf "The answer is: ~s\n" result)])
                            result)))))])
       (let* ([first-key (test-m06.2 '(dispatch-start start 'foo))]
              [second-key (test-m06.2 `(dispatch ,the-dispatch (list (deserialize (serialize ,first-key)) 1)))]
              [third-key (test-m06.2 `(dispatch ,the-dispatch (list (deserialize (serialize ,first-key)) -7)))])
         (check = 3 (test-m06.2 `(call-with-web-prompt (lambda () (dispatch ,the-dispatch (list ,second-key 2))))))
         (check = 4 (test-m06.2 `(dispatch ,the-dispatch (list ,second-key 3))))
         (check-true (zero? (test-m06.2 `(dispatch ,the-dispatch (list ,second-key -1)))))
         (check = -7 (test-m06.2 `(dispatch ,the-dispatch (list ,third-key 0))))
         (check-true (zero? (test-m06.2 `(dispatch ,the-dispatch (list ,third-key 7)))))))))
   
   (test-suite
    "Test the certification process"
    
    (test-suite
     "Splicing tests"
     
     (test-case
      "quasi-quote with splicing: need to recertify context for qq-append"
      (let-values ([(test-m01.1)
                    (make-module-eval
                     (module m01.1 web-server/lang
                       (provide start)
                       (define (start initial)
                         `(,@(list 1 2 initial)))))])         
        (check equal? (list 1 2 3) (test-m01.1 '(dispatch-start start 3)))
        (check equal? (list 1 2 'foo) (test-m01.1 '(dispatch-start start 'foo)))))
     
     (test-case
      "recertify context test (1)"
      (let-values ([(test-m01.2)
                    (make-module-eval
                     (module m01.1 web-server/lang
                       (provide start)
                       (define (start initial)
                         `(foo ,@(list 1 2 3)))))])
        (check-true #t)))
     
     (test-case
      "recertify context test (2)"
      (let-values ([(test-m01.3)
                    (make-module-eval  
                     (module m01.3 web-server/lang
                       (provide start)
                       (define (start n)
                         `(n ,@(list 1 2 3)))))])
        (check-true #t)))
     
     (test-case
      "recertify context test (3)"
      (let-values ([(test-m01.4)
                    (make-module-eval
                     (module m1 web-server/lang
                       (provide start)
                       (define (start initial)
                         (define (bar n)
                           `(n ,@(list 1 2 3)))
                         (bar 7))))])
        (check-true #t)))))
   
   (test-suite
    "Tests Involving letrec"
    
    (test-case
     "mutually recursive even? and odd?"
     (let-values ([(test-m07)
                   (make-module-eval
                    (module m07 web-server/lang
                      (provide start)
                      (define (start initial)
                        (letrec ([even? (lambda (n)
                                          (or (zero? n)
                                              (odd? (sub1 n))))]
                                 [odd? (lambda (n)
                                         (and (not (zero? n))
                                              (even? (sub1 n))))])
                          (even? initial)))))])         
       (check-true (test-m07 '(dispatch-start start 0)))
       (check-true (test-m07 '(dispatch-start start 16)))
       (check-false (test-m07 '(dispatch-start start 1)))
       (check-false (test-m07 '(dispatch-start start 7)))))
    
    (test-case
     "call-with-serializable-current-continuation on rhs of letrec binding forms"
     (let-values ([(test-m08)
                   (make-module-eval
                    (module m08 web-server/lang
                      (provide start)
                      (define (gn which)
                        (cadr
                         (call-with-serializable-current-continuation
                          (lambda (k)
                            (let ([ignore (printf "Please send the ~a number.\n" which)])
                              k)))))
                      
                      (define (start ignore)
                        (letrec ([f (let ([n (gn "first")])
                                      (lambda (m) (+ n m)))]
                                 [g (let ([n (gn "second")])
                                      (lambda (m) (+ n (f m))))])
                          (let ([result (g (gn "third"))])
                            (let ([ignore (printf "The answer is: ~s\n" result)])
                              result))))))])
       (let* ([k0 (test-m08 '(serialize (dispatch-start start 'foo)))]
              [k1 (test-m08 `(serialize (dispatch ,the-dispatch (list (deserialize ',k0) 1))))]
              [k2 (test-m08 `(serialize (dispatch ,the-dispatch (list (deserialize ',k1) 2))))])
         (check = 6 (test-m08 `(dispatch ,the-dispatch (list (deserialize ',k2) 3))))
         (check = 9 (test-m08 `(dispatch ,the-dispatch (list (deserialize ',k2) 6))))
         (let* ([k1.1 (test-m08 `(serialize (dispatch ,the-dispatch (list (deserialize ',k0) -1))))]
                [k2.1 (test-m08 `(serialize (dispatch ,the-dispatch (list (deserialize ',k1.1) -2))))])
           (check-true (zero? (test-m08 `(dispatch ,the-dispatch (list (deserialize ',k2.1) 3)))))
           (check = 6 (test-m08 `(dispatch ,the-dispatch (list (deserialize ',k2) 3)))))))))
   
   (test-suite
    "Unsafe Context Condition Tests"
    
    (test-case
     "simple attempt to capture a continuation from an unsafe context"
     
     (let-values ([(nta-eval)
                   (make-module-eval
                    (module nta racket
                      (provide non-tail-apply)
                      
                      (define (non-tail-apply f . args)
                        (let ([result (apply f args)])
                          (printf "result = ~s\n" result)
                          result))))])
       (nta-eval '(module m09 web-server/lang
                    (require 'nta)
                    (provide start)
                    (define (start ignore)
                      (non-tail-apply (lambda (x) (let/cc k (k x))) 7))))
       
       (nta-eval '(require 'm09))
       
       (check-true (catch-unsafe-context-exn
                    (lambda () (nta-eval '(dispatch-start start 'foo)))))))
    
    (test-case
     "sanity-check: capture continuation from safe version of context"
     
     (let-values ([(m10-eval)
                   (make-module-eval
                    (module m10 web-server/lang
                      (provide start)
                      (define (nta f arg)
                        (let ([result (f arg)])
                          (printf "result = ~s\n" result)
                          result))
                      (define (start ignore)
                        (nta (lambda (x) (let/cc k (k x))) 7))))])         
       (check = 7 (m10-eval '(dispatch-start start 'foo)))))
    
    (test-case
     "attempt continuation capture from standard call to map"
     
     (let-values ([(m11-eval)
                   (make-module-eval
                    (module m11 web-server/lang
                      (provide start)
                      (define (start ignore)
                        (map
                         (lambda (x) (let/cc k k))
                         (list 1 2 3)))))])         
       (check-true (catch-unsafe-context-exn
                    (lambda () (m11-eval '(dispatch-start start 'foo)))))))
    
    ;; if the continuation-capture is attempted in tail position then we
    ;; should be just fine.
    (test-case
     "continuation capture from tail position of untranslated procedure"
     
     (let ([ta-eval
            (make-module-eval
             (module ta racket
               (provide tail-apply)
               
               (define (tail-apply f . args)
                 (apply f args))))])
       
       (ta-eval '(module m12 web-server/lang
                   (require 'ta)
                   (provide start)
                   (define (start initial)
                     (+ initial 
                        (tail-apply (lambda (x) (let/cc k (k x))) 1)))))
       
       (ta-eval '(require 'm12))
       
       (check = 2 (ta-eval '(dispatch-start start 1)))))
    
    (test-case
     "attempt call-with-serializable-current-continuation from standard call to map"
     
     (let-values ([(m13-eval)
                   (make-module-eval
                    (module m11 web-server/lang
                      (provide start)
                      (define (start initial)
                        (map
                         (lambda (n) (call-with-serializable-current-continuation
                                      (lambda (k)
                                        (let ([ignore (printf "n = ~s\n" n)])
                                          k))))
                         (list 1 2 3)))))])
       (check-true (catch-unsafe-context-exn
                    (lambda () (m13-eval '(dispatch-start start 'foo)))))))
    
    (test-case
     "attempt call-with-serializable-current-continuation from tail position of untranslated procedure"
     
     (let-values ([(ta-eval)
                   (make-module-eval
                    (module ta racket
                      (provide tail-apply)
                      
                      (define (tail-apply f . args)
                        (apply f args))))])
       
       (ta-eval '(module m14 web-server/lang
                   (require 'ta)
                   (provide start)
                   (define (start ignore)
                     (+ 1 (tail-apply
                           (lambda (n)
                             (cadr
                              (call-with-serializable-current-continuation
                               (lambda (k)
                                 (let ([ignore (printf "n = ~s\n" n)])
                                   k))))) 7)))))
       (ta-eval '(require 'm14))
       
       (let ([k0 (ta-eval '(dispatch-start start 'foo))])
         (check = 3 (ta-eval `(dispatch ,the-dispatch (list ,k0 2))))
         (check = 0 (ta-eval `(dispatch ,the-dispatch (list ,k0 -1))))))))
   
   (test-suite
    "Weird Cases"
    
    (test-case
     "provide/contract: simple"
     (check-not-exn
      (lambda ()
        (make-module-eval
         (module data web-server/lang
           (require mzlib/contract)
           
           (define x 1)
           (provide/contract
            [x integer?]))))))
    
    
    (test-case
     "provide/contract: struct"
     (check-not-exn
      (lambda ()
        (make-module-eval
         (module data web-server/lang
           (require mzlib/contract)
           
           (define-struct posn (x y) #:mutable)
           (provide/contract
            [struct posn ([x integer?] [y integer?])]))))))
    
    (test-case
     "define-values error"
     (check-not-exn
      (lambda ()
        (make-module-eval
         (module test web-server/lang
           (define (show-user)
             (define-values (point i) (values #t 1))
             i)))))))))
