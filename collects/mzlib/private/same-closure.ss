#|

does not work for the 
3m garbage collector
(it has a different
closure representation)

assumes the JIT is
compiled into mzscheme,
but not necc enabled.
(If the JIT isn't compiled in,
the closure representation 
changes.)


|#

(module same-closure mzscheme
  (require (lib "foreign.ss"))
  (provide same-closure? closure-size)
  
  (unsafe!)
  
  (define 3m? (regexp-match #rx#"3m$" (path->bytes (system-library-subpath))))
  
  (define-cstruct _scheme-object
    ((so _short)))
  
  (define-cstruct _scheme-inclhash-object
    ((so _short)
     (key _short)))
  
  (define-cstruct _scheme-closure-data
    ((iso _scheme-inclhash-object)
     (num-params _int)
     (max-let-depth _int)
     (closure-size _int)
     ;; more fields here in reality,
     ;; but don't matter for this code.
     ))
  
  (define-cstruct _scheme-closure
    ((so _short) 
     (code _scheme-closure-data-pointer)
     ;; don't include the array at the end, so
     ;; the indexing computation below is right.
     ))
     
  (define-cstruct _scheme-native-closure-data
    ((code _pointer)
     (arity-stuff _pointer)
     (arity-code _pointer)
     (max-let-depth _int)
     (closure-size _int)))
  
  (define-cstruct _scheme-native-closure
    ((so _short)
     (code _scheme-native-closure-data-pointer)
     ;; vals go here -- an array of pointers stuck on the end
     ;; of this struct.
     ))
  
  (define closure-size 
    (if 3m?
        (λ (a) (error 'closure-size "not supported for 3m"))
        (λ (a)
          (cond
            [(not (procedure? a)) 
             (error 'closure-size "expected a procedure, got ~e" a)]
            [else
             (let ([ptr-a (malloc _pointer)])
               (ptr-set! ptr-a _scheme a)
               (let* ([so-a (ptr-ref ptr-a _scheme-object-pointer)]
                      [a-type (scheme-object-so so-a)])
                 (case a-type 
                   [(28)
                    (do-size-work ptr-a so-a
                                  _scheme-closure-pointer
                                  scheme-closure-code
                                  scheme-closure-data-closure-size)]
                   [(29) #f]
                   [(33)
                    (do-size-work ptr-a so-a
                                  _scheme-native-closure-pointer
                                  scheme-native-closure-code
                                  scheme-native-closure-data-closure-size)]
                   [else #f])))]))))
  
  (define (do-size-work ptr-a so-a _ptr-type code-selector size-selector)
    (let ([closure-data-a (code-selector (ptr-ref ptr-a _ptr-type))])
      (size-selector closure-data-a)))
  
  (define same-closure?
    (if 3m?
        (λ (a b) (error 'same-closure? "not supported for 3m"))
        (λ (a b)
          (cond
            [(not (procedure? a)) 
             (error 'same-closure? "expected a procedure as first argument, got ~e" a)]
            [(not (procedure? b)) 
             (error 'same-closure? "expected a procedure as first argument, got ~e" b)]
            [(eq? a b) #t]
            [else
             (let ([ptr-a (malloc _pointer)]
                   [ptr-b (malloc _pointer)])
               (ptr-set! ptr-a _scheme a)
               (ptr-set! ptr-b _scheme b)
               (let* ([so-a (ptr-ref ptr-a _scheme-object-pointer)]
                      [a-type (scheme-object-so so-a)]
                      [so-b (ptr-ref ptr-b _scheme-object-pointer)]
                      [b-type (scheme-object-so so-b)])
                 (if (= a-type b-type)
                     (case a-type 
                       [(28)
                        (do-work ptr-a ptr-b so-a so-b
                                 _scheme-closure-pointer
                                 scheme-closure-code
                                 scheme-closure-data-closure-size
                                 _scheme-closure)]
                       [(29)
                        ;; case lambda
                        ;; cop out for now
                        (eq? a b)]
                       [(33)
                        (do-work ptr-a ptr-b so-a so-b
                                 _scheme-native-closure-pointer
                                 scheme-native-closure-code
                                 scheme-native-closure-data-closure-size
                                 _scheme-native-closure)]
                       [else
                        ;(printf "unknown type ~s ~s\n" a a-type)
                        (eq? a b)])
                     #f)))]))))
  
  (define (do-work ptr-a ptr-b so-a so-b _ptr-type code-selector size-selector _type)
    (let ([closure-data-a (code-selector (ptr-ref ptr-a _ptr-type))]
          [closure-data-b (code-selector (ptr-ref ptr-b _ptr-type))])
      (and (ptr-equal? closure-data-a closure-data-b)
           (let ([size (size-selector closure-data-a)])
             (let loop ([i 0])
               (or (= i size)
                   (let ([index (+ (ctype-sizeof _type)
                                   (* (ctype-sizeof _pointer) i))])
                     (and (ptr-equal?
                           (ptr-ref so-a _pointer 'abs index)
                           (ptr-ref so-b _pointer 'abs index))
                          (loop (+ i 1))))))))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; test cases 
  ;; (use eval to construct functions so we can control
  ;;  whether or not the JIT is enabled.)
  ;;
  
  #;
  (begin
    (require (lib "etc.ss")
             (lib "list.ss")
             "test.scm")
    
    (define (run-tests)
      (test (eval `(,closure-size (lambda (x) x)))
            0)
      (test (eval `(,closure-size ((lambda (x) (lambda (y) x)) 1)))
            1)
      (test (eval `(,closure-size ((lambda (x y) (lambda (z) (x y))) 1 2)))
            2)
      (test (eval `(,closure-size (((lambda (x y) (lambda (p q) (lambda (z) (x y p q)))) 1 2) 3 4)))
            4)
      
      (test (eval `(,same-closure? (lambda (x) x) (lambda (x) x)))
            #f)
      (test (eval `(,same-closure? (call/cc values) (call/cc values)))
            #f)
      (test (eval `(,same-closure? + -))
            #f)
      (test (eval `(,same-closure? + +))
            #t)
      (test (eval `(let ([f (lambda (x) (lambda (y) x))])
                     (,same-closure? (f 1) (f 1))))
            #t)
      (test (eval `(let ([f (lambda (x) (lambda (y) x))])
                     (,same-closure? (f f) (f f))))
            #t)
      
      (test (eval `(let ([f (lambda (x) (lambda (y) x))])
                     (,same-closure? (f 1) (f 2))))
            #f)
      (test (eval `(let ([f (lambda (x) (lambda (y) x))])
                     (,same-closure? (f 1) (f f))))
            #f)
      (test (eval `(let ([f 1])
                     (,same-closure? 
                       (lambda (x) f) 
                       (lambda (x) f))))
            #f)
      (test (eval `(let ([f (lambda (x y z p) (lambda (y) (x y z p)))])
                     (,same-closure? (f 1 2 3 4) (f 1 2 3 4))))
            #t)
      (test (eval `(let ([f (lambda (x y z p) (lambda (y) (x y z p)))])
                     (,same-closure? (f 1 2 3 5) (f 1 2 3 4))))
            #f)
      (test (eval `(let ([f (lambda () (lambda (y) +))])
                     (,same-closure? (f) (f))))
            #t)
      (test (eval `(,same-closure? (lambda (y) -) (lambda (y) +)))
            #f)
      (test (eval `(begin (module m mzscheme
                            (provide ans)
                            (define (f y z) (lambda (x) (+ x y z)))
                            (define ans (,same-closure? (f 1 2) (f 1 2))))
                          (require m)
                          ans))
            #t)
      (test (eval `(begin (module m mzscheme
                            (provide ans)
                            (define (f y z) (lambda (x) (+ x y z)))
                            (define ans (,same-closure? (f 1 2) (f 2 1))))
                          (require m)
                          ans))
            #f)
      (test (eval `(let ([f (λ (x)
                              (case-lambda 
                                [() x]
                                [(x) x]))])
                     (,same-closure? (f 1) (f f))))
            #f)
      
      ;; this test fails, because case-lambda isn't handled yet.
      #; 
      (test (eval `(let ([f (λ (x)
                              (case-lambda 
                                [() x]
                                [(x) x]))])
                     (,same-closure? (f 1) (f 1))))
            #t)
      
      
      ;; make some big closures
      (let* ([size 4000]
             [vars (build-list size (λ (x) (string->symbol (format "x~a" x))))]
             [lam (eval `(λ ,vars
                           (λ (x)
                             (list ,@vars))))]
             [diff-list (map values vars)])
        (set-car! (last-pair diff-list) 2) ;; set up difference
        (test (same-closure? (apply lam vars) (apply lam vars))
              #t)
        (test (same-closure? (apply lam vars) (apply lam diff-list))
              #f)))
    
    
    (printf "non-jit tests\n")
    (parameterize ([eval-jit-enabled #f]) (run-tests))
    (printf "jit tests\n")
    (parameterize ([eval-jit-enabled #t]) (run-tests))
    (printf "tests done\n")
    
    (define (timing-test)
      (let* ([f (λ (x) (λ (y) x))]
             [f1 (f 1)]
             [f2 (f 2)])
        (let loop ([i 10000])
          (unless (zero? i)
            (same-closure? f1 f2) (same-closure? f1 f2) (same-closure? f1 f2) (same-closure? f1 f2)
            (same-closure? f1 f2) (same-closure? f1 f2) (same-closure? f1 f2) (same-closure? f1 f2)
            (same-closure? f1 f2) (same-closure? f1 f2) (same-closure? f1 f2) (same-closure? f1 f2)
            (same-closure? f1 f2) (same-closure? f1 f2) (same-closure? f1 f2) (same-closure? f1 f2)
            (loop (- i 1))))))))
