#lang racket


(require rackunit rackunit/text-ui unstable/options)


(define-syntax-rule (test-pass test-name expr) 
  (test-case test-name (check-pass expr)))

(define-syntax-rule (check-pass expr)
  (check-not-exn (λ () expr)))

(define-syntax-rule (test-fail test-name expr error-msg) 
  (test-case test-name (check-fail expr error-msg)))

(define-syntax-rule (check-fail expr error-msg)
  (check-exn (λ (exn)
               (and
                (exn:fail? exn)
                (regexp-match?
                 (regexp-quote error-msg)
                 (exn-message exn))))
             (λ () expr)))


(define-syntax test-contract-fail
  (syntax-rules ()
    [(test-contract-fail test-name expr error-msg)
     (test-case test-name (check-contract-fail expr error-msg))]
    [(test-contract-fail test-name  expr error-msg extra-msg)
     (test-case test-name 
                (check-contract-fail expr error-msg extra-msg))]))


(define-syntax check-contract-fail
  (syntax-rules ()
    [(check-contract-fail expr error-msg)
     (check-exn (λ (exn)
                  (and (exn:fail? exn)
                       (has-proper-blame? error-msg (exn-message exn))))
                (λ () expr))]
    [(check-contract-fail expr error-msg extra-msg)
     (check-exn (λ (exn)
                  (and (exn:fail? exn)
                       (has-proper-blame? error-msg (exn-message exn) extra-msg)))
                (λ () expr))]))

(define (has-proper-blame? blame msg [extra ""])
  (define options-preface 
    (regexp-quote "blaming multiple parties:"))
  (define (multi-blame->regexp blame)
    (foldr 
     (λ (fst rst) (string-append (format "\n  ~a" fst) rst))
     ""
     blame))
  (define reg
    (cond
      [(string? blame) (string-append "blaming: " (regexp-quote blame))]
      [(list? blame)  (string-append
                       options-preface
                       (multi-blame->regexp blame))]
      [else #f]))
  (define extra-reg (regexp-quote extra))
  (and reg (regexp-match? reg msg) (regexp-match? extra-reg msg)))


(define-syntax-rule (script e ...)
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require '(for-syntax racket/base))
    (eval 'e) ...))


(run-tests
 (test-suite "options.rkt"
             
             
             (test-suite "options"
                         
                         (test-suite "option/c"
                                     
                                     (test-contract-fail 
                                      "failed tester"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec 
                                                    (option/c
                                                     any/c
                                                     #:tester sorted?)]))
                                         (define vec (vector 1 3 2 4 5))
                                         (define (sorted? vec)
                                           (for/and ([el vec]
                                                     [cel (vector-drop vec 1)])
                                             (<= el cel))))
                                       (require 'server))
                                      "server"
                                      "option contract tester")
                                     
                                     (test-contract-fail 
                                      "failed option/c with misbehaving tester (indy)"
                                      (script
                                       (module ctc racket
                                         (require unstable/options)
                                         (provide indy-ctc)
                                         (define indy-ctc
                                           (option/c
                                            (-> number? number?)
                                            #:tester (λ (f) (f 'foo)))))
                                       (module server racket
                                         (require unstable/options)
                                         (require 'ctc)
                                         (provide (contract-out [f indy-ctc]))
                                         (define f values))
                                       (require 'server))
                                      "ctc")
                                     
                                     (test-contract-fail 
                                      "failed option/c no invariant but immutable"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (option/c 
                                                         any/c
                                                         #:immutable #t)]))
                                         (define vec (vector 1 2 3 4 5)))
                                       (require 'server))
                                      "server"
                                      "an invariant keyword argument (based on presence of other keyword arguments)")
                                     
                                     (test-contract-fail 
                                      "failed option/c no invariant but flat"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (option/c 
                                                         any/c
                                                         #:flat? #t)]))
                                         (define vec (vector 1 2 3 4 5)))
                                       (require 'server))
                                      "server"
                                      "an invariant keyword argument (based on presence of other keyword arguments)")
                                     
                                     (test-contract-fail 
                                      "failed option/c no invariant but flat and immutable"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (option/c 
                                                         any/c
                                                         #:flat? #t
                                                         #:immutable #t)]))
                                         (define vec (vector 1 2 3 4 5)))
                                       (require 'server))
                                      "server"
                                      "an invariant keyword argument (based on presence of other keyword arguments)")  
                                     
                                     (test-pass
                                      "passes with option/c on function with keyword arguments"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [f (option/c 
                                                       (-> number? #:more number? number?))]))
                                         (define (f x #:more y) y))
                                       (require unstable/options)
                                       (require 'server)
                                       (f 2 #:more 3)
                                       ((exercise-option f) 2 #:more 3))) 
                                     
                                     (test-pass
                                      "passes with option/c on function with optional keyword arguments"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [f (option/c (->* (number?) (#:more number?) number?))]))
                                         (define (f x #:more [y 3]) y))
                                       (require unstable/options)
                                       (require 'server)
                                       (f 2)
                                       (f 2 #:more 4)
                                       ((exercise-option f) 2 #:more 4)
                                       ((exercise-option f) 2))) 
                                     
                                     (test-pass
                                      "passes with option/c on function with case-lambda"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [f (option/c (case-> 
                                                                 (-> number? number? number?)
                                                                 (-> number? number?)))]))
                                         (define f (case-lambda 
                                                     [(lo hi) (max lo hi)]
                                                     [(single) single])))
                                       (require unstable/options)
                                       (require 'server)
                                       (f 2)
                                       (f 2 4)
                                       ((exercise-option f) 2 4)
                                       ((exercise-option f) 2))) 
                                     
                                     (test-contract-fail
                                      "fails with option/c on function with case-lambda"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [f (option/c (case-> 
                                                                 (-> number? number? number?)
                                                                 (-> number? number?)))]))
                                         (define f (case-lambda 
                                                     [(lo hi) (max lo hi)]
                                                     [(single) single])))
                                       (require unstable/options)
                                       (require 'server)
                                       (f 2)
                                       (f 2 4)
                                       ((exercise-option f) 2 "boo")
                                       ((exercise-option f) 2))
                                      "top-level") 
                                     
                                     (test-pass
                                      "passes with option/c with invariant and flat and immutable"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (option/c 
                                                         any/c
                                                         #:invariant values
                                                         #:flat? #t
                                                         #:immutable #t)]))
                                         (define vec #(1 2 3 4 5)))
                                       (require 'server))) 
                                     
                                     (test-contract-fail
                                      "failed derived invariant/c (immutable) "
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (option/c 
                                                         any/c
                                                         #:invariant values
                                                         #:immutable #t)]))
                                         (define vec (vector 1 2 3 4 5)))
                                       (module client racket
                                         (require unstable/options)
                                         (require 'server)
                                         (exercise-option vec))
                                       (require 'client))
                                      "server") 
                                     
                                     (test-contract-fail
                                      "failed derived invariant/c (procedure) "
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [f (option/c 
                                                       any/c
                                                       #:invariant values)]))
                                         (define f values))
                                       (module client racket
                                         (require unstable/options)
                                         (require 'server)
                                         (exercise-option f))
                                       (require 'client))
                                      "server")
                                     
                                     (test-fail 
                                      "failed option/c for struct (unbound struct id)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [s (option/c 
                                                       any/c
                                                       #:struct boo)]))
                                         (struct foo (a b))
                                         (define s (foo 2 2)))
                                       (require 'server))
                                      "expected a struct identifier") 
                                     
                                     (test-contract-fail 
                                      "failed option/c for struct (missing struct id)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [s (option/c any/c)]))
                                         (struct foo (a b))
                                         (define s (foo 2 2)))
                                       (require 'server))
                                      "server"
                                      "a vector or a hash") 
                                     
                                     (test-contract-fail 
                                      "failed option/c for struct (wrong struct id)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [s (option/c 
                                                       any/c
                                                       #:struct boo)]))
                                         (struct foo (a b))
                                         (struct boo (a b))
                                         (define s (foo 2 2)))
                                       (require 'server))
                                      "server"
                                      "a struct of type boo")  
                                     
                                     (test-contract-fail 
                                      "failed option/c for struct (vector for struct id)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [s (option/c 
                                                       any/c
                                                       #:struct foo)]))
                                         (struct foo (a b))
                                         (define s (vector 2 2)))
                                       (require 'server))                                      
                                      "server"
                                      "a struct of type foo"))         
                         
                         (test-suite "option/c with contract"
                                     
                                     (test-pass
                                      "passes with simple procedure contract"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo 
                                                                 (option/c
                                                                  (-> number? number?)
                                                                  #:with-contract #t)]))
                                         (define (boo x) x))
                                       (require 'server)
                                       (boo 42)))
                                     
                                     (test-contract-fail
                                      "fails (negative) with simple procedure contract"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo 
                                                                 (option/c
                                                                  (-> number? number?)
                                                                  #:with-contract #t)]))
                                         (define (boo x) 42))
                                       (require 'server)
                                       (boo 'wrong))
                                      "top-level")
                                     
                                     (test-contract-fail
                                      "fails (positive) with simple procedure contract"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo 
                                                                 (option/c
                                                                  (-> number? number?)
                                                                  #:with-contract #t)]))
                                         (define (boo x) 'wrong))
                                       (require unstable/options 'server)
                                       (boo 42))
                                      "server")
                                     
                                     (test-contract-fail 
                                      "failed tester"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec 
                                                    (option/c
                                                     any/c
                                                     #:with-contract #t
                                                     #:tester sorted?)]))
                                         (define vec (vector 1 3 2 4 5))
                                         (define (sorted? vec)
                                           (for/and ([el vec]
                                                     [cel (vector-drop vec 1)])
                                             (<= el cel))))
                                       (require 'server))
                                      "server"
                                      "option contract tester")
                                     
                                     (test-contract-fail 
                                      "failed option/c with misbehaving tester (indy)"
                                      (script
                                       (module ctc racket
                                         (require unstable/options)
                                         (provide indy-ctc)
                                         (define indy-ctc
                                           (option/c
                                            (-> number? number?)
                                            #:with-contract #t 
                                            #:tester (λ (f) (f 'foo)))))
                                       (module server racket
                                         (require unstable/options)
                                         (require 'ctc)
                                         (provide (contract-out [f indy-ctc]))
                                         (define f values))
                                       (require 'server))
                                      "ctc")
                                     
                                     (test-contract-fail 
                                      "failed option/c no invariant but immutable"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (option/c 
                                                         any/c
                                                         #:with-contract #t
                                                         #:immutable #t)]))
                                         (define vec (vector 1 2 3 4 5)))
                                       (require 'server))
                                      "server"
                                      "an invariant keyword argument (based on presence of other keyword arguments)")
                                     
                                     (test-contract-fail 
                                      "failed option/c no invariant but flat"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (option/c 
                                                         any/c
                                                         #:with-contract #t
                                                         #:flat? #t)]))
                                         (define vec (vector 1 2 3 4 5)))
                                       (require 'server))
                                      "server"
                                      "an invariant keyword argument (based on presence of other keyword arguments)")
                                     
                                     (test-contract-fail 
                                      "failed option/c no invariant but flat and immutable"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (option/c 
                                                         any/c
                                                         #:flat? #t
                                                         #:with-contract #t
                                                         #:immutable #t)]))
                                         (define vec (vector 1 2 3 4 5)))
                                       (require 'server))
                                      "server"
                                      "an invariant keyword argument (based on presence of other keyword arguments)")  
                                     
                                     (test-pass
                                      "passes with option/c on function with keyword arguments"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [f (option/c 
                                                       (-> number? #:more number? number?)
                                                       #:with-contract #t)]))
                                         (define (f x #:more y) y))
                                       (require unstable/options)
                                       (require 'server)
                                       (f 2 #:more 3)
                                       ((exercise-option f) 2 #:more 3))) 
                                     
                                     (test-pass
                                      "passes with option/c on function with optional keyword arguments"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [f (option/c
                                                       (->* (number?) (#:more number?) number?)
                                                       #:with-contract #t)]))
                                         (define (f x #:more [y 3]) y))
                                       (require unstable/options)
                                       (require 'server)
                                       (f 2)
                                       (f 2 #:more 4)
                                       ((exercise-option f) 2 #:more 4)
                                       ((exercise-option f) 2))) 
                                     
                                     (test-pass
                                      "passes with option/c on function with case-lambda"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [f (option/c 
                                                       (case-> 
                                                        (-> number? number? number?)
                                                        (-> number? number?))
                                                       #:with-contract #t)]))
                                         (define f (case-lambda 
                                                     [(lo hi) (max lo hi)]
                                                     [(single) single])))
                                       (require unstable/options)
                                       (require 'server)
                                       (f 2)
                                       (f 2 4)
                                       ((exercise-option f) 2 4)
                                       ((exercise-option f) 2))) 
                                     
                                     (test-contract-fail
                                      "fails with option/c on function with case-lambda"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [f (option/c
                                                       (case-> 
                                                        (-> number? number? number?)
                                                        (-> number? number?))
                                                       #:with-contract #t)]))
                                         (define f (case-lambda 
                                                     [(lo hi) (max lo hi)]
                                                     [(single) single])))
                                       (require unstable/options)
                                       (require 'server)
                                       (f 2 "boo"))
                                      "top-level") 
                                     
                                     (test-pass
                                      "passes with option/c with invariant and flat and immutable"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (option/c 
                                                         any/c
                                                         #:with-contract #t
                                                         #:invariant values
                                                         #:flat? #t
                                                         #:immutable #t)]))
                                         (define vec #(1 2 3 4 5)))
                                       (require 'server))) 
                                     
                                     (test-contract-fail
                                      "failed derived invariant/c (immutable) "
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (option/c 
                                                         any/c
                                                         #:with-contract #t
                                                         #:invariant values
                                                         #:immutable #t)]))
                                         (define vec (vector 1 2 3 4 5)))
                                       (require 'server))
                                      "server") 
                                     
                                     (test-contract-fail
                                      "failed derived invariant/c (procedure) "
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [f (option/c 
                                                       any/c
                                                       #:with-contract #t
                                                       #:invariant values)]))
                                         (define f values))
                                       (require 'server))
                                      "server")
                                     
                                     (test-fail 
                                      "failed option/c for struct (unbound struct id)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [s (option/c 
                                                       any/c
                                                       #:with-contract #t
                                                       #:struct boo)]))
                                         (struct foo (a b))
                                         (define s (foo 2 2)))
                                       (require 'server))
                                      "expected a struct identifier") 
                                     
                                     (test-contract-fail 
                                      "failed option/c for struct (missing struct id)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [s (option/c any/c #:with-contract #t)]))
                                         (struct foo (a b))
                                         (define s (foo 2 2)))
                                       (require 'server))
                                      "server"
                                      "a vector or a hash") 
                                     
                                     (test-contract-fail 
                                      "failed option/c for struct (wrong struct id)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [s (option/c 
                                                       any/c
                                                       #:struct boo
                                                       #:with-contract #t)]))
                                         (struct foo (a b))
                                         (struct boo (a b))
                                         (define s (foo 2 2)))
                                       (require 'server))
                                      "server"
                                      "a struct of type boo")  
                                     
                                     (test-contract-fail 
                                      "failed option/c for struct (vector for struct id)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [s (option/c 
                                                       any/c
                                                       #:struct foo               
                                                       #:with-contract #t)]))
                                         (struct foo (a b))
                                         (define s (vector 2 2)))
                                       (require 'server))                                      
                                      "server"
                                      "a struct of type foo"))
                         
                         (test-suite "transfer/c"
                                     
                                     (test-pass
                                      "passes after two transfers"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c (-> number? number?))]))
                                         (define (boo x) x))
                                       (module middle0 racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (module middle1 racket
                                         (require unstable/options 'middle0)
                                         (provide (contract-out [boo transfer/c])))
                                       (require unstable/options 'middle1)
                                       (boo 1)))
                                     
                                     (test-pass
                                      "passes after two transfers (with contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo 
                                                                 (option/c 
                                                                  (-> number? number?)
                                                                  #:with-contract #t)]))
                                         (define (boo x) x))
                                       (module middle0 racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (module middle1 racket
                                         (require unstable/options 'middle0)
                                         (provide (contract-out [boo transfer/c])))
                                       (require unstable/options 'middle1)
                                       (boo 1)))
                                     
                                     
                                     
                                     (test-contract-fail
                                      "fails (positive) after one transfer (with-contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo 
                                                                 (option/c
                                                                  (-> number? number?)
                                                                  #:with-contract #t)]))
                                         (define (boo x) 'wrong))
                                       (module client racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (require 'client)
                                       (displayln (boo 42))
                                       (boo 42))
                                      (list "client" "server"))
                                     
                                     (test-contract-fail
                                      "fails (negative) after one transfer (with-contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo 
                                                                 (option/c
                                                                  (-> number? number?)
                                                                  #:with-contract #t)]))
                                         (define (boo x) x))
                                       (module client racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (require 'client)
                                       (boo 'wrong))
                                      (list "top-level" "top-level"))
                                     
                                     (test-contract-fail
                                      "fails (positive) after two transfers (with contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo 
                                                                 (option/c 
                                                                  (-> number? number?)
                                                                  #:with-contract #t)]))
                                         (define (boo x) 'wrong))
                                       (module middle0 racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (module middle1 racket
                                         (require unstable/options 'middle0)
                                         (provide (contract-out [boo transfer/c])))
                                       (require unstable/options 'middle1)
                                       (boo 1))
                                      (list "middle1" "middle0" "server")) 
                                     
                                     (test-contract-fail
                                      "fails (negative) after two transfers (with contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo 
                                                                 (option/c 
                                                                  (-> number? number?)
                                                                  #:with-contract #t)]))
                                         (define (boo x) x))
                                       (module middle0 racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (module middle1 racket
                                         (require unstable/options 'middle0)
                                         (provide (contract-out [boo transfer/c])))
                                       (require unstable/options 'middle1)
                                       (boo 'wrong))
                                      (list "top-level" "top-level" "top-level"))
                                     
                                     (test-pass
                                      "passes after void transfer"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo transfer/c]))
                                         (define (boo x) x))
                                       (require 'server)))
                                     
                                     (test-pass
                                      "passes after void client's transfer"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (-> number? number?)]))
                                         (define (boo x) x))
                                       (module client racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (require 'client)
                                       (boo 42)))
                                     
                                     (test-pass
                                      "passes after void client's transfer after exercise"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c (-> number? number?))]))
                                         (define (boo x) x))
                                       (module client racket
                                         (require unstable/options 'server)
                                         (define e-boo (exercise-option boo))
                                         (provide (contract-out [e-boo transfer/c])))
                                       (require 'client)
                                       (e-boo 42)))
                                     
                                     (test-pass
                                      "passes after void client's transfer after exercise (with-contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c (-> number? number?))]))
                                         (define (boo x) x))
                                       (module client racket
                                         (require unstable/options 'server)
                                         (define e-boo (exercise-option boo))
                                         (provide (contract-out [e-boo transfer/c])))
                                       (require 'client)
                                       (e-boo 42)))
                                     
                                     (test-contract-fail
                                      "fails (positive-ho) after three transfers and exercise (with-contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo 
                                                                 (option/c
                                                                 (-> number? number?)
                                                                 #:with-contract #t)]
                                                                [bar (-> transfer/c number?)]))
                                         (define (boo x) 'wrong)
                                         (define (bar f) ((exercise-option f) 42)))
                                       (module client racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c]
                                                                [bar transfer/c])))
                                       (module client1 racket
                                         (require unstable/options 'client)
                                         (provide (contract-out [boo transfer/c]
                                                                [bar transfer/c])))                                      
                                       (require 'client1)
                                       (bar boo))
                                      (list "top-level" "client1" "client" "server"))
                                     
                                     (test-contract-fail
                                      "fails (negative-ho) after three transfers and exercise (with-contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo 
                                                                 (option/c
                                                                 (-> number? number?)
                                                                 #:with-contract #t)]
                                                                [bar (-> transfer/c number?)]))
                                         (define (boo x) 'wrong)
                                         (define (bar f) ((exercise-option f) 'wrong)))
                                       (module client racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c]
                                                                [bar transfer/c])))
                                       (module client1 racket
                                         (require unstable/options 'client)
                                         (provide (contract-out [boo transfer/c]
                                                                [bar transfer/c])))                                      
                                       (require 'client1)
                                       (bar boo))
                                      (list "server" "top-level" "top-level" "top-level"))) 
                         
                         (test-suite "exercise-option"
                                     
                                     (test-pass
                                      "passes after two transfers and exercise"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c (-> number? number?))]))
                                         (define (boo x) x))
                                       (module middle0 racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (module middle1 racket
                                         (require unstable/options 'middle0)
                                         (provide (contract-out [boo transfer/c])))
                                       (require unstable/options 'middle1)
                                       ((exercise-option boo) 1)))
                                     
                                     (test-contract-fail
                                      "positive contract failure after successful transfer and exercise"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c (-> number? number?))]))
                                         (define (boo x) "wrong!"))
                                       (module middle racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (module client racket
                                         (require unstable/options 'middle)
                                         ((exercise-option boo) 42))
                                       (require 'client))
                                      (list "middle" "server"))   
                                     
                                     (test-contract-fail
                                      "negative contract failure after successful transfer and exercise"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c (-> number? number?))]))
                                         (define (boo x) x))
                                       (module middle racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (module client racket
                                         (require unstable/options 'middle)
                                         ((exercise-option boo) "wrong!"))
                                       (require 'client))
                                      (list "client" "client"))   
                                     
                                     (test-pass
                                      "passes after void exercise"
                                      (script
                                       (require unstable/options)
                                       (define (boo x) x)
                                       (exercise-option boo)))
                                     
                                     (test-contract-fail
                                      "passes after exercise after succesful exercise"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c (-> number? number?))]))
                                         (define (boo x) x))
                                       (module client racket
                                         (require unstable/options 'server)
                                         ((exercise-option (exercise-option boo)) "error"))
                                       (require 'client))
                                      "client")
                                     
                                     (test-pass
                                      "passes after transfer after succesful exercise"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c (-> number? number?))]))
                                         (define (boo x) x))
                                       (module client racket
                                         (require unstable/options 'server)
                                         (define e-boo (exercise-option boo))
                                         (provide (contract-out [e-boo transfer/c])))
                                       (require 'client)))
                                     
                                     
                                     (test-pass
                                      "passes after two transfers and exercise (with contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c
                                                                      (-> number? number?)
                                                                      #:with-contract #t)]))
                                         (define (boo x) x))
                                       (module middle0 racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (module middle1 racket
                                         (require unstable/options 'middle0)
                                         (provide (contract-out [boo transfer/c])))
                                       (require unstable/options 'middle1)
                                       ((exercise-option boo) 1)))
                                     
                                     (test-contract-fail
                                      "positive contract failure after successful transfer and exercise (with contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c 
                                                                      (-> number? number?)
                                                                      #:with-contract #t)]))
                                         (define (boo x) "wrong!"))
                                       (module middle racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (module client racket
                                         (require unstable/options 'middle)
                                         ((exercise-option boo) 42))
                                       (require 'client))
                                      (list "middle" "server"))   
                                     
                                     (test-contract-fail
                                      "negative contract failure after successful transfer and exercise (with contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c 
                                                                      (-> number? number?)
                                                                      #:with-contract #t)]))
                                         (define (boo x) x))
                                       (module middle racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (module client racket
                                         (require unstable/options 'middle)
                                         ((exercise-option boo) "wrong!"))
                                       (require 'client))
                                      (list "client" "client"))   
                                     
                                     
                                     (test-contract-fail
                                      "passes after exercise after succesful exercise (with contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c 
                                                                      (-> number? number?)
                                                                      #:with-contract #t)]))
                                         (define (boo x) x))
                                       (module client racket
                                         (require unstable/options 'server)
                                         ((exercise-option (exercise-option boo)) "error"))
                                       (require 'client))
                                      "client")
                                     
                                     (test-pass
                                      "passes after transfer after succesful exercise (with contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c 
                                                                      (-> number? number?)
                                                                      #:with-contract #t)]))
                                         (define (boo x) x))
                                       (module client racket
                                         (require unstable/options 'server)
                                         (define e-boo (exercise-option boo))
                                         (provide (contract-out [e-boo transfer/c])))
                                       (require 'client))))
                         
                         
                         (test-suite "waive-option"
                                     
                                     (test-pass
                                      "passes after two transfers and waive"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c (-> number? number?))]))
                                         (define (boo x) x))
                                       (module middle0 racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (module middle1 racket
                                         (require unstable/options 'middle0)
                                         (provide (contract-out [boo transfer/c])))
                                       (require unstable/options 'middle1)
                                       ((waive-option boo) 1)))
                                     
                                     
                                     (test-pass
                                      "passes after waive"
                                      (script
                                       (require unstable/options)
                                       (define (boo x) x)
                                       (waive-option boo)))
                                     
                                     (test-pass
                                      "passes after waive after succesful waive"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c (-> number? number?))]))
                                         (define (boo x) x))
                                       (require unstable/options 'server)
                                       ((waive-option (waive-option boo)) "error")))
                                     
                                     (test-contract-fail
                                      "fails with waive after succesful exercise"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c (-> number? number?))]))
                                         (define (boo x) x))
                                       (module client racket
                                         (require unstable/options 'server)
                                         ((waive-option (exercise-option boo)) "error"))
                                       (require 'client))
                                      "client")
                                     
                                     (test-pass
                                      "passes with transfer after succesful waive"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c (-> number? number?))]))
                                         (define (boo x) x))
                                       (module client racket
                                         (require unstable/options 'server)
                                         (define e-boo (waive-option boo))
                                         (provide (contract-out [e-boo transfer/c])))
                                       (require 'client)))
                                     
                                     (test-pass
                                      "passes after two transfers and waive (with contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c
                                                                      (-> number? number?)
                                                                      #:with-contract #t)]))
                                         (define (boo x) x))
                                       (module middle0 racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (module middle1 racket
                                         (require unstable/options 'middle0)
                                         (provide (contract-out [boo transfer/c])))
                                       (require unstable/options 'middle1)
                                       ((waive-option boo) 1)))          
                                     
                                     
                                     (test-pass
                                      "passes after waive after succesful waive (with contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c 
                                                                      (-> number? number?)
                                                                      #:with-contract #t)]))
                                         (define (boo x) x))
                                       (require unstable/options 'server)
                                       ((waive-option (waive-option boo)) "error")))
                                     
                                     (test-contract-fail
                                      "fails with waive after succesful exercise (with contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c 
                                                                      (-> number? number?)
                                                                      #:with-contract #t)]))
                                         (define (boo x) x))
                                       (module client racket
                                         (require unstable/options 'server)
                                         ((waive-option (exercise-option boo)) "error"))
                                       (require 'client))
                                      "client")
                                     
                                     (test-pass
                                      "passes with transfer after succesful waive (with contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c 
                                                                      (-> number? number?)
                                                                      #:with-contract #t)]))
                                         (define (boo x) x))
                                       (module client racket
                                         (require unstable/options 'server)
                                         (define e-boo (waive-option boo))
                                         (provide (contract-out [e-boo transfer/c])))
                                       (require 'client))))
                         
                         (test-suite "tweak-option"
                                     
                                     (test-pass
                                      "passes after two transfers and tweak"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c (-> number? number?))]))
                                         (define (boo x) x))
                                       (module middle0 racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (module middle1 racket
                                         (require unstable/options 'middle0)
                                         (provide (contract-out [boo transfer/c])))
                                       (require unstable/options 'middle1)
                                       ((tweak-option boo) 1)))
                                     
                                     (test-contract-fail
                                      "fails (negative) after two transfers and tweak"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c (-> number? number?))]))
                                         (define (boo x) x))
                                       (module middle0 racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (module middle1 racket
                                         (require unstable/options 'middle0)
                                         (provide (contract-out [boo transfer/c])))
                                       (require unstable/options 'middle1)
                                       ((tweak-option boo) 'wrong))
                                      (list "top-level" "top-level" "top-level"))
                                     
                                     (test-contract-fail
                                      "fails (positive) after two transfers and tweak"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c (-> number? number?))]))
                                         (define (boo x) 'wrong))
                                       (module middle0 racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (module middle1 racket
                                         (require unstable/options 'middle0)
                                         (provide (contract-out [boo transfer/c])))
                                       (require unstable/options 'middle1)
                                       ((tweak-option boo) 42))
                                      (list "middle1" "middle0" "server"))
                                     
                                     (test-contract-fail 
                                      "fails (negative) two transfers and tweak"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c (-> number? number?))]))
                                         (define (boo x) x))
                                       (module middle0 racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (module middle1 racket
                                         (require unstable/options 'middle0)
                                         (provide (contract-out [boo transfer/c])))
                                       (require unstable/options 'middle1)
                                       ((tweak-option boo) 42)
                                       ((tweak-option boo) 'wrong))
                                      (list "top-level" "top-level" "top-level"))
                                     
                                     (test-pass
                                      "passes after two transfers and tweak (with-contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c 
                                                                      (-> number? number?)
                                                                      #:with-contract #t)]))
                                         (define (boo x) x))
                                       (module middle0 racket
                                         (require unstable/options 'server)
                                         (provide (contract-out [boo transfer/c])))
                                       (module middle1 racket
                                         (require unstable/options 'middle0)
                                         (provide (contract-out [boo transfer/c])))
                                       (require unstable/options 'middle1)
                                       ((tweak-option boo) 1)))
                                     
                  
                                     (test-pass
                                      "passes after tweak"
                                      (script
                                       (require unstable/options)
                                       (define (boo x) x)
                                       (tweak-option boo)))
                                     
                                     (test-pass
                                      "fails after tweak after succesful tweak"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c (-> number? number?))]))
                                         (define (boo x) x))
                                       (require unstable/options 'server)
                                       ((tweak-option (tweak-option boo)) "error")))
                                     
                                     (test-contract-fail
                                      "fails after tweak after succesful tweak (with contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c 
                                                                      (-> number? number?)
                                                                      #:with-contract #t)]))
                                         (define (boo x) x))
                                       (require unstable/options 'server)
                                       ((tweak-option (tweak-option boo)) "error"))
                                      "top-level")
                                      
                                     
                                     (test-contract-fail
                                      "fails with tweak after succesful exercise"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c (-> number? number?))]))
                                         (define (boo x) x))
                                       (module client racket
                                         (require unstable/options 'server)
                                         ((tweak-option (exercise-option boo)) "error"))
                                       (require 'client))
                                      "client")
                                     
                                     (test-contract-fail
                                      "fails with exercise after tweak exercise"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c (-> number? number?))]))
                                         (define (boo x) x))
                                       (module client racket
                                         (require unstable/options 'server)
                                         ((exercise-option (tweak-option boo)) "error"))
                                       (require 'client))
                                      "client")
                                     
                                     (test-contract-fail
                                      "fails with exercise after tweak exercise (with contract)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out [boo (option/c 
                                                                      (-> number? number?)
                                                                      #:with-contract #t)]))
                                         (define (boo x) x))
                                       (module client racket
                                         (require unstable/options 'server)
                                         ((exercise-option (tweak-option boo)) "error"))
                                       (require 'client))
                                      "client")))
             
             
             
             
             (test-suite "invariant/c"
                         
                         (test-suite "general"
                                     
                                     (test-contract-fail
                                      "failed invariant/c on invalid kind of data"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [f (invariant/c 
                                                       any/c
                                                       values)]))
                                         (define f (λ (x) x)))
                                       (require 'server))
                                      "server"
                                      "a vector or a hash")
                                     
                                     (test-contract-fail
                                      "failed invariant/c because of misbehaving invariant (indy)"
                                      (script
                                       (module ctc racket
                                         (require unstable/options)
                                         (provide  indy-ctc)
                                         (define (sorted? vec)
                                           (vector-set! vec 1 'a)
                                           (for/and ([el vec]
                                                     [cel (vector-drop vec 1)])
                                             (<= el cel)))
                                         (define indy-ctc
                                           (invariant/c (vectorof number?) sorted?)))
                                       (module server racket
                                         (require unstable/options)
                                         (require 'ctc)
                                         (provide (contract-out 
                                                   [vec indy-ctc]))
                                         (define vec (vector 1 2 3 4 5)))
                                       (require 'server))
                                      "ctc"))                   
                         
                         
                         
                         (test-suite "vectors"
                                     
                                     (test-pass 
                                      "passes with invariant for vector"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (invariant/c 
                                                         any/c
                                                         sorted?)]))
                                         (define vec (vector 1 2 3 4 5))
                                         (define (sorted? vec)
                                           (for/and ([el vec]
                                                     [cel (vector-drop vec 1)])
                                             (<= el cel))))
                                       (require 'server)))
                                     
                                     (test-contract-fail 
                                      "failed invariant for vector"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (invariant/c 
                                                         any/c
                                                         sorted?)]))
                                         (define vec (vector 1 3 2 4 5))
                                         (define (sorted? vec)
                                           (for/and ([el vec]
                                                     [cel (vector-drop vec 1)])
                                             (<= el cel))))
                                       (require 'server))
                                      "server"
                                      "expected vector that satisfies")
                                     
                                     (test-contract-fail 
                                      "failed underlying vector/c contract"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (invariant/c 
                                                         (vectorof number?)
                                                         sorted?)
                                                        ]))
                                         (define vec (vector 1 'foo 3 4 5))
                                         (define (sorted? vec)
                                           (for/and ([el vec]
                                                     [cel (vector-drop vec 1)])
                                             (<= el cel))))
                                       (require 'server))
                                      "server") 
                                     
                                     (test-contract-fail 
                                      "failed immutable invariant/c for vector"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (invariant/c 
                                                         any/c
                                                         sorted?
                                                         #:immutable #t)]))
                                         (define vec (vector 1 2 3 4 5))
                                         (define (sorted? vec)
                                           (for/and ([el vec]
                                                     [cel (vector-drop vec 1)])
                                             (<= el cel))))
                                       (require 'server))
                                      "server"
                                      "immutable data")
                                     
                                     (test-contract-fail 
                                      "failed mutable invariant/c for vector"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (invariant/c 
                                                         any/c
                                                         sorted?
                                                         #:immutable #f)]))
                                         (define vec #(1 2 3 4 5))
                                         (define (sorted? vec)
                                           (for/and ([el vec]
                                                     [cel (vector-drop vec 1)])
                                             (<= el cel))))
                                       (require 'server))
                                      "server"
                                      "mutable data")
                                     
                                     
                                     (test-pass 
                                      "passes with immutable invariant/c for vector"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (invariant/c 
                                                         any/c
                                                         sorted?
                                                         #:immutable #t)]))
                                         (define vec #(1 2 3 4 5))
                                         (define (sorted? vec)
                                           (for/and ([el vec]
                                                     [cel (vector-drop vec 1)])
                                             (<= el cel))))
                                       (require 'server)))
                                     
                                     (test-pass 
                                      "passes with mutable invariant/c for vector"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (invariant/c 
                                                         any/c
                                                         sorted?
                                                         #:immutable #f)]))
                                         (define vec (vector 1 2 3 4 5))
                                         (define (sorted? vec)
                                           (for/and ([el vec]
                                                     [cel (vector-drop vec 1)])
                                             (<= el cel))))
                                       (require 'server)))
                                     
                                     (test-pass 
                                      "passes with mutable flat invariant/c for vector"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (invariant/c 
                                                         any/c
                                                         sorted?
                                                         #:immutable #f
                                                         #:flat? #t)]))
                                         (define vec (vector 1 2 3 4 5))
                                         (define (sorted? vec)
                                           (for/and ([el vec]
                                                     [cel (vector-drop vec 1)])
                                             (<= el cel))))
                                       (require 'server)))
                                     
                                     (test-pass 
                                      "passes with mutation on mutable flat invariant/c for vector"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (invariant/c 
                                                         any/c
                                                         sorted?
                                                         #:immutable #f
                                                         #:flat? #t)]))
                                         (define vec (vector 1 2 3 4 5))
                                         (define (sorted? vec)
                                           (for/and ([el vec]
                                                     [cel (vector-drop vec 1)])
                                             (<= el cel))))
                                       (module client racket
                                         (require unstable/options)
                                         (require 'server)
                                         (vector-set! vec 1 10))
                                       (require 'client)))
                                     
                                     (test-contract-fail 
                                      "failed mutation on mutable invariant/c for vector"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [vec (invariant/c 
                                                         any/c
                                                         sorted?
                                                         #:immutable #f
                                                         #:flat? #f)]))
                                         (define vec (vector 1 2 3 4 5))
                                         (define (sorted? vec)
                                           (for/and ([el vec]
                                                     [cel (vector-drop vec 1)])
                                             (<= el cel))))
                                       (module client racket
                                         (require unstable/options)
                                         (require 'server)
                                         (vector-set! vec 1 10))
                                       (require 'client))
                                      "client"
                                      "expected vector that satisfies")
                                     
                                     (test-contract-fail 
                                      "failed read after callback mutation on mutable invariant/c for vector"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide
                                          change
                                          (contract-out 
                                           [vec (invariant/c 
                                                 any/c
                                                 sorted?
                                                 #:immutable #f
                                                 #:flat? #f)]))
                                         (define vec (vector 1 2 3 4 5))
                                         (define (change) (vector-set! vec 1 10))
                                         (define (sorted? vec)
                                           (for/and ([el vec]
                                                     [cel (vector-drop vec 1)])
                                             (<= el cel))))
                                       (module client racket
                                         (require unstable/options)
                                         (require 'server)
                                         (change)
                                         (vector-ref vec 2))
                                       (require 'client))
                                      "server"
                                      "expected vector that satisfies"))
                         
                         
                         (test-suite "hashes"
                                     
                                     (test-pass 
                                      "passes with invariant for hash"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [h (invariant/c 
                                                       any/c
                                                       a-is-one)]))
                                         (define h (hash 'a 1 'b 2 'c 3 'd 4 'd 5))
                                         (define (a-is-one h)
                                           (= (hash-ref h 'a) 1)))
                                       (require 'server)))
                                     
                                     (test-contract-fail 
                                      "failed invariant for hash"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [h (invariant/c 
                                                       any/c
                                                       a-is-one)]))
                                         (define h (hash 'a 2 'b 2 'c 3 'd 4 'd 5))
                                         (define (a-is-one h)
                                           (= (hash-ref h 'a) 1)))
                                       (require 'server))
                                      "server"
                                      "expected hash that satisfies")
                                     
                                     (test-contract-fail 
                                      "failed underlying hash/c contract"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [h (invariant/c 
                                                       (hash/c symbol? number?)
                                                       a-is-one)]))
                                         (define h (hash 'a 2 'b 'b 'c 3 'd 4 'd 5))
                                         (define (a-is-one h)
                                           (= (hash-ref h 'a) 1)))
                                       (require 'server))
                                      "server") 
                                     
                                     (test-contract-fail 
                                      "failed immutable invariant/c for hash"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [h (invariant/c 
                                                       (hash/c symbol? number?)
                                                       a-is-one
                                                       #:immutable #t)]))
                                         (define h (make-hash (list (cons 'a 2) (cons 'b 2))))
                                         (define (a-is-one h)
                                           (= (hash-ref h 'a) 1)))
                                       (require 'server))
                                      "server"
                                      "immutable data")
                                     
                                     (test-contract-fail 
                                      "failed mutable invariant/c for hash"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [h (invariant/c 
                                                       (hash/c symbol? number?)
                                                       a-is-one
                                                       #:immutable #f)]))
                                         (define h (hash  'a 2 'b 2))
                                         (define (a-is-one h)
                                           (= (hash-ref h 'a) 1)))
                                       (require 'server))
                                      "server"
                                      "mutable data")
                                     
                                     (test-pass 
                                      "passes with immutable invariant/c for hash"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [h (invariant/c 
                                                       (hash/c symbol? number?)
                                                       a-is-one
                                                       #:immutable #t)]))
                                         (define h (hash  'a 1 'b 2))
                                         (define (a-is-one h)
                                           (= (hash-ref h 'a) 1)))
                                       (require 'server)))
                                     
                                     (test-pass 
                                      "passes with mutable invariant/c for hash"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [h (invariant/c 
                                                       (hash/c symbol? number?)
                                                       a-is-one
                                                       #:immutable #f)]))
                                         (define h (make-hash  (list (cons 'a 1) (cons 'b 2))))
                                         (define (a-is-one h)
                                           (= (hash-ref h 'a) 1)))
                                       (require 'server))) 
                                     
                                     
                                     (test-pass 
                                      "passes with mutable flat invariant/c for hash"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [h (invariant/c 
                                                       (hash/c symbol? number?)
                                                       a-is-one
                                                       #:flat? #t
                                                       #:immutable #f)]))
                                         (define h (make-hash  (list (cons 'a 1) (cons 'b 2))))
                                         (define (a-is-one h)
                                           (= (hash-ref h 'a) 1)))
                                       (require 'server))) 
                                     
                                     (test-pass 
                                      "passes with mutation on mutable flat invariant/c for hash"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [h (invariant/c 
                                                       (hash/c symbol? number?)
                                                       a-is-one
                                                       #:flat? #t
                                                       #:immutable #f)]))
                                         (define h (make-hash  (list (cons 'a 1) (cons 'b 2))))
                                         (define (a-is-one h)
                                           (= (hash-ref h 'a) 1)))
                                       (module client racket
                                         (require unstable/options)
                                         (require 'server)
                                         (hash-set! h 'a 10))
                                       (require 'client))) 
                                     
                                     
                                     (test-contract-fail 
                                      "failed mutation on mutable invariant/c for hash"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [h (invariant/c 
                                                       (hash/c symbol? number?)
                                                       a-is-one
                                                       #:flat? #f
                                                       #:immutable #f)]))
                                         (define h (make-hash  (list (cons 'a 1) (cons 'b 2))))
                                         (define (a-is-one h)
                                           (= (hash-ref h 'a) 1)))
                                       (module client racket
                                         (require unstable/options)
                                         (require 'server)
                                         (hash-set! h 'a 10))
                                       (require 'client))
                                      "client"
                                      "expected hash that satisfies")
                                     
                                     (test-contract-fail 
                                      "failed read after callback mutation on mutable invariant/c for hash"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide
                                          change
                                          (contract-out 
                                           [h (invariant/c 
                                               (hash/c symbol? number?)
                                               a-is-one
                                               #:flat? #f
                                               #:immutable #f)]))
                                         (define h (make-hash  (list (cons 'a 1) (cons 'b 2))))
                                         (define (change) (hash-set! h 'a 2))
                                         (define (a-is-one h)
                                           (= (hash-ref h 'a) 1)))
                                       (module client racket
                                         (require unstable/options)
                                         (require 'server)
                                         (change)
                                         (hash-ref h 'b))
                                       (require 'client))
                                      "server"
                                      "expected hash that satisfies"))
                         
                         (test-suite "structs"
                                     
                                     (test-pass 
                                      "passes with invariant for struct"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide 
                                          (contract-out 
                                           [s (invariant/c 
                                               any/c
                                               a-is-one
                                               #:struct foo)]))
                                         (struct foo (a b))
                                         (define s (foo 1 2))
                                         (define (a-is-one s)
                                           (= (foo-a s) 1)))
                                       (require 'server)))
                                     
                                     (test-contract-fail 
                                      "failed invariant for struct"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [s (invariant/c 
                                                       any/c
                                                       a-is-one
                                                       #:struct foo)]))
                                         (struct foo (a b))
                                         (define s (foo 2 2))
                                         (define (a-is-one s)
                                           (= (foo-a s) 1)))
                                       (require 'server))
                                      "server"
                                      "expected struct that satisfies")
                                     
                                     
                                     (test-fail 
                                      "failed invariant/c for struct (unbound struct id)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [s (invariant/c 
                                                       any/c
                                                       a-is-one
                                                       #:struct boo)]))
                                         (struct foo (a b))
                                         (define s (foo 2 2))
                                         (define (a-is-one s)
                                           (= (foo-a s) 1)))
                                       (require 'server))
                                      "expected a struct identifier") 
                                     
                                     (test-contract-fail 
                                      "failed invariant/c for struct (missing struct id)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [s (invariant/c 
                                                       any/c
                                                       a-is-one)]))
                                         (struct foo (a b))
                                         (struct boo (a b))
                                         (define s (foo 2 2))
                                         (define (a-is-one s)
                                           (= (foo-a s) 1)))
                                       (require 'server))
                                      "server"
                                      "a vector or a hash") 
                                     
                                     (test-contract-fail 
                                      "failed invariant/c for struct (wrong struct id)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [s (invariant/c 
                                                       any/c
                                                       a-is-one
                                                       #:struct boo)]))
                                         (struct foo (a b))
                                         (struct boo (a b))
                                         (define s (foo 2 2))
                                         (define (a-is-one s)
                                           (= (foo-a s) 1)))
                                       (require 'server))
                                      "server"
                                      "a struct of type boo")  
                                     
                                     (test-contract-fail 
                                      "failed invariant/c for struct (vector for struct id)"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [s (invariant/c 
                                                       any/c
                                                       a-is-one
                                                       #:struct foo)]))
                                         (struct foo (a b))
                                         (define s (vector 2 2))
                                         (define (a-is-one s)
                                           (= (foo-a s) 1)))
                                       (require 'server))
                                      "server"
                                      "a struct of type foo") 
                                     
                                     
                                     (test-contract-fail 
                                      "failed underlying struct/dc contract"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [s (invariant/c 
                                                       (struct/dc foo [a number?] [b number?])
                                                       a-is-one
                                                       #:struct foo)]))
                                         (struct foo (a b))
                                         (define s (foo '3 2))
                                         (define (a-is-one s)
                                           (= (foo-a s) 1)))
                                       (require 'server))
                                      "server") 
                                     
                                     (test-contract-fail 
                                      "failed immutable invariant/c for struct"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [s (invariant/c 
                                                       (struct/dc foo [a number?] [b number?])
                                                       a-is-one
                                                       #:immutable #t
                                                       #:struct foo)]))
                                         (struct foo (a b) #:mutable)
                                         (define s (foo '3 2))
                                         (define (a-is-one s)
                                           (= (foo-a s) 1)))
                                       (require 'server))
                                      "server"
                                      "immutable data") 
                                     
                                     (test-contract-fail 
                                      "failed mutable invariant/c for struct"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [s (invariant/c 
                                                       (struct/dc foo [a number?] [b number?])
                                                       a-is-one
                                                       #:immutable #f
                                                       #:struct foo)]))
                                         (struct foo (a b))
                                         (define s (foo '3 2))
                                         (define (a-is-one s)
                                           (= (foo-a s) 1)))
                                       (require 'server))
                                      "server"
                                      "mutable data") 
                                     
                                     (test-pass 
                                      "passes with immutable invariant/c for struct"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [s (invariant/c 
                                                       (struct/dc foo [a number?] [b number?])
                                                       a-is-one
                                                       #:immutable #t
                                                       #:struct foo)]))
                                         (struct foo (a b))
                                         (define s (foo 1 2))
                                         (define (a-is-one s)
                                           (= (foo-a s) 1)))
                                       (require 'server))) 
                                     
                                     (test-pass 
                                      "passes with mutable invariant/c for struct"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [s (invariant/c 
                                                       (struct/dc foo [a number?] [b number?])
                                                       a-is-one
                                                       #:immutable #f
                                                       #:struct foo)]))
                                         (struct foo (a b) #:mutable)
                                         (define s (foo 1 2))
                                         (define (a-is-one s)
                                           (= (foo-a s) 1)))
                                       (require 'server))) 
                                     
                                     (test-pass 
                                      "passes with mutable flat invariant/c for struct"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide (contract-out 
                                                   [s (invariant/c 
                                                       (struct/dc foo [a number?] [b number?])
                                                       a-is-one
                                                       #:flat? #t
                                                       #:immutable #f
                                                       #:struct foo)]))
                                         (struct foo (a b) #:mutable)
                                         (define s (foo 1 2))
                                         (define (a-is-one s)
                                           (= (foo-a s) 1)))
                                       (require 'server)))                                                                        
                                     
                                     (test-pass 
                                      "passes with mutation on mutable flat invariant/c for struct"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide 
                                          set-foo-a!
                                          (contract-out 
                                           [s (invariant/c 
                                               (struct/dc foo [a number?] [b number?])
                                               a-is-one
                                               #:flat? #t
                                               #:immutable #f
                                               #:struct foo)]))
                                         (struct foo (a b) #:mutable)
                                         (define s (foo 1 2))
                                         (define (a-is-one s)
                                           (= (foo-a s) 1)))
                                       (module client racket
                                         (require unstable/options)
                                         (require 'server)
                                         (set-foo-a! s 10))
                                       (require 'client)))                                              
                                     
                                     
                                     (test-contract-fail 
                                      "failed mutation on mutable invariant/c for struct"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide 
                                          set-foo-a!
                                          foo-b
                                          (contract-out 
                                           [s (invariant/c 
                                               (struct/dc foo [a (-> any/c number?)] [b number?])
                                               a-is-one
                                               #:immutable #f
                                               #:struct foo)]))
                                         (struct foo (a b) #:mutable)
                                         (define s (foo (λ (x) 1) 2))
                                         (define (a-is-one s)
                                           (= ((foo-a s) 'anything) 1)))
                                       (module client racket
                                         (require unstable/options)
                                         (require 'server)
                                         (set-foo-a! s (λ (x) 2))
                                         (foo-b s))
                                       (require 'client))    
                                      "client"
                                      "expected struct that satisfies")
                                     
                                     (test-contract-fail 
                                      "failed mutation on mutable invariant/c for struct"
                                      (script
                                       (module server racket
                                         (require unstable/options)
                                         (provide 
                                          change
                                          foo-b
                                          (contract-out 
                                           [s (invariant/c 
                                               (struct/dc foo [a (-> any/c number?)] [b number?])
                                               a-is-one
                                               #:immutable #f
                                               #:struct foo)]))
                                         (struct foo (a b) #:mutable)
                                         (define s (foo (λ (x) 1) 2))
                                         (define (change) (set-foo-a! s (λ (x) 2))) 
                                         (define (a-is-one s)
                                           (= ((foo-a s) 'anything) 1)))
                                       (module client racket
                                         (require unstable/options)
                                         (require 'server)
                                         (change)
                                         (foo-b s))
                                       (require 'client))    
                                      "server"
                                      "expected struct that satisfies")))))


















