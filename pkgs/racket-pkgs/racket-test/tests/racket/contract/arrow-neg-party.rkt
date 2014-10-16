#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace (make-basic-contract-namespace
                                            'racket/contract/private/prop
                                            'racket/contract/private/guts
                                            'racket/contract/private/blame
                                            'racket/contract/private/arrow-val-first
                                            'racket/contract/private/arity-checking)])
  (contract-eval
   '(define (neg-party-fn c val)
      (define blame (make-blame (srcloc #f #f #f #f #f)
                                'a-name
                                (λ () (contract-name c))
                                'pos
                                #f #t))
      (wrapped-extra-arg-arrow-extra-neg-party-argument
       (((contract-struct-val-first-projection c) blame) val))))
  
  (test/spec-passed/result 
   'arity-as-string1
   '(arity-as-string (let ([f (λ (x) x)]) f))
   "f accepts: 1 argument")
  (test/spec-passed/result 
   'arity-as-string2
   '(raw-arity-as-string (λ x x))
   "0 or arbitrarily many more arguments")
  (test/spec-passed/result 
   'arity-as-string3
   '(raw-arity-as-string (case-lambda [() 1] [(x) x]))
   "0 or 1 arguments")
  (test/spec-passed/result 
   'arity-as-string4
   '(raw-arity-as-string (case-lambda [() 1] [(x) x] [(x y) x]))
   "0, 1, or 2 arguments")
  (test/spec-passed/result 
   'arity-as-string5
   '(raw-arity-as-string (case-lambda [() 1] [(x y . z) x]))
   "0, 2, or arbitrarily many more arguments")
  (test/spec-passed/result 
   'arity-as-string6
   '(raw-arity-as-string (case-lambda [() 1] [(x) x] [(x y q r . w) x]))
   "0, 1, 4, or arbitrarily many more arguments")
  (test/spec-passed/result 
   'arity-as-string7
   '(raw-arity-as-string (λ (x y #:z [z 1] #:w w) 1))
   "2 normal arguments, the mandatory keyword #:w, and the optional keyword #:z")
  (test/spec-passed/result 
   'arity-as-string8
   '(raw-arity-as-string (λ (x y #:z1 [z1 1] #:z2 [z2 2] #:z3 [z3 3] #:w1 w #:w2 w2 #:w3 w3) 1))
   (string-append "2 normal arguments, the mandatory keywords #:w1,"
                  " #:w2, and #:w3, and the optional keywords #:z1, #:z2, and #:z3"))
  (test/spec-passed/result 
   'arity-as-string7
   '(raw-arity-as-string (λ (x y #:z [z 1] #:w w . q) 1))
   (string-append "2 or arbitrarily many more normal arguments,"
                  " the mandatory keyword #:w, and the optional keyword #:z"))
  
  (test/spec-passed
   '->neg-party1
   '((neg-party-fn
      (-> integer? integer?)
      (λ (x) x))
     'neg 1))
  
  (test/neg-blame
   '->neg-party2
   '((neg-party-fn
      (-> integer? integer?)
      (λ (x) x))
     'neg #f))
  
  (test/pos-blame
   '->neg-party3
   '((neg-party-fn
      (-> integer? integer?)
      (λ (x) #f))
     'neg 1))
  
  (test/pos-blame
   '->neg-party4
   '((neg-party-fn
      (-> #:x integer? integer?)
      (λ (#:x x) #f))
     'neg #:x 1))
  
  (test/neg-blame
   '->neg-party5
   '((neg-party-fn
      (-> #:x integer? integer?)
      (λ (#:x x) #f))
     'neg #:x #f))
  
  (test/pos-blame
   '->neg-party6
   '((neg-party-fn
      (-> integer? integer?)
      (λ (x) (values x x)))
     'neg 1))
  
  (test/spec-passed
   '->*neg-party1
   '((neg-party-fn
      (->* (integer?) integer?)
      (λ (x) x))
     'neg 1))
  
  (test/neg-blame
   '->*neg-party2
   '((neg-party-fn
      (->* (integer?) integer?)
      (λ (x) x))
     'neg #f))
  
  (test/neg-blame
   '->*neg-party3
   '((neg-party-fn
      (->* (integer?) (integer?) any)
      (λ (x [y #f]) y))
     'neg 1 #f))
  
  (test/neg-blame
   '->*neg-party4
   '((neg-party-fn
      (->* (integer?) (#:x integer?) any)
      (λ (x #:x [y #f]) y))
     'neg 1 #:x #f))
  
  (test/neg-blame
   '->*neg-party5
   '((neg-party-fn
      (->* (integer?) #:pre #f any)
      (λ (x) y))
     'neg 1))
  
  (test/pos-blame
   '->*neg-party6
   '((neg-party-fn
      (->* (integer?) integer? #:post #f)
      (λ (x) 1))
     'neg 1))
  
  (test/spec-passed
   '->*neg-party7
   '((neg-party-fn
      (->* (integer?) #:rest (listof integer?) integer?)
      (λ (x . y) 1))
     'neg 1 2 3 4))
  
  (test/neg-blame
   '->*neg-party8
   '((neg-party-fn
      (->* (integer?) #:rest (listof integer?) integer?)
      (λ (x . y) 1))
     'neg 1 2 3 'four))
  
  (test/spec-passed
   '->*neg-party9
   '((neg-party-fn
      (->* () (boolean? char? integer?) any)
      (λ args 1))
     'neg #f #\f #xf))
  
  (test/spec-passed
   '->*neg-party10
   '((neg-party-fn
      (->* (#:i integer? #:b boolean?) (#:c (listof char?) #:r regexp?) any)
      (λ (#:i i #:b b #:c [c '(#\a)] #:r [r #rx"x"]) 1))
     'neg #:i 1 #:b #t))
  
  (test/neg-blame
   '->*neg-party11
   '((neg-party-fn
      (->* (#:i integer? #:b boolean?) (#:c char? #:r regexp?) any)
      (λ (#:i i #:b b #:c [c #\a] #:r [r #rx"x"]) 1))
     'neg #:i 1 #:b #t #:zzz 'whatever))
  
  (test/neg-blame
   '->*neg-party12
   '((neg-party-fn
      (->* (#:i integer? #:b boolean?) (#:c char? #:r regexp?) any)
      (λ (#:i i #:b b #:c [c #\a] #:r [r #rx"x"]) 1))
     'neg #:i 1 #:b #t #:a 'whatever))
  
  (test/neg-blame
   '->*neg-party13
   '((neg-party-fn
      (->* (#:i integer? #:b boolean?) (#:c char? #:r regexp?) any)
      (λ (#:i i #:b b #:c [c #\a] #:r [r #rx"x"]) 1))
     'neg #:i 1))
  
  (test/neg-blame
   '->*neg-party14
   '((neg-party-fn
      (->* (#:i integer? #:b boolean?) (#:c char? #:r regexp?) any)
      (λ (#:i i #:b b #:c [c #\a] #:r [r #rx"x"]) 1))
     'neg #:b #t))
  
  (test/spec-passed
   '->*neg-party15
   '((neg-party-fn
      (->* () (#:c char? #:i integer? #:r regexp?) any)
      (λ (#:c [c #\a] #:i [i 1] #:r [r #rx"x"]) 1))
     'neg #:i 1))
  
  (test/spec-passed
   '->*neg-party16
   '((neg-party-fn 
      (->* [(and/c hash? immutable?)]
           [#:combine
            (-> any/c any/c any/c)]
           #:rest (listof hash?)
           (and/c hash? immutable?))
      (λ (h #:combine [x void] #:combine/key [y void] . rest)
        (hash)))
     'neg (hash) (hash)))
  
  (test/neg-blame
   '->*neg-party17
   '((neg-party-fn 
      (->* [(and/c hash? immutable?)]
           [#:combine
            (-> any/c any/c any/c)]
           #:rest (listof hash?)
           (and/c hash? immutable?))
      (λ (h #:combine [x void] #:combine/key [y void] . rest)
        (hash)))
     'neg (hash) 11 12))
  
  (test/spec-passed/result
   '->*neg-party18
   '((neg-party-fn 
      (->* (#:user string?)
           (#:database (or/c string? #f)
                       #:password (or/c string? (list/c 'hash string?) #f)
                       #:port (or/c exact-positive-integer? #f))
           any/c)
      (λ (#:user user
                 #:database [db #f]
                 #:password [password #f]
                 #:port [port #f])
        (list user db password port)))
     'neg #:database "db" #:password "password" #:user "user")
   (list "user" "db" "password" #f)))
