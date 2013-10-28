#lang racket
(require racklog
         tests/eli-tester)

(test
 ; Unification tests
 (test 
  #:failure-prefix "unify"
  (local [(define-syntax-rule (once t1 t2)
            (test (%which () (%= t1 t2)) => empty
                  (%more) => #f
                  (let ([tmp t1]) (%which () (%== tmp tmp))) => empty
                  (%more) => #f
                  (%which () (%let (f s) (%and (%freeze t1 f) (%melt f s) (%= s t2)))) => empty
                  (%more) => #f
                  (%which () (%let (f s) (%and (%freeze t1 f) (%melt-new f s) (%= s t2)))) => empty
                  (%more) => #f
                  (%which () (%let (s) (%and (%copy t1 s) (%= s t2)))) => empty
                  (%more) => #f))
          (define-syntax-rule (constant t1)
            (test (%which () (%constant t1)) => empty
                  (%more) => #f
                  (%which () (%compound t1)) => #f))
          (define-syntax-rule (compound t1)
            (test (%which () (%compound t1)) => empty
                  (%more) => #f
                  (%which () (%constant t1)) => #f))
          (define-syntax-rule (never t1 t2)
            (test (%which () (%= t1 t2)) => #f
                  (%which () (%== t1 t2)) => #f
                  (%which () (%let (f s) (%and (%freeze t1 f) (%melt f s) (%= s t2)))) => #f
                  (%which () (%let (f s) (%and (%freeze t1 f) (%melt-new f s) (%= s t2)))) => #f
                  (%which () (%let (s) (%and (%copy t1 s) (%= s t2)))) => #f))]
    (once #t #t)
    (once #f #f)
    (never #f #t)
    (never #t #f)
    (constant #t)
    (constant #f)
    
    (once 1 1)
    (never 1 2)
    (constant 1)
    
    (once "foo" "foo")
    (never "foo" "bar")
    (constant "foo")
    
    (once #"foo" #"foo")
    (never #"foo" #"bar")
    (constant #"foo")
    
    (once #\a #\a)
    (never #\a #\b)
    (constant #\a)
    
    (once 'a 'a)
    (never 'a 'b)
    (constant 'a)
    
    (let ([rx #rx"a"]) (once rx rx))
    (never #rx"a" #rx"b")
    (let ([rx #px"a"]) (once rx rx))
    (never #px"a" #rx"b")
    (let ([rx #rx#"a"]) (once rx rx))
    (never #rx#"a" #rx#"b")
    (let ([rx #px#"a"]) (once rx rx))
    (never #px#"a" #px#"b")
    
    (constant #rx"a")
    (constant #px"a")
    (constant #rx#"a")
    (constant #px#"a")
    
    (let ([a (string->keyword "a")]
          [b (string->keyword "b")])
      (once a a)
      (never a b)
      (constant a))
    
    (once empty empty)
    (never empty #f)
    (constant empty)
    
    (let ([a add1]
          [b sub1])
      (once a a)
      (never a b)
      (constant a))
    
    (once (void) (void))
    (never (void) #f)
    (constant (void))
    
    (once (cons 1 1) (cons 1 1))
    (never (cons 1 2) (cons 1 1))
    (never (cons 2 1) (cons 1 1))
    (compound (cons 1 1))
    
    (once (vector 1 1) (vector 1 1))
    (never (vector 1) (vector 1 1))
    (never (vector 1 2) (vector 1 1))
    (never (vector 2 1) (vector 1 1))
    (compound (vector 1 1))
    
    (once (mcons 1 1) (mcons 1 1))
    (never (mcons 1 2) (mcons 1 1))
    (never (mcons 2 1) (mcons 1 1))
    (compound (mcons 1 1))
    
    (once (box 1) (box 1))
    (never (box 2) (box 1))
    (compound (box 1))
    
    (local [(define-syntax-rule (hash-test hash)
              (test
               (once (hash 1 2) (hash 1 2))
               (never (hash 2 2) (hash 1 2))
               (never (hash 1 1) (hash 1 2))
               (never (hash 1 1) (hash 2 2))
               (compound (hash 1 2))))]
      (hash-test (λ (k v) (make-hash (list (cons k v)))))
      (hash-test (λ (k v) (make-hasheq (list (cons k v)))))
      (hash-test (λ (k v) (make-hasheqv (list (cons k v)))))
      (hash-test (λ (k v) (make-weak-hash (list (cons k v)))))
      (hash-test (λ (k v) (make-weak-hasheq (list (cons k v)))))
      (hash-test (λ (k v) (make-weak-hasheqv (list (cons k v)))))
      (hash-test (λ (k v) (make-immutable-hash (list (cons k v)))))
      (hash-test (λ (k v) (make-immutable-hasheq (list (cons k v)))))
      (hash-test (λ (k v) (make-immutable-hasheqv (list (cons k v))))))
    
    (local [(define-syntax-rule (set-test set)
              (test
               (once (set 1 2) (set 1 2))
               (never (set 2 2) (set 1 2))
               (never (set 1 1) (set 1 2))
               (never (set 1 1) (set 2 2))
               (constant (set 1 2))))]
      (set-test set)
      (set-test seteqv)
      (set-test seteq))
    
    (local [(define (cs-struct-test make-foo)
              (once (make-foo 1 1) (make-foo 1 1))
              (never (make-foo 2 1) (make-foo 1 1))
              (never (make-foo 1 2) (make-foo 1 1))
              (never (make-foo 2 2) (make-foo 1 1))
              (compound (make-foo 1 1)))
            (define (a-struct-test make-foo)
              (never (make-foo 1 1) (make-foo 1 1))
              (never (make-foo 2 1) (make-foo 1 1))
              (never (make-foo 1 2) (make-foo 1 1))
              (never (make-foo 2 2) (make-foo 1 1))
              (constant (make-foo 1 1)))]
      (local [(define-struct foo (x y))]
        (test #:failure-prefix "default"
              (a-struct-test make-foo)))
      (local [(define-struct foo (x y) #:prefab)]
        (test #:failure-prefix "prefab"
              (cs-struct-test make-foo)))
      (local [(define-struct foo (x y) #:transparent)] 
        (test #:failure-prefix "transparent"
              (cs-struct-test make-foo)))
      (local [(define another-inspector (make-inspector))
              (define-struct foo (x y) #:inspector another-inspector)]
        (test #:failure-prefix "child"
              (cs-struct-test make-foo)))
      (local [(define some-inspector (make-sibling-inspector))
              (define another-inspector (make-inspector some-inspector))
              (define-struct foo (x y) #:inspector another-inspector)]
        (test #:failure-prefix "sibling -> child"
              (a-struct-test make-foo))))))
 
 (test
  #:failure-prefix "unify + logic var"
  (local [(define-syntax-rule (uni x v has-x has-v)
            (test (%which (x) (%= has-x has-v)) => (list (cons 'x v))
                  (%more) => #f
                  (%which () (%let (x) (%var has-x))) => empty
                  (%more) => #f
                  (%which () (%var has-v)) => #f))
          (define-syntax-rule (iduni ve)
            (let ([v ve])
              (uni x v x v)))
          (define-syntax-rule (nouni x v has-x has-v)
            (test (%which (x) (%= has-x has-v)) => #f
                  (%which () (%let (x) (%var has-x))) => #f
                  (%which () (%var has-v)) => #f))]

    (iduni #t)
    (iduni #f)
    (iduni 1)
    (iduni "foo")
    (iduni #"foo")
    (iduni #\a)
    (iduni 'a)    
    (iduni #rx"a")
    (iduni #px"a")
    (iduni #rx#"a")
    (iduni #px#"a")
      (iduni (string->keyword "a"))
    (iduni empty)
    (iduni add1)
    (iduni (void))
    
    (uni x 1 (cons x 2) (cons 1 2))
    (uni x 2 (cons 1 x) (cons 1 2))
    
    (uni x 1 (vector x 2) (vector 1 2))
    (uni x 2 (vector 1 x) (vector 1 2))
    
    (uni x 1 (mcons x 2) (mcons 1 2))
    (uni x 2 (mcons 1 x) (mcons 1 2))
    
    (uni x 1 (box x) (box 1))
    
    (local [(define-syntax-rule (hash-test hash)
              (test
               ; Logic variables not allowed as hash keys
               (nouni x 1 (hash x 2) (hash 1 2))
               (uni x 2 (hash 1 x) (hash 1 2))))]
      (hash-test (λ (k v) (make-hash (list (cons k v)))))
      (hash-test (λ (k v) (make-hasheq (list (cons k v)))))
      (hash-test (λ (k v) (make-hasheqv (list (cons k v)))))
      (hash-test (λ (k v) (make-weak-hash (list (cons k v)))))
      (hash-test (λ (k v) (make-weak-hasheq (list (cons k v)))))
      (hash-test (λ (k v) (make-weak-hasheqv (list (cons k v)))))
      (hash-test (λ (k v) (make-immutable-hash (list (cons k v)))))
      (hash-test (λ (k v) (make-immutable-hasheq (list (cons k v)))))
      (hash-test (λ (k v) (make-immutable-hasheqv (list (cons k v))))))
    
    (nouni x 1 (set x) (set 1))
    
    (local [(define (cs-struct-test make-foo)
              (test (uni x 1 (make-foo x 2) (make-foo 1 2))
                    (uni x 2 (make-foo 1 x) (make-foo 1 2))))
            (define (a-struct-test make-foo)
              (test (nouni x 1 (make-foo x 2) (make-foo 1 2))
                    (nouni x 2 (make-foo 1 x) (make-foo 1 2))))]
      (local [(define-struct foo (x y))]
        (test #:failure-prefix "default"
              (a-struct-test make-foo)))
      (local [(define-struct foo (x y) #:prefab)]
        (test #:failure-prefix "prefab"
              (cs-struct-test make-foo)))
      (local [(define-struct foo (x y) #:transparent)] 
        (test #:failure-prefix "transparent"
              (cs-struct-test make-foo)))
      (local [(define another-inspector (make-inspector))
              (define-struct foo (x y) #:inspector another-inspector)]
        (test #:failure-prefix "child"
              (cs-struct-test make-foo)))
      (local [(define some-inspector (make-sibling-inspector))
              (define another-inspector (make-inspector some-inspector))
              (define-struct foo (x y) #:inspector another-inspector)]
        (test #:failure-prefix "sibling -> child"
              (a-struct-test make-foo))))))
 
 ; Unit tests
 (logic-var? (_))
 
 (%which () (%/= 1 1)) => #f
 (%which () (%/= 1 2)) => empty
 (%more) => #f
 
 (%which (x) (%/== x x)) => #f
 (%which (x y) (%/== x y)) => `((x . _) (y . _))
 (%more) => #f
 
 (%which () (%/== 1 1)) => #f
 (%which () (%/== 1 2)) => empty
 (%more) => #f
 
 (%which () (%< 1 2)) => empty
 (%more) => #f
 (%which () (%< 1 1)) => #f
 (%which () (%< 2 1)) => #f
 (%which () (%< 'a 2)) => #f
 (%which () (%< 1 'b)) => #f
 (%which () (%< 'a 'b)) => #f
 
 (%which () (%<= 1 2)) => empty
 (%more) => #f
 (%which () (%<= 1 1)) => empty
 (%more) => #f
 (%which () (%<= 2 1)) => #f
 (%which () (%<= 'a 2)) => #f
 (%which () (%<= 1 'b)) => #f
 (%which () (%<= 'a 'b)) => #f
 
 (%which () (%= 1 1)) => empty
 (%more) => #f
 (%which () (%= 'a 'a)) => empty
 (%more) => #f
 (%which () (%= (cons 1 2) (cons 1 2))) => empty
 (%more) => #f
 (%which () (%= (vector 1 2) (vector 1 2))) => empty
 (%more) => #f
 (%which (x) (%= x 1)) => `((x . 1))
 (%more) => #f
 (%which (x) (%= (cons x 2) (cons 1 2))) => `((x . 1))
 (%more) => #f
 (%which (x) (%= (vector x 2) (vector 1 2))) => `((x . 1))
 (%more) => #f
 (%which (x) (%and (%= x 1) (%= x 2))) => #f
 (%which () (%= 1 2)) => #f
 
 (%which () (%=/= 1 1)) => #f
 (%which () (%=/= 'a 'a)) => #f
 (%which () (%=/= 1 2)) => empty
 (%more) => #f
 
 (%which () (%=:= 1 1)) => empty
 (%more) => #f
 (%which () (%=:= 'a 'a)) => #f
 (%which () (%=:= 1 2)) => #f
 
 (%which () (%== 1 1)) => empty
 (%more) => #f
 (%which (x) (%== x x)) => `((x . _))
 (%more) => #f
 (%which (x) (%== (cons 1 x) (cons 1 x))) => `((x . _))
 (%more) => #f
 ; XXX This answer seems wrong
 (%which (x) (%and (%= x 1) (%== x 1))) => `((x . 1))
 (%more) => #f
 ; XXX This answer seems wrong
 (%which (x y) (%and (%= x 1) (%= y 1) (%== x y))) => `((x . 1) (y . 1))
 (%more) => #f
 (%which (x y) (%== x y)) => #f
 (%which (x y) (%== (cons 1 x) (cons y 2))) => #f
 
 (%which () (%> 2 1)) => empty
 (%more) => #f
 (%which () (%> 1 1)) => #f
 (%which () (%> 1 2)) => #f
 (%which () (%> 'a 2)) => #f
 (%which () (%> 1 'b)) => #f
 (%which () (%> 'a 'b)) => #f
 
 (%which () (%>= 2 1)) => empty
 (%more) => #f
 (%which () (%>= 1 1)) => empty
 (%more) => #f
 (%which () (%>= 1 2)) => #f
 (%which () (%>= 'a 2)) => #f
 (%which () (%>= 1 'b)) => #f
 (%which () (%>= 'a 'b)) => #f 
 
 (%which () (%and %true %true)) => empty
 (%more) => #f
 (%which () (%and %fail %true)) => #f
 (%more) => #f
 
 (%which () (%append empty empty empty)) => empty
 (%more) => #f
 (%which () (%append (list 1) empty (list 1))) => empty
 (%more) => #f
 (%which () (%append empty (list 2) (list 2))) => empty
 (%more) => #f
 (%which () (%append (list 1) (list 2) (list 1 2))) => empty
 (%more) => #f
 
 (let ([rel %empty-rel])
   (test (%which (y) (rel 'x y)) => #f
         (%assert! rel () [('x 1)])
         (%which (y) (rel 'x y)) => `([y . 1])
         (%more) => #f
         (%assert-after! rel () [('x 2)])
         (%which (y) (rel 'x y)) => `([y . 2])
         (%more) => `([y . 1])
         (%more) => #f
         (%assert! rel () [('x 3)])
         (%which (y) (rel 'x y)) => `([y . 2])
         (%more) => `([y . 1])
         (%more) => `([y . 3])
         (%more) => #f))
 
 (%which (y) (%let (x) (%bag-of x (%or (%= x 1) (%= x 1) (%= x 2)) y))) => `([y . (1 1 2)])
 (%more) => #f
 ; XXX I don't know a program that would get these outputs
 ;(%which (y) (%let (x) (%bag-of x XXX y))) => `([y . ()])
 (%more) => #f
 (%which (y) (%let (x) (%bag-of-1 x (%or (%= x 1) (%= x 1) (%= x 2)) y))) => `([y . (1 1 2)])
 (%more) => #f
 ; XXX I don't know a program that would get these outputs
 ;(%which (y) (%let (x) (%bag-of-1 x XXX y))) => #f
 
 (%which () (%compound (cons 1 1))) => empty
 (%more) => #f
 (%which () (%compound (vector 1 1))) => empty
 (%more) => #f
 (%which () (%let (x) (%and (%= x (cons 1 1)) (%compound x)))) => empty
 (%more) => #f
 (%which () (%compound 1)) => #f 
 (%which () (%compound "1")) => #f 
 (%which () (%compound '1)) => #f 
 (%which () (%compound empty)) => #f 
 
 (%which () (%constant (cons 1 1))) => #f
 (%which () (%constant (vector 1 1))) => #f
 (%which () (%let (x) (%and (%= x 1) (%constant x)))) => empty
 (%more) => #f
 (%which () (%constant 1)) => empty
 (%more) => #f
 (%which () (%constant "1")) => empty
 (%more) => #f
 (%which () (%constant '1)) => empty
 (%more) => #f
 (%which () (%constant empty)) => empty
 (%more) => #f
 
 (%which (x) (%let (y) (%and (%copy x y) (%= y 1)))) => `([x . _])
 (%more) => #f
 
 ! =error> "syntactically"
 
 (%which () (%cut-delimiter %true)) => empty
 (%more) => #f
 (%which () (%cut-delimiter !)) => empty
 (%more) => #f
 (%which () (%cut-delimiter %fail)) => #f
 (%which () (%or %true %true)) => empty
 (%more) => empty
 (%more) => #f
 (%which () (%cut-delimiter (%or (%and ! %true) %true))) => empty
 (%more) => #f
 
 (%which () (%empty-rel 1 1)) => #f
 
 (%which () %fail) => #f
 
 ; %free-vars example from documentation
 (local [(define %knows
           (%rel ()
                 [('Odysseus 'TeX)]
                 [('Odysseus 'Scheme)]
                 [('Odysseus 'Prolog)]
                 [('Odysseus 'Penelope)]
                 [('Penelope 'TeX)]
                 [('Penelope 'Prolog)]
                 [('Penelope 'Odysseus)]
                 [('Telemachus 'TeX)]
                 [('Telemachus 'calculus)]))]
   (test (%which (someone things-known)
                 (%let (x)
                       (%set-of x (%knows someone x)
                                things-known)))
         =>
         `((someone . _) (things-known TeX Scheme Prolog Penelope Odysseus calculus))
         (%more) => #f
         (%which (someone things-known)
                 (%let (x)
                       (%bag-of x
                                (%free-vars (someone)
                                            (%knows someone x))
                                things-known)))
         =>
         `((someone . Odysseus) (things-known TeX Scheme Prolog Penelope))
         (%more) =>
         `((someone . Penelope) (things-known TeX Prolog Odysseus))
         (%more) =>
         `((someone . Telemachus) (things-known TeX calculus))
         (%more) =>
         #f))
 
 (%which (x) (%let (y) (%and (%freeze x y) (%nonvar y)))) => `([x . _])
 
 (%which () (%if-then-else %true %true %true)) => empty
 (%more) => #f
 (%which () (%if-then-else %true %fail %true)) => #f
 (%which () (%if-then-else %fail %true %true)) => empty
 (%more) => #f
 
 (%which (x) (%is x (* 6 7))) => `([x . 42])
 (%more) => #f
 (%which (x) (%let (y) (%and (%= y 7) (%is x (* 6 y))))) => `([x . 42])
 (%more) => #f
 
 (%which () (%let (x) (%= x x))) => empty
 (%more) => #f
 
 (%which (x) (%let (y z) (%and (%freeze x y) (%melt y z) (%= z 1)))) => `([x . 1])
 (%more) => #f
 
 (%which (x) (%let (y z) (%and (%freeze x y) (%melt-new y z) (%= z 1)))) => `([x . _])
 (%more) => #f
 
 (%which () (%member 3 (list 1 2 3))) => empty
 (%more) => #f
 (%which () (%member 3 (list 1 2 3 3))) => empty
 (%more) => empty
 (%more) => #f
 (%which () (%member 3 (list 1 2))) => #f
 
 (%which () (%let (x) (%nonvar x))) => #f
 (%which () (%let (x) (%nonvar (cons 1 x)))) => #f
 (%which () (%let (x) (%nonvar (vector x)))) => #f
 (%which () (%let (x) (%nonvar 1))) => empty
 (%more) => #f
 (%which () (%let (x) (%and (%= x 1) (%nonvar x)))) => empty
 (%more) => #f
 
 (%which () (%not %true)) => #f
 (%which () (%not %fail)) => empty
 (%more) => #f
 
 (%which () (%or %true %true)) => empty
 (%more) => empty
 (%more) => #f
 (%which () (%or %true %fail %true)) => empty
 (%more) => empty
 (%more) => #f
 
 (let ([rel (%rel () [(1)] [(2) %fail])])
   (test (%which () (rel 1)) => empty
         (%more) => #f
         (%which () (rel 2)) => #f))
 (let ([rel (%rel () [(1) !] [(1) (%repeat)])])
   (test (%which () (rel 1)) => empty
         (%more) => #f))
 
 (local [(define (many-%more n)
           (if (zero? n)
               empty
               (and (%more)
                    (many-%more (sub1 n)))))]
   (test (%which () (%repeat)) => empty
         (many-%more (random 50)) => empty))
 
 (parameterize ([use-occurs-check? #f])
   (%which () (%let (x) (%= x (cons 1 x))))) 
 => empty
 (parameterize ([use-occurs-check? #t])
   (%which () (%let (x) (%= x (cons 1 x)))))
 => #f
 
 (%which (y) (%let (x) (%set-of x (%or (%= x 1) (%= x 1) (%= x 2)) y))) => `([y . (1 2)])
 (%more) => #f
 ; XXX I don't know a program that would get these outputs
 ;(%which (y) (%let (x) (%set-of x XXX y))) => `([y . ()])
 (%more) => #f
 (%which (y) (%let (x) (%set-of-1 x (%or (%= x 1) (%= x 1) (%= x 2)) y))) => `([y . (1 2)])
 (%more) => #f
 ; XXX I don't know a program that would get these outputs
 ;(%which (y) (%let (x) (%set-of-1 x XXX y))) => #f
 
 (%which () %true) => empty
 (%more) => #f
 
 (%which () (%let (x) (%var x))) => empty
 (%more) => #f
 (%which () (%let (x) (%var (cons 1 x)))) => empty
 (%more) => #f
 (%which () (%let (x) (%var (vector x)))) => empty
 (%more) => #f
 (%which () (%let (x) (%var 1))) => #f
 (%which () (%let (x) (%and (%= x 1) (%var x)))) => #f

 (%which () %true %true) => empty
 (%more) => #f
 
 )
