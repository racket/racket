(module datatype-test mzscheme
  (require plai/datatype
           plai/test-harness
           mzlib/contract)
  
  (print-tests 'stop)
  
  ;; TODO:
  ;; ? Make sure the contract expressions are only evaluated once
  ;; - Test that helpful error messages are generated
  
  (define-type (Option a)
    [Some (item a)]
    [None])
  
  (define-type (List a)
      [Cons (item a) (rest (List-of/c a))]
      [Nil])
  
  (define-type (TreeMap a b)
    [RedNode (left (TreeMap-of/c a b)) (key a) (val b) (right (TreeMap-of/c a b))]
    [BlackNode (left (TreeMap-of/c a b)) (key a) (val b) (right (TreeMap-of/c a b))]
    [Leaf])
  
  (define-type (a-List a)
    [a-Cons (item a) (rest (b-List-of/c a))]
    [a-Nil])
  
  (define-type (b-List b)
    [b-Cons (item b) (rest (a-List-of/c b))]
    [b-Nil])
  
  ;; Error Messages
  (test/exn (lambda () (expand #'(define-type (BadOption a)
                                  [Some (item a)]
                                  [Some (item a)])))
            "duplicate")
  
  (test/exn (lambda () (expand #'(define-type (BadList 123)
                                   [Cons (item a) (rest (BadList-of/c a))]
                                   [Nil])))
            "expected an identifier")
  
  ;; Option Test
  (test/pred (Some 1) Option?)
  (test/pred (Some 1) Some?)
  (test/pred (None) Option?)
  (test/pred (None) None?)
  
  (test ((Some-of integer?) 1) (Some 1))
  (test (contract (Option-of/c integer?) (Some 1) 'p 'n) (Some 1))
  (test (contract (Some-of/c integer?) (Some 1) 'p 'n) (Some 1))
  
  (test ((None-of integer?)) (None))
  (test (contract (Option-of/c integer?) (None) 'p 'n) (None))
  (test (contract (None-of/c symbol?) (None) 'p 'n) (None))
  
  (test (Some-item (Some 1)) 1) 
  
  (test/exn (lambda () ((Some-of integer?) 'a)) "broke the contract")
  (test/exn (lambda () (contract (Some-of/c integer?) (Some 'symbol) 'p 'n)) "broke the contract")
  (test/exn (lambda () (contract (Option-of/c integer?) (Some 'symbol) 'p 'n)) "broke the contract") 
  
  ;; List Test
  (test/pred (Nil) List?)
  (test/pred (Nil) Nil?)
  (test/pred (Cons 1 (Nil)) List?)
  (test/pred (Cons 1 (Nil)) Cons?)
  
  (test ((Cons-of integer?) 1 (Cons 2 (Cons 3 (Nil))))
        (Cons 1 (Cons 2 (Cons 3 (Nil)))))
  (test (contract (List-of/c integer?) (Cons 1 (Cons 2 (Cons 3 (Nil)))) 'p 'n)
        (Cons 1 (Cons 2 (Cons 3 (Nil)))))
  (test (contract (Cons-of/c integer?) (Cons 1 (Cons 2 (Cons 3 (Nil)))) 'p 'n)
        (Cons 1 (Cons 2 (Cons 3 (Nil)))))

  (test ((Nil-of integer?)) (Nil))
  (test (contract (List-of/c integer?) (Nil) 'p 'n) (Nil))
  (test (contract (Nil-of/c integer?) (Nil) 'p 'n) (Nil))
  
  (test/exn (lambda () ((Cons-of integer?) 1 2)) "broke the contract") 
  (test/exn (lambda () (contract (Cons-of/c integer?) (Cons 1 2) 'p 'n)) "broke the contract")
  (test/exn (lambda () ((Cons-of integer?) 1 (Cons 2 (Cons 'symbol (Nil))))) "broke the contract")
  
  ;; TreeMap Test
  (test/pred (Leaf) TreeMap?)
  (test/pred (Leaf) Leaf?)
  (test/pred (RedNode (Leaf) 1 2 (Leaf)) TreeMap?)
  (test/pred (RedNode (Leaf) 1 2 (Leaf)) RedNode?)
  (test/pred (BlackNode (Leaf) 1 2 (Leaf)) TreeMap?)
  (test/pred (BlackNode (Leaf) 1 2 (Leaf)) BlackNode?)
  
  ;; Alternating List Test
  (test/pred (a-Nil) a-List?)
  (test/pred (a-Nil) a-Nil?)
  (test/pred (b-Nil) b-List?)
  (test/pred (b-Nil) b-Nil?)
  (test/pred (a-Cons 1 (b-Nil)) a-List?)
  (test/pred (a-Cons 1 (b-Nil)) a-Cons?)
  (test/pred (b-Cons 1 (a-Nil)) b-List?)
  (test/pred (b-Cons 1 (a-Nil)) b-List?)
  
  (test ((a-Cons-of integer?) 1 (b-Nil)) (a-Cons 1 (b-Nil)))
  (test (contract (a-List-of/c integer?) (a-Cons 1 (b-Nil)) 'p 'n) (a-Cons 1 (b-Nil)))
  (test (contract (a-Cons-of/c integer?) (a-Cons 1 (b-Nil)) 'p 'n) (a-Cons 1 (b-Nil)))
  (test ((b-Cons-of integer?) 1 (a-Nil)) (b-Cons 1 (a-Nil)))
  (test (contract (b-List-of/c integer?) (b-Cons 1 (a-Nil)) 'p 'n) (b-Cons 1 (a-Nil)))
  (test (contract (b-Cons-of/c integer?) (b-Cons 1 (a-Nil)) 'p 'n) (b-Cons 1 (a-Nil)))
  
  (test/exn (lambda () ((a-Cons-of integer?) 1 (a-Nil))) "broke the contract")
  )
