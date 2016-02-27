#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace (make-basic-contract-namespace)])
  (test/no-error '(listof any/c))
  (test/no-error '(listof (lambda (x) #t)))
  (test/no-error '(((lambda (x) x) listof) #t))
  (test/no-error '(non-empty-listof any/c))
  (test/no-error '(non-empty-listof (lambda (x) #t)))
  
  (test/no-error '(list/c 'x "x" #t #f #\c #rx"a" #rx#"b"))
  
  (test/pos-blame 'list1 '(contract (list/c 1 2) (list 1 3) 'pos 'neg))
  (test/no-error '(contract (list/c 1 2) (list 1 2) 'pos 'neg))
  
  (test/spec-passed/result 
   'listof1
   '(contract (listof integer?) '(1 2 3) 'pos 'neg)
   '(1 2 3))
  (test/spec-passed/result 
   'listof2
   '(contract (listof integer?) '() 'pos 'neg)
   '())
  (test/pos-blame 
   'listof3
   '(contract (listof integer?) #f 'pos 'neg))
  (test/pos-blame 
   'listof4
   '(contract (listof integer?) (cons 1 2) 'pos 'neg))
  (test/pos-blame 
   'listof5
   '(contract (listof integer?) (list #f #t) 'pos 'neg))
  
  (test/spec-passed/result 
   'nelistof1
   '(contract (non-empty-listof integer?) '(1 2 3) 'pos 'neg)
   '(1 2 3))
  (test/pos-blame  
   'nelistof2
   '(contract (non-empty-listof integer?) '() 'pos 'neg))
  (test/pos-blame 
   'nelistof3
   '(contract (non-empty-listof integer?) #f 'pos 'neg))
  (test/pos-blame 
   'nelistof4
   '(contract (non-empty-listof integer?) (cons 1 2) 'pos 'neg))
  (test/pos-blame 
   'nelistof5
   '(contract (non-empty-listof integer?) (list #f #t) 'pos 'neg))
  
  (test/spec-passed/result 
   'imlistof1
   '(contract (list*of integer?) '(1 2 . 3) 'pos 'neg)
   '(1 2 . 3))
  (test/pos-blame
   'imlistof2
   '(contract (list*of integer?) '() 'pos 'neg))
  (test/pos-blame 
   'imlistof3
   '(contract (list*of integer?) #f 'pos 'neg))
  (test/pos-blame 
   'imlistof4
   '(contract (list*of integer?) (list 1 2) 'pos 'neg))
  (test/pos-blame 
   'imlistof5
   '(contract (list*of integer?) (cons #f #t) 'pos 'neg))

  (test/spec-passed/result 
   'imlistof6
   '(contract (list*of integer? char?) '(1 2 . #\3) 'pos 'neg)
   '(1 2 . #\3))
  (test/pos-blame
   'imlistof7
   '(contract (list*of integer? char?) '() 'pos 'neg))
  (test/pos-blame 
   'imlistof8
   '(contract (list*of integer? char?) #f 'pos 'neg))
  (test/pos-blame 
   'imlistof9
   '(contract (list*of integer? char?) (list 1 2) 'pos 'neg))
  (test/pos-blame 
   'imlistof10
   '(contract (list*of integer? char?) (cons #f #t) 'pos 'neg))
  (test/spec-passed
   'imlistof11
   '(contract (list*of (-> integer? integer?)
                       (-> boolean? boolean? boolean?))
              (cons (λ (x) x) (cons (λ (y) y) (λ (a b) a)))
              'pos 'neg))   
  
  (test/pos-blame
   'cons/dc1
   '(contract (cons/dc [hd integer?] [tl (hd) integer?])
              1
              'pos
              'neg))
  
  (test/spec-passed
   'cons/dc2
   '(contract (cons/dc [hd integer?] [tl (hd) integer?])
              (cons 1 0)
              'pos
              'neg))
  
  (test/pos-blame
   'cons/dc3
   '(contract (cons/dc [hd integer?] [tl (hd) integer?])
              (cons #f 0)
              'pos
              'neg))
  
  (test/pos-blame
   'cons/dc4
   '(contract (cons/dc [hd integer?] [tl (hd) integer?])
              (cons 0 #f)
              'pos
              'neg))
  
  (test/pos-blame
   'cons/dc5
   '(contract (cons/dc [hd integer?] [tl (hd) (<=/c hd)])
              (cons 0 2)
              'pos
              'neg))
  
  (test/pos-blame
   'cons/dc6
   '(contract (cons/dc [hd (tl) integer?] [tl integer?])
              1
              'pos
              'neg))
  
  (test/spec-passed
   'cons/dc7
   '(contract (cons/dc [hd (tl) integer?] [tl integer?])
              (cons 1 0)
              'pos
              'neg))
  
  (test/pos-blame
   'cons/dc8
   '(contract (cons/dc [hd (tl) integer?] [tl integer?])
              (cons #f 0)
              'pos
              'neg))
  
  (test/pos-blame
   'cons/dc9
   '(contract (cons/dc [hd (tl) integer?] [tl integer?])
              (cons 0 #f)
              'pos
              'neg))
  
  (test/pos-blame
   'cons/dc10
   '(contract (cons/dc [hd (tl) (<=/c tl)] [tl integer?])
              (cons 2 0)
              'pos
              'neg))
  
  (test/spec-passed/result
   'cons/dc11
   '(chaperone-contract? (cons/dc [hd integer?] [tl (hd) integer?]))
   #t)
  (test/spec-passed/result
   'cons/dc12
   '(flat-contract? (cons/dc [hd integer?] [tl (hd) integer?] #:flat))
   #t)
  (test/spec-passed/result
   'cons/dc13
   '(contract? (cons/dc [hd integer?] [tl (hd) integer?] #:impersonator))
   #t)

  (test/spec-passed/result
   '*list/c1
   '(contract (*list/c integer? char? boolean?) '(1 2 3 #\a #f) 'pos 'neg)
   '(1 2 3 #\a #f))
  (test/pos-blame
   '*list/c2
   '(contract (*list/c integer? char? boolean?) '(1 2 #\a #\a #f) 'pos 'neg))
  (test/spec-passed/result
   '*list/c3
   '((car (contract (*list/c (-> integer? integer?) (-> boolean? boolean?))
                    (list (λ (x) x) (λ (y) y)) 'pos 'neg))
     1)
   1)
  (test/neg-blame
   '*list/c4
   '((car (contract (*list/c (-> integer? integer?) (-> boolean? boolean?))
                    (list (λ (x) x) (λ (y) y)) 'pos 'neg))
     #f))
  (test/spec-passed/result
   '*list/c5
   '((cadr (contract (*list/c (-> integer? integer?) (-> boolean? boolean?))
                     (list (λ (x) x) (λ (y) y)) 'pos 'neg))
     #f)
   #f)
  (test/neg-blame
   '*list/c6
   '((cadr (contract (*list/c (-> integer? integer?) (-> boolean? boolean?))
                     (list (λ (x) x) (λ (y) y)) 'pos 'neg))
     1))
  (test/pos-blame
   '*list/c7
   '((caddr (contract (*list/c (-> integer? integer?) (-> boolean? boolean?) (-> char? char?))
                      (list (λ (x) x) (λ (y) y) (λ (y) 'not-a-bool) (λ (y) y)) 'pos 'neg))
     #f))
  
  )