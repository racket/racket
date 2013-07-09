#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract
                                               'racket/match)])
  (test/spec-passed
   'define-struct/contract1
   '(let ()
      (define-struct/contract foo ([x number?] [y number?]))
      1))
  
  (test/spec-passed
   'define-struct/contract2
   '(let ()
      (define-struct/contract foo ([x number?] [y number?]))
      (make-foo 1 2)))
  
  (test/spec-failed
   'define-struct/contract3
   '(let ()
      (define-struct/contract foo ([x number?] [y number?]))
      (make-foo 1 #t))
   "top-level")
  
  (test/spec-passed
   'define-struct/contract4
   '(let ()
      (define-struct/contract foo ([x number?] [y number?]))
      (foo-y (make-foo 2 3))))
  
  (test/spec-failed
   'define-struct/contract5
   '(let ()
      (define-struct/contract foo ([x number?] [y number?]))
      (foo-y 1))
   "top-level")
  
  (test/spec-passed
   'define-struct/contract6
   '(let ()
      (define-struct/contract foo ([x number?] [y number?]) #:mutable)
      (set-foo-y! (make-foo 1 2) 3)
      (set-foo-x! (make-foo 1 2) 3)))
  
  (test/spec-failed
   'define-struct/contract7
   '(let ()
      (define-struct/contract foo ([x number?] [y number?]) #:mutable)
      (set-foo-y! (make-foo 1 2) #f))
   "top-level")
  
  (test/spec-passed
   'define-struct/contract8
   '(let ()
      (define-struct/contract foo ([(x #:mutable) number?] [y number?]))
      (set-foo-x! (make-foo 1 2) 4)))
  
  (test/spec-failed
   'define-struct/contract9
   '(let ()
      (define-struct/contract foo ([(x #:mutable) number?] [y number?]))
      (set-foo-x! (make-foo 1 2) #f))
   "top-level")
  
  (test/spec-failed
   'define-struct/contract10
   '(let ()
      (define-struct/contract foo ([x number?] [(y #:auto) number?]))
      (make-foo 1))
   "(struct foo)")
  
  (test/spec-passed
   'define-struct/contract11
   '(let ()
      (define-struct/contract foo ([x number?] [(y #:auto) number?]) #:auto-value 3)
      (make-foo 1)))
  
  (test/spec-passed
   'define-struct/contract12
   '(let ()
      (define-struct/contract foo ([x number?] [(y #:auto #:mutable) number?]) #:auto-value 3)
      (set-foo-y! (make-foo 1) 3)))
  
  (test/spec-failed
   'define-struct/contract13
   '(let ()
      (define-struct/contract foo ([x number?] [(y #:auto #:mutable) number?]) #:auto-value 3)
      (set-foo-y! (make-foo 1) #t))
   "top-level")
  
  (test/spec-passed
   'define-struct/contract14
   '(let ()
      (define-struct/contract foo ([x number?] [y number?]) #:transparent)
      1))
  
  (test/spec-passed
   'define-struct/contract15
   '(let ()
      (define-struct foo (x))
      (define-struct/contract (bar foo) ([z string?]))
      (make-bar 2 "x")))
  
  (test/spec-failed
   'define-struct/contract16
   '(let ()
      (define-struct foo (x))
      (define-struct/contract (bar foo) ([z string?]))
      (make-bar 2 #f))
   "top-level")
  
  (test/spec-passed
   'define-struct/contract17
   '(let ()
      (define-struct foo (x))
      (define-struct/contract (bar foo) ([z string?]) #:mutable)
      (set-bar-z! (make-bar 2 "x") "y")))
  
  (test/spec-failed
   'define-struct/contract18
   '(let ()
      (define-struct foo (x))
      (define-struct/contract (bar foo) ([z string?]) #:mutable)
      (set-bar-z! (make-bar 2 "x") #f))
   "top-level")
  
  (test/spec-passed
   'define-struct/contract19
   '(let ()
      (define-struct foo (x))
      (define-struct/contract (bar foo) ([z string?]))
      (define-struct/contract (baz bar) ([x number?]))
      (make-baz 2 "x" 5)))
  
  (test/spec-failed
   'define-struct/contract20
   '(let ()
      (define-struct foo (x))
      (define-struct/contract (bar foo) ([z string?]))
      (define-struct/contract (baz bar) ([x number?]))
      (make-baz 2 "x" #f))
   "top-level")
  
  (test/spec-failed
   'define-struct/contract21
   '(let ()
      (define-struct foo (x))
      (define-struct/contract (bar foo) ([z string?]))
      (define-struct/contract (baz bar) ([x number?]))
      (make-baz 2 #f 3))
   "top-level")
  
  (test/spec-passed
   'define-struct/contract21
   '(let ()
      (define-struct foo (x) #:mutable)
      (define-struct/contract (bar foo) ([z string?]))
      (set-foo-x! (make-bar 2 "x") #f)))
  
  (test/spec-passed
   'define-struct/contract22
   '(define-struct/contract foo ([x number?] [y number?]) #:mutable #:transparent))
  
  (test/spec-passed
   'define-struct/contract23
   '(define-struct/contract foo ([x number?] [y number?])
      #:mutable #:transparent
      #:property prop:custom-write
      (lambda (a b c) (void))))
  
  (test/spec-passed/result
   'define-struct/contract24
   '(let ()
      (define-struct/contract point
        ([x number?] [y number?])
        #:transparent)
      (define-struct/contract (color-point point)
        ([c symbol?])
        #:transparent)
      
      (match (make-color-point 1 2 'red)
        [(struct color-point [dx dy color])
         (list dx dy color)]
        [(struct point [dx dy]) (list dx dy)]
        [v (box v)]))
   (list 1 2 'red))
  
  (test/spec-passed
   'define-struct/contract25
   '(let ()
      (define-struct/contract point
        ([x number?] [y number?])
        #:transparent)
      (point 1 2)))
  
  (test/spec-failed
   'define-struct/contract26
   '(let ()
      (define-struct/contract point
        ([x number?] [y number?])
        #:transparent)
      (point 1 #t))
   "top-level"))