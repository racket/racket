#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract
                                               'racket/match)])
  (test/spec-passed
   'struct/contract1
   '(let ()
      (struct/contract foobar ([x number?] [y number?]))
      1))

  (test/spec-passed
   'struct/contract2
   '(let ()
      (struct/contract foobar ([x number?] [y number?]))
      (foobar 1 2)))

  (test/spec-failed
   'struct/contract3
   '(let ()
      (struct/contract foobar ([x number?] [y number?]))
      (foobar 1 #t))
   "top-level")

  (test/spec-passed
   'struct/contract4
   '(let ()
      (struct/contract foobar ([x number?] [y number?]))
      (foobar-y (foobar 2 3))))

  (test/spec-failed
   'struct/contract5
   '(let ()
      (struct/contract foobar ([x number?] [y number?]))
      (foobar-y 1))
   "top-level")

  (test/spec-passed
   'struct/contract6
   '(let ()
      (struct/contract foobar ([x number?] [y number?]) #:mutable)
      (set-foobar-y! (foobar 1 2) 3)
      (set-foobar-x! (foobar 1 2) 3)))

  (test/spec-failed
   'struct/contract7
   '(let ()
      (struct/contract foobar ([x number?] [y number?]) #:mutable)
      (set-foobar-y! (foobar 1 2) #f))
   "top-level")

  (test/spec-passed
   'struct/contract8
   '(let ()
      (struct/contract foobar ([(x #:mutable) number?] [y number?]))
      (set-foobar-x! (foobar 1 2) 4)))

  (test/spec-failed
   'struct/contract9
   '(let ()
      (struct/contract foobar ([(x #:mutable) number?] [y number?]))
      (set-foobar-x! (foobar 1 2) #f))
   "top-level")

  (test/spec-failed
   'struct/contract10
   '(let ()
      (struct/contract foobar ([x number?] [(y #:auto) number?]))
      (foobar 1))
   "(struct foobar)")

  (test/spec-passed
   'struct/contract11
   '(let ()
      (struct/contract foobar ([x number?] [(y #:auto) number?]) #:auto-value 3)
      (foobar 1)))

  (test/spec-passed
   'struct/contract12
   '(let ()
      (struct/contract foobar ([x number?] [(y #:auto #:mutable) number?]) #:auto-value 3)
      (set-foobar-y! (foobar 1) 3)))

  (test/spec-failed
   'struct/contract13
   '(let ()
      (struct/contract foobar ([x number?] [(y #:auto #:mutable) number?]) #:auto-value 3)
      (set-foobar-y! (foobar 1) #t))
   "top-level")

  (test/spec-passed
   'struct/contract14
   '(let ()
      (struct/contract foobar ([x number?] [y number?]) #:transparent)
      1))

  (test/spec-passed
   'struct/contract15
   '(let ()
      (define-struct foobar (x))
      (struct/contract bar foobar ([z string?]))
      (bar 2 "x")))

  (test/spec-failed
   'struct/contract16
   '(let ()
      (define-struct foobar (x))
      (struct/contract bar foobar ([z string?]))
      (bar 2 #f))
   "top-level")

  (test/spec-passed
   'struct/contract17
   '(let ()
      (define-struct foobar (x))
      (struct/contract bar foobar ([z string?]) #:mutable)
      (set-bar-z! (bar 2 "x") "y")))

  (test/spec-failed
   'struct/contract18
   '(let ()
      (define-struct foobar (x))
      (struct/contract bar foobar ([z string?]) #:mutable)
      (set-bar-z! (bar 2 "x") #f))
   "top-level")

  (test/spec-passed
   'struct/contract19
   '(let ()
      (define-struct foobar (x))
      (struct/contract bar foobar ([z string?]))
      (struct/contract baz bar ([x number?]))
      (baz 2 "x" 5)))

  (test/spec-failed
   'struct/contract20
   '(let ()
      (define-struct foobar (x))
      (struct/contract bar foobar ([z string?]))
      (struct/contract baz bar ([x number?]))
      (baz 2 "x" #f))
   "top-level")

  (test/spec-failed
   'struct/contract21
   '(let ()
      (define-struct foobar (x))
      (struct/contract bar foobar ([z string?]))
      (struct/contract baz bar ([x number?]))
      (baz 2 #f 3))
   "top-level")

  (test/spec-passed
   'struct/contract21
   '(let ()
      (define-struct foobar (x) #:mutable)
      (struct/contract bar foobar ([z string?]))
      (set-foobar-x! (bar 2 "x") #f)))

  (test/spec-passed
   'struct/contract22
   '(struct/contract foobar ([x number?] [y number?]) #:mutable #:transparent))

  (test/spec-passed
   'struct/contract23
   '(struct/contract foobar ([x number?] [y number?])
      #:mutable #:transparent
      #:property prop:custom-write
      (lambda (a b c) (void))))

  (test/spec-passed/result
   'struct/contract24
   '(let ()
      (struct/contract point
        ([x number?] [y number?])
        #:transparent)
      (struct/contract color-point point
        ([c symbol?])
        #:transparent)

      (match (color-point 1 2 'red)
        [(struct color-point [dx dy color])
         (list dx dy color)]
        [(struct point [dx dy]) (list dx dy)]
        [v (box v)]))
   (list 1 2 'red))

  (test/spec-passed
   'struct/contract25
   '(let ()
      (struct/contract point
        ([x number?] [y number?])
        #:transparent)
      (point 1 2)))

  (test/spec-failed
   'struct/contract26
   '(let ()
      (struct/contract point
        ([x number?] [y number?])
        #:transparent)
      (point 1 #t))
   "top-level"))
