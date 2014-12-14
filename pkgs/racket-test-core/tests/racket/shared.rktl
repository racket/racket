
(load-relative "loadtest.rktl")

(Section 'shared)

(require racket/shared
         racket/undefined)

(require (only-in mzscheme define-struct))
(load-relative "shared-tests.rktl")

(err/rt-test (shared ([x x]) x) exn:fail:contract:variable?)
(err/rt-test (shared ([x y][y x]) x) exn:fail:contract:variable?)

(test 3 'unbox (shared ([y (box 3)]
                        [x (box (unbox y))])
                 (unbox x)))

(namespace-require/copy 'scheme/base)
(require (only-in mzscheme define-struct))
(load-relative "shared-tests.rktl")

;; Check that `shared' works with `struct':
(let ()
  (struct a (x y) #:mutable #:transparent)
  (define an-a (a 1 2))
  (set-a-y! an-a an-a)
  (test an-a 'an-a (shared ([t (a 1 t)])
                     t)))

;; Check that `shared' works with auto fields:
(let ()
  (struct a (x y [z #:auto]) #:mutable #:transparent)
  (define an-a (a 1 2))
  (set-a-y! an-a an-a)
  (test an-a 'an-a (shared ([t (a 1 t)])
                     t)))

(report-errs)
