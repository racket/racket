
(load-relative "loadtest.rktl")

(Section 'shared)

(require mzlib/shared)

(require (only-in mzscheme define-struct))
(load-relative "shared-tests.rktl")

(stest (letrec ([x x]) x) (shared ([x x]) x))
(stest (letrec ([x x]) x) (shared ([x y][y x]) x))

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
