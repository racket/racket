
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

(report-errs)
