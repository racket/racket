
(load-relative "loadtest.ss")

(Section 'shared)

(require mzlib/shared)

(require (only-in mzscheme define-struct))
(load-relative "shared-tests.ss")

(stest (letrec ([x x]) x) (shared ([x x]) x))
(stest (letrec ([x x]) x) (shared ([x y][y x]) x))

(namespace-require/copy 'scheme/base)
(require (only-in mzscheme define-struct))
(load-relative "shared-tests.ss")

(report-errs)
