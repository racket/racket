
(load-relative "loadtest.ss")

(Section 'shared)

(require (lib "shared.ss"))

(load-relative "shared-tests.ss")

(require mzscheme)
(load-relative "shared-tests.ss")

(report-errs)
