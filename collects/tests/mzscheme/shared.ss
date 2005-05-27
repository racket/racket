
(load-relative "loadtest.ss")

(SECTION 'shared)

(require (lib "shared.ss"))

(load-relative "shared-tests.ss")
  
(require mzscheme)
(load-relative "shared-tests.ss")
  
(report-errs)
