(load-relative "../mzscheme/loadtest.ss")

(Section 'planet)

(require planet/util
         scheme/contract)

;; Testing: #lang planet

(test (void)
      'add-hard-link
      (with-handlers ([exn:fail? values])
        (add-hard-link "plt" "dummy-package.plt" 1 0
                       (string->path "examples/dummy-package"))))

(test '(successful test result)
      'hash-lang-planet
      (with-handlers ([exn:fail? values])
        (dynamic-require "examples/dummy-module.ss" 'result)))

(test (void)
      'remove-hard-link
      (with-handlers ([exn:fail? values])
        (remove-hard-link "plt" "dummy-package.plt" 1 0)))

(report-errs)
