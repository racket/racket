(load-relative "loadtest.ss")

(Section 'planet)

(require planet/util)

;; Testing: #lang planet
(add-hard-link "plt" "dummy-package.plt" 1 0
               (string->path "planet-stuff/dummy-package"))
(test 15 "#lang planet"
      (dynamic-require "planet-stuff/dummy-module.ss" 'result))
(remove-hard-link "plt" "dummy-package.plt" 1 0)

(report-errs)
