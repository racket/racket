#lang scheme/base

(require planet/util
         scheme/runtime-path
         tests/eli-tester)

(define-runtime-path here ".")
(define (in-here path) (path->string (build-path here path)))

(test

 ;; Testing: #lang planet
 (add-hard-link "plt" "dummy-package.plt" 1 0
                (string->path (in-here "examples/dummy-package")))
 => (void)
 (dynamic-require `(file ,(in-here "examples/dummy-module.rkt")) 'result)
 => '(successful test result)
 (dynamic-require '(planet plt/dummy-package/me) 'me)
 => '("plt" "dummy-package.plt" 1 0)
 (remove-hard-link "plt" "dummy-package.plt" 1 0)
 => (void)

 )
