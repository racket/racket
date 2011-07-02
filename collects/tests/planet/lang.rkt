#lang scheme/base

(require planet/util
         scheme/runtime-path
         "../eli-tester.rkt")

(define-runtime-path here ".")
(define (in-here path) (path->string (build-path here path)))

(test

 ;; Testing: #lang planet
 (add-hard-link "plt" "dummy-package.plt" 1 0
                (string->path (in-here "examples/dummy-package")))
 => (void)
 (dynamic-require `(file ,(in-here "examples/dummy-module.rkt")) 'result)
 => '(successful test result)
 (remove-hard-link "plt" "dummy-package.plt" 1 0)
 => (void)

 )
