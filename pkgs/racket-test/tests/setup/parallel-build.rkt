#lang racket/base
(require
  racket/file
  rackunit
  setup/parallel-build)

(check-exn exn:fail:contract? (λ () (parallel-compile ".")))
(check-exn exn:fail:contract? (λ () (parallel-compile-files (list) #:worker-count 2.5)))
(check-exn exn:fail:contract? (λ () (parallel-compile-files (list) #:handler 5)))

(define temporary-directory (make-temporary-file "parallel-build~a" 'directory))
(check-false (parallel-compile-files (list temporary-directory)))
(check-false (parallel-compile-files (list (build-path temporary-directory "new-directory" ".."))))
(delete-directory/files temporary-directory)
