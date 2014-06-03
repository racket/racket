#lang scheme/base

(require scheme/runtime-path
         (for-syntax scheme/base))
(define-runtime-path file '(lib "etc.ss")) ; in mzlib
(with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
  (lambda () (printf "This is 1c\n"))
  #:exists 'append)

