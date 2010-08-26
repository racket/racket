#lang scheme/base

(require scheme/runtime-path
         (for-syntax scheme/base))
(define-runtime-path file '(lib "etc.ss")) ; in mzlib
(with-output-to-file "stdout"
  (lambda () (printf "This is 1c\n"))
  #:exists 'append)

