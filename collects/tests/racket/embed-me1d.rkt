#lang scheme/base

(require scheme/runtime-path
         (for-syntax scheme/base))
(define-runtime-path file '(lib "file.gif" "icons"))
(with-output-to-file "stdout"
  (lambda () (printf "This is 1d\n"))
  #:exists 'append)
