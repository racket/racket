#lang scheme/base

(require scheme/runtime-path
         (for-syntax scheme/base))
(define-runtime-path file '(lib "icons/file.gif"))
(with-output-to-file "stdout"
  (lambda () (printf "This is 1b\n"))
  #:exists 'append)

