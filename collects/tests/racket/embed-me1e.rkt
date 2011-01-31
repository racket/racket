#lang scheme/base

(require scheme/runtime-path
         (for-syntax scheme/base))
(define-runtime-path file '(lib "html"))
(with-output-to-file "stdout"
  (lambda () (printf "This is 1e\n"))
  #:exists 'append)
