#lang scheme/base

(require scheme/runtime-path
         (for-syntax scheme/base))
(define-runtime-path file '(lib "icons/file.gif"))
(with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
  (lambda () (printf "This is 1b\n"))
  #:exists 'append)

