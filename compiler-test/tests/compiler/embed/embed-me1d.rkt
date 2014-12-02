#lang scheme/base

(require scheme/runtime-path
         (for-syntax scheme/base))
(define-runtime-path file '(lib "file.gif" "icons"))
(with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
  (lambda () (printf "This is 1d\n"))
  #:exists 'append)
