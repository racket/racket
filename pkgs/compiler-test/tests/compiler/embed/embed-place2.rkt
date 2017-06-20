#lang racket
(provide start-place)
(define (start-place)
  (place pch
         (with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
           (lambda () (printf "Hello from a place!\n"))
           #:exists 'append)))
