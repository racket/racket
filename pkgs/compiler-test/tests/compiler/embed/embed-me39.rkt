#lang racket/base

(module+ main
  (require (for-syntax racket/base)
           racket/runtime-path)
  (define-runtime-path license '(share "LICENSE-libscheme.txt"))
  (with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
    #:exists 'append
    (lambda ()
      (if (absolute-path? license)
          (displayln "found license")
          (displayln "not found")))))
