#lang racket/base

;; Try to trigger master GCs:
(for ([i 100000])
  (make-shared-bytes 1024))
