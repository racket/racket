#lang racket/base
;; Reader extension used by the optimizer tests to make changes to the logging less disruptive to
;; existing logging.

(provide read-syntax)

(define (read-syntax name port)
  (read-line port)
  (when (port-counts-lines? port)
    (set-port-next-location! port 1 0 1))
  (make-special-comment 'typed-racket/optimizer/reset-port))
