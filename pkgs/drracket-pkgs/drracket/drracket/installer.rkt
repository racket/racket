#lang racket/base
(require launcher)
(provide installer)

(define (installer plthome)
  (do-installation)
  (set! do-installation void))

(define (do-installation)
  (for-each install-variation (available-mred-variants)))

(define (install-variation variant)
  (parameterize ([current-launcher-variant variant])
    (make-mred-launcher
     (list "-ZmvqL" "drracket.rkt" "drracket")
     (mred-program-launcher-path "DrScheme")
     (cons
      `(exe-name . "DrRacket")
      (build-aux-from-path (build-path (collection-path "drracket") "drracket"))))))
