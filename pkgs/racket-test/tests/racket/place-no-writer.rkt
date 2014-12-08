#lang racket/base
(require racket/place)

;; Check that syncing on a place (as opposed to place channel)
;; known to have no writers will fail to sync (as opposed to
;; returning the place itself, for example, which was the bug
;; that triggered this test).

(define (main)
  (for ([i 10])
    (displayln i)
    (define p (place pch (void)))
    (place-wait p)
    (collect-garbage)
    (define v (sync/timeout 0 p))
    (when v
      (error "failed: " v))))

(module+ main (main))
(module+ test (main))