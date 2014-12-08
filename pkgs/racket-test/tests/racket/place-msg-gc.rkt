#lang racket

;; Check that a master GC doesn't drop a byte string
;; that is referenced only as a pending channel message

(define-values (i o) (place-channel))

(place-channel-put i (list (make-shared-bytes 10)))

(for ([i 100])
  (make-shared-bytes (* 1024 1024)))

(let ([l (place-channel-get o)])
  (bytes-ref (car l) 9))
