#lang racket
(require ffi/unsafe)

(provide main)

;; Make sure that `#:in-original-place?' doesn't lead to deadlock:

(define scheme_malloc_atomic
  (get-ffi-obj 'GC_malloc_atomic #f (_fun #:in-original-place? #t _long -> _pointer)))

(define (main)
  (define ps
    (for/list ([j (in-range 4)])
      (place pch
             ;; Start a thread that keep having to wait on the original place:
             (thread
              (lambda ()
                (let loop ()
                  (scheme_malloc_atomic 10)
                  (loop))))
             ;; Create a lot of message channels to trigger master GC:
             (for ([i (in-range 100000)])
               (place-channel))
             (printf "done\n"))))
  (for-each place-wait ps))
