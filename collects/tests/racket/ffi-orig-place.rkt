#lang racket
(require ffi/unsafe)

(module+ test
  (main))

;; Make sure that `#:in-original-place?' doesn't lead to deadlock:

(define scheme_malloc_atomic
  (get-ffi-obj 'GC_malloc_atomic #f (_fun #:in-original-place? #t _long -> _pointer)))

(define (x-main)
  (define ps
    (for/list ([j (in-range 4)])
      (define p
        (place pch
               (define j (place-channel-get pch))
               ;; Start a thread that keep having to wait on the original place:
               (thread
                (lambda ()
                  (let loop ()
                    (scheme_malloc_atomic 10)
                    (loop))))
               ;; Create a lot of message channels to trigger master GC:
               (for ([i (in-range 10000)])
                 (place-channel))
               (printf "done\n")
               ;; Helps check exit handling:
               (when (even? j) (exit 1))))
      (place-channel-put p j)
      p))
  
  (for-each place-wait ps))

(define (main)
  (for ([i 5])
    (printf "iter ~a\n" i)
    (x-main)))
