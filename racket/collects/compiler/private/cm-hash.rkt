#lang racket/base

(provide install-module-hashes!)

(define (install-module-hashes! s [start 0] [len (bytes-length s)])
  (define vlen (bytes-ref s (+ start 2)))
  (define vmlen (bytes-ref s (+ start 3 vlen)))
  (define mode (integer->char (bytes-ref s (+ start 4 vlen vmlen))))
  (case mode
    [(#\B)
     ;; A linklet bundle:
     (define h (sha1-bytes s start (+ start len)))
     ;; Write sha1 for bundle hash:
     (bytes-copy! s (+ start 5 vlen vmlen) h)]
    [(#\D)
     ;; A linklet directory. The format starts with <count>,
     ;; and then it's <count> records of the format:
     ;;  <name-len> <name-bytes> <bund-pos> <bund-len> <left-pos> <right-pos>
     (define (read-num rel-pos)
       (define pos (+ start rel-pos))
       (integer-bytes->integer s #t #f pos (+ pos 4)))
     (define count (read-num (+ 5 vlen vmlen)))
     (for/fold ([pos (+ 9 vlen vmlen)]) ([i (in-range count)])
       (define pos-pos (+ pos 4 (read-num pos)))
       (define bund-start (read-num pos-pos))
       (define bund-len (read-num (+ pos-pos 4)))
       (install-module-hashes! s (+ start bund-start) bund-len)
       (+ pos-pos 16))
     (void)]
    [else 
     ;; ?? unknown mode
     (void)]))

