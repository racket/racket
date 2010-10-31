#lang racket

(require "sexp-trie.rkt")

(define trace-path
  (command-line #:args (path) path))

(define (time-trace insert lookup empty)
  (call-with-input-file trace-path
    (λ (trace)
      (time
       (let loop ([set empty])
        (match (read trace)
          [(? eof-object?)
           (void)]
          [(cons 'lookup t)
           (begin
             (lookup t set)
             (loop set))]
          [(cons 'insert t)
           (loop (insert t set))]))))))

(printf "Hash\n")
(time-trace (λ (t s) (hash-set! s t #t) s)
            (λ (t s) (hash-ref s t #f))
            (make-hash))

(printf "Trie\n")
(time-trace (λ (t s) (insert t #t s)) lookup empty-sexp-trie)