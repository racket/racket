#lang racket/load

(displayln "The printouts below are designed to trick drdr into graphing them;")
(displayln "they aren't times, but memory usage.")

(define (print-memory)
  (for ((i 10))
    (collect-garbage))
  (let ([n (current-memory-use)])
   (printf "cpu time: ~a real time: ~a gc time: ~a\n" n n n)))



(print-memory)
(module tr typed/racket)
(print-memory)
(require 'tr)
(print-memory)
