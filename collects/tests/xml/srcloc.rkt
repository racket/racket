#lang racket
(require xml
         tests/eli-tester)

(define p (open-input-string "abcdef"))
(port-count-lines! p)
(for ([x (in-range 0 6)]) (read-char p))
(define-values (line col pos) (port-next-location p))

(define exn
  (with-handlers ((exn:fail? values))
    (read-xml/element p)))

(test
 pos => 7
 (srcloc-position (first ((exn:srclocs-accessor exn) exn))) => 7)
