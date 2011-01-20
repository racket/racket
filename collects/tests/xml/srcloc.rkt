#lang racket
(require xml
         tests/eli-tester)

(define (ppos p) 
  (define-values (line col pos) (port-next-location p))
  pos)

; open-input-string is broken
(define (the-test first? second?)
  (define is (open-input-string "abcdef"))
  
  (test #:failure-prefix (format "~a ~a" first? second?)
        (test
         (when first? (port-count-lines! is))
         (ppos is) => 1 (read-char is)
         (ppos is) => 2 (read-char is)
         (ppos is) => 3 (read-char is)
         (when second? (port-count-lines! is))
         (ppos is) => 4 (read-char is)
         (ppos is) => 5 (read-char is)
         (ppos is) => 6 (read-char is))))
(test (the-test #f #f)
      (the-test #t #f)
      (the-test #f #t)
      (the-test #t #t))

(define p (open-input-string "abcdef"))
(for ([x (in-range 0 6)]) (read-char p))
(define pos (ppos p))

(define exn
  (with-handlers ((exn:fail? values))
    (read-xml/element p)))

(test
 pos => 7
 (srcloc-position (first ((exn:srclocs-accessor exn) exn))) => 7)