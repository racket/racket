#lang racket
(require unstable/byte-counting-port
         tests/eli-tester)

(define name (gensym))
(define cp (make-byte-counting-port name))
(define (test-cp cp)
  (for/fold ([l 0])
    ([i (in-range 100)])
    (define n (random 25))
    (test
     (file-position cp) => l
     (write-bytes (make-bytes n) cp))
    (+ l n)))
(test
 (object-name cp) => name
 (test-cp cp)
 (test-cp (make-byte-counting-port)))