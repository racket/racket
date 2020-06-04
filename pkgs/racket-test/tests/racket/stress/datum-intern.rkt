#lang racket/base

(define (check label mk)
  (let loop ([tries 0])
    (printf "~a ~a\n" label tries)
    (collect-garbage)
    (define pre (current-memory-use))
    (for ([j 100])
      (for ([i (in-range 10000)])
        (datum-intern-literal (mk i j))))
    (collect-garbage)
    (when ((current-memory-use) . > . pre)
      (when (= tries 10)
        (error 'check "didn't find non-growing iteration"))
      (loop (add1 tries)))))

(pseudo-random-generator->vector (current-pseudo-random-generator))
(check "strings" (lambda (i j) (format "string-~a-~a" i j)))
(check "flonums" (lambda (i j) (random)))
