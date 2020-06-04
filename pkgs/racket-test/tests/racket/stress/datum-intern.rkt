#lang racket/base

(define (check label mk)
  (collect-garbage)
  (define pre (current-memory-use))
  (let loop ([tries 0] [pre pre])
    (printf "~a ~a @ ~s\n" label tries pre)
    (for ([j 100])
      (for ([i (in-range 10000)])
        (datum-intern-literal (mk i j))))
    (collect-garbage)
    (define post (current-memory-use))
    (when (post . > . pre)
      (when (= tries 20)
        (error 'check "didn't find non-growing iteration"))
      (loop (add1 tries) post))))

(pseudo-random-generator->vector (current-pseudo-random-generator))
(check "strings" (lambda (i j) (format "string-~a-~a" i j)))
(check "flonums" (lambda (i j) (random)))


(module+ test
  (module config info
    (define random? #t)))
