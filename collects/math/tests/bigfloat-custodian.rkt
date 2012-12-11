#lang racket/base

;; Not crashing means success for this stress test:

(let loop ([i 0] [l null])
  (unless (= i 100)
    (displayln "next")
    (let ([l (if (zero? (random 5))
                 (begin
                   (printf "shutdown\n")
                   (for-each custodian-shutdown-all l)
                   null)
                 l)])
      (let ([c (make-custodian)])
        (parameterize ([current-custodian c])
          (for ([i (random 10)]) (thread (lambda () (sleep 10))))
          (parameterize ([current-namespace (make-base-namespace)])
            (dynamic-require `math/bigfloat #f)))
        (loop (add1 i) (cons c l))))))
