#lang racket

(require (prefix-in d: "list-machine/list-machine-base.rkt"))

(define (f make-term)
  (define (depth x)
    (let loop ([x x])
      (cond
        [(list? x)
         (if (empty? x)
             0
             (+ 1 (apply max (map loop x))))]
        [else 0])))
  (define depths
    (build-list 1000
                (λ (i)
                  (when (zero? (modulo i 20))
                    (printf "~a " i)
                    (flush-output))
                  (depth (make-term)))))
  (printf "done\n")
  (values (/ (apply + depths) (length depths) 1.)
          (apply max depths)))

(f d:generate-enum-term)