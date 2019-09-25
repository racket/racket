;; This file is meant to be run in Scheme

(load "setup.rktl")
(load "config.rktl")

;; Not inlined:
(define (f x) x)

(show '----------------------------------------)

;; Baseline loop performance
(show 'loop)
(show
 (times
  (let loop ([i N])
    (if (zero? i)
        0
        (loop (sub1 i))))))

;; How much does it const to allocate a pair each time, since
;;  pushing a continuation attachment will do that?
;; Answer: makes the loop about 25% slower
(show 'pair-loop)
(show
 (times
  (let loop ([i N] [p #f])
    (if (zero? i)
        p
        (loop (sub1 i) (cons i i))))))

;; Baseline for continuation-growing non-tail recursion (note `M` instead of `N`)
(show 'nontail)
(show
 (times
  (let loop ([i M])
    (if (zero? i)
        0
        (add1 (loop (sub1 i)))))))

;; Baseline for instantiating the continuation in a non-tail recursion
;; Most of the time here is GC time, because
;;  this creates a chain of continuation records
(show 'k-nontail)
(show
 (times
  (let loop ([i M])
    (if (zero? i)
        0
        (add1
         (call/cc
          (lambda (k)
            (loop (sub1 i)))))))))

;; How much more does it add to allocate pairs?
(show 'pairs-nontail)
(show
 (times
  (let loop ([i M] [p #f])
    (if (zero? i)
        (car p)
        (add1 (loop (sub1 i) (cons i (cons i p))))))))

;; Baseline for non-tail calls to an unknown function `f`
(show 'indirect-nontail-argument-loop)
(show
 (times
  (let ([f f])
    (let loop ([i N])
      (if (zero? i)
          0
          (loop (f (sub1 i))))))))
