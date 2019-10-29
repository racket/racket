;; This file is meant to be run in Scheme

(load "setup.rktl")
(load "config.rktl")

(show '----------------------------------------)

;; Capturing continuations
(show 'capture)
(show
 (times
  (let loop ([i M] [k #f])
    (if (zero? i)
        k
        (loop (sub1 i) (call/cc (lambda (k) k)))))))


;; Applying a continuation
(show 'apply)
(show
 (times
  (let ([loop #f])
    (let ([i (call/cc
              (lambda (k)
                (set! loop k)
                M))])
      (if (zero? i)
          0
          (loop (sub1 i)))))))

