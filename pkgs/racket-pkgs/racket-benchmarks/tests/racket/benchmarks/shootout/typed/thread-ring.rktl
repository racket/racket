;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;;
;; Uses Racket threads

(require racket/cmdline)

;; Each thread runs this loop:
(: run (Integer Thread -> Void))
(define (run id next)
  (let ([v (assert (thread-receive) exact-integer?)])
    (cond
     [(zero? v) ;; Done
      (printf "~a\n" id)
      (exit)]
     [else ;; Keep going
      (thread-send next (sub1 v))
      (run id next)])))
                       

(let ([n (command-line #:args (n) (assert (string->number (assert n string?)) exact-integer?))])
  ;; The original thread is #503. Create the rest:
  (let ([t1 (for/fold: : Thread
                       ([next : Thread (current-thread)])
                       ([id : Integer (in-range 502 0 -1)])
              (thread (lambda () (run id next))))])
    ;; Start:
    (thread-send t1 n)
    (run 503 t1)))
