#lang racket/base

;; An improved `time' variant: better output, and repetitions with averages
(provide time*)

(require racket/list)

(define (time/proc thunk times)
  (define throw
    (if (<= times 0)
      (error 'time "bad count: ~e" times)
      (floor (* times 2/7))))
  (define results #f)
  (define timings '())
  (define (run n)
    (when (<= n times)
      (when (> times 1) (printf "; run #~a..." n) (flush-output))
      (let ([r (call-with-values (lambda () (time-apply thunk '())) list)])
        (set! results (car r))
        (set! timings (cons (cdr r) timings))
        (when (> times 1)
          (printf " ->")
          (if (null? results)
            (printf " (0 values returned)")
            (begin (printf " ~.s" (car results))
                   (for ([r (in-list (cdr results))]) (printf ", ~s" r))
                   (newline))))
        (run (add1 n)))))
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (run 1)
  (set! timings (sort timings < #:key car)) ; sort by cpu-time
  (set! timings (drop timings throw))       ; throw extreme bests
  (set! timings (take timings (- (length timings) throw))) ; and worsts
  (set! timings (let ([n (length timings)]) ; average
                  (map (lambda (x) (round (/ x n))) (apply map + timings))))
  (let-values ([(cpu real gc) (apply values timings)])
    (when (> times 1)
      (printf "; ~a runs, ~a best/worst removed, ~a left for average:\n"
              times throw (- times throw throw)))
    (printf "; cpu time: ~sms = ~sms + ~sms gc; real time: ~sms\n"
            cpu (- cpu gc) gc real))
  (apply values results))

(define-syntax time*
  (syntax-rules ()
    [(_ n expr0 expr ...) (time/proc (lambda () expr0 expr ...) n)]
    [(_ expr0 expr ...)   (time/proc (lambda () expr0 expr ...) 1)]))
