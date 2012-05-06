#lang racket
(require (prefix-in racket: racket)
         "formats.rkt"
         "status.rkt")

(define replay-event
  (match-lambda
    [(struct stdout (bs)) (printf "~a\n" bs)]
    [(struct stderr (bs)) (eprintf "~a\n" bs)]))

(define (replay-status s)
  (for-each replay-event (status-output-log s))
  #;(when (timeout? s)
    (eprintf "[replay-log] TIMEOUT!\n"))
  #;(when (exit? s)
    (eprintf "[replay-log] Exit code: ~a\n" (exit-code s)))
  #;(printf "[replay-log] Took ~a\n"
          (format-duration-ms (status-duration s)))
  (replay-exit-code s))

(define (replay-exit-code s)
  (when (exit? s)
    (racket:exit (exit-code s))))

(provide/contract
 [replay-exit-code (status? . -> . void)]
 [replay-status (status? . -> . void)])
