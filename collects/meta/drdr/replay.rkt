#lang scheme
(require (prefix-in scheme: scheme)
         "formats.ss"
         "status.ss")

(define replay-event
  (match-lambda
    [(struct stdout (bs))
     (fprintf (current-output-port) "~a\n" bs)]
    [(struct stderr (bs))
     (fprintf (current-error-port) "~a\n" bs)]))

(define (replay-status s)
  (for-each replay-event (status-output-log s))
  #;(when (timeout? s)
    (fprintf (current-error-port) "[replay-log] TIMEOUT!\n"))
  #;(when (exit? s)
    (fprintf (current-error-port) "[replay-log] Exit code: ~a\n" (exit-code s)))
  #;(printf "[replay-log] Took ~a\n"
          (format-duration-ms (status-duration s)))
  (replay-exit-code s))

(define (replay-exit-code s)
  (when (exit? s)
    (scheme:exit (exit-code s))))

(provide/contract
 [replay-exit-code (status? . -> . void)]
 [replay-status (status? . -> . void)])
