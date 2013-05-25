#lang racket
(require "scm.rkt"
         "retry.rkt")

(define current-monitoring-interval-seconds
  (make-parameter 60))

(define (monitor-scm repos start-rev notify-newer! notify-user!)
  (define (monitor-w/o-wait prev-rev)
    (define new-revs
      (scm-revisions-after prev-rev repos))
    (match new-revs
      [(list)
       ; There has not yet been more revisions
       (monitor prev-rev)]
      [(cons new-rev newer)
       (scm-update repos)
       ; Notify of newer ones
       (notify-newer! newer)
       ; There was a commit that we care about. Notify, then recur
       (retry-until-success
        (format "Notifying of revision ~a" new-rev)
        (notify-user! prev-rev new-rev))       
       (monitor new-rev)]))
  (define (monitor prev-rev)
    (sleep (current-monitoring-interval-seconds))
    (monitor-w/o-wait prev-rev))
  (monitor-w/o-wait start-rev))

(provide/contract
 [current-monitoring-interval-seconds 
  (parameter/c exact-nonnegative-integer?)]
 [monitor-scm
  (path-string? exact-nonnegative-integer? 
           ((listof exact-nonnegative-integer?) . -> . void)
           (exact-nonnegative-integer? exact-nonnegative-integer? . -> . void)
           . -> . any)])
