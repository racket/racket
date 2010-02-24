#lang scheme
(require "svn.ss"
         "retry.ss")

(define current-monitoring-interval-seconds
  (make-parameter 60))

(define (monitor-svn repos start-rev notify-newer! notify-user!)
  (define (monitor-w/o-wait prev-rev)
    (define all-logs
      (svn-revision-logs-after prev-rev repos))
    (define new-logs
      (filter-not 
       (lambda (l) (= (svn-rev-log-num l) prev-rev))
       all-logs))
    (match new-logs
      [(list)
       ; There has not yet been more revisions
       (monitor prev-rev)]
      [(cons log newer)
       (define new-rev (svn-rev-log-num log))
       ; Notify of newer ones
       (notify-newer! newer)
       ; There was a commit that we care about. Notify, then recur
       (retry-until-success
        (format "Notifying of revision ~a" new-rev)
        (notify-user! prev-rev new-rev log))       
       (monitor new-rev)]))
  (define (monitor prev-rev)
    (sleep (current-monitoring-interval-seconds))
    (monitor-w/o-wait prev-rev))
  (monitor-w/o-wait start-rev))

(provide/contract
 [current-monitoring-interval-seconds 
  (parameter/c exact-nonnegative-integer?)]
 [monitor-svn 
  (string? exact-nonnegative-integer? 
           ((listof svn-rev-log?) . -> . void)
           (exact-nonnegative-integer? exact-nonnegative-integer? svn-rev-log? . -> . void)
           . -> . any)])