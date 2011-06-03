#lang racket/base

(require racket/set racket/string racket/match
         unstable/syntax)

(provide log-optimization log-missed-optimization
         optimization-log-key
         print-log clear-log)

(define (line+col->string stx)
  (let ([line (syntax-line stx)]
        [col  (syntax-column stx)])
    (if (and line col)
        (format "~a:~a" line col)
        "(no location)")))

(struct log-entry (msg line col) #:transparent)

;; to identify log messages that come from the optimizer
;; to be stored in the data section of log messages
;; external tools/scripts (like the test harness) can look for it
;; since this is used across phases, can't be a gensym
(define optimization-log-key 'log-message-coming-from-the-TR-optimizer)

;; we keep track of log entries, to avoid repetitions that would be
;; caused by traversing the same syntax multiple times (which is not
;; a problem per se)
(define log-so-far (set))

(define (gen-log-message msg stx)
  (format "~a ~a ~a -- ~a"
          (syntax-source-file-name stx)
          (line+col->string stx)
          (syntax->datum stx)
          msg))

(define (do-logging msg stx)
  (let* ([new-message (gen-log-message msg stx)]
         [new-entry (log-entry new-message
                               (syntax-line stx)
                               (syntax-column stx))])
    (unless (set-member? log-so-far new-entry)
      (set! log-so-far (set-add log-so-far new-entry)))))

;; once the optimizer is done, we sort the log according to source
;; location, then print it
(define (print-log)
  (define logger (current-logger))
  ;; add missed optimizations messages to the log, now that we know all of them
  (for-each (lambda (x) (do-logging (missed-optimization-msg x)
                                    (missed-optimization-stx x)))
            missed-optimizations-log)
  (for-each (lambda (x) (log-message logger 'warning (log-entry-msg x)
                                     optimization-log-key))
            (sort (set->list log-so-far)
                  (lambda (x y)
                    (match* (x y)
                      [((log-entry msg-x line-x col-x)
                        (log-entry msg-y line-y col-y))
                       (cond [(not (or line-x line-y))
                              ;; neither have location, sort by message
                              (string<? msg-x msg-y)]
                             [(not line-y) #f]
                             [(not line-x) #t]
                             [else
                              ;; both have location
                              (let* ([loc-x (+ (* 1000 line-x) col-x)]
                                     ;; 1000 is a conservative bound
                                     [loc-y (+ (* 1000 line-y) col-y)])
                                (cond [(= loc-x loc-y)
                                       ;; same location, sort by message
                                       (string<? msg-x msg-y)]
                                      ;; sort by source location
                                      [else (< loc-x loc-y)]))])])))))
(define (clear-log)
  (set! log-so-far (set))
  (set! missed-optimizations-log '()))

(define (log-optimization kind stx) (do-logging kind stx))


;; Keep track of optimizations that "almost" happened, with the intention
;; of reporting them to the user.
;; This is meant to help users understand what hurts the performance of
;; their programs.

;; badness : Integer. crude measure of how severe the missed optimizations are
;;  currently, it's simply a count of how many missed optimizations occur
;;  within a given syntax object
(struct missed-optimization (msg stx irritants [badness #:mutable])
        #:transparent)

(define missed-optimizations-log '())

(define (log-missed-optimization kind stx [irritants '()])
  ;; for convenience, if a single irritant is given, wrap it in a list
  ;; implicitly
  (let* ([irritants (if (list? irritants) irritants (list irritants))]
         [msg (if (not (null? irritants))
                  (format "~a -- caused by: ~a"
                          kind
                          (string-join
                           (map (lambda (irritant)
                                  (format "~a ~a"
                                          (line+col->string irritant)
                                          (syntax->datum irritant)))
                                irritants)
                           ", "))
                  kind)])
    (set! missed-optimizations-log
          (cons (missed-optimization msg stx irritants 1)
                missed-optimizations-log))))
