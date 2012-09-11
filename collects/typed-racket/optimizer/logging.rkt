#lang racket/base

(require racket/set racket/string racket/match racket/list
         unstable/syntax unstable/logging
         "../utils/tc-utils.rkt")

(provide log-optimization log-missed-optimization
         log-message-from-tr-opt?
         with-tr-logging-to-port
         (struct-out log-entry)
         (struct-out opt-log-entry)
         (struct-out missed-opt-log-entry))

;;--------------------------------------------------------------------

(define TR-logging-level 'debug)
(define TR-logger (make-logger 'TR-optimizer (current-logger)))

(define (emit-log-message l)
  (log-message TR-logger TR-logging-level
               (format-log-entry l)
               (cons optimization-log-key l)))

;; producing logs can be expensive, don't do it if no-one's listening
;; to the logs
(define (anyone-listening?) (log-level? TR-logger TR-logging-level))

;; to identify log messages that come from the optimizer
;; to be stored in the data section of log messages
;; external tools/scripts (like the test harness) can look for it
;; since this is used across phases, can't be a gensym
(define optimization-log-key 'log-message-coming-from-the-TR-optimizer)

;; msg is for consumption by the DrRacket tool
(struct log-entry (kind msg stx located-stx pos provenance) #:prefab)
;; for optimizations only (not missed optimizations, those are below)
(struct opt-log-entry log-entry () #:prefab)


(define (log-optimization kind msg stx)
  (when (anyone-listening?)
    (emit-log-message
     (opt-log-entry kind msg stx (locate-stx stx) (syntax-position stx)
                    'typed-racket))))

;;--------------------------------------------------------------------

;; Keep track of optimizations that "almost" happened, with the intention
;; of reporting them to the user.
;; This is meant to help users understand what hurts the performance of
;; their programs.

;; badness : Integer. crude measure of how severe the missed optimizations are
;;  currently, it's simply a count of how many missed optimizations occur
;;  within a given syntax object
;; irritants are the original, potentially indirect causes of the miss
;; merged-irritants are intermediate steps between stx and an irritant
;; they are not actual irritants anymore because they were the stx for a miss
;; that got merged into this miss. we need to keep them around to detect
;; future potential merges.
(struct missed-opt-log-entry log-entry
        (irritants merged-irritants badness)
        #:prefab)

;; Attempts to merge the incoming missed optimization with existing ones.
;; Otherwise, adds the new one to the log.
(define (log-missed-optimization kind msg stx [irritants '()])
  (when (anyone-listening?)
    (let (;; for convenience, if a single irritant is given, wrap it in a list
          ;; implicitly
          [irritants (if (list? irritants) irritants (list irritants))])
      (emit-log-message
       (missed-opt-log-entry kind msg
                             stx (locate-stx stx) (syntax-position stx)
                             'typed-racket
                             irritants '() 1)))))


;;--------------------------------------------------------------------

(define (line+col->string stx)
  (let ([line (syntax-line stx)]
        [col  (syntax-column stx)])
    (if (and line col)
        (format "~a:~a" line col)
        "(no location)")))

(define (format-irritants irritants)
  (if (null? irritants)
      ""
      (format " -- caused by: ~a"
              (string-join
               (map (lambda (irritant)
                      (let ([irritant (locate-stx irritant)])
                        (format "~a ~s"
                                (line+col->string irritant)
                                (syntax->datum irritant))))
                    (sort irritants <
                          #:key (lambda (x)
                                  (or (syntax-position x) 0))))
               ", "))))

;; For command-line printing purposes.
;; Not as user friendly as what's produced by the DrRacket tool.
(define (format-log-entry entry)
  (define stx (log-entry-located-stx entry))
  (define msg
    (format "~a ~a ~s -- ~a"
            (syntax-source-file-name stx)
            (line+col->string stx)
            (syntax->datum stx)
            (log-entry-kind entry)))
  (cond [(opt-log-entry? entry)
         (format "TR opt: ~a" msg)]
        [(missed-opt-log-entry? entry)
         (define badness (missed-opt-log-entry-badness entry))
         (format "TR missed opt: ~a~a~a"
                 msg
                 (format-irritants (missed-opt-log-entry-irritants entry))
                 (if (> badness 1)
                     (format " (~a times)" badness)
                     ""))]))

;;--------------------------------------------------------------------

(define (log-message-from-tr-opt? l)
  (let ([data (vector-ref l 2)])
    (and (pair? data)
         (eq? (car data) optimization-log-key))))

(define (with-tr-logging-to-port port thunk)
  (with-intercepted-logging
   (lambda (l)
     (displayln ; print log message
      (string-trim (vector-ref l 1) "TR-optimizer: ") ; remove logger prefix
      port))
   thunk
   'debug 'TR-optimizer))
