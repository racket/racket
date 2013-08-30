#lang racket/base

(require racket/string
         unstable/syntax unstable/logging syntax/parse
         data/queue
         "../utils/tc-utils.rkt")

(provide log-optimization log-missed-optimization log-optimization-info
         log-opt log-opt-info
         with-tr-logging-to-queue
         (struct-out log-entry)
         (struct-out opt-log-entry)
         (struct-out missed-opt-log-entry)
         (struct-out info-log-entry))

;;--------------------------------------------------------------------

(define TR-logging-level 'debug)
(define TR-logger (make-logger 'TR-optimizer (current-logger)))

(define (emit-log-message l)
  (log-message TR-logger TR-logging-level (format-log-entry l) l))

;; producing logs can be expensive, don't do it if no-one's listening
;; to the logs
(define (anyone-listening?) (log-level? TR-logger TR-logging-level))

;; msg is for consumption by the DrRacket tool
(struct log-entry (kind msg stx located-stx pos) #:prefab)
;; for optimizations only (not missed optimizations, those are below)
(struct opt-log-entry log-entry () #:prefab)


(define (log-optimization kind msg stx)
  (when (anyone-listening?)
    (emit-log-message
     (opt-log-entry kind msg stx (locate-stx stx) (syntax-position stx)))))

(define-syntax-rule (log-opt kind msg)
  (log-optimization kind msg this-syntax))

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

(define (log-missed-optimization kind msg stx [irritants '()])
  (when (anyone-listening?)
    (let (;; for convenience, if a single irritant is given, wrap it in a list
          ;; implicitly
          [irritants (if (list? irritants) irritants (list irritants))])
      (emit-log-message
       (missed-opt-log-entry kind msg
                             stx (locate-stx stx) (syntax-position stx)
                             irritants '() 1)))))


;;--------------------------------------------------------------------

;; Log information that is neither an optimization nor a missed optimization,
;; but can come in handy, in combination with other information, to detect
;; near misses.

(struct info-log-entry log-entry () #:prefab)

(define (log-optimization-info kind stx)
  (when (anyone-listening?)
    (emit-log-message
     ;; no actual message, since it's not meant for user consumption
     (info-log-entry kind "" stx (locate-stx stx) (syntax-position stx)))))


(define-syntax-rule (log-opt-info kind)
  (log-optimization-info kind this-syntax))

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
                     ""))]
        [(info-log-entry? entry)
         (format "TR info: ~a" msg)]))

;;--------------------------------------------------------------------

(define (with-tr-logging-to-queue queue thunk)
  (with-intercepted-logging
   (lambda (l)
     (enqueue! queue
      (string-trim (vector-ref l 1) "TR-optimizer: "))) ; remove logger prefix
   thunk
   'debug 'TR-optimizer))
