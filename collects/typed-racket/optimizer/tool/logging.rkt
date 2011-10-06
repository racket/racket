#lang racket/base

(require "utilities.rkt"
         typed-racket/optimizer/logging
         unstable/logging unstable/syntax racket/match)

(provide with-intercepted-opt-logging)

;; Intercepts both TR optimizer logging and mzc optimizer logging.
;; Interceptor accepts log-entry structs.
(define (with-intercepted-opt-logging interceptor thunk)
  (with-intercepted-logging #:level 'debug
    (lambda (l)
      (cond [(log-message-from-tr-opt? l)
             ;; From TR, use the log-entry struct provided.
             (interceptor (cdr (vector-ref l 2)))]
            ;; We look at the message to tell if it's from mzc.
            [(log-message-from-mzc-opt? (vector-ref l 1))
             ;; From mzc, create a log-entry from the info.
             (interceptor (mzc-opt-log-message->log-entry (vector-ref l 1)))]))
    thunk))


(define mzc-optimizer-regexp "^mzc optimizer: ")
(define success-regexp (string-append mzc-optimizer-regexp "inlining: "))
(define failure-regexp (string-append mzc-optimizer-regexp "no inlining: "))

(define (log-message-from-mzc-opt? l)
  (regexp-match mzc-optimizer-regexp l))

;; String (message from the mzc optimizer) -> log-entry
(define (mzc-opt-log-message->log-entry l)
  (define forged-stx (inlining-event->forged-stx l))
  (cond [(regexp-match success-regexp l)
         (inlining-success->log-entry forged-stx)]
        [(regexp-match failure-regexp l)
         (inlining-failure->log-entry forged-stx)]
        [else
         (error "Unknown log message type" l)]))

(define inlining-event-regexp
  ;; Last bit is `generated?'. We don't care about that.
  ;; The middle elements of the vector are numbers of #f.
  "#\\(([^ ]+) #<path:(.+)> ([^ ]+) ([^ ]+) ([^ ]+) ([^ ]+) [^ ]+\\)")

(define (inlining-event->forged-stx l)
  (match (regexp-match inlining-event-regexp l)
    [`(,all ,name ,path ,line ,col ,pos ,span)
     (datum->syntax #'here (string->symbol name)
                    (list path
                          (string->number line)
                          (string->number col)
                          (string->number pos)
                          (string->number span)))]
    [_ (error "ill-formed inlining log entry" l)]))

(define (inlining-success->log-entry forged-stx)
  (opt-log-entry "Inlining" "Inlining"
                 forged-stx ; stx
                 forged-stx ; located-stx
                 (syntax-position forged-stx)))
(define (inlining-failure->log-entry forged-stx)
  (missed-opt-log-entry "Failed Inlining" "Failed Inlining"
                        forged-stx
                        forged-stx
                        (syntax-position forged-stx)
                        '() '() 1)) ; irritants, merged-irritants badness
