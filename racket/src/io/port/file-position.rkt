#lang racket/base
(require "../common/check.rkt"
         "../common/class.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "check.rkt")

(provide file-position
         file-position*

         do-simple-file-position)

(define/who file-position
  (case-lambda
    [(p)
     (do-simple-file-position who p
                              (lambda ()
                                (raise
                                 (exn:fail:filesystem
                                  (string-append
                                   "file-position: the port's current position is not known\n port: "
                                   ((error-value->string-handler) p (error-print-width)))
                                  (current-continuation-marks)))))]
    [(p pos)
     (unless (or (input-port? p) (output-port? p))
       (raise-argument-error who "port?" p))
     (check who
            (lambda (p) (or (exact-nonnegative-integer? p) (eof-object? p)))
            #:contract "(or/c exact-nonnegative-integer? eof-object?)"
            pos)
     (let ([cp (cond
                 [(input-port? p) (->core-input-port p)]
                 [else (->core-output-port p)])])
       (define file-position (method core-port cp file-position))
       (cond
         [(and (procedure? file-position) (procedure-arity-includes? file-position 2))
          (atomically
           (check-not-closed who cp)
           (file-position cp pos))]
         [else
          (raise-arguments-error who
                                 "setting position allowed for file-stream and string ports only"
                                 "port" p
                                 "position" pos)]))]))

(define/who (file-position* p)
  (do-simple-file-position who p (lambda () #f)))

(define (do-simple-file-position who orig-p fail-k)
  (let ([p (cond
             [(input-port? orig-p) (->core-input-port orig-p)]
             [(output-port? orig-p) (->core-output-port orig-p)]
             [else (raise-argument-error who "port?" orig-p)])])
    (start-atomic)
    (check-not-closed who p)
    (define file-position (method core-port p file-position))
    (cond
      [(or (input-port? file-position)
           (output-port? file-position))
       (end-atomic)
       (do-simple-file-position who file-position fail-k)]
      [else
       (define pos (or (and file-position
                            (file-position p))
                       (get-core-port-offset p)))
       (end-atomic)
       (or pos (fail-k))])))
