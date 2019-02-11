#lang racket/base
(require "../common/check.rkt"
         "../common/class.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "output-port.rkt"
         "parameter.rkt"
         "count.rkt")

(provide write-special
         write-special-avail*
         write-special-evt
         port-writes-special?)

(define/who (port-writes-special? o)
  (check who output-port? o)
  (let ([o (->core-output-port o)])
    (and (method core-output-port o write-out-special) #t)))

(define (do-write-special who v orig-o #:retry? retry?)
  (check who output-port? orig-o)
  (let port-loop ([o orig-o] [extra-count-os null])
    (let ([o (->core-output-port o)])
      (define write-out-special (method core-output-port o write-out-special))
      (unless write-out-special
        (raise-arguments-error who
                               "port does not support special values"
                               "port" orig-o))
      (cond
        [(output-port? write-out-special)
         (port-loop write-out-special (cons o extra-count-os))]
        [else
         (let loop ()
           (start-atomic)
           (define r (write-out-special o v (not retry?) #f))
           (let result-loop ([r r])
             (cond
               [(not r)
                (end-atomic)
                (if retry?
                    (loop)
                    #f)]
               [(evt? r)
                (end-atomic)
                (and retry?
                     (result-loop (sync r)))]
               [else
                (port-count-all! o extra-count-os 1 #"x" 0)
                (end-atomic)
                #t])))]))))

(define/who (write-special v [o (current-output-port)])
  (do-write-special who #:retry? #t v o))

(define/who (write-special-avail* v [o (current-output-port)])
  (do-write-special who #:retry? #f v o))

(define/who (write-special-evt v [o (current-output-port)])
  (check who output-port? o)
  (let ([o (->core-output-port o)])
    (define get-write-special-evt (method core-output-port o get-write-special-evt))
    (unless get-write-special-evt
      (raise-arguments-error who
                             "port does not support special-value events"
                             "port" o))
    (get-write-special-evt o v)))
