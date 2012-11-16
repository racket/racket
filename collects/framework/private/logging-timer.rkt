#lang racket/base

(require racket/gui/base
         racket/class
         (for-syntax racket/base))

(define timeline-logger (make-logger 'timeline (current-logger)))

(provide logging-timer%
         (struct-out timeline-info)
         log-timeline)

(define logging-timer%
  (class timer%
    (init notify-callback)
    (define name (object-name notify-callback))
    (define wrapped-notify-callback
      (λ ()
        (log-timeline
         (format "~a timer fired" name)
         (notify-callback))))
    (super-new [notify-callback wrapped-notify-callback])
    (define/override (start msec [just-once? #f])
      (log-timeline (format "~a timer started; msec ~s just-once? ~s" name msec just-once?))
      (super start msec just-once?))))


(define-syntax (log-timeline stx)
  (syntax-case stx ()
    [(_ info-string expr)
     #'(log-timeline/proc 
        (and (log-level? timeline-logger 'debug)
             info-string)
        (λ () expr))]
    [(_ info-string)
     #'(log-timeline/proc 
        (and (log-level? timeline-logger 'debug)
             info-string)
        #f)]))

(define (log-timeline/proc info expr)
  (define start-time (current-inexact-milliseconds))
  (when info
    (log-message timeline-logger 'debug 
                 (format "~a start" info)
                 (timeline-info (if expr 'start 'once)
                                (current-process-milliseconds)
                                start-time)))
  (when expr 
    (begin0
      (expr)
      (when info
        (define end-time (current-inexact-milliseconds))
        (log-message timeline-logger 'debug 
                     (format "~a   end; delta ms ~a" info (- end-time start-time))
                     (timeline-info start-time
                                    end-time
                                    (current-inexact-milliseconds)))))))


;; what : (or/c 'start 'once flonum)
;;    flonum means that this is an 'end' event and there should be
;;    a start event corresponding to it with that milliseconds 
;; process-milliseconds : fixnum
;; milliseconds : flonum -- time of this event
(struct timeline-info (what process-milliseconds milliseconds) #:transparent)
