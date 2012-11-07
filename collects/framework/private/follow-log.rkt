#lang racket/base

(require racket/list
         racket/class
         racket/match
         racket/pretty
         racket/gui/base
         framework/private/logging-timer)

#|

This file sets up a log receiver and then
starts up DrRacket. It catches log messages and 
organizes them on event boundaries, printing
out the ones that take the longest
(possibly dropping those where a gc occurs)

The result shows, for each gui event, the
log messages that occured during its dynamic
extent as well as the number of milliseconds
from the start of the gui event before the
log message was reported.

|#


(define lr (make-log-receiver (current-logger)
                              'debug 'racket/engine
                              'debug 'GC
                              'debug 'gui-event
                              'debug 'framework/colorer
                              'debug 'timeline))

(define top-n-events 50)
(define drop-gc? #t)
(define start-right-away? #f)

(define done-chan (make-channel))
(define start-chan (make-channel))
(void 
 (thread
  (λ ()
    (let loop () 
      (sync start-chan)
      (let loop ([events '()])
        (sync
         (handle-evt
          lr
          (λ (info)
            (loop (cons info events))))
         (handle-evt
          done-chan
          (λ (resp-chan)
            (channel-put resp-chan events)))))
      (loop)))))

(define controller-frame-eventspace (make-eventspace))
(define f (parameterize ([current-eventspace controller-frame-eventspace])
            (new frame% [label "Log Follower"])))
(define sb (new button% [label "Start"] [parent f]
               [callback
                (λ (_1 _2)
                  (sb-callback))]))
(define db (new button% [label "Stop && Dump"] [parent f] [enabled #f]
                [callback
                 (λ (_1 _2)
                   (define resp (make-channel))
                   (channel-put done-chan resp)
                   (show-results (channel-get resp))
                   (send db enable #f)
                   (send sb enable #t))]))
(define (sb-callback)
  (send sb enable #f)
  (send db enable #t)
  (channel-put start-chan #t))
(send f show #t)

(struct gui-event (start end name) #:prefab)

(define (show-results evts)
  (define gui-events (filter (λ (x) 
                               (define i (vector-ref x 2))
                               (and (gui-event? i)
                                    (number? (gui-event-end i))))
                             evts))
  (define interesting-gui-events
    (take (sort gui-events > #:key (λ (x) 
                                     (define i (vector-ref x 2))
                                     (- (gui-event-end i)
                                        (gui-event-start i))))
          top-n-events))
    
  (define with-other-events
    (for/list ([gui-evt (in-list interesting-gui-events)])
      (match (vector-ref gui-evt 2)
        [(gui-event start end name)
         (define in-the-middle
           (append (map (λ (x) (list (list 'δ (- (get-start-time x) start)) x))
                        (sort
                         (filter (λ (x) (and (not (gui-event? (vector-ref x 2)))
                                             (<= start (get-start-time x) end)))
                                 evts)
                         <
                         #:key get-start-time))
                   (list (list (list 'δ (- end start)) 'end-of-gui-event))))
         (list* (- end start)
                gui-evt
                in-the-middle)])))
  
  (define (has-a-gc-event? x)
    (define in-the-middle (cddr x))
    (ormap (λ (x) 
             (and (vector? (list-ref x 1))
                  (gc-info? (vector-ref (list-ref x 1) 2))))
           in-the-middle))
  
  (pretty-print
   (if drop-gc?
       (filter (λ (x) (not (has-a-gc-event? x)))
               with-other-events)
       with-other-events)))

(struct gc-info (major? pre-amount pre-admin-amount code-amount
                        post-amount post-admin-amount
                        start-process-time end-process-time
                        start-time end-time)
  #:prefab)
(struct engine-info (msec name) #:prefab)

(define (get-start-time x)
  (cond
    [(gc-info? (vector-ref x 2))
     (gc-info-start-time (vector-ref x 2))]
    [(engine-info? (vector-ref x 2))
     (engine-info-msec (vector-ref x 2))]
    [(regexp-match #rx"framework" (vector-ref x 1))
     (vector-ref x 2)]
    [(timeline-info? (vector-ref x 2))
     (timeline-info-milliseconds (vector-ref x 2))]         
    [else
     (unless (regexp-match #rx"^GC: 0:MST @" (vector-ref x 1))
       (eprintf "unk: ~s\n" x))
     0]))


(module+ main
  (when start-right-away?
    (parameterize ([current-eventspace controller-frame-eventspace])
      (queue-callback sb-callback)))
  (dynamic-require 'drracket #f))

