#lang racket/base
(require racket/contract
         racket/async-channel)

(define-struct timer (evt expire-seconds action)
  #:mutable)

(define timer-ch (make-async-channel))

; start-timer-manager : -> void
; The timer manager thread   
(define (start-timer-manager)
  (thread
   (lambda ()
     (let loop ([timers null])
       #;(printf "Timers: ~a\n" (length timers))
       ;; Wait for either...
       (apply sync
              ;; ... a timer-request message ...
              (handle-evt
               timer-ch
               (lambda (req)
                 ;; represent a req as a (timer-list -> timer-list) function:
                 ;; add/remove/change timer evet:
                 (loop (req timers))))
              ;; ... or a timer
              (map (lambda (timer)
                     (handle-evt
                      (timer-evt timer)
                      (lambda (_)
                        ;; execute timer
                        ((timer-action timer))
                        (loop (remq timer timers)))))
                   timers)))))
  (void))

;; Limitation on this add-timer: thunk cannot make timer
;;  requests directly, because it's executed directly by
;;  the timer-manager thread
;; add-timer : number (-> void) -> timer
(define (add-timer msecs thunk)
  (define now (current-inexact-milliseconds))
  (define timer
    (make-timer (alarm-evt (+ now msecs))
                (+ now msecs)
                thunk))
  (async-channel-put 
   timer-ch 
   (lambda (timers)
     (list* timer timers)))
  timer)

; revise-timer! : timer msecs (-> void) -> timer
; revise the timer to ring msecs from now
(define (revise-timer! timer msecs thunk)
  (define now (current-inexact-milliseconds))
  (async-channel-put 
   timer-ch 
   (lambda (timers)
     (set-timer-evt! timer (alarm-evt (+ now msecs)))
     (set-timer-expire-seconds! timer (+ now msecs))
     (set-timer-action! timer thunk)
     timers)))

(define (cancel-timer! timer)
  (async-channel-put
   timer-ch
   (lambda (timers)
     (remq timer timers))))

; start-timer : num (-> void) -> timer
; to make a timer that calls to-do after sec from make-timer's application
(define (start-timer secs to-do)
  (add-timer (* 1000 secs) to-do))

; reset-timer : timer num -> void
; to cause timer to expire after sec from the adjust-msec-to-live's application
(define (reset-timer! timer secs)
  (revise-timer! timer (* 1000 secs) (timer-action timer)))

; increment-timer! : timer num -> void
; add secs to the timer, rather than replace
(define (increment-timer! timer secs)
  (revise-timer! timer
                 (+ (- (timer-expire-seconds timer) (current-inexact-milliseconds))
                    (* 1000 secs))
                 (timer-action timer)))


(provide/contract
 [struct timer ([evt evt?]
                [expire-seconds number?]
                [action (-> void)])]   
 [start-timer-manager (-> void)]
 [start-timer (number? (-> void) . -> . timer?)]
 [reset-timer! (timer? number? . -> . void)]
 [increment-timer! (timer? number? . -> . void)]
 [cancel-timer! (timer? . -> . void)])

; --- timeout plan

; start timeout on connection startup
; for POST requests increase the timeout proportionally when content-length is read
; adjust timeout in read-to-eof
; adjust timeout to starting timeout for next request with persistent connections

; adjust timeout proportionally when responding
; for servlet - make it a day until the output is produced
