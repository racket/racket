#lang racket/base
(require racket/contract
         racket/async-channel)

(struct timer-manager (thread timer-ch))
(define-struct timer (tm evt expire-seconds action)
  #:mutable)

;; start-timer-manager : -> timer-manager?
;; The timer manager thread
(define (start-timer-manager)
  (define timer-ch (make-async-channel))
  (timer-manager
   (thread
    (lambda ()
      (let loop ([timers null])
        ;; (printf "Timers: ~a\n" (length timers))
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
   timer-ch))

;; Limitation on this add-timer: thunk cannot make timer
;;  requests directly, because it's executed directly by
;;  the timer-manager thread
;; add-timer : timer-manager number (-> void) -> timer
(define (add-timer manager msecs thunk)
  (define now (current-inexact-milliseconds))
  (define t
    (timer manager
           (alarm-evt (+ now msecs))
           (+ now msecs)
           thunk))
  (async-channel-put
   (timer-manager-timer-ch manager)
   (lambda (timers)
     (list* t timers)))
  t)

;; revise-timer! : timer msecs (-> void) -> timer
;; revise the timer to ring msecs from now
(define (revise-timer! timer msecs thunk)
  (define now (current-inexact-milliseconds))
  (async-channel-put
   (timer-manager-timer-ch (timer-tm timer))
   (lambda (timers)
     (set-timer-evt! timer (alarm-evt (+ now msecs)))
     (set-timer-expire-seconds! timer (+ now msecs))
     (set-timer-action! timer thunk)
     timers)))

(define (cancel-timer! timer)
  (async-channel-put
   (timer-manager-timer-ch (timer-tm timer))
   (lambda (timers)
     (remq timer timers))))

;; start-timer : timer-manager num (-> void) -> timer
;; to make a timer that calls to-do after sec from make-timer's application
(define (start-timer tm secs to-do)
  (add-timer tm (* 1000 secs) to-do))

;; reset-timer : timer num -> void
;; to cause timer to expire after sec from the adjust-msec-to-live's application
(define (reset-timer! timer secs)
  (revise-timer! timer (* 1000 secs) (timer-action timer)))

;; increment-timer! : timer num -> void
;; add secs to the timer, rather than replace
(define (increment-timer! timer secs)
  (revise-timer! timer
                 (+ (- (timer-expire-seconds timer) (current-inexact-milliseconds))
                    (* 1000 secs))
                 (timer-action timer)))


(provide/contract
 [timer-manager?
  (-> any/c boolean?)]
 [struct timer ([tm timer-manager?]
                [evt evt?]
                [expire-seconds number?]
                [action (-> void)])]
 [start-timer-manager (-> timer-manager?)]
 [start-timer (timer-manager? number? (-> void) . -> . timer?)]
 [reset-timer! (timer? number? . -> . void)]
 [increment-timer! (timer? number? . -> . void)]
 [cancel-timer! (timer? . -> . void)])

;; --- timeout plan

;; start timeout on connection startup
;; for POST requests increase the timeout proportionally when content-length is read
;; adjust timeout in read-to-eof
;; adjust timeout to starting timeout for next request with persistent connections

;; adjust timeout proportionally when responding
;; for servlet - make it a day until the output is produced
