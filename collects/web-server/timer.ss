(module timer mzscheme
  (provide timer? start-timer reset-timer)

  ; BUG: reducing the timeout is ineffective
  ; efficiency: too many threads

  (define-struct timer (expire-seconds))

  ; start-timer : num (-> void) -> timer
  ; to make a timer that calls to-do after msec from make-timer's application
  (define (start-timer sec to-do)
    (let ([timer (make-timer (+ (current-seconds) sec))])
      (letrec ([snooze
                (lambda ()
                  (let ([remaining (- (timer-expire-seconds timer) (current-seconds))])
                    (cond
                      [(<= remaining 0)
                       ; use call-in-nested-thread or something when a single thread is used for all timeouts
                       (to-do)]
                      [else (sleep remaining)
                            (snooze)])))])
        (thread snooze))
      timer))

  ; reset-timer : timer num -> void
  ; to cause timer to expire after sec from the adjust-msec-to-live's application
  (define (reset-timer timer sec)
    (set-timer-expire-seconds! timer (+ sec (current-seconds)))))



; --- timeout plan

; start timeout on connection startup
; for POST requests increase the timeout proportionally when content-length is read
; adjust timeout in read-to-eof
; adjust timeout to starting timeout for next request with persistent connections

; adjust timeout proportionally when responding
; for servlet - make it a day until the output is produced
