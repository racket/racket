#lang racket/base
(require (only-in "private/drracket-test-util.rkt"
                  fire-up-separate-drracket-and-run-tests
                  queue-callback/res)
         racket/class
         racket/date)

(define (run-tests)
  (start-up-on-day  2 14 "Valentine's Day")
  (start-up-on-day  3  2 "Texas Indepenence Day")
  (start-up-on-day  3 26 "Prince Kuhio Day")
  (start-up-on-day  6 11 "King Kamehameha Day")
  (start-up-on-day  7 30 "Eli's birthday")
  (start-up-on-day 10 29 "Matthias's birthday")
  (start-up-on-day 10 31 "Halloween")
  (start-up-on-day 11  1 "Matthew's birthday")
  (start-up-on-day 12 25 "Christmas")
  
  (define now (current-seconds))
  (define week-day (date-week-day (seconds->date now)))
  (define seconds-in-a-day (* 60 60 24))
  (define sunday-secs (+ (* (- 7 week-day) seconds-in-a-day) now))
  (define sunday (seconds->date sunday-secs))
  (define monday (seconds->date (+ sunday-secs seconds-in-a-day)))
  (start-up-on-day (date-month sunday)
                   (date-day sunday)
                   "Weekend")
  (start-up-on-day (date-month monday)
                   (date-day monday)
                   "Weekday"))

(define (start-up-on-day month day what)
  (define the-seconds (find-seconds 1 0 0
                                    day month 
                                    (date-year (seconds->date (current-seconds)))))
  (printf "trying ~a, ~a/~a PLTDREASTERSECONDS=~a\n" what month day the-seconds)
  (unless (putenv "PLTDREASTERSECONDS" (number->string the-seconds))
    (error 'splash.rkt "putenv failed"))
  (fire-up-separate-drracket-and-run-tests
   (λ ()
     
     (define test:keystroke (dynamic-require 'framework 'test:keystroke))
     (define test:run-one (dynamic-require 'framework 'test:run-one))
     (define test:use-focus-table (dynamic-require 'framework 'test:use-focus-table))
     (define test:get-active-top-level-window (dynamic-require 'framework 'test:get-active-top-level-window))
     (define current-eventspace (dynamic-require 'racket/gui/base 'current-eventspace))
     
     (define (wait-for-run-to-finish drr-frame)
       (define (run-finished)
         (send (send drr-frame get-execute-button) is-enabled?))
       (wait-for-something run-finished))
     
     (define (wait-for-drracket-frame)
       (define (drracket-frame-frontmost)
         (define active (test:get-active-top-level-window))
         (and active 
              (method-in-interface? 'get-execute-button (object-interface active))
              active))
       (wait-for-something drracket-frame-frontmost))
     
     (define (wait-for-something thing?)
       (define total-time-to-wait 20) ;; in seconds
       (define time-to-wait-in-one-iteration 1/10) ;; also in seconds
       (let loop ([n (/ total-time-to-wait time-to-wait-in-one-iteration)])
         (cond
           [(thing?) => values]
           [(zero? n)
            (error 'wait-for-something "~s didn't happen" thing?)]
           [else
            (sleep time-to-wait-in-one-iteration)
            (loop (- n 1))])))
     
     (queue-callback/res (λ () (test:use-focus-table #t)))
     (test:use-focus-table #t)
     (define drr-frame (wait-for-drracket-frame))
     (queue-callback/res (λ () (send (send (send drr-frame get-definitions-text) get-canvas) focus)))
     (for ([x (in-string "(car 'x)")])
       (test:keystroke x))
     (let ([button (queue-callback/res (λ () (send drr-frame get-execute-button)))])
       (test:run-one (lambda () (send button command))))
     (wait-for-run-to-finish drr-frame)
     (define res 
       (queue-callback/res (λ () (send (send drr-frame get-interactions-text) get-text))))
     (unless (regexp-match (regexp-quote "car: expects argument of type <pair>; given: 'x")
                           res)
       (eprintf "splash.rkt: testing on ~a ~a, interactions looks wrong; got: ~s\n"
                month day
                res)))))

(run-tests)
