#lang racket/base

(require racket/date racket/contract racket/match unstable/latent-contract/defthing
         (prefix-in srfi-date: srfi/19)
         db
         "contract.rkt"
         "math.rkt"
         "format.rkt")

(provide (all-defined-out))

(define seconds-per-minute 60)
(define seconds-per-hour (* 60 seconds-per-minute))
(define seconds-per-day (* 24 seconds-per-hour))
(define seconds-per-week (* 7 seconds-per-day))
(define avg-seconds-per-year (* #e365.2425 seconds-per-day))
(define avg-seconds-per-month (* 1/12 avg-seconds-per-year))

;; ===================================================================================================
;; UTC dates for plotting

;; A date is always represented by the number of seconds since the platform-specific, UTC epoch

(define (date*->seconds dt [local-time? #t])
  (match-define (date* s mn h d m y wd yd dst? tz ns tz-name)
    dt)
  (+ (date->seconds (date s mn h d m y wd yd dst? tz) local-time?)
     (/ ns 1000000000)))

(define (date*->utc-seconds dt)
  (- (date*->seconds dt #f) (date-time-zone-offset dt)))

(define (date->utc-seconds dt)
  (- (date->seconds dt #f) (date-time-zone-offset dt)))

(define (utc-seconds-second secs)
  (define w (floor secs))
  (define f (- secs w))
  (+ f (date-second (seconds->date w #f))))

(define (utc-seconds-round-year secs)
  (define dt (seconds->date secs #f))
  (define y1 (date-year dt))
  ;; Find start of this year, start of next year, and difference between them in UTC seconds
  (define s1 (date->seconds (date 0 0 0 1 1 y1 0 0 #f 0) #f))
  (define s2 (date->seconds (date 0 0 0 1 1 (+ y1 1) 0 0 #f 0) #f))
  (define diff (- s2 s1))
  ;; Round by 1) subtracting this year; 2) rounding to this year or next; 3) adding this year
  (+ (* (round (/ (- secs s1) diff)) diff) s1))

(define (utc-seconds-round-month secs)
  (define dt (seconds->date secs #f))
  (define m1 (date-month dt))
  (define y1 (date-year dt))
  ;; Find start of this month, start of next month, and difference between them in UTC seconds
  (define s1 (date->seconds (date 0 0 0 1 m1 y1 0 0 #f 0) #f))
  (define-values (m2 y2) (cond [((+ m1 1) . > . 12)  (values 1 (+ y1 1))]
                               [else                 (values (+ m1 1) y1)]))
  (define s2 (date->seconds (date 0 0 0 1 m2 y2 0 0 #f 0) #f))
  (define diff (- s2 s1))
  ;; Round by 1) subtracting this month; 2) rounding to this month or next; 3) adding this month
  (+ (* (round (/ (- secs s1) diff)) diff) s1))

;; ===================================================================================================
;; Time

;; A date-independent representation of time

(struct plot-time (second minute hour day) #:transparent)

(defproc (seconds->plot-time [s real?]) plot-time?
  (let* ([s  (inexact->exact s)]
         [day  (floor (/ s seconds-per-day))]
         [s  (- s (* day seconds-per-day))]
         [hour  (floor (/ s seconds-per-hour))]
         [s  (- s (* hour seconds-per-hour))]
         [minute  (floor (/ s seconds-per-minute))]
         [s  (- s (* minute seconds-per-minute))])
    (plot-time s minute hour day)))

(defproc (plot-time->seconds [t plot-time?]) real?
  (match-define (plot-time second minute hour day) t)
  (+ second
     (* minute seconds-per-minute)
     (* hour seconds-per-hour)
     (* day seconds-per-day)))

(defproc (datetime->real [x (or/c plot-time? date? date*? sql-date? sql-time? sql-timestamp?)]) real?
  (match x
    [(? plot-time?)    (plot-time->seconds x)]
    [(? date*?)        (date*->utc-seconds x)]
    [(? date?)         (date->utc-seconds x)]
    [(sql-date y m d)  (date->utc-seconds (date 0 0 0 d m y 0 0 #t 0))]
    [(sql-time h m s ns tz)  (plot-time->seconds (- (plot-time (+ s (/ ns 1000000000)) m h 0)
                                                    (if tz tz 0)))]
    [(sql-timestamp y m d h mn s ns tz)  (date*->utc-seconds
                                          (date* s mn h d m y 0 0 #t (if tz tz 0) ns "UTC"))]))

;; ===================================================================================================
;; Formatting following SRFI 19, with alterations

#|
Supported format specifiers:

~a  locale's abbreviated weekday name (Sun...Sat)
~A  locale's full weekday name (Sunday...Saturday)
~b  locale's abbreviate month name (Jan...Dec)
~B  locale's full month day (January...December)
~d  day of month, zero padded (01...31)
~D  date (mm/dd/yy)
~e  day of month, blank padded ( 1...31)
~h  same as ~b
~H  hour, zero padded, 24-hour clock (00...23)
~I  hour, zero padded, 12-hour clock (01...12)
~j  day of year, zero padded
~k  hour, blank padded, 24-hour clock (00...23)
~l  hour, blank padded, 12-hour clock (01...12)
~m  month, zero padded (01...12)
~M  minute, zero padded (00...59)
~N  nanosecond, zero padded
~p  locale's AM or PM
~r  time, 12 hour clock, same as "~I:~M:~S ~p"
~S  second, zero padded (00...60)
~f  seconds+fractional seconds, using locale's decimal separator (e.g. 5.2).
~s  number of full seconds since "the epoch" (in UTC)
~T  time, 24 hour clock, same as "~H:~M:~S"
~U  week number of year with Sunday as first day of week (00...53)
~V  week number of year with Monday as first day of week (01...52)
~w  day of week (0...6)
~W  week number of year with Monday as first day of week (01...52)
~x  week number of year with Monday as first day of week (00...53)
~X  locale's date representation, for example: "07/31/00"
~y  last two digits of year (00...99)
~Y  year
~1  ISO-8601 year-month-day format
~3  ISO-8601 hour-minute-second format
~5  ISO-8601 year-month-day-hour-minute-second format
|#

(define (plot-date-formatter x-min x-max)
  (define digits (digits-for-range x-min x-max))
  (λ (fmt secs)
    (case fmt
      [(~f)  (define s (utc-seconds-second secs))
             (define str (real->string/trunc s (max 0 digits)))
             (if (s . < . 10) (format "0~a" str) str)]
      [(~s)  (real->plot-label secs digits)]
      [(~a ~A ~b ~B ~d ~D ~e ~h ~H ~I ~j ~k ~l ~m ~M ~N
           ~p ~r ~S ~f ~s ~T ~U ~V ~w ~W ~x ~X ~y ~Y ~1 ~3 ~5)
       (match-define (date* s mn h d m y _wd _yd _dst? tz ns _tz-name)
         (seconds->date secs #f))
       (srfi-date:date->string (srfi-date:make-date ns s mn h d m y tz) (symbol->string fmt))]
      [else  #f])))

#|
Supported format specifiers:

~d  day
~H  hour, zero padded, 24-hour clock (00...23)
~I  hour, zero padded, 12-hour clock (01...12)
~k  hour, blank padded, 24-hour clock ( 0...23)
~l  hour, blank padded, 12-hour clock ( 1...12)
~p  locale's AM or PM
~M  minute, zero padded (00...59)
~S  second, zero padded (00...60)
~f  seconds+fractional seconds, using locale's decimal separator (e.g. 5.2).
~s  second, formatted (nanoseconds, etc.)
~r  time, 12 hour clock, same as "~I:~M:~S ~p"
~T  time, 24 hour clock, same as "~H:~M:~S"
~3  ISO-8601 hour-minute-second format
|#

(define (plot-time-formatter x-min x-max)
  (define digits (digits-for-range x-min x-max))
  (λ (fmt secs)
    (case fmt
      [(~H ~I ~k ~l ~p ~M ~S ~f ~s ~r ~T ~3)
       ((plot-date-formatter x-min x-max) fmt (real-modulo secs seconds-per-day))]
      [(~d)  (define digits (digits-for-range (/ x-min seconds-per-day) (/ x-max seconds-per-day)))
             (real->plot-label (plot-time-day (seconds->plot-time secs)) digits)]
      [else  #f])))
