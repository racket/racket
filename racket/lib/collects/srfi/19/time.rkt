#lang racket/base

;;;
;;; <time.ss> ---- SRFI 19 Time Data Types and Procedures port to Racket
;;; Time-stamp: <2004-07-21 12:57:06 solsona>
;;;
;;; Usually, I would add a copyright notice, and the announce that
;;; this code is under the LGPL licence.  Nevertheless, I only did the
;;; port to PLT Scheme v200, and here is the copyright notice,
;;; comments, and licence from the original source:

;;; Based on a corrected version by Will F. Feb/2003

;; SRFI-19: Time Data Types and Procedures.
;;
;; Copyright (C) I/NET, Inc. (2000, 2002, 2003). All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; -- Racket implementation
;;
;; The only Racket specific features of this implementation is
;; CURRENT-SECONDS, the DEFINE-STRUCT procedure (SRFI 9: Defining Record Types
;; could be used), and the constants tm:time-at-zero-seconds
;; and tm:julian-day-at-zero-seconds, which refer to the '0' of CURRENT-SECONDS.
;;
;; SRFI-6, String Ports, and SRFI-8, RECEIVE: Binding Multiple Values,
;; are also used. Racket has String Ports built-in. The RECEIVE form
;; is copied below.
;;
; srfi-8: receive
;(require-library "synrule.rkt") -- PLT doesn't like DEFINE-SYNTAX.

;(define-syntax receive
;  (syntax-rules ()
;    ((receive formals expression body ...)
;     (call-with-values (lambda () expression)
;                       (lambda formals body ...)))))

;; -- Multiple helper procedures. TM:xxx procedures are meant to be
;; internal.

(require racket/serialize
         srfi/8/receive
         srfi/29
         srfi/optional)

(provide time-tai time-utc time-monotonic time-thread time-process time-duration time-gc
         current-date current-julian-day current-modified-julian-day current-time time-resolution
         ;; Time object and accessors
         make-time time? time-type time-nanosecond
         deserialize-info:tm:time-v0
         time-second set-time-type! set-time-nanosecond! set-time-second!
         copy-time
         ;; Time comparison
         time<=? time<? time=? time>=? time>?
         ;; Time arithmetic
         time-difference time-difference! add-duration add-duration! subtract-duration subtract-duration!
         ;; Date object and accessors
         ;; date structure is provided by core Racket, we just extended tu support miliseconds:
         srfi:make-date srfi:date? lax-date?
         deserialize-info:tm:date-v0
         date-nanosecond srfi:date-second srfi:date-minute srfi:date-hour srfi:date-day srfi:date-month
         srfi:date-year date-zone-offset
         ;; This are not part of the date structure (as they are in the original Racket's date)
         srfi:date-year-day srfi:date-week-day date-week-number

         ;; The following procedures work with this modified version.

         ;; Time/Date/Julian Day/Modified Julian Day Converters
         date->julian-day date->modified-julian-day date->time-monotonic date->time-tai date->time-utc
         julian-day->date julian-day->time-monotonic julian-day->time-tai julian-day->time-utc modified-julian-day->date
         modified-julian-day->time-monotonic modified-julian-day->time-tai modified-julian-day->time-utc time-monotonic->date
         time-monotonic->julian-day time-monotonic->modified-julian-day time-monotonic->time-tai time-monotonic->time-tai!
         time-monotonic->time-utc time-monotonic->time-utc! time-tai->date time-tai->julian-day time-tai->modified-julian-day
         time-tai->time-monotonic time-tai->time-monotonic! time-tai->time-utc time-tai->time-utc! time-utc->date
         time-utc->julian-day time-utc->modified-julian-day time-utc->time-monotonic time-utc->time-monotonic!
         time-utc->time-tai time-utc->time-tai!
         ;; Date to String/String to Date Converters
         date->string string->date
         )

(define localized? #f)
(define localized-message
  (lambda (message-name)
    (unless localized?
      ;; SRFI-29: Localization initialization:
      (re-read-locale)
      (or (load-bundle! (list* 'srfi-19
                               (current-language)
                               (current-country)
                               (current-locale-details)))
          ;; A little bit less specific
          (load-bundle! (list 'srfi-19
                              (current-language)
                              (current-country)))
          ;; less specific
          (load-bundle! (list 'srfi-19 (current-language)))
          ;; the least specific one (this one *does* exist!, it comes with this srfi) don't worry:
          (load-bundle! (list 'srfi-19)))
      (set! localized? #t))

    (localized-template 'srfi-19 message-name)))

;; Constants
(define time-tai 'time-tai)
(define time-utc 'time-utc)
(define time-monotonic 'time-monotonic)
(define time-thread 'time-thread)
(define time-process 'time-process)
(define time-duration 'time-duration)

;; example of extension (Racket specific)
(define time-gc 'time-gc)


;;-- LOCALE dependent constants

(define tm:locale-number-separator 'separator)

(define tm:locale-abbr-weekday-vector (vector 'sun 'mon 'tue 'wed 'thu 'fri 'sat))
(define tm:locale-long-weekday-vector (vector 'sunday 'monday 'tuesday 'wednesday
                                              'thursday 'friday 'saturday))
;; note empty string in 0th place.
(define tm:locale-abbr-month-vector   (vector 'jan 'feb 'mar
                                              'apr 'may 'jun 'jul
                                              'aug 'sep 'oct 'nov
                                              'dec))

(define tm:locale-long-month-vector   (vector 'january 'february
                                              'march 'april 'may
                                              'june 'july 'august
                                              'september 'october
                                              'november 'december))

(define tm:locale-pm 'pm)
(define tm:locale-am 'am)

;; See date->string
(define tm:locale-date-time-format 'date-time)
(define tm:locale-short-date-format 'date)
(define tm:locale-time-format 'time)
(define tm:iso-8601-date-time-format 'iso8601)
;;-- Miscellaneous Constants.
;;-- only the tm:tai-epoch-in-jd might need changing if
;;   a different epoch is used.

(define tm:nano (expt 10 9))
(define tm:sid  86400)          ; seconds in a day
(define tm:sihd 43200)          ; seconds in a half day
(define tm:tai-epoch-in-jd 4881175/2) ; julian day number for 'the epoch'

;; A Very simple Error system for the time procedures
;;
(define tm:time-error-types
  '(invalid-clock-type
    unsupported-clock-type
    incompatible-time-types
    not-duration
    dates-are-immutable
    bad-date-format-string
    bad-date-template-string
    invalid-month-specification
    ))

(define (tm:time-error caller type value)
  (if (member type tm:time-error-types)
      (if value
          (error caller "TIME-ERROR type ~S: ~S" type value)
          (error caller "TIME-ERROR type ~S" type))
      (error caller "TIME-ERROR unsupported error type ~S" type)))

;; A table of leap seconds
;; See ftp://maia.usno.navy.mil/ser7/tai-utc.dat
;; and update as necessary.
;; this procedures reads the file in the abover
;; format and creates the leap second table
;; it also calls the almost standard, but not R5 procedures read-line
;; & open-input-string
;; ie (set! tm:leap-second-table (tm:read-tai-utc-date "tai-utc.dat"))

(define (tm:read-tai-utc-data filename)
  (define (convert-jd jd)
    (* (- (inexact->exact jd) tm:tai-epoch-in-jd) tm:sid))
  (define (convert-sec sec)
    (inexact->exact sec))
  (let ( (port (open-input-file filename))
         (table '()) )
    (let loop ((line (read-line port)))
      (unless (eq? line eof)
        (let* ( (data (read (open-input-string (string-append "(" line ")"))))
                (year (car data))
                (jd   (cadddr (cdr data)))
                (secs (cadddr (cdddr data))) )
          (when (>= year 1972)
            (set! table (cons (cons (convert-jd jd) (convert-sec secs)) table)))
          (loop (read-line port)))))
    table))

;; each entry is ( utc seconds since epoch . # seconds to add for tai )
;; note they go higher to lower, and end in 1972.
;;
;; up-to-date as of 2013-01-18
(define tm:leap-second-table
  '((1341100800 . 35) ; 2012-07-01
    (1230768000 . 34) ; 2009-01-01
    (1136073600 . 33) ; 2006-01-01
    (915148800  . 32) ; 1999-01-01
    (867715200  . 31) ; 1997-07-01
    (820454400  . 30) ; 1996-01-01
    (773020800  . 29) ; 1994-07-01
    (741484800  . 28) ; 1993-07-01
    (709948800  . 27) ; 1992-07-01
    (662688000  . 26) ; 1991-01-01
    (631152000  . 25) ; 1990-01-01
    (567993600  . 24) ; 1988-01-01
    (489024000  . 23) ; 1985-07-01
    (425865600  . 22) ; 1983-07-01
    (394329600  . 21) ; 1982-07-01
    (362793600  . 20) ; 1981-07-01
    (315532800  . 19) ; 1980-01-01
    (283996800  . 18) ; 1979-01-01
    (252460800  . 17) ; 1978-01-01
    (220924800  . 16) ; 1977-01-01
    (189302400  . 15) ; 1976-01-01
    (157766400  . 14) ; 1975-01-01
    (126230400  . 13) ; 1974-01-01
    (94694400   . 12) ; 1973-01-01
    (78796800   . 11) ; 1972-07-01
    (63072000   . 10) ; 1972-01-01
    ))

(define (read-leap-second-table filename)
  (set! tm:leap-second-table (tm:read-tai-utc-data filename))
  (values))


(define (tm:leap-second-delta utc-seconds)
  (letrec ( (lsd (lambda (table)
                   (cond ((>= utc-seconds (caar table))
                          (cdar table))
                         (else (lsd (cdr table)))))) )
    (if (< utc-seconds  (* (- 1972 1970) 365 tm:sid)) 0
        (lsd  tm:leap-second-table))))

;; going from tai seconds to utc seconds ...
(define (tm:leap-second-neg-delta tai-seconds)
  (letrec ( (lsd (lambda (table)
                   (cond ((null? table) 0)
                         ((<= (cdar table) (- tai-seconds (caar table)))
                          (cdar table))
                         (else (lsd (cdr table)))))) )
    (if (< tai-seconds  (* (- 1972 1970) 365 tm:sid)) 0
        (lsd  tm:leap-second-table))))

(define-values (tm:time make-time time? tm:time-ref tm:time-set!)
  (make-struct-type
   'tm:time #f 3 0 #f
   (list (cons prop:serializable
               (make-serialize-info
                (lambda (t)
                  (vector (time-type t)
                          (time-nanosecond t)
                          (time-second t)))
                #'deserialize-info:tm:time-v0
                #f
                (or (current-load-relative-directory)
                    (current-directory)))))
   (make-inspector) #f null))

(define deserialize-info:tm:time-v0
  (make-deserialize-info
   make-time
   (lambda ()
     (let ([t0 (make-time #f #f #f)])
       (values t0 (lambda (t1)
                    (set-time-type!       t0 (time-type t1))
                    (set-time-nanosecond! t0 (time-nanosecond t1))
                    (set-time-second!     t0 (time-second t1))))))))

(define (time-type t)       (tm:time-ref t 0))
(define (time-nanosecond t) (tm:time-ref t 1))
(define (time-second t)     (tm:time-ref t 2))

(define (set-time-type! t type)     (tm:time-set! t 0 type))
(define (set-time-nanosecond! t ns) (tm:time-set! t 1 ns))
(define (set-time-second! t s)      (tm:time-set! t 2 s))

(define (copy-time time)
  (let ((ntime (make-time #f #f #f)))
    (set-time-type! ntime (time-type time))
    (set-time-second! ntime (time-second time))
    (set-time-nanosecond! ntime (time-nanosecond time))
    ntime))


;;; specific time getters.
;;; These should be rewritten to be OS specific.
;;
;; -- using GNU gettimeofday() would be useful here -- gets
;;    second + millisecond
;;    let's pretend we do, using Racket's current-seconds & current-milliseconds
;;    this is supposed to return UTC.
;;

(define (tm:get-time-of-day)
  (let* ((total-msecs (inexact->exact (floor (current-inexact-milliseconds)))))
    (quotient/remainder total-msecs 1000)))

(define (tm:current-time-utc)
  (receive (seconds ms) (tm:get-time-of-day)
           (make-time  time-utc (* ms 1000000) seconds)))

(define (tm:current-time-tai)
  (receive (seconds ms) (tm:get-time-of-day)
           (make-time time-tai
                      (* ms 1000000)
                      (+ seconds (tm:leap-second-delta seconds))
                      )))

(define (tm:current-time-ms-time time-type proc)
  (let ((current-ms (proc)))
    (make-time time-type
               (* (remainder current-ms 1000) 1000000)
               (quotient current-ms 1000000)
               )))

;; -- we define it to be the same as TAI.
;;    A different implemation of current-time-montonic
;;    will require rewriting all of the time-monotonic converters,
;;    of course.

(define (tm:current-time-monotonic)
  (receive (seconds ms) (tm:get-time-of-day)
           (make-time time-monotonic
                      (* ms 1000000)
                      (+ seconds (tm:leap-second-delta seconds))
                      )))

(define (tm:current-time-thread)
  (tm:current-time-ms-time time-process current-process-milliseconds))

(define (tm:current-time-process)
  (tm:current-time-ms-time time-process current-process-milliseconds))

(define (tm:current-time-gc)
  (tm:current-time-ms-time time-gc current-gc-milliseconds))

(define (current-time [clock-type time-utc])
  (cond
    ((eq? clock-type time-tai) (tm:current-time-tai))
    ((eq? clock-type time-utc) (tm:current-time-utc))
    ((eq? clock-type time-monotonic) (tm:current-time-monotonic))
    ((eq? clock-type time-thread) (tm:current-time-thread))
    ((eq? clock-type time-process) (tm:current-time-process))
    ((eq? clock-type time-gc) (tm:current-time-gc))
    (else (tm:time-error 'current-time 'invalid-clock-type clock-type))))


;; -- Time Resolution
;; This is the resolution of the clock in nanoseconds.
;; This will be implementation specific.

(define (time-resolution [clock-type time-utc])
  (cond
    ((eq? clock-type time-tai) 1000000)
    ((eq? clock-type time-utc) 1000000)
    ((eq? clock-type time-monotonic) 1000000)
    ((eq? clock-type time-thread) 1000000)
    ((eq? clock-type time-process) 1000000)
    ((eq? clock-type time-gc) 1000000)
    (else (tm:time-error 'time-resolution 'invalid-clock-type clock-type))))

(define (tm:time-compare-check time1 time2 caller)
  (if (or (not (and (time? time1) (time? time2)))
          (not (eq? (time-type time1) (time-type time2))))
      (tm:time-error caller 'incompatible-time-types #f)
      #t))

(define (time=? time1 time2)
  (tm:time-compare-check time1 time2 'time=?)
  (and (= (time-second time1) (time-second time2))
       (= (time-nanosecond time1) (time-nanosecond time2))))

(define (time>? time1 time2)
  (tm:time-compare-check time1 time2 'time>?)
  (or (> (time-second time1) (time-second time2))
      (and (= (time-second time1) (time-second time2))
           (> (time-nanosecond time1) (time-nanosecond time2)))))

(define (time<? time1 time2)
  (tm:time-compare-check time1 time2 'time<?)
  (or (< (time-second time1) (time-second time2))
      (and (= (time-second time1) (time-second time2))
           (< (time-nanosecond time1) (time-nanosecond time2)))))

(define (time>=? time1 time2)
  (tm:time-compare-check time1 time2 'time>=?)
  (or (> (time-second time1) (time-second time2))
      (and (= (time-second time1) (time-second time2))
           (>= (time-nanosecond time1) (time-nanosecond time2)))))

(define (time<=? time1 time2)
  (tm:time-compare-check time1 time2 'time<=?)
  (or (< (time-second time1) (time-second time2))
      (and (= (time-second time1) (time-second time2))
           (<= (time-nanosecond time1) (time-nanosecond time2)))))

;; -- Time arithmetic
(define (tm:time->nanoseconds time)
  (define (sign1 n)
    (if (negative? n) -1 1))
  (+ (* (time-second time) tm:nano)
     (time-nanosecond time)))

(define (tm:nanoseconds->time time-type nanoseconds)
  (make-time time-type
             (remainder nanoseconds tm:nano)
             (quotient nanoseconds tm:nano)))

(define (tm:nanoseconds->values nanoseconds)
  (values (abs (remainder nanoseconds tm:nano))
          (quotient nanoseconds tm:nano)))

(define (tm:time-difference time1 time2 time3)
  (when (or (not (and (time? time1) (time? time2)))
            (not (eq? (time-type time1) (time-type time2))))
    (tm:time-error 'time-difference 'incompatible-time-types #f))
  (set-time-type! time3 time-duration)
  (if (time=? time1 time2)
      (begin
        (set-time-second! time3 0)
        (set-time-nanosecond! time3 0))
      (receive
       (nanos secs)
       (tm:nanoseconds->values (- (tm:time->nanoseconds time1)
                                  (tm:time->nanoseconds time2)))
       (set-time-second! time3 secs)
       (set-time-nanosecond! time3 nanos)))
  time3)

(define (time-difference time1 time2)
  (tm:time-difference time1 time2 (make-time #f #f #f)))

(define (time-difference! time1 time2)
  (tm:time-difference time1 time2 time1))

(define (tm:add-duration time1 duration time3)
  (unless (and (time? time1) (time? duration))
    (tm:time-error 'add-duration 'incompatible-time-types #f))
  (if (not (eq? (time-type duration) time-duration))
      (tm:time-error 'add-duration 'not-duration duration)
      (let ( (sec-plus (+ (time-second time1) (time-second duration)))
             (nsec-plus (+ (time-nanosecond time1) (time-nanosecond duration))) )
        (let ((r (remainder nsec-plus tm:nano))
              (q (quotient nsec-plus tm:nano)))
          ; (set-time-type! time3 (time-type time1))
          (if (negative? r)
              (begin
                (set-time-second! time3 (+ sec-plus q -1))
                (set-time-nanosecond! time3 (+ tm:nano r)))
              (begin
                (set-time-second! time3 (+ sec-plus q))
                (set-time-nanosecond! time3 r)))
          time3))))

(define (add-duration time1 duration)
  (tm:add-duration time1 duration (make-time (time-type time1) #f #f)))

(define (add-duration! time1 duration)
  (tm:add-duration time1 duration time1))

(define (tm:subtract-duration time1 duration time3)
  (unless (and (time? time1) (time? duration))
    (tm:time-error 'add-duration 'incompatible-time-types #f))
  (if (not (eq? (time-type duration) time-duration))
      (tm:time-error 'tm:subtract-duration 'not-duration duration)
      (let ( (sec-minus  (- (time-second time1) (time-second duration)))
             (nsec-minus (- (time-nanosecond time1) (time-nanosecond duration))) )
        (let ((r (remainder nsec-minus tm:nano))
              (q (quotient nsec-minus tm:nano)))
          (if (negative? r)
              (begin
                (set-time-second! time3 (- sec-minus q 1))
                (set-time-nanosecond! time3 (+ tm:nano r)))
              (begin
                (set-time-second! time3 (- sec-minus q))
                (set-time-nanosecond! time3 r)))
          time3))))

(define (subtract-duration time1 duration)
  (tm:subtract-duration time1 duration (make-time (time-type time1) #f #f)))

(define (subtract-duration! time1 duration)
  (tm:subtract-duration time1 duration time1))


;; -- Converters between types.

(define (tm:time-tai->time-utc! time-in time-out caller)
  (unless (eq? (time-type time-in) time-tai)
    (tm:time-error caller 'incompatible-time-types time-in))
  (set-time-type! time-out time-utc)
  (set-time-nanosecond! time-out (time-nanosecond time-in))
  (set-time-second!     time-out (- (time-second time-in)
                                    (tm:leap-second-neg-delta
                                     (time-second time-in))))
  time-out)

(define (time-tai->time-utc time-in)
  (tm:time-tai->time-utc! time-in (make-time #f #f #f) 'time-tai->time-utc))


(define (time-tai->time-utc! time-in)
  (tm:time-tai->time-utc! time-in time-in 'time-tai->time-utc!))


(define (tm:time-utc->time-tai! time-in time-out caller)
  (unless (eq? (time-type time-in) time-utc)
    (tm:time-error caller 'incompatible-time-types time-in))
  (set-time-type! time-out time-tai)
  (set-time-nanosecond! time-out (time-nanosecond time-in))
  (set-time-second!     time-out (+ (time-second time-in)
                                    (tm:leap-second-delta
                                     (time-second time-in))))
  time-out)


(define (time-utc->time-tai time-in)
  (tm:time-utc->time-tai! time-in (make-time #f #f #f) 'time-utc->time-tai))

(define (time-utc->time-tai! time-in)
  (tm:time-utc->time-tai! time-in time-in 'time-utc->time-tai!))

;; -- these depend on time-monotonic having the same definition as time-tai!
(define (time-monotonic->time-utc time-in)
  (unless (eq? (time-type time-in) time-monotonic)
    (tm:time-error 'time-monotoinc->time-utc 'incompatible-time-types time-in))
  (let ((ntime (copy-time time-in)))
    (set-time-type! ntime time-tai)
    (tm:time-tai->time-utc! ntime ntime 'time-monotonic->time-utc)))

(define (time-monotonic->time-utc! time-in)
  (unless (eq? (time-type time-in) time-monotonic)
    (tm:time-error 'time-monotonic->time-utc! 'incompatible-time-types time-in))
  (set-time-type! time-in time-tai)
  (tm:time-tai->time-utc! time-in time-in 'time-monotonic->time-utc))

(define (time-monotonic->time-tai time-in)
  (unless (eq? (time-type time-in) time-monotonic)
    (tm:time-error 'time-monotonic->time-tai 'incompatible-time-types time-in))
  (let ((ntime (copy-time time-in)))
    (set-time-type! ntime time-tai)
    ntime))

(define (time-monotonic->time-tai! time-in)
  (unless (eq? (time-type time-in) time-monotonic)
    (tm:time-error 'time-monotonic->time-tai! 'incompatible-time-types time-in))
  (set-time-type! time-in time-tai)
  time-in)

(define (time-utc->time-monotonic time-in)
  (unless (eq? (time-type time-in) time-utc)
    (tm:time-error 'time-utc->time-monotonic 'incompatible-time-types time-in))
  (let ((ntime (tm:time-utc->time-tai! time-in (make-time #f #f #f)
                                       'time-utc->time-monotonic)))
    (set-time-type! ntime time-monotonic)
    ntime))


(define (time-utc->time-monotonic! time-in)
  (unless (eq? (time-type time-in) time-utc)
    (tm:time-error 'time-utc->time-montonic! 'incompatible-time-types time-in))
  (let ((ntime (tm:time-utc->time-tai! time-in time-in
                                       'time-utc->time-monotonic!)))
    (set-time-type! ntime time-monotonic)
    ntime))


(define (time-tai->time-monotonic time-in)
  (unless (eq? (time-type time-in) time-tai)
    (tm:time-error 'time-tai->time-monotonic 'incompatible-time-types time-in))
  (let ((ntime (copy-time time-in)))
    (set-time-type! ntime time-monotonic)
    ntime))

(define (time-tai->time-monotonic! time-in)
  (unless (eq? (time-type time-in) time-tai)
    (tm:time-error 'time-tai->time-monotonic!  'incompatible-time-types time-in))
  (set-time-type! time-in time-monotonic)
  time-in)

;; -- Date Structures

;; These identifiers originally referred to a separate date type,
;; but they now use Racket's native date type
(define (srfi:make-date nanosecond second minute
                        hour day month
                        year zone-offset)
  (with-handlers ([exn:fail:contract?
                   (lambda (e)
                     (lax-date nanosecond second minute hour
                               day month year zone-offset))])
    (date* second minute hour
           day month year
           ;; compute derived fields
           (tm:week-day day month year)
           ;; date* and srfi/19 are off-by-1 on year day
           (- (tm:year-day day month year) 1)
           #f
           zone-offset
           nanosecond
           "")))

;; A struct type that emulates the old srfi/19 type
;; This is lax about its contents, unlike date*
(struct lax-date (nanosecond second minute
                  hour day month
                  year zone-offset)
        #:transparent)

;; Try to convert srfi-19 date to date*
(define (lax-date->date* date)
  (srfi:make-date (lax-date-nanosecond date)
                  (lax-date-second date)
                  (lax-date-minute date)
                  (lax-date-hour date)
                  (lax-date-day date)
                  (lax-date-month date)
                  (lax-date-year date)
                  (lax-date-zone-offset date)))

;; Predicate for dates
(define (srfi:date? d)
  (or (lax-date? d) (date? d)))

;; Racket's date structure has the following:
;;   * second : 0 to 61 (60 and 61 are for unusual leap-seconds)
;;   * minute : 0 to 59
;;   * hour : 0 to 23
;;   * day : 1 to 31
;;   * month : 1 to 12
;;   * year : e.g., 1996
;;   * week-day : 0 (Sunday) to 6 (Saturday)
;;   * year-day : 0 to 365 (364 in non-leap years)
;;   * dst? : #t (daylight savings time) or #f
;;   * time-zone-offset : the number of seconds east of GMT for this time zone (e.g., Pacific Standard Time is -28800), an exact integer 36

;; These accessors work over either style of date
(define-syntax-rule (define-date-accessor accessor srfi-19-accessor date-accessor)
  (define (accessor d)
    (if (lax-date? d)
        (srfi-19-accessor d)
        (date-accessor d))))

(define-date-accessor date-nanosecond lax-date-nanosecond date*-nanosecond)
(define-date-accessor srfi:date-second lax-date-second date-second)
(define-date-accessor srfi:date-minute lax-date-minute date-minute)
(define-date-accessor srfi:date-hour lax-date-hour date-hour)
(define-date-accessor srfi:date-day lax-date-day date-day)
(define-date-accessor srfi:date-month lax-date-month date-month)
(define-date-accessor srfi:date-year lax-date-year date-year)
(define-date-accessor date-zone-offset
  lax-date-zone-offset date-time-zone-offset)

;; Serialization support for old srfi-19 structs
(define deserialize-info:tm:date-v0
  (make-deserialize-info
   srfi:make-date
   (lambda ()
     (error 'deserialize-info:tm:date-v0 "cycles not allowed"))))

;; gives the julian day which starts at noon.
(define (tm:encode-julian-day-number day month year)
  (let* ((a (quotient (- 14 month) 12))
         (y (- (+ year 4800) a (if (negative? year) -1  0)))
         (m (- (+ month (* 12 a)) 3)))
    (+ day
       (quotient (+ (* 153 m) 2) 5)
       (* 365 y)
       (quotient y 4)
       (- (quotient y 100))
       (quotient y 400)
       -32045)))

(define (tm:char-pos char str index len)
  (cond
    ((>= index len) #f)
    ((char=? (string-ref str index) char)
     index)
    (else
     (tm:char-pos char str (+ index 1) len))))

; return a string representing the decimal expansion of the fractional
; portion of a number, limited by a specified precision
(define (tm:decimal-expansion r precision)
  (let loop ([num (- r (round r))]
             [p precision])
    (if (or (= p 0) (= num 0))
        ""
        (let* ([num-times-10 (* 10 num)]
               [round-num-times-10 (round num-times-10)])
          (string-append (number->string (inexact->exact round-num-times-10))
                         (loop (- num-times-10 round-num-times-10) (- p 1)))))))

;; gives the seconds/date/month/year
(define (tm:decode-julian-day-number jdn)
  (let* ((days (truncate jdn))
         (a (+ days 32044))
         (b (quotient (+ (* 4 a) 3) 146097))
         (c (- a (quotient (* 146097 b) 4)))
         (d (quotient (+ (* 4 c) 3) 1461))
         (e (- c (quotient (* 1461 d) 4)))
         (m (quotient (+ (* 5 e) 2) 153))
         (y (+ (* 100 b) d -4800 (quotient m 10))))
    (values ; seconds date month year
     (* (- jdn days) tm:sid)
     (+ e (- (quotient (+ (* 153 m) 2) 5)) 1)
     (+ m 3 (* -12 (quotient m 10)))
     (if (>= 0 y) (- y 1) y))
    ))

;; relies on the fact that we named our time zone accessor
;; differently from Racket's....
;; This should be written to be OS specific.
(define (tm:local-tz-offset)
  (date-time-zone-offset (seconds->date (current-seconds))))

;; special thing -- ignores nanos
(define (tm:time->julian-day-number seconds tz-offset)
  (+ (/ (+ seconds
           tz-offset
           tm:sihd)
        tm:sid)
     tm:tai-epoch-in-jd))

(define (tm:find proc l)
  (if (null? l)
      #f
      (if (proc (car l))
          #t
          (tm:find proc (cdr l)))))

(define (tm:tai-before-leap-second? second)
  (tm:find (lambda (x)
             (= second (- (+ (car x) (cdr x)) 1)))
           tm:leap-second-table))

(define (tm:time->date time tz-offset ttype)
  (unless (eq? (time-type time) ttype)
    (tm:time-error 'time->date 'incompatible-time-types  time))
  (let* ( (offset (:optional tz-offset (tm:local-tz-offset))) )
    (receive (secs date month year)
             (tm:decode-julian-day-number
              (tm:time->julian-day-number (time-second time) offset))
             (let* ( (hours    (quotient secs (* 60 60)))
                     (rem      (remainder secs (* 60 60)))
                     (minutes  (quotient rem 60))
                     (seconds  (remainder rem 60)) )
               (srfi:make-date (time-nanosecond time)
                               seconds
                               minutes
                               hours
                               date
                               month
                               year
                               offset)))))



(define (time-tai->date time . tz-offset)
  (if (tm:tai-before-leap-second? (time-second time))
      ;; if it's *right* before the leap, we need to pretend to subtract a second ...
      (let ((d (tm:time->date (subtract-duration! (time-tai->time-utc time)
                                                  (make-time time-duration 0 1))
                              tz-offset time-utc)))
        (srfi:make-date (date-nanosecond d)
                        60
                        (srfi:date-minute d)
                        (srfi:date-hour d)
                        (srfi:date-day d)
                        (srfi:date-month d)
                        (srfi:date-year d)
                        (date-zone-offset d)))
      (tm:time->date (time-tai->time-utc time) tz-offset time-utc)))

(define (time-utc->date time . tz-offset)
  (tm:time->date time tz-offset time-utc))

;; again, time-monotonic is the same as time tai
(define (time-monotonic->date time . tz-offset)
  (tm:time->date time tz-offset time-monotonic))

(define (date->time-utc date)
  (let ( (nanosecond (date-nanosecond date))
         (second (srfi:date-second date))
         (minute (srfi:date-minute date))
         (hour (srfi:date-hour date))
         (day (srfi:date-day date))
         (month (srfi:date-month date))
         (year (srfi:date-year date))
         (offset (date-zone-offset date)) )
    (let ( (jdays (- (tm:encode-julian-day-number day month year)
                     tm:tai-epoch-in-jd)) )
      (make-time
       time-utc
       nanosecond
       (+ (* (- jdays 1/2) 24 60 60)
          (* hour 60 60)
          (* minute 60)
          second
          (- offset))
       ))))

(define (date->time-tai d)
  (if (= (srfi:date-second d) 60)
      (subtract-duration! (time-utc->time-tai! (date->time-utc d)) (make-time time-duration 0 1))
      (time-utc->time-tai! (date->time-utc d))))

(define (date->time-monotonic date)
  (time-utc->time-monotonic! (date->time-utc date)))


(define (tm:leap-year? year)
  (or (= (modulo year 400) 0)
      (and (= (modulo year 4) 0) (not (= (modulo year 100) 0)))))

(define (leap-year? date)
  (tm:leap-year? (srfi:date-year date)))

;; tm:year-day fixed: adding wrong number of days.
(define  tm:month-assoc '((0 . 0) (1 . 31)  (2 . 59)   (3 . 90)   (4 . 120)
                                  (5 . 151) (6 . 181)  (7 . 212)  (8 . 243)
                                  (9 . 273) (10 . 304) (11 . 334)))

(define (tm:year-day day month year)
  (let ((days-pr (assoc (- month 1) tm:month-assoc)))
    (unless days-pr
      (tm:time-error 'date-year-day 'invalid-month-specification month))
    (if (and (tm:leap-year? year) (> month 2))
        (+ day (cdr days-pr) 1)
        (+ day (cdr days-pr)))))

(define (srfi:date-year-day date)
  (tm:year-day (srfi:date-day date) (srfi:date-month date) (srfi:date-year date)))

;; from calendar faq
(define (tm:week-day day month year)
  (let* ((a (quotient (- 14 month) 12))
         (y (- year a))
         (m (+ month (* 12 a) -2)))
    (modulo (+ day y (quotient y 4) (- (quotient y 100))
               (quotient y 400) (quotient (* 31 m) 12))
            7)))

(define (srfi:date-week-day date)
  (tm:week-day (srfi:date-day date) (srfi:date-month date) (srfi:date-year date)))

(define (tm:days-before-first-week date day-of-week-starting-week)
  (let* ( (first-day (srfi:make-date 0 0 0 0
                                     1
                                     1
                                     (srfi:date-year date)
                                     #f))
          (fdweek-day (srfi:date-week-day first-day))  )
    (modulo (- day-of-week-starting-week fdweek-day)
            7)))

(define (date-week-number date day-of-week-starting-week)
  (quotient (- (srfi:date-year-day date)
               (tm:days-before-first-week  date day-of-week-starting-week))
            7))

(define (current-date . tz-offset)
  (time-utc->date (current-time time-utc)
                  (:optional tz-offset (tm:local-tz-offset))))

;; given a 'two digit' number, find the year within 50 years +/-
(define (tm:natural-year n)
  (let* ( (current-year (srfi:date-year (current-date)))
          (current-century (* (quotient current-year 100) 100)) )
    (cond
      ((>= n 100) n)
      ((<  n 0) n)
      ((<=  (- (+ current-century n) current-year) 50)
       (+ current-century n))
      (else
       (+ (- current-century 100) n)))))

(define (date->julian-day date)
  (let ( (nanosecond (date-nanosecond date))
         (second (srfi:date-second date))
         (minute (srfi:date-minute date))
         (hour (srfi:date-hour date))
         (day (srfi:date-day date))
         (month (srfi:date-month date))
         (year (srfi:date-year date))
         (offset (date-zone-offset date)) )
    (+ (tm:encode-julian-day-number day month year)
       (- 1/2)
       (+ (/ (+ (* hour 60 60)
                (* minute 60)
                second
                (/ nanosecond tm:nano)
                (- offset))
             tm:sid)))))

(define (date->modified-julian-day date)
  (- (date->julian-day date)
     4800001/2))


(define (time-utc->julian-day time)
  (unless (eq? (time-type time) time-utc)
    (tm:time-error 'time->date 'incompatible-time-types  time))
  (+ (/ (+ (time-second time) (/ (time-nanosecond time) tm:nano))
        tm:sid)
     tm:tai-epoch-in-jd))

(define (time-utc->modified-julian-day time)
  (- (time-utc->julian-day time)
     4800001/2))

(define (time-tai->julian-day time)
  (unless (eq? (time-type time) time-tai)
    (tm:time-error 'time->date 'incompatible-time-types  time))
  (+ (/ (+ (- (time-second time)
              (tm:leap-second-delta (time-second time)))
           (/ (time-nanosecond time) tm:nano))
        tm:sid)
     tm:tai-epoch-in-jd))

(define (time-tai->modified-julian-day time)
  (- (time-tai->julian-day time)
     4800001/2))

;; this is the same as time-tai->julian-day
(define (time-monotonic->julian-day time)
  (unless (eq? (time-type time) time-monotonic)
    (tm:time-error 'time->date 'incompatible-time-types  time))
  (+ (/ (+ (- (time-second time)
              (tm:leap-second-delta (time-second time)))
           (/ (time-nanosecond time) tm:nano))
        tm:sid)
     tm:tai-epoch-in-jd))


(define (time-monotonic->modified-julian-day time)
  (- (time-monotonic->julian-day time)
     4800001/2))


(define (julian-day->time-utc jdn)
  (let ( (nanosecs (* tm:nano tm:sid (- jdn tm:tai-epoch-in-jd))) )
    (make-time time-utc
               (remainder nanosecs tm:nano)
               (floor (/ nanosecs tm:nano)))))

(define (julian-day->time-tai jdn)
  (time-utc->time-tai! (julian-day->time-utc jdn)))

(define (julian-day->time-monotonic jdn)
  (time-utc->time-monotonic! (julian-day->time-utc jdn)))

(define (julian-day->date jdn . tz-offset)
  (let ((offset (:optional tz-offset (tm:local-tz-offset))))
    (time-utc->date (julian-day->time-utc jdn) offset)))

(define (modified-julian-day->date jdn . tz-offset)
  (let ((offset (:optional tz-offset (tm:local-tz-offset))))
    (julian-day->date (+ jdn 4800001/2) offset)))

(define (modified-julian-day->time-utc jdn)
  (julian-day->time-utc (+ jdn 4800001/2)))

(define (modified-julian-day->time-tai jdn)
  (julian-day->time-tai (+ jdn 4800001/2)))

(define (modified-julian-day->time-monotonic jdn)
  (julian-day->time-monotonic (+ jdn 4800001/2)))

(define (current-julian-day)
  (time-utc->julian-day (current-time time-utc)))

(define (current-modified-julian-day)
  (time-utc->modified-julian-day (current-time time-utc)))

;; returns a string rep. of number N, of minimum LENGTH,
;; padded with character PAD-WITH. If PAD-WITH if #f,
;; no padding is done, and it's as if number->string was used.
;; if string is longer than LENGTH, it's as if number->string was used.
(define (tm:padding n pad-with length)
  (let* ( (str (number->string n))
          (str-len (string-length str)) )
    (if (or (> str-len length)
            (not pad-with))
        str
        (let* ( (new-str (make-string length pad-with))
                (new-str-offset (- (string-length new-str)
                                   str-len)) )
          (do ((i 0 (+ i 1)))
            ((>= i (string-length str)))
            (string-set! new-str (+ new-str-offset i)
                         (string-ref str i)))
          new-str))))

(define (tm:last-n-digits i n)
  (abs (remainder i (expt 10 n))))

(define (tm:locale-abbr-weekday n)
  (localized-message (vector-ref tm:locale-abbr-weekday-vector n)))

(define (tm:locale-long-weekday n)
  (localized-message (vector-ref tm:locale-long-weekday-vector n)))

(define (tm:locale-abbr-month n)
  (localized-message (vector-ref tm:locale-abbr-month-vector (- n 1))))

(define (tm:locale-long-month n)
  (localized-message (vector-ref tm:locale-long-month-vector (- n 1))))

(define (tm:vector-find needle haystack comparator)
  (let ((len (vector-length haystack)))
    (define (tm:vector-find-int index)
      (cond
        ((>= index len) #f)
        ((comparator needle (localized-message (vector-ref haystack index))) (+ index 1))
        (else (tm:vector-find-int (+ index 1)))))
    (tm:vector-find-int 0)))

(define (tm:locale-abbr-weekday->index string)
  (tm:vector-find string tm:locale-abbr-weekday-vector string=?))

(define (tm:locale-long-weekday->index string)
  (tm:vector-find string tm:locale-long-weekday-vector string=?))

(define (tm:locale-abbr-month->index string)
  (tm:vector-find string tm:locale-abbr-month-vector string=?))

(define (tm:locale-long-month->index string)
  (tm:vector-find string tm:locale-long-month-vector string=?))



;; do nothing.
;; Your implementation might want to do something...
;;
(define (tm:locale-print-time-zone date port)
  (values))

;; Again, locale specific.
(define (tm:locale-am/pm hr)
  (localized-message
   (if (> hr 11) tm:locale-pm tm:locale-am)))

(define (tm:tz-printer offset port)
  (display
   (cond [(= offset 0) "Z"]
         [else (let ([sign (cond [(negative? offset) "-"]
                                 [else               "+"])]
                     [hours (abs (quotient offset (* 60 60)))]
                     [minutes (abs (quotient (remainder offset (* 60 60)) 60))])
                 (string-append sign (tm:padding hours #\0 2) (tm:padding minutes #\0 2)))])
   port))

;; A table of output formatting directives.
;; the first time is the format char.
;; the second is a procedure that takes the date, a padding character
;; (which might be #f), and the output port.
;;
(define tm:directives
  (list
   (cons #\~ (lambda (date pad-with port) (display #\~ port)))

   (cons #\a (lambda (date pad-with port)
               (display (tm:locale-abbr-weekday (srfi:date-week-day date))
                        port)))
   (cons #\A (lambda (date pad-with port)
               (display (tm:locale-long-weekday (srfi:date-week-day date))
                        port)))
   (cons #\b (lambda (date pad-with port)
               (display (tm:locale-abbr-month (srfi:date-month date))
                        port)))
   (cons #\B (lambda (date pad-with port)
               (display (tm:locale-long-month (srfi:date-month date))
                        port)))
   (cons #\c (lambda (date pad-with port)
               (display (date->string date (localized-message tm:locale-date-time-format)) port)))
   (cons #\d (lambda (date pad-with port)
               (display (tm:padding (srfi:date-day date)
                                    #\0 2)
                        port)))
   (cons #\D (lambda (date pad-with port)
               (display (date->string date "~m/~d/~y") port)))
   (cons #\e (lambda (date pad-with port)
               (display (tm:padding (srfi:date-day date)
                                    #\Space 2)
                        port)))
   (cons #\f (lambda (date pad-with port)
               (if (> (date-nanosecond date)
                      tm:nano)
                   (display (tm:padding (+ (srfi:date-second date) 1)
                                        pad-with 2)
                            port)
                   (display (tm:padding (srfi:date-second date)
                                        pad-with 2)
                            port))
               (let ([f (tm:decimal-expansion (/ (date-nanosecond date) tm:nano) 6)])
                 (when (> (string-length f) 0)
                   (display (localized-message tm:locale-number-separator) port)
                   (display f port)))))
   (cons #\h (lambda (date pad-with port)
               (display (date->string date "~b") port)))
   (cons #\H (lambda (date pad-with port)
               (display (tm:padding (srfi:date-hour date)
                                    pad-with 2)
                        port)))
   (cons #\I (lambda (date pad-with port)
               (let ((hr (srfi:date-hour date)))
                 (if (> hr 12)
                     (display (tm:padding (- hr 12)
                                          pad-with 2)
                              port)
                     (display (tm:padding hr
                                          pad-with 2)
                              port)))))
   (cons #\j (lambda (date pad-with port)
               (display (tm:padding (srfi:date-year-day date)
                                    pad-with 3)
                        port)))
   (cons #\k (lambda (date pad-with port)
               (display (tm:padding (srfi:date-hour date)
                                    #\0 2)
                        port)))
   (cons #\l (lambda (date pad-with port)
               (let ((hr (if (> (srfi:date-hour date) 12)
                             (- (srfi:date-hour date) 12) (srfi:date-hour date))))
                 (display (tm:padding hr  #\Space 2)
                          port))))
   (cons #\m (lambda (date pad-with port)
               (display (tm:padding (srfi:date-month date)
                                    pad-with 2)
                        port)))
   (cons #\M (lambda (date pad-with port)
               (display (tm:padding (srfi:date-minute date)
                                    pad-with 2)
                        port)))
   (cons #\n (lambda (date pad-with port)
               (newline port)))
   (cons #\N (lambda (date pad-with port)
               (display (tm:padding (date-nanosecond date)
                                    pad-with 9)
                        port)))
   (cons #\p (lambda (date pad-with port)
               (display (tm:locale-am/pm (srfi:date-hour date)) port)))
   (cons #\r (lambda (date pad-with port)
               (display (date->string date "~I:~M:~S ~p") port)))
   (cons #\s (lambda (date pad-with port)
               (display (time-second (date->time-utc date)) port)))
   (cons #\S (lambda (date pad-with port)
               (if (> (date-nanosecond date)
                      tm:nano)
                   (display (tm:padding (+ (srfi:date-second date) 1)
                                        pad-with 2)
                            port)
                   (display (tm:padding (srfi:date-second date)
                                        pad-with 2)
                            port))))
   (cons #\t (lambda (date pad-with port)
               (display #\Tab port)))
   (cons #\T (lambda (date pad-with port)
               (display (date->string date "~H:~M:~S") port)))
   (cons #\U (lambda (date pad-with port)
               (if (> (tm:days-before-first-week date 0) 0)
                   (display (tm:padding (+ (date-week-number date 0) 1)
                                        #\0 2) port)
                   (display (tm:padding (date-week-number date 0)
                                        #\0 2) port))))
   (cons #\V (lambda (date pad-with port)
               (display (tm:padding (date-week-number date 1)
                                    #\0 2) port)))
   (cons #\w (lambda (date pad-with port)
               (display (srfi:date-week-day date) port)))
   (cons #\x (lambda (date pad-with port)
               (display (date->string date (localized-message tm:locale-short-date-format)) port)))
   (cons #\X (lambda (date pad-with port)
               (display (date->string date (localized-message tm:locale-time-format)) port)))
   (cons #\W (lambda (date pad-with port)
               (if (> (tm:days-before-first-week date 1) 0)
                   (display (tm:padding (+ (date-week-number date 1) 1)
                                        #\0 2) port)
                   (display (tm:padding (date-week-number date 1)
                                        #\0 2) port))))
   (cons #\y (lambda (date pad-with port)
               (display (tm:padding (tm:last-n-digits
                                     (srfi:date-year date) 2)
                                    pad-with
                                    2)
                        port)))
   (cons #\Y (lambda (date pad-with port)
               (display (srfi:date-year date) port)))
   (cons #\z (lambda (date pad-with port)
               (tm:tz-printer (date-zone-offset date) port)))
   (cons #\Z (lambda (date pad-with port)
               (tm:locale-print-time-zone date port)))
   (cons #\1 (lambda (date pad-with port)
               (display (date->string date "~Y-~m-~d") port)))
   (cons #\2 (lambda (date pad-with port)
               (display (date->string date "~k:~M:~S~z") port)))
   (cons #\3 (lambda (date pad-with port)
               (display (date->string date "~k:~M:~S") port)))
   (cons #\4 (lambda (date pad-with port)
               (display (date->string date "~Y-~m-~dT~k:~M:~S~z") port)))
   (cons #\5 (lambda (date pad-with port)
               (display (date->string date "~Y-~m-~dT~k:~M:~S") port)))
   ))


(define (tm:get-formatter char)
  (let ( (associated (assoc char tm:directives)) )
    (if associated (cdr associated) #f)))

(define (tm:date-printer date index format-string str-len port)
  (if (>= index str-len)
      (values)
      (let ( (current-char (string-ref format-string index)) )
        (if (not (char=? current-char #\~))
            (begin
              (display current-char port)
              (tm:date-printer date (+ index 1) format-string str-len port))
            (if (= (+ index 1) str-len) ; bad format string.
                (tm:time-error 'tm:date-printer 'bad-date-format-string
                               format-string)
                (let ( (pad-char? (string-ref format-string (+ index 1))) )
                  (cond
                    ((char=? pad-char? #\-)
                     (if (= (+ index 2) str-len) ; bad format string.
                         (tm:time-error 'tm:date-printer 'bad-date-format-string
                                        format-string)
                         (let ( (formatter (tm:get-formatter
                                            (string-ref format-string
                                                        (+ index 2)))) )
                           (if (not formatter)
                               (tm:time-error 'tm:date-printer 'bad-date-format-string
                                              format-string)
                               (begin
                                 (formatter date #f port)
                                 (tm:date-printer date (+ index 3)
                                                  format-string str-len port))))))

                    ((char=? pad-char? #\_)
                     (if (= (+ index 2) str-len) ; bad format string.
                         (tm:time-error 'tm:date-printer 'bad-date-format-string
                                        format-string)
                         (let ( (formatter (tm:get-formatter
                                            (string-ref format-string
                                                        (+ index 2)))) )
                           (if (not formatter)
                               (tm:time-error 'tm:date-printer 'bad-date-format-string
                                              format-string)
                               (begin
                                 (formatter date #\Space port)
                                 (tm:date-printer date (+ index 3)
                                                  format-string str-len port))))))
                    (else
                     (let ( (formatter (tm:get-formatter
                                        (string-ref format-string
                                                    (+ index 1)))) )
                       (if (not formatter)
                           (tm:time-error 'tm:date-printer 'bad-date-format-string
                                          format-string)
                           (begin
                             (formatter date #\0 port)
                             (tm:date-printer date (+ index 2)
                                              format-string str-len port))))))))))))


(define (date->string date [format-string "~c"])
  (unless (string? format-string)
    (raise-type-error 'date->string "string" 1 date format-string))
  (let ( (str-port (open-output-string)) )
    (tm:date-printer date 0 format-string (string-length format-string) str-port)
    (get-output-string str-port)))

(define (tm:char->int ch)
  (cond
    ((char=? ch #\0) 0)
    ((char=? ch #\1) 1)
    ((char=? ch #\2) 2)
    ((char=? ch #\3) 3)
    ((char=? ch #\4) 4)
    ((char=? ch #\5) 5)
    ((char=? ch #\6) 6)
    ((char=? ch #\7) 7)
    ((char=? ch #\8) 8)
    ((char=? ch #\9) 9)
    (else (tm:time-error 'bad-date-template-string
                         'digit-char
                         ch))))

;; read an integer upto n characters long on port; upto -> #f if any length
(define (tm:integer-reader upto port)
  (define (accum-int port accum nchars)
    (let ((ch (peek-char port)))
      (if (or (eof-object? ch)
              (not (char-numeric? ch))
              (and upto (>= nchars  upto )))
          accum
          (accum-int port (+ (* accum 10) (tm:char->int (read-char port))) (+ nchars 1)))))
  (accum-int port 0 0))

(define (tm:make-integer-reader upto)
  (lambda (port)
    (tm:integer-reader upto port)))

;; read an fractional integer upto n characters long on port; upto -> #f if any length
;;
;; The return value is normalized to upto decimal places. For example, if upto is 9 and
;; the string read is "123", the return value is 123000000.
(define (tm:fractional-integer-reader upto port)
  (define (accum-int port accum nchars)
    (let ((ch (peek-char port)))
      (if (or (eof-object? ch)
              (not (char-numeric? ch))
              (and upto (>= nchars  upto )))
          (* accum (expt 10 (- upto nchars)))
          (accum-int port (+ (* accum 10) (tm:char->int (read-char port))) (+ nchars 1)))))
  (accum-int port 0 0))

(define (tm:make-fractional-integer-reader upto)
  (lambda (port)
    (tm:fractional-integer-reader upto port)))

;; read *exactly* n characters and convert to integer; could be padded
(define (tm:integer-reader-exact n port)
  (let ( (padding-ok #t) )
    (define (accum-int port accum nchars)
      (let ((ch (peek-char port)))
        (cond
          ((>= nchars n) accum)
          ((eof-object? ch)
           (tm:time-error 'string->date 'bad-date-template-string
                          "Premature ending to integer read."))
          ((char-numeric? ch)
           (set! padding-ok #f)
           (accum-int port (+ (* accum 10) (tm:char->int (read-char port)))
                      (+ nchars 1)))
          (padding-ok
           (read-char port)             ; consume padding
           (accum-int port accum (+ nchars 1)))
          (else                 ; padding where it shouldn't be
           (tm:time-error 'string->date 'bad-date-template-string
                          "Non-numeric characters in integer read.")))))
    (accum-int port 0 0)))


(define (tm:make-integer-exact-reader n)
  (lambda (port)
    (tm:integer-reader-exact n port)))

(define (tm:zone-reader port)
  (let ( (offset 0)
         (positive? #f) )
    (let ( (ch (read-char port)) )
      (when (eof-object? ch)
        (tm:time-error 'string->date 'bad-date-template-string
                       (list "Invalid time zone +/-" ch)))
      (if (or (char=? ch #\Z) (char=? ch #\z))
          0
          (begin
            (cond
              ((char=? ch #\+) (set! positive? #t))
              ((char=? ch #\-) (set! positive? #f))
              (else
               (tm:time-error 'string->date 'bad-date-template-string
                              (list "Invalid time zone +/-" ch))))
            (let ((ch (read-char port)))
              (when (eof-object? ch)
                (tm:time-error 'string->date 'bad-date-template-string (list "Invalid time zone number" ch)))
              (set! offset (* (tm:char->int ch)
                              10 60 60)))
            (let ((ch (read-char port)))
              (unless (eof-object? ch)
                ;; FIXME: non-existing values should be considered Zero instead of an error
                ;; (tm:time-error 'string->date 'bad-date-template-string (list "Invalid time zone number" ch)))
                (set! offset (+ offset (* (tm:char->int ch) 60 60)))))
            (let ((ch (read-char port)))
              (unless (eof-object? ch)
                ;; FIXME: non-existing values should be considered Zero instead of an error
                ;; (tm:time-error 'string->date 'bad-date-template-string (list "Invalid time zone number" ch)))
                (set! offset (+ offset (* (tm:char->int ch) 10 60)))))
            (let ((ch (read-char port)))
              (unless (eof-object? ch)
                ;; FIXME: non-existing values should be considered Zero instead of an error
                ;; (tm:time-error 'string->date 'bad-date-template-string (list "Invalid time zone number" ch)))
                (set! offset (+ offset (* (tm:char->int ch) 60)))))
            (if positive? offset (- offset)))))))

;; looking at a char, read the char string, run thru indexer, return index
(define (tm:locale-reader port indexer)
  (let ( (string-port (open-output-string)) )
    (define (read-char-string)
      (let ((ch (peek-char port)))
        (if (char-alphabetic? ch)
            (begin (write-char (read-char port) string-port)
                   (read-char-string))
            (get-output-string string-port))))
    (let* ( (str (read-char-string))
            (index (indexer str)) )
      (if index index (tm:time-error 'string->date
                                     'bad-date-template-string
                                     (list "Invalid string for " indexer))))))

(define (tm:make-locale-reader indexer)
  (lambda (port)
    (tm:locale-reader port indexer)))

(define (tm:make-char-id-reader char)
  (lambda (port)
    (if (char=? char (read-char port))
        char
        (tm:time-error 'string->date
                       'bad-date-template-string
                       "Invalid character match."))))

;; A List of formatted read directives.
;; Each entry is a list.
;; 1. the character directive;
;; a procedure, which takes a character as input & returns
;; 2. #t as soon as a character on the input port is acceptable
;; for input,
;; 3. a port reader procedure that knows how to read the current port
;; for a value. Its one parameter is the port.
;; 4. a action procedure, that takes the value (from 3.) and some
;; object (here, always the date) and (probably) side-effects it.
;; In some cases (e.g., ~A) the action is to do nothing
(define tm:read-directives
  (let ( (ireader4 (tm:make-integer-reader 4))
         (ireader2 (tm:make-integer-reader 2))
         (fireader9 (tm:make-fractional-integer-reader 9))
         (ireaderf (tm:make-integer-reader #f))
         (eireader2 (tm:make-integer-exact-reader 2))
         (eireader4 (tm:make-integer-exact-reader 4))
         (locale-reader-abbr-weekday (tm:make-locale-reader
                                      tm:locale-abbr-weekday->index))
         (locale-reader-long-weekday (tm:make-locale-reader
                                      tm:locale-long-weekday->index))
         (locale-reader-abbr-month   (tm:make-locale-reader
                                      tm:locale-abbr-month->index))
         (locale-reader-long-month   (tm:make-locale-reader
                                      tm:locale-long-month->index))
         (char-fail (lambda (ch) #t))
         (do-nothing (lambda (val object) (values)))
         )

    (list
     (list #\~ char-fail (tm:make-char-id-reader #\~) do-nothing)
     (list #\a char-alphabetic? locale-reader-abbr-weekday do-nothing)
     (list #\A char-alphabetic? locale-reader-long-weekday do-nothing)
     (list #\b char-alphabetic? locale-reader-abbr-month
           (lambda (val object)
             (struct-copy lax-date object [month val])))
     (list #\B char-alphabetic? locale-reader-long-month
           (lambda (val object)
             (struct-copy lax-date object [month val])))
     (list #\d char-numeric? ireader2
           (lambda (val object)
             (struct-copy lax-date object [day val])))
     (list #\e char-fail eireader2
           (lambda (val object)
             (struct-copy lax-date object [day val])))
     (list #\h char-alphabetic? locale-reader-abbr-month
           (lambda (val object)
             (struct-copy lax-date object [month val])))
     (list #\H char-numeric? ireader2
           (lambda (val object)
             (struct-copy lax-date object [hour val])))
     (list #\k char-fail eireader2
           (lambda (val object)
             (struct-copy lax-date object [hour val])))
     (list #\m char-numeric? ireader2
           (lambda (val object)
             (struct-copy lax-date object [month val])))
     (list #\M char-numeric? ireader2
           (lambda (val object)
             (struct-copy lax-date object [minute val])))
     (list #\N char-numeric? fireader9
           (lambda (val object)
             (struct-copy lax-date object [nanosecond val])))
     (list #\S char-numeric? ireader2
           (lambda (val object)
             (struct-copy lax-date object [second val])))
     (list #\y char-fail eireader2
           (lambda (val object)
             (struct-copy lax-date object
                          [year (tm:natural-year val)])))
     (list #\Y char-numeric? ireader4
           (lambda (val object)
             (struct-copy lax-date object [year val])))
     (list #\z (lambda (c)
                 (or (char=? c #\Z)
                     (char=? c #\z)
                     (char=? c #\+)
                     (char=? c #\-)))
           tm:zone-reader
           (lambda (val object)
             (struct-copy lax-date object [zone-offset val])))
     ; PLT-specific extension for 2- or 4-digit years:
     (list #\? char-numeric? ireader4
           (lambda (val object)
             (struct-copy lax-date object
                          [year (tm:natural-year val)])))
     )))

(define (tm:string->date date index format-string str-len port template-string)
  (define (skip-until port skipper)
    (let ((ch (peek-char port)))
      (if (eof-object? ch)
          (tm:time-error 'string->date 'bad-date-format-string template-string)
          (unless (skipper ch)
            (read-char port)
            (skip-until port skipper)))))
  (if (>= index str-len)
      date
      (let ( (current-char (string-ref format-string index)) )
        (if (not (char=? current-char #\~))
            (let ((port-char (read-char port)))
              (when (or (eof-object? port-char)
                        (not (char=? current-char port-char)))
                (tm:time-error 'string->date 'bad-date-format-string template-string))
              (tm:string->date date (+ index 1) format-string str-len port template-string))
            ;; otherwise, it's an escape, we hope
            (if (> (+ index 1) str-len)
                (tm:time-error 'string->date 'bad-date-format-string template-string)
                (let* ( (format-char (string-ref format-string (+ index 1)))
                        (format-info (assoc format-char tm:read-directives)) )
                  (if (not format-info)
                      (tm:time-error 'string->date 'bad-date-format-string template-string)
                      (begin
                        (let ((skipper (cadr format-info))
                              (reader  (caddr format-info))
                              (actor   (cadddr format-info)))
                          (skip-until port skipper)
                          (define new-date
                            (let ((val (reader port)))
                              (if (eof-object? val)
                                  (tm:time-error 'string->date 'bad-date-format-string template-string)
                                  (actor val date))))
                          (tm:string->date new-date (+ index 2) format-string str-len port template-string))))))))))


(define (string->date input-string template-string)
  (define (tm:date-ok? date)
    (and (date-nanosecond date)
         (srfi:date-second date)
         (srfi:date-minute date)
         (srfi:date-hour date)
         (srfi:date-day date)
         (srfi:date-month date)
         (srfi:date-year date)
         (date-zone-offset date)))
  (let* ([initial (lax-date 0 0 0 0 #t #t #t (tm:local-tz-offset))]
         [newdate (tm:string->date
                   initial
                   0
                   template-string
                   (string-length template-string)
                   (open-input-string input-string)
                   template-string)])
    (if (tm:date-ok? newdate)
        (lax-date->date* newdate)
        (tm:time-error 'string->date 'bad-date-format-string (list "Incomplete date read. " newdate template-string)))))



