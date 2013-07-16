#lang racket/base

;; Tests by Will Fitzgerald, augmented by:
;;   John Clements                                -- 2004-08-16
;;   Dave Gurnell  (string->date, date->string)   -- 2007-09-14
;;   Dave Gurnell  (time{=,<,>,<=,>=}?)           -- 2009-11-26
;;   John Clements (nanoseconds off by x100)      -- 2009-12-15
;;   Dave Gurnell  (serializable dates and times) -- 2010-03-03
;;   Dave Gurnell  (added ~x for string->date)    -- 2010-03-10
(require (only-in racket/date date->julian/scalinger)
         racket/serialize
         srfi/19/time)

(require rackunit
         rackunit/text-ui)

(define-check (check-comparisons comparison times expected)
  (for ([time0    (in-list times)]
        [expected (in-list expected)])
    (for ([time1    (in-list times)]
          [expected (in-list expected)])
      (with-check-info (['comparison comparison]
                        ['time0      time0]
                        ['time1      time1])
        (let ([actual (comparison time0 time1)])
          (check-equal? actual expected))))))

(define cur-tz (date-zone-offset (current-date)))

; Test suite -------------------------------------

(define srfi-19-test-suite
  (test-suite "Tests for SRFI 19"

    (test-not-exn "Creating time structures"
      (lambda ()
        (list (current-time 'time-tai)
              (current-time 'time-utc)
              (current-time 'time-monotonic)
              (current-time 'time-thread)
              (current-time 'time-process))))

    (test-not-exn "Testing time resolutions"
      (lambda ()
        (list (time-resolution 'time-tai)
              (time-resolution 'time-utc)
              (time-resolution 'time-monotonic)
              (time-resolution 'time-thread)
              (time-resolution 'time-process))))

    (test-case "Time comparisons (time=?, etc.)"
      (let ([t0 (make-time 'time-utc 0 1)]
            [t1 (make-time 'time-utc 0 1)]
            [t2 (make-time 'time-utc 1 1)]
            [t3 (make-time 'time-utc 0 2)])
        (check-comparisons time=?  (list t0 t1 t2 t3) '((#t #t #f #f)
                                                        (#t #t #f #f)
                                                        (#f #f #t #f)
                                                        (#f #f #f #t)))
        (check-comparisons time<?  (list t0 t1 t2 t3) '((#f #f #t #t)
                                                        (#f #f #t #t)
                                                        (#f #f #f #t)
                                                        (#f #f #f #f)))
        (check-comparisons time>?  (list t0 t1 t2 t3) '((#f #f #f #f)
                                                        (#f #f #f #f)
                                                        (#t #t #f #f)
                                                        (#t #t #t #f)))
        (check-comparisons time<=? (list t0 t1 t2 t3) '((#t #t #t #t)
                                                        (#t #t #t #t)
                                                        (#f #f #t #t)
                                                        (#f #f #f #t)))
        (check-comparisons time>=? (list t0 t1 t2 t3) '((#t #t #f #f)
                                                        (#t #t #f #f)
                                                        (#t #t #t #f)
                                                        (#t #t #t #t)))))

    (test-case "Time difference"
      (let ((t1 (make-time 'time-utc 0 3000))
            (t2 (make-time 'time-utc 0 1000))
            (t3 (make-time 'time-duration 0 2000))
            (t4 (make-time 'time-duration 0 -2000)))
        (check time=? t3 (time-difference t1 t2))
        (check time=? t4 (time-difference t2 t1))))

    (test-case "TAI-UTC Conversions"
      (check-one-utc-tai-edge 915148800  32 31)
      (check-one-utc-tai-edge 867715200  31 30)
      (check-one-utc-tai-edge 820454400  30 29)
      (check-one-utc-tai-edge 773020800  29 28)
      (check-one-utc-tai-edge 741484800  28 27)
      (check-one-utc-tai-edge 709948800  27 26)
      (check-one-utc-tai-edge 662688000  26 25)
      (check-one-utc-tai-edge 631152000  25 24)
      (check-one-utc-tai-edge 567993600  24 23)
      (check-one-utc-tai-edge 489024000  23 22)
      (check-one-utc-tai-edge 425865600  22 21)
      (check-one-utc-tai-edge 394329600  21 20)
      (check-one-utc-tai-edge 362793600  20 19)
      (check-one-utc-tai-edge 315532800  19 18)
      (check-one-utc-tai-edge 283996800  18 17)
      (check-one-utc-tai-edge 252460800  17 16)
      (check-one-utc-tai-edge 220924800  16 15)
      (check-one-utc-tai-edge 189302400  15 14)
      (check-one-utc-tai-edge 157766400  14 13)
      (check-one-utc-tai-edge 126230400  13 12)
      (check-one-utc-tai-edge 94694400   12 11)
      (check-one-utc-tai-edge 78796800   11 10)
      (check-one-utc-tai-edge 63072000   10 0)
      (check-one-utc-tai-edge 0           0 0)   ;; at the epoch
      (check-one-utc-tai-edge 10          0 0)   ;; close to it ...
      (check-one-utc-tai-edge 1045789645 32 32)) ;; about now ...

    (test-case "TAI-Date Conversions"
      (check tm:date= (time-tai->date (make-time time-tai 0 (+ 915148800 29)) 0)
             (srfi:make-date 0 58 59 23 31 12 1998 0))
      (check tm:date= (time-tai->date (make-time time-tai 0 (+ 915148800 30)) 0)
             (srfi:make-date 0 59 59 23 31 12 1998 0))
      (check tm:date= (time-tai->date (make-time time-tai 0 (+ 915148800 31)) 0)
             (srfi:make-date 0 60 59 23 31 12 1998 0))
      (check tm:date= (time-tai->date (make-time time-tai 0 (+ 915148800 32)) 0)
             (srfi:make-date 0 0 0 0 1 1 1999 0)))

    (test-case "Date-UTC Conversions"
      (check time=? (make-time time-utc 0 (- 915148800 2))
             (date->time-utc (srfi:make-date 0 58 59 23 31 12 1998 0)))
      (check time=? (make-time time-utc 0 (- 915148800 1))
             (date->time-utc (srfi:make-date 0 59 59 23 31 12 1998 0)))
      ;; yes, I think this is actually right.
      (check time=? (make-time time-utc 0 (- 915148800 0))
             (date->time-utc (srfi:make-date 0 60 59 23 31 12 1998 0)))
      (check time=? (make-time time-utc 0 (- 915148800 0))
             (date->time-utc (srfi:make-date 0 0 0 0 1 1 1999 0)))
      (check time=? (make-time time-utc 0 (+ 915148800 1))
             (date->time-utc (srfi:make-date 0 1 0 0 1 1 1999 0))))

    (test-case "TZ Offset conversions"
      (let ((ct-utc (make-time time-utc 6320000 1045944859))
            (ct-tai (make-time time-tai 6320000 1045944891))
            (cd (srfi:make-date 6320000 19 14 15 22 2 2003 -18000)))
        (check time=? ct-utc (date->time-utc cd))
        (check time=? ct-tai (date->time-tai cd))))


    ;; NOTE: documentation doesn't fully specify, e.g., zero-padding on ~c option, so I'm just going
    ;; to change the test case to match the implementation...
    (test-case "date->string conversions"
      (check-equal? (date->string (srfi:make-date 1000 2 3 4 5 6 2007 (* 60 -120))
                                  "~~ @ ~a @ ~A @ ~b @ ~B @ ~c @ ~d @ ~D @ ~e @ ~f @ ~h @ ~H")
                    "~ @ Tue @ Tuesday @ Jun @ June @ Tue Jun 05 04:03:02-0200 2007 @ 05 @ 06/05/07 @  5 @ 02.000001 @ Jun @ 04"))



    (test-case "date->string conversions of dates with nanosecond components"
      (check-equal? (date->string (srfi:make-date 123456789 2 3 4 5 6 2007 0) "~N") "123456789")
      (check-equal? (date->string (srfi:make-date 12345678  2 3 4 5 6 2007 0) "~N") "012345678")
      (check-equal? (date->string (srfi:make-date 1234567   2 3 4 5 6 2007 0) "~N") "001234567")
      (check-equal? (date->string (srfi:make-date 123456    2 3 4 5 6 2007 0) "~N") "000123456")
      (check-equal? (date->string (srfi:make-date 12345     2 3 4 5 6 2007 0) "~N") "000012345")
      (check-equal? (date->string (srfi:make-date 1234      2 3 4 5 6 2007 0) "~N") "000001234")
      (check-equal? (date->string (srfi:make-date 123       2 3 4 5 6 2007 0) "~N") "000000123")
      (check-equal? (date->string (srfi:make-date 12        2 3 4 5 6 2007 0) "~N") "000000012")
      (check-equal? (date->string (srfi:make-date 1         2 3 4 5 6 2007 0) "~N") "000000001"))

    (test-case "string->date conversions of dates with nanosecond components"
      (check-equal? (string->date "12:00:00.123456789" "~H:~M:~S.~N") (srfi:make-date 123456789 0 0 12 #t #t #t cur-tz) "check 1")
      (check-equal? (string->date "12:00:00.12345678"  "~H:~M:~S.~N") (srfi:make-date 123456780 0 0 12 #t #t #t cur-tz) "check 2")
      (check-equal? (string->date "12:00:00.1234567"   "~H:~M:~S.~N") (srfi:make-date 123456700 0 0 12 #t #t #t cur-tz) "check 3")
      (check-equal? (string->date "12:00:00.123456"    "~H:~M:~S.~N") (srfi:make-date 123456000 0 0 12 #t #t #t cur-tz) "check 4")
      (check-equal? (string->date "12:00:00.12345"     "~H:~M:~S.~N") (srfi:make-date 123450000 0 0 12 #t #t #t cur-tz) "check 5")
      (check-equal? (string->date "12:00:00.1234"      "~H:~M:~S.~N") (srfi:make-date 123400000 0 0 12 #t #t #t cur-tz) "check 6")
      (check-equal? (string->date "12:00:00.123"       "~H:~M:~S.~N") (srfi:make-date 123000000 0 0 12 #t #t #t cur-tz) "check 7")
      (check-equal? (string->date "12:00:00.12"        "~H:~M:~S.~N") (srfi:make-date 120000000 0 0 12 #t #t #t cur-tz) "check 8")
      (check-equal? (string->date "12:00:00.1"         "~H:~M:~S.~N") (srfi:make-date 100000000 0 0 12 #t #t #t cur-tz) "check 9")
      (check-equal? (string->date "12:00:00.123456789" "~H:~M:~S.~N") (srfi:make-date 123456789 0 0 12 #t #t #t cur-tz) "check 10")
      (check-equal? (string->date "12:00:00.012345678" "~H:~M:~S.~N") (srfi:make-date 12345678  0 0 12 #t #t #t cur-tz) "check 11")
      (check-equal? (string->date "12:00:00.001234567" "~H:~M:~S.~N") (srfi:make-date 1234567   0 0 12 #t #t #t cur-tz) "check 12")
      (check-equal? (string->date "12:00:00.000123456" "~H:~M:~S.~N") (srfi:make-date 123456    0 0 12 #t #t #t cur-tz) "check 13")
      (check-equal? (string->date "12:00:00.000012345" "~H:~M:~S.~N") (srfi:make-date 12345     0 0 12 #t #t #t cur-tz) "check 14")
      (check-equal? (string->date "12:00:00.000001234" "~H:~M:~S.~N") (srfi:make-date 1234      0 0 12 #t #t #t cur-tz) "check 15")
      (check-equal? (string->date "12:00:00.000000123" "~H:~M:~S.~N") (srfi:make-date 123       0 0 12 #t #t #t cur-tz) "check 16")
      (check-equal? (string->date "12:00:00.000000012" "~H:~M:~S.~N") (srfi:make-date 12        0 0 12 #t #t #t cur-tz) "check 17")
      (check-equal? (string->date "12:00:00.000000001" "~H:~M:~S.~N") (srfi:make-date 1         0 0 12 #t #t #t cur-tz) "check 18"))

    (test-case "interpretation of 1- to 4-digit years by ~y, ~Y and ~?:"
      ; ~y:
      (check-exn exn:fail? (lambda () (string->date    "1-03-02" "~y-~m-~d")))
      (check-not-exn (lambda () (check-equal? (string->date   "10-03-02" "~y-~m-~d") (srfi:make-date 0 0 0 0 2 3 2010 cur-tz))))
      (check-exn exn:fail? (lambda () (string->date  "100-03-02" "~y-~m-~d")))
      (check-exn exn:fail? (lambda () (string->date "1000-03-02" "~y-~m-~d")))
      ; ~Y:
      (check-not-exn (lambda () (check-equal? (string->date    "1-03-02" "~Y-~m-~d") (srfi:make-date 0 0 0 0 2 3    1 cur-tz))))
      (check-not-exn (lambda () (check-equal? (string->date   "10-03-02" "~Y-~m-~d") (srfi:make-date 0 0 0 0 2 3   10 cur-tz))))
      (check-not-exn (lambda () (check-equal? (string->date  "100-03-02" "~Y-~m-~d") (srfi:make-date 0 0 0 0 2 3  100 cur-tz))))
      (check-not-exn (lambda () (check-equal? (string->date "1000-03-02" "~Y-~m-~d") (srfi:make-date 0 0 0 0 2 3 1000 cur-tz))))
      ; ~? (PLT-specific extension for 2- or 4-digit years:
      (check-not-exn (lambda () (check-equal? (string->date    "1-03-02" "~?-~m-~d") (srfi:make-date 0 0 0 0 2 3 2001 cur-tz))))
      (check-not-exn (lambda () (check-equal? (string->date   "10-03-02" "~?-~m-~d") (srfi:make-date 0 0 0 0 2 3 2010 cur-tz))))
      (check-not-exn (lambda () (check-equal? (string->date  "100-03-02" "~?-~m-~d") (srfi:make-date 0 0 0 0 2 3  100 cur-tz))))
      (check-not-exn (lambda () (check-equal? (string->date "1000-03-02" "~?-~m-~d") (srfi:make-date 0 0 0 0 2 3 1000 cur-tz)))))

    (test-case "type-like error on date->string"
               (check-exn
                (lambda (exn)
                  (regexp-match #px"expects type <string>"
                                (exn-message exn)))
                (lambda () (date->string (srfi:make-date 1000 2 3 4 2 5 2011 (* 60 -120)) #t))))


    (test-case "date<->julian-day conversion"
      (check = 365 (- (date->julian-day (srfi:make-date 0 0 0 0 1 1 2004 0))
                      (date->julian-day (srfi:make-date 0 0 0 0 1 1 2003 0))))
      (let ([test-date (srfi:make-date 0 0 0 0 1 1 2003 -7200)])
        (check tm:date= test-date (julian-day->date (date->julian-day test-date) -7200))))

    (test-case "date->modified-julian-day conversion"
      (check = 365 (- (date->modified-julian-day (srfi:make-date 0 0 0 0 1 1 2004 0))
                      (date->modified-julian-day (srfi:make-date 0 0 0 0 1 1 2003 0))))
      (let ([test-date (srfi:make-date 0 0 0 0 1 1 2003 -7200)])
        (check tm:date= test-date (modified-julian-day->date (date->modified-julian-day test-date) -7200))))

    (test-case "serialize and deserialize"
      (check-equal? (deserialize (serialize (make-time time-utc 0 1))) (make-time time-utc 0 1))
      (check-equal? (deserialize (serialize (make-time time-tai 2 3))) (make-time time-tai 2 3))
      (check-equal? (deserialize (serialize (srfi:make-date 0 1 2 3 4 5 6 7))) (srfi:make-date 0 1 2 3 4 5 6 7)))

    ;; make sure that older srfi-19 structures still deserialize
    (test-case "old deserialization"
      (check-equal? (deserialize '((3) 1 (((lib "srfi/19/time.rkt") . deserialize-info:tm:date-v0))
                                   0 () () (0 0 1 2 3 4 5 6 7)))
                    (srfi:make-date 0 1 2 3 4 5 6 7))
      (check-equal? (deserialize '((3) 1 (((lib "srfi/19/time.rkt") . deserialize-info:tm:date-v0))
                                   0 () () (0 0 0 0 0 1 1 2004 0)))
                    (srfi:make-date 0 0 0 0 1 1 2004 0)))

    ;; test racket/date, lax-date functionality
    (test-case "check date? and lax-date?"
               (check-true (date? (srfi:make-date 0 4 5 0 1 10 2013 0)))
               (check-true (date? (srfi:make-date 0 0 0 0 1 1 2004 0)))
               (check-equal? (date->julian/scalinger (srfi:make-date 0 0 0 0 1 1 2004 0))
                             2453006)
               (check-equal? (date->julian-day (srfi:make-date 0 0 0 0 1 1 2004 0))
                             4906011/2)
               (check-true (lax-date? (srfi:make-date 0 0 0 0 0 1 2004 0)))
               (check-true (lax-date? (srfi:make-date 0 0 0 0 #t 1 2004 0)))
               (check-true (lax-date? (srfi:make-date 0 0 0 0 #t #t 2004 0)))
               (check-true (lax-date? (srfi:make-date 0 0 0 0 #t #t #t 0)))
               (check-equal? (srfi:date-year (srfi:make-date 0 0 0 0 1 1 2004 #t))
                             2004)
               (check-equal? (srfi:date-year (srfi:make-date 0 0 0 0 1 1 2004 0))
                             2004))

    ;; test leap seconds
    (test-case "check latest leap seconds"
               (check-equal?
                1230768032
                (time-second (date->time-tai (srfi:make-date 0 59 59 23 31 12 2008 0))))
               (check-equal?
                1230768033
                (time-second (date->time-tai (srfi:make-date 0 60 59 23 31 12 2008 0))))
               (check-equal?
                1230768034
                (time-second (date->time-tai (srfi:make-date 0 0 0 0 1 1 2009 0))))
               (check-equal?
                1341100833
                (time-second (date->time-tai (srfi:make-date 0 59 59 23 30 6 2012 0))))
               (check-equal?
                1341100834
                (time-second (date->time-tai (srfi:make-date 0 60 59 23 30 6 2012 0))))
               (check-equal?
                1341100835
                (time-second (date->time-tai (srfi:make-date 0 0 0 0 1 7 2012 0)))))

    ;; test off-by-1 year-day behavior
    ;; srfi/19's year day and Racket's year day are 1 and 0 indexed, respectively
    (test-case "test off-by-1 year day"
               (check-false (lax-date? (time-tai->date (make-time 'time-tai 0 1230768033))))
               (check-true (lax-date? (srfi:make-date 0 58 59 18 31 12 2008 #f)))
               (check-equal? (srfi:date-year-day (srfi:make-date 0 58 59 18 31 12 2008 #f))
                             366)
               (check-equal? (srfi:date-year-day (time-tai->date (make-time 'time-tai 0 1230768033)))
                             366)
               (check-equal? (date-year-day (time-tai->date (make-time 'time-tai 0 1230768033)))
                             365))

    ;; nanoseconds off by a factor of 100...
    (test-case "nanosecond order-of-magnitude"
      ;; half a second should be within 1/10th of 10^9 / 2 nanoseconds (currently off by a factor of 100)
      (check-within (let ([t (date-nanosecond (current-date))])
                      (sleep 0.5)
                      (abs (- (date-nanosecond (current-date)) t)))
                    (* 1/2  (expt 10 9))
                    (* 1/10 (expt 10 9))))))

; Helper checks and procedures -----------------

(define-simple-check (check-within actual expected epsilon)
  (< (abs (- actual expected)) epsilon))

(define-check (check-one-utc-tai-edge utc tai-diff tai-last-diff)
  (let* (;; right on the edge they should be the same
         (utc-basic (make-time 'time-utc 0 utc))
         (tai-basic (make-time 'time-tai 0 (+ utc tai-diff)))
         (utc->tai-basic (time-utc->time-tai utc-basic))
         (tai->utc-basic (time-tai->time-utc tai-basic))

         ;; a second before they should be the old diff
         (utc-basic-1 (make-time 'time-utc 0 (- utc 1)))
         (tai-basic-1 (make-time 'time-tai 0 (- (+ utc tai-last-diff) 1)))
         (utc->tai-basic-1 (time-utc->time-tai utc-basic-1))
         (tai->utc-basic-1 (time-tai->time-utc tai-basic-1))

         ;; a second later they should be the new diff
         (utc-basic+1 (make-time 'time-utc 0 (+ utc 1)))
         (tai-basic+1 (make-time 'time-tai 0 (+ (+ utc tai-diff) 1)))
         (utc->tai-basic+1 (time-utc->time-tai utc-basic+1))
         (tai->utc-basic+1 (time-tai->time-utc tai-basic+1))

         ;; ok, let's move the clock half a month or so plus half a second
         (shy (* 15 24 60 60))
         (hs (/ (expt 10 9) 2))
         ;; a second later they should be the new diff
         (utc-basic+2 (make-time 'time-utc hs (+ utc shy)))
         (tai-basic+2 (make-time 'time-tai hs (+ (+ utc tai-diff) shy)))
         (utc->tai-basic+2 (time-utc->time-tai utc-basic+2))
         (tai->utc-basic+2 (time-tai->time-utc tai-basic+2)))

    (check time=? utc-basic tai->utc-basic)
    (check time=? tai-basic utc->tai-basic)
    (check time=? utc-basic-1 tai->utc-basic-1)
    (check time=? tai-basic-1 utc->tai-basic-1)
    (check time=? utc-basic+1 tai->utc-basic+1)
    (check time=? tai-basic+1 utc->tai-basic+1)
    (check time=? utc-basic+2 tai->utc-basic+2)
    (check time=? tai-basic+2 utc->tai-basic+2)))

(define (tm:date= d1 d2)
  (and (= (srfi:date-year d1) (srfi:date-year d2))
       (= (srfi:date-month d1) (srfi:date-month d2))
       (= (srfi:date-day d1) (srfi:date-day d2))
       (= (srfi:date-hour d1) (srfi:date-hour d2))
       (= (srfi:date-second d1) (srfi:date-second d2))
       (= (date-nanosecond d1) (date-nanosecond d2))
       (= (date-zone-offset d1) (date-zone-offset d2))))

; Main module body -------------------------------

(run-tests srfi-19-test-suite)
