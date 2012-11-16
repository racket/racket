#lang racket/base

(require racket/promise
         (only-in frtime/frtime [provide frtime:provide]))

(frtime:provide (lifted date->string
                        date-display-format
                        find-seconds
                        date->julian/scalinger
                        julian/scalinger->string))


;; Support for Julian calendar added by Shriram;
;; current version only works until 2099 CE Gregorian

(define legal-formats
  (list 'american 'chinese 'german 'indian 'irish 'julian 'iso-8601 'rfc2822))

(define date-display-format 
  (make-parameter 'american
                  (lambda (s)
                    (unless (memq s legal-formats)
                      (raise-type-error 'date-display-format
                                        (format "symbol in ~a" legal-formats)
                                        s))
                    s)))

(define month/number->string
  (lambda (x)
    (case x
      [(12) "December"] [(1) "January"]  [(2) "February"]
      [(3) "March"]     [(4) "April"]    [(5) "May"]
      [(6) "June"]      [(7) "July"]     [(8) "August"]
      [(9) "September"] [(10) "October"] [(11) "November"]
      [else ""])))

(define day/number->string
  (lambda (x)
    (case x
      [(0) "Sunday"]
      [(1) "Monday"]
      [(2) "Tuesday"]
      [(3) "Wednesday"]
      [(4) "Thursday"]
      [(5) "Friday"]
      [(6) "Saturday"]
      [else ""])))

(define date->string
  (case-lambda 
    [(date) (date->string date #f)]
    [(date time?)
     (let* ((add-zero (lambda (n) (if (< n 10)
                                      (string-append "0" (number->string n))
                                      (number->string n))))
            (year (number->string (date-year date)))
            (num-month (number->string (date-month date)))
            (week-day (day/number->string (date-week-day date)))
            (week-day-num (date-week-day date))
            (month (month/number->string (date-month date)))
            (day (number->string (date-day date)))
            (day-th (if (<= 11 (date-day date) 13)
                        "th"
                        (case (modulo (date-day date) 10)
                          [(1) "st"]
                          [(2) "nd"]
                          [(3) "rd"]
                          [(0 4 5 6 7 8 9) "th"])))
            (hour (date-hour date))
            (am-pm (if (>= hour 12) "pm" "am"))
            (hour24 (add-zero hour))
            (hour12 (number->string 
                     (cond
                       [(zero? hour) 12]
                       [(> hour 12) (- hour 12)]
                       [else hour])))
            (minute (add-zero (date-minute date)))
            (second (add-zero (date-second date))))
       (let-values
           ([(day time)
             (case (date-display-format)
               [(american) 
                (values (list week-day ", " month " " day day-th ", " year)
                        (list " " hour12 ":" minute ":" second am-pm))]
               [(chinese)
                (values
                 (list year "/" num-month "/" day
                       " \u661F\u671F" (case (date-week-day date)
                                         [(0) "\u5929"]
                                         [(1) "\u4E00"]
                                         [(2) "\u4E8C"]
                                         [(3) "\u4e09"]
                                         [(4) "\u56DB"]
                                         [(5) "\u4E94"]
                                         [(6) "\u516D"]
                                         [else ""]))
                 (list " " hour24 ":" minute ":" second))]
               [(indian) 
                (values (list day "-" num-month "-" year)
                        (list " " hour12 ":" minute ":" second am-pm))]
               [(german) 
                (values (list day ". " 
                              (case (date-month date)
                                [(1) "Januar"]
                                [(2) "Februar"]
                                [(3) "M\344rz"]
                                [(4) "April"]
                                [(5) "Mai"]
                                [(6) "Juni"]
                                [(7) "Juli"]
                                [(8) "August"]
                                [(9) "September"]
                                [(10) "Oktober"]
                                [(11) "November"]
                                [(12) "Dezember"]
                                [else ""])
                              " " year)
                        (list ", " hour24 "." minute))]
               [(irish) 
                (values (list week-day ", " day day-th " " month " " year)
                        (list ", " hour12 ":" minute am-pm))]
               [(julian)
                (values (list (julian/scalinger->string
                               (date->julian/scalinger date)))
                        (list ", " hour24 ":" minute ":" second))]
               [(iso-8601)
                (values
                 (list year "-" (add-zero (date-month date)) "-" (add-zero (date-day date)))
                 (list " " hour24 ":" minute ":" second))]
               [(rfc2822)
                (values
                 (list (substring week-day 0 3) ", " day " " (substring month 0 3) " " year)
                 (list* " " hour24 ":" minute ":" second " "
                        (let* ([delta (date-time-zone-offset date)]
                               [hours (quotient delta 3600)]
                               [minutes (modulo (quotient delta 60) 60)])
                          (list
                           (if (negative? delta) "-" "")
                           (add-zero (abs hours))
                           (add-zero minutes)))))]
               [else (error 'date->string "unknown date-display-format: ~s"
                            (date-display-format))])])
         (apply string-append (if time?
                                  (append day time)
                                  day))))]))

(define leap-year?
  (lambda (year)
    (or (= 0 (modulo year 400))
        (and (= 0 (modulo year 4))
             (not (= 0 (modulo year 100)))))))

;; it's not clear what months mean in this context -- use days
(define-struct date-offset (second minute hour day year))

(define date-
  (lambda (date1 date2)
    (let* ((second (- (date-second date1) (date-second date2)))
           (minute (+ (- (date-minute date1) (date-minute date2))
                      (if (< second 0) -1 0)))
           (hour (+ (- (date-hour date1) (date-hour date2))
                    (if (< minute 0) -1 0)
                    (cond [(equal? (date-dst? date1) (date-dst? date2)) 0]
                          [(date-dst? date1) -1]
                          [(date-dst? date2) 1])))
           (day (+ (- (date-year-day date1) (date-year-day date2))
                   (if (< hour 0) -1 0)))
           (year (+ (- (date-year date1) (date-year date2))
                    (if (< day 0) -1 0)))
           (fixup (lambda (s x) (if (< s 0) (+ s x) s))))
      (make-date-offset (fixup second 60)
                        (fixup minute 60)
                        (fixup hour 24)
                        (fixup day (if (leap-year? (date-year date1)) 366 365))
                        year))))


(define date-offset->string
  (let ((first car)
        (second cadr))
    (case-lambda 
      [(date) (date-offset->string date #f)]
      [(date seconds?)
       (let* ((fields (list (list (date-offset-year date) "year")
                            (list (date-offset-day date) "day")
                            (list (date-offset-hour date) "hour")
                            (list (date-offset-minute date) "minute")
                            (list (if seconds? (date-offset-second date) 0) "second")))
              (non-zero-fields (foldl (lambda (x l)
                                        (if (= 0 (first x))
                                            l
                                            (cons x l)))
                                      null
                                      fields))
              (one-entry (lambda (b)
                           (string-append
                            (number->string (first b))
                            " "
                            (second b)
                            (if (= 1 (first b)) "" "s")))))
         (cond
           [(null? non-zero-fields) ""]
           [(null? (cdr non-zero-fields)) (one-entry (car non-zero-fields))]
           [else (foldl (lambda (b string)
                          (cond
                            [(= 0 (first b)) string]
                            [(string=? string "")
                             (string-append "and "
                                            (one-entry b)
                                            string)]
                            [else (string-append (one-entry b) ", " string)]))
                        ""
                        non-zero-fields)]))])))

(define days-per-month
  (lambda (year month)
    (cond
      [(and (= month 2) (leap-year? year)) 29]
      [(= month 2) 28]
      [(<= month 7) (+ 30 (modulo month 2))]
      [else (+ 30 (- 1 (modulo month 2)))])))

(define find-extreme-date-seconds
  (lambda (start offset)
    (let/ec found
      (letrec ([find-between
                (lambda (lo hi)
                  (let ([mid (floor (/ (+ lo hi) 2))])
                    (if (or (and (positive? offset) (= lo mid))
                            (and (negative? offset) (= hi mid)))
                        (found lo)
                        (let ([mid-ok?
                               (with-handlers ([exn:fail? (lambda (exn) #f)])
                                 (seconds->date mid)
                                 #t)])
                          (if mid-ok?
                              (find-between mid hi)
                              (find-between lo mid))))))])
        (let loop ([lo start][offset offset])
          (let ([hi (+ lo offset)])
            (with-handlers ([exn:fail? 
                             (lambda (exn) 
                               ; failed - must be between lo & hi
                               (find-between lo hi))])
              (seconds->date hi))
            ; succeeded; double offset again
            (loop hi (* 2 offset))))))))

(define get-min-seconds
  (let ([d (delay (find-extreme-date-seconds (current-seconds) -1))])
    (lambda ()
      (force d))))
(define get-max-seconds
  (let ([d (delay (find-extreme-date-seconds (current-seconds) 1))])
    (lambda ()
      (force d))))

(define find-seconds
  (lambda (sec min hour day month year)
    (let ([signal-error
           (lambda (msg)
             (error 'find-secs (string-append 
                                msg 
                                " (inputs: ~a ~a ~a ~a ~a ~a)")
                    sec min hour day month year))])
      (let loop ([below-secs (get-min-seconds)]
                 [secs (floor (/ (+ (get-min-seconds) (get-max-seconds)) 2))]
                 [above-secs (get-max-seconds)])
        (let* ([date (seconds->date secs)]
               [compare
                (let loop ([inputs (list year month day 
                                         hour min sec)]
                           [tests (list (date-year date)
                                        (date-month date)
                                        (date-day date)
                                        (date-hour date)
                                        (date-minute date)
                                        (date-second date))])
                  (cond
                    [(null? inputs) 'equal]
                    [else (let ([input (car inputs)]
                                [test (car tests)])
                            (if (= input test)
                                (loop (cdr inputs) (cdr tests))
                                (if (<= input test)
                                    'input-smaller
                                    'test-smaller)))]))])
          ; (printf "~a ~a ~a\n" compare secs (date->string date))
          (cond
            [(eq? compare 'equal) secs]
            [(or (= secs below-secs) (= secs above-secs))
             (signal-error "non-existent date")]
            [(eq? compare 'input-smaller) 
             (loop below-secs (floor (/ (+ secs below-secs) 2)) secs)]
            [(eq? compare 'test-smaller) 
             (loop secs (floor (/ (+ above-secs secs) 2)) above-secs)]))))))

;; date->julian/scalinger :
;; date -> number [julian-day]

;; Note: This code is correct until 2099 CE Gregorian

(define (date->julian/scalinger date)
  (let ((day (date-day date))
        (month (date-month date))
        (year (date-year date)))
    (let ((year (+ 4712 year)))
      (let ((year (if (< month 3) (sub1 year) year)))
        (let ((cycle-number (quotient year 4))
              (cycle-position (remainder year 4)))
          (let ((base-day (+ (* 1461 cycle-number) (* 365 cycle-position))))
            (let ((month-day-number (case month
                                      ((3) 0)
                                      ((4) 31)
                                      ((5) 61)
                                      ((6) 92)
                                      ((7) 122)
                                      ((8) 153)
                                      ((9) 184)
                                      ((10) 214)
                                      ((11) 245)
                                      ((12) 275)
                                      ((1) 306)
                                      ((2) 337))))
              (let ((total-days (+ base-day month-day-number day)))
                (let ((total-days/march-adjustment (+ total-days 59)))
                  (let ((gregorian-adjustment (cond
                                                ((< year 1700) 11)
                                                ((< year 1800) 12)
                                                (else 13))))
                    (let ((final-date (- total-days/march-adjustment
                                         gregorian-adjustment)))
                      final-date)))))))))))

;; julian/scalinger->string :
;; number [julian-day] -> string [julian-day-format]

(define (julian/scalinger->string julian-day)
  (apply string-append
         (cons "JD "
               (reverse
                (let loop ((reversed-digits (map number->string
                                                 (let loop ((jd julian-day))
                                                   (if (zero? jd) null
                                                       (cons (remainder jd 10)
                                                             (loop (quotient jd 10))))))))
                  (cond
                    ((or (null? reversed-digits)
                         (null? (cdr reversed-digits))
                         (null? (cdr (cdr reversed-digits))))
                     (list (apply string-append reversed-digits)))
                    (else (cons (apply string-append
                                       (list " "
                                             (caddr reversed-digits)
                                             (cadr reversed-digits)
                                             (car reversed-digits)))
                                (loop (cdr (cdr (cdr reversed-digits))))))))))))
