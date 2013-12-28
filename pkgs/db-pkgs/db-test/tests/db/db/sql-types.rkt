#lang racket/unit
(require rackunit
         racket/class
         racket/list
         racket/math
         racket/match
         racket/string
         (prefix-in srfi: srfi/19)
         db/base
         db/util/datetime
         db/util/geometry
         db/util/postgresql
         "../config.rkt")
(import config^ database^)
(export test^)

(define current-type (make-parameter #f))

(define-syntax-rule (type-test-case types . body)
  (type-test-case* types (lambda () . body)))

(define (type-test-case* types proc)
  (let* ([known-types
          (if (ANYFLAGS 'sqlite3)
              '(bigint double text blob)
              (send dbsystem get-known-types +inf.0))]
         [type (for/or ([type types])
                 (and (member type known-types) type))])
    (when type
      (test-case (format "~s" type)
        (parameterize ((current-type type)) (proc))))))

(define (check-timestamptz-equal? a b)
  (cond [(and (sql-timestamp? a) (sql-timestamp? b))
         (check srfi:time=?
                (srfi:date->time-utc (sql-datetime->srfi-date a))
                (srfi:date->time-utc (sql-datetime->srfi-date b)))]
        [(and (pg-range? a) (pg-range? b))
         (match (list a b)
           [(list (pg-range alb ali? aub aui?) (pg-range blb bli? bub bui?))
            (and (check-timestamptz-equal? alb blb)
                 (check-equal? ali? bli?)
                 (check-timestamptz-equal? aub bub)
                 (check-equal? aui? bui?))])]
        [else (check-equal? a b)]))

(define (check-bits-equal? a b)
  (check-equal? (sql-bits->string a) (sql-bits->string b)))

(define (supported? option)
  (send dbsystem has-support? option))

(define (pg-type-name type)
  (case type
    ((double) 'float8)
    ((char1) "\"char\"")
    (else
     (cond [(regexp-match #rx"^(.*)-array$" (format "~a" type))
            => (lambda (m)
                 (format "~a[]" (pg-type-name (string->symbol (cadr m)))))]
           [else type]))))

(define (check-roundtrip* c value check-equal?)
  (cond [(ANYFLAGS 'postgresql 'ispg)
         (let* ([tname (pg-type-name (current-type))]
                [q (sql (format "select $1::~a" tname))])
           (check-equal? (query-value c q value)
                         value))]
        [(ANYFLAGS 'mysql 'ismy)
         ;; FIXME: can do better once prepare supports types
         (let ([stmt
                (case (current-type)
                  ((varchar) "select ?") ;;  "select cast(? as char)" gives ODBC problems
                  ((blob) "select cast(? as binary)") ;; ???
                  ((integer) "select cast(? as signed integer)")
                  ((real) "select (? * 1.0)")
                  ((decimal numeric) "select cast(? as decimal(40,10))")
                  ((date) "select cast(? as date)")
                  ((time) "select cast(? as time)")
                  ((datetime) "select cast(? as datetime)")
                  ;; FIXME: more types
                  (else #f))])
           (when stmt
             (check-equal? (query-value c stmt value) value)))]
        [(eq? dbsys 'sqlite3) ;; no ODBC-sqlite3, too painful
         (check-equal? (query-value c "select ?" value) value)]
        [(ANYFLAGS 'isora 'isdb2)
         (let ([stmt
                (case (current-type)
                  ((varchar)
                   (cond [(ANYFLAGS 'isdb2)
                          "cast(? as varchar(200) ccsid unicode)"]
                         [(ANYFLAGS 'isora)
                          "cast(? as varchar2(200))"]))
                  ((blob) "cast(? as binary)") ;; ???
                  ((integer) "cast(? as integer)")
                  ((real) #f)
                  ((numeric)
                   (cond [(ANYFLAGS 'isdb2)
                          "cast(? as decimal)"]
                         [(ANYFLAGS 'isora)
                          "cast(? as decimal(20,10))"]))
                  ((date)
                   (cond [(ANYFLAGS 'isdb2)
                          "cast(? as date)"]
                         [(ANYFLAGS 'isora)
                          #f]))
                  ((time)
                   (cond [(ANYFLAGS 'isdb2)
                          "cast(? as time)"]
                         [(ANYFLAGS 'isora) ;; FIXME: bug?
                          #f]))
                  ((datetime) "cast(? as datetime)")
                  ;; FIXME: more types
                  (else #f))])
           (when stmt
             (check-equal? (query-value c (select-val stmt) value) value)))]))

(define-check (check-roundtrip c value)
  (check-roundtrip* c value check-equal?))

;; FIXME: change to testing flag?
(define (temp-table-ok?)
  (ANYFLAGS 'postgresql 'mysql))

(define (setup-temp-table c type)
  (query-exec c (format "create temporary table testing_temp_table (v ~a)" type)))

(define (check-roundtrip*/table c value check-equal?)
  (query-exec c "delete from testing_temp_table")
  (query-exec c (sql "insert into testing_temp_table (v) values ($1)") value)
  (check-equal? (query-value c "select v from testing_temp_table") value))

(define-check (check-roundtrip/table c value)
  (check-roundtrip*/table c value check-equal?))

(define (check-value/text* c val text check-val-equal? check-text-equal?)
  (cond [(ANYFLAGS 'postgresql)
         (let* ([tname (pg-type-name (current-type))]
                [q-text->val (sql (format "select ($1::text)::~a" tname))]
                [q-val->text (sql (format "select ($1::~a)::text" tname))])
           (when check-val-equal?
             (check-val-equal? (query-value c q-text->val text) val))
           (when check-text-equal?
             (check-text-equal? (query-value c q-val->text val) text)))]
        ;; FIXME: mysql just test val->text since text->val irregular
        [else (void)]))

(define-check (check-value/text c val text)
  (check-value/text* c val text check-equal? check-equal?))

(define-check (check-varchar c str)
  ;; Check roundtrip (only checks same when arrives back at client)
  (check-roundtrip c str)
  ;; Also must check correct on server side, so...
  ;;  - check the string has the right length
  (let ([len-fun (case dbsys
                   ((postgresql sqlite3) "length")
                   ((mysql) "char_length")
                   ((odbc) (cond [(TESTFLAGS 'ispg) "length"]
                                 [(TESTFLAGS 'ismy) "char_length"])))])
    (when (string? len-fun)
      (check-equal? (query-value c (sql (format "select ~a($1)" len-fun)) str)
                    (string-length str)
                    "check server-side length"))))

(define-check (check-1char c str)
  (check-varchar c str)
  (when (= (string-length str) 1)
    ;;  - if one char, check server-side char->int
    (let ([ci-fun (case dbsys
                    ((postgresql) "ascii") ;; yes, returns unicode code point too (if utf8)
                    ((mysql sqlite3) #f) ;; ???
                    ((odbc) (cond [(TESTFLAGS 'ispg) "ascii"])))])
      (when (string? ci-fun)
        (check-equal? (query-value c (sql (format "select ~a($1)" ci-fun)) str)
                      (char->integer (string-ref str 0))
                      "check server-side char->int")))
    ;;  - if one char, check server-side int->char
    (let ([ic-fmt (case dbsys
                    ((postgresql) "select chr(~a)")
                    ((mysql) "select char(~a using utf8)")
                    ((sqlite3) #f)
                    ((odbc) (cond [(TESTFLAGS 'ispg) "select chr(~a)"]
                                  [(TESTFLAGS 'ismy) "select char(~a using utf8)"])))])
      (when (string? ic-fmt)
        (check-equal? (query-value c
                        (format ic-fmt
                                (if (ANYFLAGS 'mysql 'ismy)
                                    (string-join
                                     (map number->string
                                          (bytes->list (string->bytes/utf-8 str)))
                                     ", ")
                                    (char->integer (string-ref str 0)))))
                      str
                      "check server-side int->char")))))

(define-check (check-=any c elt lst in?)
  (check-equal? (query-value c (format "select ~a = any ($1)" elt) (list->pg-array lst))
                in?))

(define-check (check-trim-string=? a b)
  (check-equal? (string-trim a) (string-trim b)))

(define some-dates
  `((,(sql-date 1776 07 04) "1776-07-04")
    (,(sql-date 2000 01 01) "2000-01-01")
    (,(sql-date 2012 02 14) "2012-02-14")))

(define some-times
  `((,(sql-time 01 02 03 0 #f) "01:02:03")
    (,(sql-time 12 34 56 0 #f) "12:34:56")
    (,(sql-time 17 30 01 0 #f) "17:30:01")
    (,(sql-time 01 02 03 #e4e7 #f) "01:02:03.04")
    (,(sql-time 12 34 56 123456000 #f) "12:34:56.123456")))

(define some-timetzs
  `((,(sql-time 01 02 03 0 3600) "01:02:03+01")
    (,(sql-time 12 34 56 0 3600) "12:34:56+01")
    (,(sql-time 17 30 01 0 -7200) "17:30:01-02")
    (,(sql-time 01 02 03 #e4e7 3600) "01:02:03.04+01")
    (,(sql-time 12 34 56 123456000 3600) "12:34:56.123456+01")
    (,(sql-time 12 34 56 123456000 -7200) "12:34:56.123456-02")))

(define some-timestamps
  `((,(sql-timestamp 2000 01 01 12 34 56 0 #f) "2000-01-01 12:34:56")
    (,(sql-timestamp 1776 07 04 12 34 56 0 #f) "1776-07-04 12:34:56")
    (,(sql-timestamp 2012 02 14 12 34 56 0 #f) "2012-02-14 12:34:56")
    (,(sql-timestamp 2000 01 01 12 34 56 123456000 #f) "2000-01-01 12:34:56.123456")
    (,(sql-timestamp 1776 07 04 12 34 56 123456000 #f) "1776-07-04 12:34:56.123456")
    (,(sql-timestamp 2012 02 14 12 34 56 123456000 #f) "2012-02-14 12:34:56.123456")
    (-inf.0 "-infinity")
    (+inf.0 "infinity")))

(define some-timestamptzs
  `((,(sql-timestamp 2000 01 01 12 34 56 0 -14400) "2000-01-01 12:34:56-04")
    (,(sql-timestamp 1776 07 04 12 34 56 0 -14400) "1776-07-04 12:34:56-04")
    (,(sql-timestamp 2012 02 14 12 34 56 0 7200) "2012-02-14 12:34:56+02")
    (,(sql-timestamp 2000 01 01 12 34 56 123456000 14400) "2000-01-01 12:34:56.123456+04")
    (,(sql-timestamp 1776 07 04 12 34 56 123456000 -14400) "1776-07-04 12:34:56.123456-04")
    (,(sql-timestamp 2012 02 14 12 34 56 123456000 -7200) "2012-02-14 12:34:56.123456-02")
    (-inf.0 "-infinity")
    (+inf.0 "infinity")))

(define some-intervals
  `((,(sql-interval 0 0 3 4 5 6 0) "3 days 04:05:06")
    (,(sql-interval 87 1 0 0 0 0 0) "87 years 1 mon")
    (,(sql-interval 1 2 3 4 5 6 45000) "1 year 2 mons 3 days 04:05:06.000045")
    (,(sql-interval 0 0 -3 -4 -5 -6 0) "-3 days -04:05:06")
    (,(sql-interval -87 -1 0 0 0 0 0) "-87 years -1 mons")
    (,(sql-interval -1 -2 3 4 5 6 45000) "-1 years -2 mons +3 days 04:05:06.000045")))

(define some-basic-strings
  `("Az0"
    "this is the time to remember"
    "it's like that (and that's the way it is)"
    ,(string #\\)
    ,(string #\\ #\\)
    ,(string #\')
    ,(string #\\ #\')
    "λ the ultimate"))
(define some-intl-strings
  `("αβψδεφγηιξκλμνοπρστθωςχυζ"
    "अब्च्देघिज्क्ल्म्नोप्र्स्तुव्य्"
    "شﻻؤيثبلاهتنمةىخحضقسفعرصءغئ"
    "阿あでいおうわぁ"
    "абцдефгхиклмнопљрстувњџзѕЋч"))

(define test
  (test-suite "SQL types (roundtrip, etc)"
    (type-test-case '(bool boolean)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c #t)
         (check-roundtrip c #f))))
    (type-test-case '(bytea blob)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c #"this is the time to remember")
         (check-roundtrip c #"that's the way it is")
         (check-roundtrip c (list->bytes (build-list 256 values)))
         (when (ANYFLAGS 'postgresql 'mysql 'sqlite3)
           (check-roundtrip c (make-bytes #e1e6 (char->integer #\a)))
           (check-roundtrip c (make-bytes #e1e7 (char->integer #\b)))
           #| (check-roundtrip c (make-bytes #e1e8 (char->integer #\c))) |#)
         (when (ANYFLAGS 'postgresql)
           (let ([r (query-value c "select cast(repeat('a', 10000000) as bytea)")])
             (check-pred bytes? r)
             (check-equal? r (make-bytes 10000000 (char->integer #\a))))
           (let ([r (query-value c "select cast(repeat('a', 100000000) as bytea)")])
             (check-pred bytes? r)
             (check-equal? r (make-bytes 100000000 (char->integer #\a)))))
         (when (ANYFLAGS 'mysql)
           ;; Test able to read large blobs
           ;; (depends on max_allowed_packet, though)
           (define max-allowed-packet (query-value c "select @@session.max_allowed_packet"))
           (for ([N (in-list '(#e1e7 #e1e8))])
             (when (<= N max-allowed-packet)
               (let* ([q (format "select cast(repeat('a', ~s) as binary)" N)]
                      [r (query-value c q)])
                 (check-pred bytes? r)
                 (check-equal? r (make-bytes N (char->integer #\a))))))))))
    (type-test-case '(text)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c "abcde")
         (check-roundtrip c (make-string #e1e6 #\a))
         (check-roundtrip c (make-string #e1e7 #\b))
         (check-roundtrip c (make-string #e1e8 #\c)))))
    (type-test-case '(smallint)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c 5)
         (check-roundtrip c -1)
         (check-roundtrip c #x7FFF)
         (check-roundtrip c #x-8000))))
    (type-test-case '(integer)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c 5)
         (check-roundtrip c -1)
         (check-roundtrip c #x7FFFFFFF)
         (check-roundtrip c #x-80000000))))
    (type-test-case '(bigint)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c 5)
         (check-roundtrip c -1)
         (check-roundtrip c (sub1 (expt 2 63)))
         (check-roundtrip c (- (expt 2 63))))))

    (type-test-case '(real)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c 1.0)
         (check-roundtrip c 1.5)
         (check-roundtrip c -5.5)
         (when (supported? 'real-infinities)
           (check-roundtrip c +inf.0)
           (check-roundtrip c -inf.0)
           (check-roundtrip c +nan.0)))))
    (type-test-case '(double)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c 1.0)
         (check-roundtrip c 1.5)
         (check-roundtrip c -5.5)
         (check-roundtrip c 1.1)
         (check-roundtrip c -5.8)
         (when (supported? 'real-infinities)
           (check-roundtrip c +inf.0)
           (check-roundtrip c -inf.0)
           (check-roundtrip c +nan.0)))))

    (unless (ANYFLAGS 'isdb2) ;; "Driver not capable"
      (type-test-case '(numeric decimal)
        (call-with-connection
         (lambda (c)
           (check-roundtrip c 0)
           (check-roundtrip c 10)
           (check-roundtrip c -5)
           (unless (TESTFLAGS 'odbc)
             (check-roundtrip c 1234567890)
             (check-roundtrip c -1234567890)
             (check-roundtrip c #e12345.67809)
             (check-roundtrip c #e-12345.67809)
             (unless (TESTFLAGS 'mysql)
               (check-roundtrip c 12345678901234567890)
               (check-roundtrip c -12345678901234567890)
               (check-roundtrip c #e1234567890.0987654321)
               (check-roundtrip c #e-1234567890.0987654321))
             (check-roundtrip c 1/2)
             (check-roundtrip c 1/40)
             (check-roundtrip c 1/10)
             (check-roundtrip c -1/2)
             (check-roundtrip c -1/40)
             (check-roundtrip c -1/10)
             (check-roundtrip c 1/400000))
           (when (supported? 'numeric-infinities)
             (check-roundtrip c +nan.0))))))

    (type-test-case '(varchar)
      (call-with-connection
       (lambda (c)
         (unless (ANYFLAGS 'isora) ;; Oracle treats empty string as NULL (?!)
           (check-varchar c ""))
         (for ([str some-basic-strings])
           (check-varchar c str))
         (for ([str some-intl-strings])
           (check-varchar c str)
           ;; and do the extra one-char checks:
           (check-1char c (substring str 0 1)))
         (unless (ANYFLAGS 'isora 'isdb2)
           (check-varchar c (make-string 800 #\a)))
         (unless (ANYFLAGS 'isora 'isdb2) ;; too long
           (check-varchar c (apply string-append some-intl-strings)))
         ;; one-char checks
         (check-1char c (string #\λ))
         (check-1char c (make-string 1 #\u2200))
         (check-varchar c (make-string 20 #\u2200))
         ;; check large strings
         (unless (ANYFLAGS 'isora 'isdb2) ;; too long (???)
           (check-varchar c (make-string 100 #\u2200)))
         ;; Following might not produce valid string (??)
         (unless (ANYFLAGS 'isora 'isdb2)
           (check-varchar c
                          (list->string
                           (build-list 800
                                       (lambda (n)
                                         (integer->char (add1 n))))))))))

    (type-test-case '(character)
      (call-with-connection
       (lambda (c)
         (when (temp-table-ok?)
           (setup-temp-table c "char(5)")
           (check-roundtrip*/table c "" check-trim-string=?)
           (check-roundtrip*/table c "abc" check-trim-string=?)
           (check-roundtrip*/table c "abcde" check-trim-string=?)))))

    (type-test-case '(date)
      (call-with-connection
       (lambda (c)
         (for ([d+s some-dates])
           (check-roundtrip c (car d+s))
           (check-value/text c (car d+s) (cadr d+s))))))
    (type-test-case '(time)
      (call-with-connection
       (lambda (c)
         (for ([t+s some-times])
           (unless (and (eq? dbsys 'odbc) (> (sql-time-nanosecond (car t+s)) 0))
             ;; ODBC time has no fractional part
             (check-roundtrip c (car t+s))
             (check-value/text c (car t+s) (cadr t+s)))))))
    (type-test-case '(timetz)
      (call-with-connection
       (lambda (c)
         (for ([t+s some-timetzs])
           (check-roundtrip c (car t+s))
           (check-value/text c (car t+s) (cadr t+s))))))
    (type-test-case '(timestamp datetime)
      (call-with-connection
       (lambda (c)
         (for ([t+s some-timestamps])
           (when (or (TESTFLAGS 'postgresql) (sql-timestamp? (car t+s)))
             ;; Only postgresql supports +/-inf.0
             (check-roundtrip c (car t+s))
             (check-value/text c (car t+s) (cadr t+s)))))))
    (type-test-case '(timestamptz)
      (call-with-connection
       (lambda (c)
         (for ([t+s some-timestamptzs])
           (check-roundtrip* c (car t+s) check-timestamptz-equal?)
           (check-value/text* c (car t+s) (cadr t+s) check-timestamptz-equal? #f)))))

    (type-test-case '(interval)
      (call-with-connection
       (lambda (c)
         (for ([i+s some-intervals])
           (when (or (memq dbsys '(postgresql))
                     (sql-day-time-interval? i+s)
                     (sql-year-month-interval? i+s))
             (check-roundtrip c (car i+s))
             (when (memq dbsys '(postgresql))
               (check-value/text c (car i+s) (cadr i+s))))))))

    (type-test-case '(varbit bit)
      (call-with-connection
       (lambda (c)
         (for ([s (list "1011"
                        "000000"
                        (make-string 30 #\1)
                        (string-append (make-string 10 #\1) (make-string 20 #\0)))])
           (check-roundtrip* c (string->sql-bits s) check-bits-equal?)))))

    (type-test-case '(point geometry)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (point 0 0))
         (check-roundtrip c (point 1 2))
         (check-roundtrip c (point (exp 1) pi)))))

    (type-test-case '(line-string geometry)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (line-string (list (point 0 0) (point 1 1))))
         (check-roundtrip c (line-string (list (point 0 0) (point 1 1) (point -5 7)))))))
    
    (type-test-case '(lseg geometry)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (line-string (list (point 0 0) (point 1 1)))))))

    (type-test-case '(polygon geometry)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c
          (polygon (line-string (list (point 0 0) (point 2 0) (point 1 1) (point 0 0))) '()))
         (when (memq dbsys '(mysql))
           (check-roundtrip c
            (polygon (line-string (list (point 0 0) (point 4 0) (point 2 2) (point 0 0)))
                     (list (line-string (list (point 1 1) (point 3 1)
                                              (point 1.5 1.5) (point 1 1))))))))))

    (when (ANYFLAGS 'postgresql) ;; "Driver not capable"
      (type-test-case '(path)
        (call-with-connection
         (lambda (c)
           (check-roundtrip c (pg-path #t (list (point 0 0) (point 1 1) (point -5 7))))
           (check-roundtrip c (pg-path #f (list (point -1 -1) (point (exp 1) pi)))))))
      (type-test-case '(box)
        (call-with-connection
         (lambda (c)
           (check-roundtrip c (pg-box (point 10 10) (point 2 8))))))
      (type-test-case '(circle)
        (call-with-connection
         (lambda (c)
           (check-roundtrip c (pg-circle (point 1 2) 45))))))

    (when (TESTFLAGS 'postgresql 'pg92)
      (type-test-case '(json)
        (call-with-connection
         (lambda (c)
           (define some-jsexprs
             (list #t #f 0 1 -2 pi "" "hello" "good\nbye" 'null
                   (hasheq 'a 1 'b 2 'c 'null)
                   (list #t #f 'null "a" "b")))
           (for ([j some-jsexprs])
             (check-roundtrip c j)))))
      (type-test-case '(int4range)
        (call-with-connection
         (lambda (c)
           (check-roundtrip c (pg-empty-range))
           ;; for now, only test things in canonical form... (FIXME)
           (check-roundtrip c (pg-range 0 #t 5 #f))
           (check-roundtrip c (pg-range #f #f -57 #f))
           (check-roundtrip c (pg-range 1234 #t #f #f)))))
      (type-test-case '(int8range)
        (call-with-connection
         (lambda (c)
           (check-roundtrip c (pg-empty-range))
           ;; for now, only test things in canonical form... (FIXME)
           (check-roundtrip c (pg-range 0 #t 5 #f))
           (check-roundtrip c (pg-range #f #f -57 #f))
           (check-roundtrip c (pg-range 1234 #t #f #f))
           (check-roundtrip c (pg-range (expt 2 60) #t (expt 2 61) #f)))))
      ;; FIXME: numrange
      (type-test-case '(daterange)
        (call-with-connection
         (lambda (c)
           (define d1 (car (first some-dates)))
           (define d2 (car (second some-dates)))
           (define d3 (car (third some-dates)))
           (check-roundtrip c (pg-empty-range))
           ;; for now, only test things in canonical form... (FIXME?)
           (check-roundtrip c (pg-range d1 #t d3 #f))
           (check-roundtrip c (pg-range #f #f d2 #f))
           (check-roundtrip c (pg-range d3 #t #f #f)))))
      (type-test-case '(tsrange)
        (call-with-connection
         (lambda (c)
           (define ts1 (car (second some-timestamps)))
           (define ts2 (car (first some-timestamps)))
           (define ts3 (car (third some-timestamps)))
           (check-roundtrip c (pg-empty-range))
           (check-roundtrip c (pg-range ts1 #t ts2 #t))
           (check-roundtrip c (pg-range ts1 #f ts3 #f))
           (check-roundtrip c (pg-range ts2 #f ts3 #t)))))
      (type-test-case '(tstzrange)
        (call-with-connection
         (lambda (c)
           (define ts1 (car (second some-timestamptzs)))
           (define ts2 (car (first some-timestamptzs)))
           (define ts3 (car (third some-timestamptzs)))
           (check-roundtrip c (pg-empty-range))
           (check-roundtrip* c (pg-range ts1 #t ts2 #t) check-timestamptz-equal?)
           (check-roundtrip* c (pg-range ts1 #f ts3 #f) check-timestamptz-equal?)
           (check-roundtrip* c (pg-range ts2 #f ts3 #t) check-timestamptz-equal?)))))

    ;; --- Arrays ---
    (type-test-case '(boolean-array)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (list->pg-array (list)))
         (check-roundtrip c (list->pg-array (list #t)))
         (check-roundtrip c (list->pg-array (list #t #t #f)))
         (check-roundtrip c (list->pg-array (list #t sql-null #f)))
         (check-=any c "'t'::boolean" (list) #f)
         (check-=any c "'t'::boolean" (list #f) #f)
         (check-=any c "'t'::boolean" (list sql-null #t #f) #t))))

    (type-test-case '(bytea-array)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (list->pg-array (list)))
         (check-roundtrip c (list->pg-array (list #"abc")))
         (check-roundtrip c (list->pg-array (list #"a" #"bc" #"def")))
         (check-roundtrip c (list->pg-array (list #"a" sql-null #"bc")))
         (check-=any c "'abc'::bytea" (list) #f)
         (check-=any c "'abc'::bytea" (list #"a" #"abc") #t)
         (check-=any c "'abc'::bytea" (list #"a" sql-null #"abc") #t))))

    (type-test-case '(char1-array)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (list->pg-array (list)))
         (check-roundtrip c (list->pg-array (list #\a)))
         (check-roundtrip c (list->pg-array (list #\a #\z)))
         (check-roundtrip c (list->pg-array (list #\a sql-null #\z)))
         (check-=any c "'a'::\"char\"" (list) #f)
         (check-=any c "'a'::\"char\"" (list #\a #\b) #t)
         (check-=any c "'a'::\"char\"" (list #\a sql-null #\z) #t))))

    (type-test-case '(smallint-array)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (list->pg-array (list)))
         (check-roundtrip c (list->pg-array (list 1)))
         (check-roundtrip c (list->pg-array (list 1 2 3)))
         (check-roundtrip c (list->pg-array (list 1 sql-null 3 4 5)))
         (check-=any c "1::smallint" (list) #f)
         (check-=any c "1::smallint" (list 1 2 3) #t)
         (check-=any c "1::smallint" (list sql-null 1 2 3) #t))))

    (type-test-case '(integer-array)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (list->pg-array (list)))
         (check-roundtrip c (list->pg-array (list 1)))
         (check-roundtrip c (list->pg-array (list 1 2 3)))
         (check-roundtrip c (list->pg-array (list 1 sql-null 3 4 5)))
         (check-=any c "1" (list) #f)
         (check-=any c "1" (list 1 2 3) #t)
         (check-=any c "1" (list sql-null 1 2 3) #t))))

    (type-test-case '(text-array)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (list->pg-array (list)))
         (check-roundtrip c (list->pg-array (list "abc")))
         (check-roundtrip c (list->pg-array (list "a" "" "bc" "def" "")))
         (check-roundtrip c (list->pg-array (list "a" sql-null "")))
         (check-=any c "'abc'" (list) #f)
         (check-=any c "'abc'" (list "abc" "def") #t)
         (check-=any c "'abc'" (list "abc" "def" sql-null) #t))))

    #|
    (type-test-case '(character-array)
      (call-with-connection
       (lambda (c)
         ;; NOTE: seems to infer an elt type of character(1)...
         (check-roundtrip c (list->pg-array (list)))
         (check-roundtrip c (list->pg-array (list "abc")))
         (check-roundtrip c (list->pg-array (list "a" "" "bc" "def" "")))
         (check-roundtrip c (list->pg-array (list "a" sql-null "")))
         (check-=any c "'abc'" (list) #f)
         (check-=any c "'abc'" (list "abc" "def") #t)
         (check-=any c "'abc'" (list "ab" "c" sql-null) #f))))
    |#

    (type-test-case '(varchar-array)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (list->pg-array (list)))
         (check-roundtrip c (list->pg-array (list "abc")))
         (check-roundtrip c (list->pg-array (list "a" "" "bc" "def" "")))
         (check-roundtrip c (list->pg-array (list "a" sql-null "")))
         (check-=any c "'abc'" (list) #f)
         (check-=any c "'abc'" (list "abc" "def") #t)
         (check-=any c "'abc'" (list "abc" "def" sql-null) #t))))

    (type-test-case '(bigint-array)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (list->pg-array (list)))
         (check-roundtrip c (list->pg-array (list 1)))
         (check-roundtrip c (list->pg-array (list 1 2 3)))
         (check-roundtrip c (list->pg-array (list 1 sql-null 3 4 5)))
         (check-=any c "1::bigint" (list) #f)
         (check-=any c "1::bigint" (list 1 2 3) #t)
         (check-=any c "1::bigint" (list sql-null 1 2 3) #t))))

    (type-test-case '(point-array)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (list->pg-array (list)))
         (check-roundtrip c (list->pg-array (list (point 1 2))))
         (check-roundtrip c (list->pg-array (list (point 1 2) sql-null (point 3 4))))
         #| ;; no op= for point ?!
         (check-=any c "POINT (1,2)" (list (point 1 2)) #t)
         (check-=any c "POINT (1,2)" (list (point 3 4)) #f)
         (check-=any c "POINT (1,2)" (list sql-null (point 1 2) (point 3 4)) #t)
         |#)))

    (type-test-case '(real-array)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (list->pg-array (list)))
         (check-roundtrip c (list->pg-array (list 1.0)))
         (check-roundtrip c (list->pg-array (list 1.0 2.0 3.0)))
         (check-roundtrip c (list->pg-array (list 1.0 sql-null 3.0 4.0 +inf.0)))
         (check-=any c "1.0::real" (list) #f)
         (check-=any c "1.0::real" (list 1.0) #t)
         (check-=any c "1.0::real" (list sql-null 1.0 2.0 +inf.0) #t))))

    (type-test-case '(double-array)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (list->pg-array (list)))
         (check-roundtrip c (list->pg-array (list 1.0)))
         (check-roundtrip c (list->pg-array (list 1.0 2.0 3.0)))
         (check-roundtrip c (list->pg-array (list 1.0 sql-null 3.0 4.0 +inf.0)))
         (check-=any c "1.0::float8" (list) #f)
         (check-=any c "1.0::float8" (list 1.0) #t)
         (check-=any c "1.0::float8" (list sql-null 1.0 2.0 +inf.0) #t))))

    (type-test-case '(timestamp-array)
      (call-with-connection
       (lambda (c)
         (define ts1 (sql-timestamp 1999 12 31 11 22 33 0 #f))
         (define ts2 (sql-timestamp 2011 09 13 15 17 19 0 #f))
         (check-roundtrip c (list->pg-array (list)))
         (check-roundtrip c (list->pg-array (list ts1)))
         (check-roundtrip c (list->pg-array (list ts2 sql-null)))
         (check-=any c "'1999-12-31 11:22:33'::timestamp" (list) #f)
         (check-=any c "'1999-12-31 11:22:33'::timestamp" (list ts2) #f)
         (check-=any c "'1999-12-31 11:22:33'::timestamp" (list ts1 sql-null ts2) #t))))

    ;; FIXME: add timestamptz-array test (harder due to tz conversion)

    (type-test-case '(date-array)
      (call-with-connection
       (lambda (c)
         (define ts1 (sql-date 1999 12 31))
         (define ts2 (sql-date 2011 09 13))
         (check-roundtrip c (list->pg-array (list)))
         (check-roundtrip c (list->pg-array (list ts1)))
         (check-roundtrip c (list->pg-array (list ts2 sql-null)))
         (check-=any c "'1999-12-31'::date" (list) #f)
         (check-=any c "'1999-12-31'::date" (list ts2) #f)
         (check-=any c "'1999-12-31'::date" (list ts1 sql-null ts2) #t))))

    (type-test-case '(time-array)
      (call-with-connection
       (lambda (c)
         (define ts1 (sql-time 11 22 33 0 #f))
         (define ts2 (sql-time 15 17 19 0 #f))
         (check-roundtrip c (list->pg-array (list)))
         (check-roundtrip c (list->pg-array (list ts1)))
         (check-roundtrip c (list->pg-array (list ts2 sql-null)))
         (check-=any c "'11:22:33'::time" (list) #f)
         (check-=any c "'11:22:33'::time" (list ts2) #f)
         (check-=any c "'11:22:33'::time" (list ts1 sql-null ts2) #t))))

    ;; FIXME: add timetz-array test
    ;; FIXME: add interval-array test

    (type-test-case '(interval-array)
      (call-with-connection
       (lambda (c)
         (define i1 (sql-interval 0 0 3 4 5 6 0))
         (define i2 (sql-interval 87 1 0 0 0 0 0))
         (define i3 (sql-interval 1 2 3 4 5 6 45000))
         (check-roundtrip c (list->pg-array (list)))
         (check-roundtrip c (list->pg-array (list i1)))
         (check-roundtrip c (list->pg-array (list i2 sql-null i3)))
         (check-=any c "'87 years 1 month'::interval" (list) #f)
         (check-=any c "'87 years 1 month'::interval" (list i1 i3) #f)
         (check-=any c "'87 years 1 month'::interval" (list i1 sql-null i2) #t))))

    (type-test-case '(decimal-array)
      (call-with-connection
       (lambda (c)
         (define d1 12345678901234567890)
         (define d2 1/20)
         (define d3 +nan.0)
         (define d4 -100)
         (check-roundtrip c (list->pg-array (list)))
         (check-roundtrip c (list->pg-array (list d1)))
         (check-roundtrip c (list->pg-array (list d2 sql-null d3 d4)))
         (check-=any c "'0.05'::numeric" (list) #f)
         (check-=any c "'0.05'::numeric" (list d1) #f)
         (check-=any c "'0.05'::numeric" (list d1 sql-null d2 d3 d4) #t))))

    ))
