#lang racket/unit
(require rackunit
         racket/class
         racket/math
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
  (let* ([known-types (send dbsystem get-known-types)]
         [type (for/or ([type types])
                (and (member type known-types) type))])
    (when type
      (test-case (format "~s" type)
        (parameterize ((current-type type)) (proc))))))

(define (check-timestamptz-equal? a b)
  (check srfi:time=?
         (srfi:date->time-utc (sql-datetime->srfi-date a))
         (srfi:date->time-utc (sql-datetime->srfi-date b))))

(define (check-bits-equal? a b)
  (check-equal? (sql-bits->string a) (sql-bits->string b)))

(define (supported? option)
  (send dbsystem has-support? option))

(define (check-roundtrip* c value check-equal?)
  (cond [(ANYFLAGS 'postgresql 'ispg)
         (let* ([tname (if (eq? (current-type) 'double) "float8" (current-type))]
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
                  ((numeric) "select cast(? as decimal)")
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
                    "check server-side length")))
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
         (check-roundtrip c (list->bytes (build-list 256 values))))))

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
           (unless (TESTFLAGS 'odbc 'ismy)
             (check-roundtrip c 12345678901234567890)
             (check-roundtrip c 1/2)
             (check-roundtrip c 1/40)
             (check-roundtrip c #e1234567890.0987654321)
             (check-roundtrip c 1/10)
             (check-roundtrip c 1/400000))
           (when (supported? 'numeric-infinities)
             (check-roundtrip c +nan.0))))))

    (type-test-case '(varchar)
      (call-with-connection
       (lambda (c)
         (unless (ANYFLAGS 'isora) ;; Oracle treats empty string as NULL (?!)
           (check-varchar c ""))
         (check-varchar c "Az0")
         (check-varchar c (string #\\))
         (check-varchar c (string #\\ #\\))
         (check-varchar c (string #\'))
         (check-varchar c "this is the time to remember")
         (check-varchar c "it's like that (and that's the way it is)")
         (check-varchar c (string #\\))
         (check-varchar c (string #\'))
         (check-varchar c (string #\\ #\'))
         (check-varchar c "λ the ultimate")
         (unless (ANYFLAGS 'isora 'isdb2)
           (check-varchar c (make-string 800 #\a)))
         (let ([strs '("αβψδεφγηιξκλμνοπρστθωςχυζ"
                       "अब्च्देघिज्क्ल्म्नोप्र्स्तुव्य्"
                       "شﻻؤيثبلاهتنمةىخحضقسفعرصءغئ"
                       "阿あでいおうわぁ"
                       "абцдефгхиклмнопљрстувњџзѕЋч")])
           (for ([s strs])
             (check-varchar c s)
             ;; and do the extra one-char checks:
             (check-varchar c (string (string-ref s 0))))
           (unless (ANYFLAGS 'isora 'isdb2) ;; too long
             (check-varchar c (apply string-append strs))))
         ;; one-char checks
         (check-varchar c (string #\λ))
         (check-varchar c (make-string 1 #\u2200))
         (check-varchar c (make-string 20 #\u2200))
         (unless (ANYFLAGS 'isora 'isdb2) ;; too long (???)
           (check-varchar c (make-string 100 #\u2200)))
         ;; Following might not produce valid string (??)
         (unless (ANYFLAGS 'isora 'isdb2)
           (check-varchar c
                          (list->string
                           (build-list 800
                                       (lambda (n)
                                         (integer->char (add1 n))))))))))

    (type-test-case '(date)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (make-sql-date 1980 08 17)))))
    (type-test-case '(time)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (make-sql-time 12 34 56 0 #f))
         (unless (eq? dbsys 'odbc) ;; ODBC time has no fractional part
           (check-roundtrip c (make-sql-time 12 34 56 123456000 #f))
           (check-roundtrip c (make-sql-time 12 34 56 100000000 #f))
           (check-roundtrip c (make-sql-time 12 34 56 000001000 #f))))))
    (type-test-case '(timetz)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (make-sql-time 12 34 56 0 3600))
         (check-roundtrip c (make-sql-time 12 34 56 123456000 3600))
         (check-roundtrip c (make-sql-time 12 34 56 100000000 3600))
         (check-roundtrip c (make-sql-time 12 34 56 000001000 3600)))))
    (type-test-case '(timestamp datetime)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (make-sql-timestamp 1980 08 17 12 34 56 0 #f))
         (check-roundtrip c (make-sql-timestamp 1980 08 17 12 34 56 123456000 #f))
         (check-roundtrip c (make-sql-timestamp 1980 08 17 12 34 56 100000000 #f))
         (check-roundtrip c (make-sql-timestamp 1980 08 17 12 34 56 000001000 #f)))))
    ;; Bizarrely, PostgreSQL converts timestamptz to a standard timezone
    ;; when returning them, but it doesn't for timetz.
    (type-test-case '(timestamptz)
      (call-with-connection
       (lambda (c)
         (check-roundtrip* c (make-sql-timestamp 1980 08 17 12 34 56 0 3600)
                           check-timestamptz-equal?)
         (check-roundtrip* c (make-sql-timestamp 1980 08 17 12 34 56 123456000 3600)
                           check-timestamptz-equal?)
         (check-roundtrip* c (make-sql-timestamp 1980 08 17 12 34 56 100000000 3600)
                           check-timestamptz-equal?)
         (check-roundtrip* c (make-sql-timestamp 1980 08 17 12 34 56 000001000 3600)
                           check-timestamptz-equal?))))

    (type-test-case '(interval)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (sql-interval 0 0 3 4 5 6 0))
         (check-roundtrip c (sql-interval 87 1 0 0 0 0 0))
         (when (memq dbsys '(postgresql))
           (check-roundtrip c (sql-interval 1 2 3 4 5 6 45000))))))

    (type-test-case '(varbit bit)
      (call-with-connection
       (lambda (c)
         (check-roundtrip* c (string->sql-bits "1011") check-bits-equal?)
         (check-roundtrip* c (string->sql-bits "000000") check-bits-equal?)
         (check-roundtrip* c (string->sql-bits (make-string 30 #\1)) check-bits-equal?))))

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

    (type-test-case '(pg-path)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (pg-path #t (list (point 0 0) (point 1 1) (point -5 7))))
         (check-roundtrip c (pg-path #f (list (point -1 -1) (point (exp 1) pi)))))))

    (type-test-case '(pg-box)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (pg-box (point 10 10) (point 2 8))))))

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

    (type-test-case '(pg-circle)
      (call-with-connection
       (lambda (c)
         (check-roundtrip c (pg-circle (point 1 2) 45)))))))
