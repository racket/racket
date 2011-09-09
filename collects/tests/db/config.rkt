#lang racket/base
(require racket/class
         racket/unit
         db/base)
(provide database^
         test^
         config^
         config@)

(define-signature database^
  (dbtestname
   connect
   dbsys
   dbflags
   kill-safe?))

(define-signature test^ (test))
(define-signature config^
  (connect-for-test
   connect-and-setup
   call-with-connection
   (define-syntaxes (with-connection)
     (syntax-rules ()
       [(with-connection c . body)
        (call-with-connection (lambda (c) . body))]))
   test-data
   set-equal?
   sql
   select-val
   dbsystem
   NOISY?
   TESTFLAGS
   ANYFLAGS))

(define-unit config@
  (import database^)
  (export config^)

  (define NOISY? #f)

  (define (connect-for-test)
    (cond [kill-safe? (kill-safe-connection (connect))]
          [else (connect)]))

  (define test-data
    '((0 "nothing")
      (1 "unity")
      (2 "the loneliest number since the number one")
      (4 "four")
      (5 "five")
      (6 "half a dozen")))

  (define (connect-and-setup)
    (let [(cx (connect-for-test))]

      ;; For now, we just assume Oracle, DB2 dbs are already set up.
      (unless (ANYFLAGS 'isora 'isdb2)
        (query-exec cx
          "create temporary table the_numbers (N integer primary key, descr varchar(80))")
        (for-each (lambda (p)
                    (query-exec cx
                                (format "insert into the_numbers values (~a, '~a')"
                                        (car p) (cadr p))))
                  test-data))
      cx))

  ;; set-equal? : ('a list) ('a list) -> boolean
  (define (set-equal? a b)
    (and (andmap (lambda (xa) (member xa b)) a)
         (andmap (lambda (xb) (member xb a)) b)
         #t))

  (define (call-with-connection f)
    (let [(c (connect-and-setup))]
      (dynamic-wind void
                    (lambda () (f c))
                    (lambda () (disconnect c)))))

  (define (sql str)
    (case dbsys
      ((postgresql) str)
      ((mysql sqlite3 odbc) (regexp-replace* #rx"\\$[0-9]" str "?"))
      (else (error 'sql "unsupported dbsystem: ~e" dbsys))))

  (define (select-val str)
    (cond [(TESTFLAGS 'isora)
           (sql (string-append "select " str " from DUAL"))]
          [(TESTFLAGS 'isdb2)
           (sql (string-append "values (" str ")"))]
          [else (sql (string-append "select " str))]))

  (define dbsystem
    (with-handlers ([(lambda (e) #t)
                     (lambda (e) #f)])
      (let* ([c (connect)]
             [dbsystem (send c get-dbsystem)])
        (disconnect c)
        dbsystem)))

  ;; Flags = dbflags U dbsys

  ;; Returns #t if all are set.
  (define (TESTFLAGS . xs)
    (for/and ([x xs])
      (or (eq? x dbsys)
          (and (member x dbflags) #t))))

  ;; Returns #t if any are set.
  (define (ANYFLAGS . xs)
    (for/or ([x xs])
      (or (eq? x dbsys)
          (and (member x dbflags) #t)))))
