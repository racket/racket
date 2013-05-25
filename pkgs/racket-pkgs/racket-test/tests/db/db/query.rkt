#lang racket/unit
(require racket/string
         rackunit
         "../config.rkt"
         db/base)
(import database^ config^)
(export test^)

(define-syntax-rule (with-connection c . body)
  (call-with-connection (lambda (c) . body)))

(define-syntax-rule (check-exn-fail expr)
  (check-exn exn:fail? (lambda () expr)))

;; prep-mode:
;;   'string = query w/ string
;;   'prepare = query w/ prepared
;;   'bind = query w/ prepared+bound
(define-syntax-rule (Q* prep-mode function obj stmt arg ...)
  (Q** prep-mode function obj (sql stmt) (list arg ...)))
(define (Q** prep-mode function obj stmt args)
  (case prep-mode
    ((string) (apply function obj stmt args))
    ((prepare) (apply function obj (prepare obj stmt) args))
    ((bind) (function obj (bind-prepared-statement (prepare obj stmt) args)))
    ((gen) (apply function obj (virtual-statement stmt) args))
    (else 'Q* "bad prep-mode: ~e" prep-mode)))

(define (simple-tests prep-mode)

  (define-syntax-rule (Q obj function stmt arg ...)
    (Q* prep-mode function obj stmt arg ...))

  (test-suite (format "simple (~a)" prep-mode)

    (unless (ANYFLAGS 'isora 'isdb2) ;; table isn't temp, so don't tamper with it
      (test-case "query-exec"
        (with-connection c
          (check-pred void? (Q c query-exec "insert into the_numbers values(-1, 'mysterious')"))
          (check-equal? (Q c query-value "select descr from the_numbers where N = -1")
                        "mysterious"))
        (with-connection c
          (check-pred void? (Q c query-exec "delete from the_numbers where N <> $1" 0))
          (check-equal? (Q c query-value "select count(*) from the_numbers")
                        (if (TESTFLAGS 'odbc 'issl) "1" 1))
          (check-equal? (Q c query-list "select N from the_numbers")
                        (list 0)))))

    (test-case "query-rows"
      (with-connection c
        (check set-equal?
               (Q c query-rows "select N, descr from the_numbers where N < 2")
               '(#(0 "nothing") #(1 "unity")))
        (check set-equal?
               (Q c query-rows "select N, descr from the_numbers where N < $1" 2)
               '(#(0 "nothing") #(1 "unity")))
        (unless (ANYFLAGS 'isora 'isdb2)
          (check-exn-fail
           (Q c query-rows "insert into the_numbers values (13, 'baker')")))))

    (test-case "query-list"
      (with-connection c
        (check set-equal?
               (Q c query-list "select N from the_numbers")
               (map car test-data))
        (check set-equal?
               (Q c query-list "select N from the_numbers where N < $1" 2)
               '(0 1))
        (check set-equal?
               (Q c query-list "select N from the_numbers where N > $1 and N < $2" 1 4)
               '(2))))

    (test-case "query-row"
      (with-connection c
        (check-equal? (Q c query-row "select N, descr from the_numbers where N = 0")
                      '#(0 "nothing"))
        (check-equal? (Q c query-row "select N, descr from the_numbers where N = $1" 0)
                      '#(0 "nothing"))
        (check-exn-fail (Q c query-row "select N, descr from the_numbers where N = 100"))))

    (test-case "query-maybe-row"
      (with-connection c
        (check-equal? (Q c query-maybe-row "select N, descr from the_numbers where N = 0")
                      '#(0 "nothing"))
        (check-equal? (Q c query-maybe-row "select N, descr from the_numbers where N = $1" 0)
                      '#(0 "nothing"))
        (check-equal? (Q c query-maybe-row "select N, descr from the_numbers where N = 100")
                      #f)))

    (test-case "query-value"
      (with-connection c
        (check-equal? (Q c query-value "select N from the_numbers where N < 6 and N > 4")
                      5)
        (check-equal? (Q c query-value "select N from the_numbers where N < $1 and N > $2" 6 4)
                      5)
        (check-exn-fail (Q c query-value "select N from the_numbers where N > 100"))
        (check-exn-fail (Q c query-value "select N from the_numbers"))))

    (test-case "query-maybe-value"
      (with-connection c
        (check-equal? (Q c query-maybe-value "select N from the_numbers where N < 6 and N > 4")
                      5)
        (check-equal? (Q c query-maybe-value
                         "select N from the_numbers where N < $1 and N > $2" 6 4)
                      5)
        (check-equal? (Q c query-maybe-value "select N from the_numbers where N > 100")
                      #f)
        (check-exn-fail (Q c query-maybe-value "select N from the_numbers"))))

    (test-case "in-query"
      (with-connection c
        (check set-equal?
               (for/list ([(n d)
                           (Q c in-query "select N, descr from the_numbers where N < 2")])
                 (vector n d))
               '(#(0 "nothing") #(1 "unity")))
        (check set-equal?
               (for/list ([(n d)
                           (Q c in-query "select N, descr from the_numbers where N < $1" 2)])
                 (vector n d))
               '(#(0 "nothing") #(1 "unity")))
        (unless (ANYFLAGS 'isora 'isdb2)
          (check-exn-fail
           (for ([x (Q c in-query "insert into the_numbers values ($1, 'baker')" 13)])
             (void))))
        (check-exn-fail
         (let ([stmt (sql "select N from the_numbers where N < $1")])
           (case prep-mode
             ((string) (for ([(x y) (in-query c stmt 2)]) 0))
             ((prepare) (for ([(x y) (in-query c (prepare c stmt) 2)]) 0))
             ((bind)
              (for ([(x y) (in-query c (bind-prepared-statement (prepare c stmt) (list 2)))])
                0))
             ((gen)
              (for ([(x y) (in-query c (virtual-statement stmt) 2)]) 0)))))))
    ))

(define in-query-tests
  (test-suite "in-query (cursor)"
    ;; call-with-transaction necessary for postresql
    (test-case "in-query w/ #:fetch"
      (with-connection c
        (for ([fs (in-range 1 10)])
          (check equal?
                 (call-with-transaction c
                   (lambda ()
                     (for/list ([n (in-query c "select N from the_numbers order by N asc" #:fetch fs)]) n)))
                 (map car test-data)))
        (check equal?
               (call-with-transaction c
                 (lambda ()
                   (for/first ([n (in-query c "select N from the_numbers order by N asc" #:fetch 1)]) n)))
               (for/first ([n (map car test-data)]) n))))
    (test-case "in-query multiple different"
      (with-connection c
        (check equal?
               (call-with-transaction c
                 (lambda ()
                   (for/list ([n (in-query c "select N from the_numbers order by N asc" #:fetch 1)]
                              [m (in-query c "select N from the_numbers order by N desc" #:fetch 1)])
                     (list n m))))
               (let ([nums (map car test-data)])
                 (map list nums (reverse nums))))))
    (test-case "in-query multiple same"
      (with-connection c
        (let ([pst (prepare c "select N from the_numbers order by N asc")])
          (check equal?
                 (call-with-transaction c
                   (lambda ()
                     (for/list ([n (in-query c pst #:fetch 1)]
                                [m (in-query c pst #:fetch 1)])
                       (list n m))))
                 (let ([nums (map car test-data)])
                   (map list nums nums))))))
    (test-case "in-query with interleaved queries"
      (with-connection c
        (check equal?
               (call-with-transaction c
                 (lambda ()
                   (for/list ([n (in-query c "select N from the_numbers order by N asc" #:fetch 1)])
                     (list n (query-value c (sql "select descr from the_numbers where N = $1") n)))))
               test-data)))
    ))

(define low-level-tests
  (test-suite "low-level"
    (test-case "query - select"
      (with-connection c
        (let [(q (query c "select N from the_numbers"))]
          (check-pred rows-result? q)
          (check set-equal?
                 (map vector (map car test-data))
                 (rows-result-rows q)))))
    (unless (ANYFLAGS 'isora 'isdb2)
      (test-case "query - update"
        (with-connection c
          (let [(q (query c "update the_numbers set N = -1 where N = 1"))]
            (check-pred simple-result? q)))))
    (test-case "prepared-stmt inspection"
      (with-connection c
        (let ([pst (prepare c "select n, descr from the_numbers")])
          (check-equal? (prepared-statement-parameter-types pst) '())
          (check-equal? (map cadr (prepared-statement-result-types pst))
                        (case dbsys
                          ((postgresql) '(integer varchar))
                          ((mysql) '(integer var-string))
                          ((sqlite3) '(any any))
                          ((odbc) '(integer varchar)))))
        (let* ([pst (prepare c (sql "select n from the_numbers where n = $1"))]
               [param-types (map cadr (prepared-statement-parameter-types pst))])
          (case dbsys
            ((postgresql) (check-equal? param-types '(integer)))
            ((mysql) (check-equal? param-types '(any)))
            ((sqlite3) (check-equal? param-types '(any)))
            ((odbc) (check-equal? (length param-types) 1)))) ;; actual types may vary
        (let* ([pst (prepare c (sql "insert into the_numbers values ($1, $2)"))]
               [param-types (map cadr (prepared-statement-parameter-types pst))])
          (case dbsys
            ((postgresql) (check-equal? param-types '(integer varchar)))
            ((mysql) (check-equal? param-types '(any any)))
            ((sqlite3) (check-equal? param-types '(any any)))
            ((odbc) (check-equal? (length param-types) 2))) ;; actual types may vary
          (check-equal? (prepared-statement-result-types pst) '()))))))

(define misc-tests
  (test-suite "misc correctness"
    (test-case "noninterference of nested queries"
      (with-connection c
        (define q
          (for/list ([a (query-list c
                           "select N from the_numbers where N > 0 and N < 3 order by N")])
            (query-value c 
             (format "select descr from the_numbers where N = ~s" a))))
        (define q2 
          (query-list c 
             "select descr from the_numbers where N > 0 and N < 3 order by N"))
        (check-equal? q q2)))
    (test-case "continuation safety"
      (call-with-connection
       (lambda (c)
         (let* [(search-id 1)
                (k1 #f)
                (k2 #f)
                (todo (list 
                       (lambda ()
                         (set! search-id 4)
                         (k1 #t))
                       (lambda ()
                         (set! search-id 2)
                         (k2 #t))
                       (lambda ()
                         (set! search-id 6)
                         (k1 #t))
                       (lambda ()
                         (set! search-id 5)
                         (k2 #t))))
                (q 
                 (let/cc return
                   (for-each
                    (lambda (id)
                      (let/cc k
                        (set! k2 k1)
                        (when NOISY? (printf "saw ~s~n" id))
                        (when (= id search-id)
                          (set! k1 k)
                          (when NOISY? (printf "found ~s~n~n" id))
                          (return id))))
                    (query-list c "select N from the_numbers order by N asc"))
                   (error 'search-failed "couldn't find ~s" search-id)))]
           (unless (null? todo)
             (let [(t (car todo))]
               (set! todo (cdr todo))
               (t)))))))

    ;; Added 18 May 2003: Corrected a bug which incorrectly interleaved
    ;; nulls with returned fields.
    (unless (TESTFLAGS 'odbc 'issl)
      (test-case "nulls arrive in correct order"
        (with-connection c
          ;; raw NULL has PostgreSQL type "unknown", not allowed
          (define (clean . strs)
            (select-val
             (regexp-replace* #rx"NULL" (apply string-append strs)
                              (case dbsys
                                ((postgresql) "cast(NULL as integer)")
                                (else "NULL")))))
          (check-equal? (query-row c (clean "NULL, 1, NULL"))
                        (vector sql-null 1 sql-null))
          (check-equal? (query-row c (clean "1, NULL"))
                        (vector 1 sql-null))
          (check-equal? (query-row c (clean "NULL, 1"))
                        (vector sql-null 1))
          (check-equal?
           (query-row c (clean "1, 2, 3, 4, NULL, 6, NULL, "
                               "8, 9, 10, 11, 12, NULL, 14, 15, NULL, "
                               "NULL, 18, 19, 20, NULL, "
                               "NULL, " "NULL, " "NULL, " "NULL, " "NULL, "
                               "27, 28, 29, 30, NULL, 32, 33, NULL, 35"))
           (vector 1 2 3 4 sql-null 6 sql-null 8 9 10 11 12 sql-null 14 15 sql-null
                   sql-null 18 19 20 sql-null sql-null sql-null sql-null sql-null
                   sql-null 27 28 29 30 sql-null 32 33 sql-null 35)))))))

(define tx-tests
  (test-suite "transaction functions"
    (test-case "start, commit"
      (with-connection c
        (check-pred void? (start-transaction c))
        (check-equal? (in-transaction? c) #t)
        (check-pred void? (commit-transaction c))
        (check-equal? (in-transaction? c) #f)))
    (test-case "start w/ option"
      (with-connection c
        (for ([option
               (case dbsys
                 ((postgresql) '(read-only read-write))
                 ((sqlite3) '(deferred immediate exclusive))
                 (else '()))])
          (check-pred void? (start-transaction c #:option option))
          (check-equal? (in-transaction? c) #t)
          (check-pred void? (commit-transaction c))
          (check-equal? (in-transaction? c) #f))
        (check-exn #rx"^start-transaction: "
                   (lambda () (start-transaction c #:option 'no-such-option)))))
    (test-case "start, rollback"
      (with-connection c
        (check-pred void? (start-transaction c))
        (check-equal? (in-transaction? c) #t)
        (check-pred void? (rollback-transaction c))
        (check-equal? (in-transaction? c) #f)))
    (test-case "error on managed st, unmanaged end"
      (with-connection c
        (start-transaction c)
        (check-exn #rx"statement not allowed in current transaction state.*statement type: ROLLBACK"
                   (lambda () (query-exec c "ROLLBACK")))
        (check-equal? (in-transaction? c) #t)
        ;; SQLite-ODBC is unhappy with open tx on disconnect
        (rollback-transaction c)))
    (unless (ANYFLAGS 'odbc)
      (test-case "unmanaged st, managed end ok"
        (with-connection c
          (query-exec c (cond [(ANYFLAGS 'ispg 'ismy) "START TRANSACTION"]
                              [(ANYFLAGS 'issl) "BEGIN TRANSACTION"]))
          (check-equal? (in-transaction? c) #t)
          (rollback-transaction c)
          (check-equal? (in-transaction? c) #f))))
    (test-case "error on cwt, unmanaged end"
      (with-connection c
        (check-exn #rx"statement not allowed in current transaction state.*statement type: ROLLBACK"
                   (lambda ()
                     (call-with-transaction c
                       (lambda () (query-exec c "ROLLBACK")))))
        (check-equal? (in-transaction? c) #f)))
    (when (and (ANYFLAGS 'ispg 'issl) (not (ANYFLAGS 'odbc)))
      (test-case "transactional ddl"
        (with-connection c
          (start-transaction c)
          (query-exec c "create table foo (n integer)")
          (define exists1 (table-exists? c "foo"))
          (rollback-transaction c)
          (define exists2 (table-exists? c "foo"))
          (when exists2 (query-exec c "drop table foo")) ;; shouldn't happen
          (check-equal? exists1 #t)
          (check-equal? exists2 #f))))
    (when (ANYFLAGS 'ismy 'odbc)
      (test-case "error on implicit-commit stmt"
        (with-connection c
          (start-transaction c)
          (check-exn #rx"statement not allowed.*statement type: statement with implicit commit"
                     (lambda () (query-exec c "create table foo (n integer)")))
          ;; SQLite-ODBC is unhappy with open tx on disconnect
          (rollback-transaction c))))
    (when (ANYFLAGS 'odbc)
      (test-case "error on repeated start"
        (with-connection c
          (start-transaction c)
          (check-exn #rx"already in transaction"
                     (lambda () (start-transaction c))))))
    (unless (ANYFLAGS 'odbc)
      (test-case "start, start"
        (with-connection c
          (check-pred void? (start-transaction c))
          (check-pred void? (start-transaction c))
          (check-equal? (in-transaction? c) #t)
          (check-pred void? (commit-transaction c))
          (check-equal? (in-transaction? c) #t)
          (check-pred void? (commit-transaction c))
          (check-equal? (in-transaction? c) #f))))
    (when (ANYFLAGS 'odbc)
      (test-case "start, start fails"
        (with-connection c
          (start-transaction c)
          (check-exn #rx"already in transaction"
                     (lambda () (start-transaction c)))))
      (test-case "cwt, start fails"
        (with-connection c
          (start-transaction c)
          (check-exn #rx"already in transaction"
                     (lambda () (call-with-transaction c void))))))
    (test-case "commit w/o start is no-op"
      (with-connection c
        (check-pred void? (commit-transaction c))))
    (test-case "rollback w/o start is no-op"
      (with-connection c
        (check-pred void? (rollback-transaction c))))
    (test-case "cwt normal"
      (with-connection c
        (check-equal? (call-with-transaction c
                        (lambda () (query-value c (select-val "'abc'"))))
                      "abc")))
    (test-case "cwt w/ option"
      (with-connection c
        (for ([option
               (case dbsys
                 ((postgresql) '(read-only read-write))
                 ((sqlite3) '(deferred immediate exclusive))
                 (else '()))])
          (check-equal? (call-with-transaction c #:option option
                          (lambda () (query-value c (select-val "'abc'"))))
                        "abc"))
        (check-exn #rx"^call-with-transaction"
                   (lambda ()
                     (call-with-transaction c #:option 'no-such-option
                       (lambda () (query-value c (select-val "'abc'"))))))))
    (test-case "cwt w/ error"
      (with-connection c
        (check-exn exn:fail?
                   (lambda ()
                     (call-with-transaction c
                       (lambda () (query-value c (select-val "foo"))))))
        (check-equal? (in-transaction? c) #f)))
    (test-case "cwt w/ caught error"
      (with-connection c
        (define (check-pg-exn proc)
          (if (ANYFLAGS 'ispg 'odbc) (check-exn exn:fail? proc) (proc)))
        (let ([ok? #f])
          (check-pg-exn
           (lambda ()
             (call-with-transaction c
               (lambda ()
                 (with-handlers ([exn:fail? void?])
                   (query-value c (select-val "foo")))
                 (set! ok? (in-transaction? c))))))
          (check-equal? ok? #t "still in tx after caught error")
          (check-equal? (in-transaction? c) #f))))

    (unless (ANYFLAGS 'odbc)
      (test-case "cwt w/ unclosed tx"
        (with-connection c
          (check-exn #rx"unclosed nested tr.* .within .* call-with-transaction"
                     (lambda ()
                       (call-with-transaction c
                         (lambda ()
                           (start-transaction c)
                           (query-value c (select-val "17"))))))
          (check-equal? (in-transaction? c) #f)))
      (test-case "cwt w/ unbalanced commit"
        (with-connection c
          (check-exn #rx"commit-tr.* start-tr.* .within .* call-with-transaction"
                     (lambda ()
                       (call-with-transaction c
                         (lambda ()
                           (commit-transaction c)))))
          (check-equal? (in-transaction? c) #f)))
      (test-case "cwt w/ unbalanced rollback"
        (with-connection c
          (check-exn #rx"rollback-tr.* start-tr.* .within .* call-with-transaction"
                     (lambda ()
                       (call-with-transaction c
                         (lambda ()
                           (rollback-transaction c)))))
          (check-equal? (in-transaction? c) #f)))

      ;; start-tx, then call-with-tx
      (test-case "st, cwt normal"
        (with-connection c
          (start-transaction c)
          (check-equal? (call-with-transaction c
                          (lambda () (query-value c (select-val "17"))))
                        17)
          (check-equal? (in-transaction? c) #t)))
      (test-case "st, cwt w/ error"
        (with-connection c
          (start-transaction c)
          (check-exn exn:fail?
                     (lambda ()
                       (call-with-transaction c
                         (lambda () (query-value c (select-val "foo"))))))
          (check-equal? (in-transaction? c) #t)))
      (test-case "st, cwt w/ caught error"
        (with-connection c
          (define (check-pg-exn proc)
            (if (ANYFLAGS 'ispg) (check-exn exn:fail? proc) (proc)))
          (let ([ok? #f])
            (start-transaction c)
            (check-pg-exn
             (lambda ()
               (call-with-transaction c
                 (lambda ()
                   (with-handlers ([exn:fail? void?])
                     (query-value c (select-val "foo")))
                   (set! ok? (in-transaction? c))))))
            (check-equal? ok? #t "still in tx after caught error")
            (check-equal? (in-transaction? c) #t))))
      (test-case "st, cwt w/ unclosed tx"
        (with-connection c
          (start-transaction c)
          (check-exn #rx"unclosed nested tr.* .within .* call-with-transaction"
                     (lambda ()
                       (call-with-transaction c
                         (lambda ()
                           (start-transaction c)
                           (query-value c (select-val "17"))))))
          (check-equal? (in-transaction? c) #t)))
      (test-case "st, cwt w/ unbalanced commit"
        (with-connection c
          (start-transaction c)
          (check-exn #rx"commit-tr.* start-tr.* .within .* call-with-transaction"
                     (lambda ()
                       (call-with-transaction c
                         (lambda ()
                           (commit-transaction c)))))
          (check-equal? (in-transaction? c) #t)))
      (test-case "cwt w/ unbalanced rollback"
        (with-connection c
          (start-transaction c)
          (check-exn #rx"rollback-tr.* start-tr.* .within .* call-with-transaction"
                     (lambda ()
                       (call-with-transaction c
                         (lambda ()
                           (rollback-transaction c)))))
          (check-equal? (in-transaction? c) #t))))

    (test-case "cwt misc"
      (with-connection c
        (check-equal? (call-with-transaction c
                        (lambda ()
                          (check-equal? (in-transaction? c) #t)
                          'ok))
                      'ok)
        (check-equal? (call-with-values
                          (lambda () (call-with-transaction c (lambda () (values 1 2 3))))
                        list)
                      (list 1 2 3))))))

(define error-tests
  (test-suite "errors"
    (test-case "query - not a statement"
      (with-connection c
        (check-exn exn:fail? (lambda () (query c 5)))))
    (unless (or (TESTFLAGS 'odbc 'ispg) (ANYFLAGS 'isdb2))
      (test-case "query - multiple statements in string"
        (with-connection c
          (check-exn exn:fail?
                     (lambda ()
                       (query c (string-append (select-val "3") ";"
                                               (select-val "4") ";")))))))
    (test-case "query - unowned prepared stmt"
      (with-connection c1 
        (with-connection c
          (let ([pst (prepare c1 (select-val "5"))])
            (let ([stmt (bind-prepared-statement pst null)])
              (check-exn exn:fail? (lambda () (query c stmt))))))))
    (test-case "query errors - nonfatal"
      (with-connection c
        (check-exn exn:fail? (lambda () (query-value c "select nonsuch from notthere")))
        (check-equal? (query-value c (select-val "17"))
                      (if (TESTFLAGS 'odbc 'issl) "17" 17))))))

(define virtual-statement-tests
  (let ()
    (define (check-prep-once mk-connection)
      (let* ([counter 0]
             [c (mk-connection)]
             [vstmt (virtual-statement
                     (lambda (dbsys)
                       (set! counter (add1 counter))
                       (select-val "17")))])
        (query-value c vstmt)
        (check-equal? counter 1 "first query")
        (query-value c vstmt)
        (check-equal? counter 1 "second query")
        (disconnect c)))
    (test-suite "virtual-statements"
      (test-case "prep once"
        (check-prep-once connect-and-setup))
      (test-case "prep once for virtual-connection"
        (check-prep-once
         (lambda () (virtual-connection connect-and-setup))))
      (test-case "prep once for virtual-connection/pool"
        (check-prep-once
         (lambda () (virtual-connection (connection-pool connect-and-setup))))))))

(define pool-tests
  (test-suite "connection pools"
    (test-case "lease, limit, release"
      (let* ([counter 0]
             [p (connection-pool (lambda () (set! counter (+ 1 counter)) (connect-for-test))
                                 #:max-connections 2)]
             [c1 (connection-pool-lease p)]
             [c2 (connection-pool-lease p)])
        ;; Two created
        (check-equal? counter 2)
        ;; Can't create new one yet
        (check-exn exn:fail? (lambda () (connection-pool-lease p)))
        ;; But if we free one...
        (disconnect c2)
        (check-equal? (connected? c2) #f)
        (let ([c3 (connection-pool-lease p)])
          (check-equal? counter 2 "not new") ;; used idle, not new connection
          (check-equal? (connected? c3) #t))))
    (test-case "release on evt"
      (let* ([p (connection-pool connect-for-test #:max-connections 2)]
             [sema (make-semaphore 0)]
             [c1 (connection-pool-lease p sema)])
        (check-equal? (connected? c1) #t)
        ;; Closes when evt ready
        (begin (semaphore-post sema) (sleep 0.1))
        (check-equal? (connected? c1) #f)))
    (test-case "release on custodian"
      (let* ([p (connection-pool connect-for-test #:max-connections 2)]
             [cust (make-custodian)]
             [c1 (connection-pool-lease p cust)])
        (check-equal? (connected? c1) #t)
        ;; Closes when custodian shutdown
        (begin (custodian-shutdown-all cust) (sleep 0.1))
        (check-equal? (connected? c1) #f)))))

(define test
  (test-suite "query API"
    (simple-tests 'string)
    (simple-tests 'prepare)
    (simple-tests 'bind)
    (simple-tests 'gen)
    in-query-tests
    low-level-tests
    tx-tests
    misc-tests
    virtual-statement-tests
    pool-tests
    error-tests))
