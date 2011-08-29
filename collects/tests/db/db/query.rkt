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

    (test-case "query-exec"
      (unless (ANYFLAGS 'isora 'isdb2) ;; table isn't temp, so don't tamper with it
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
              (for ([(x y) (in-query c (virtual-statement stmt) 2)]) 0)))))))))

(define low-level-tests
  (test-suite "low-level"
    (test-case "query - select"
      (with-connection c
        (let [(q (query c "select N from the_numbers"))]
          (check-pred rows-result? q)
          (check set-equal?
                 (map vector (map car test-data))
                 (rows-result-rows q)))))
    (test-case "query - update"
      (unless (ANYFLAGS 'isora 'isdb2)
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
            ((mysql) (check-equal? param-types '(var-string)))
            ((sqlite3) (check-equal? param-types '(any)))
            ((odbc) (check-equal? (length param-types) 1)))) ;; actual types may vary
        (let* ([pst (prepare c (sql "insert into the_numbers values ($1, $2)"))]
               [param-types (map cadr (prepared-statement-parameter-types pst))])
          (case dbsys
            ((postgresql) (check-equal? param-types '(integer varchar)))
            ((mysql) (check-equal? param-types '(var-string var-string)))
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
    (test-case "nulls arrive in correct order"
      (unless (TESTFLAGS 'odbc 'issl)
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

(define error-tests
  (test-suite "errors"
    (test-case "query - not a statement"
      (with-connection c
        (check-exn exn:fail? (lambda () (query c 5)))))
    (test-case "query - multiple statements in string"
      (unless (or (TESTFLAGS 'odbc 'ispg) (ANYFLAGS 'isdb2))
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

(define test
  (test-suite "query API"
    (simple-tests 'string)
    (simple-tests 'prepare)
    (simple-tests 'bind)
    (simple-tests 'gen)
    low-level-tests
    misc-tests
    error-tests))
