;; This file was created by make-log-based-eval
((require racket/class db db/util/postgresql db/util/datetime)
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((require db) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((define pgc (dsn-connect 'db-scribble-env))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((query-exec
  pgc
  "create temporary table the_numbers (n integer, d varchar(20))")
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((query-exec pgc "insert into the_numbers values (0, 'nothing')")
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((query-exec pgc "insert into the_numbers values (1, 'the loneliest number')")
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((query-exec pgc "insert into the_numbers values (2, 'company')")
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((query pgc "insert into the_numbers values (3, 'a crowd')")
 ((3)
  1
  (((lib "db/private/generic/interfaces.rkt")
    .
    deserialize-info:simple-result-v0))
  0
  ()
  ()
  (c values c (0 (c (c command u . "INSERT 0 1")))))
 #""
 #"")
((query pgc "select n, d from the_numbers where n % 2 = 0")
 ((3)
  1
  (((lib "db/private/generic/interfaces.rkt")
    .
    deserialize-info:rows-result-v0))
  0
  ()
  ()
  (c
   values
   c
   (0
    (c
     (c
      (c name u . "n")
      c
      (c typeid . 23)
      c
      (c type-size . 4)
      c
      (c type-mod . -1))
     c
     (c
      (c name u . "d")
      c
      (c typeid . 1043)
      c
      (c type-size . -1)
      c
      (c type-mod . 24)))
    (c (v! 0 (u . "nothing")) c (v! 2 (u . "company"))))))
 #""
 #"")
((query-rows pgc "select n, d from the_numbers where n % 2 = 0")
 ((3)
  0
  ()
  0
  ()
  ()
  (c values c (c (v! 0 (u . "nothing")) c (v! 2 (u . "company")))))
 #""
 #"")
((query-row pgc "select * from the_numbers where n = 0")
 ((3) 0 () 0 () () (c values c (v! 0 (u . "nothing"))))
 #""
 #"")
((query-list pgc "select d from the_numbers order by n")
 ((3)
  0
  ()
  0
  ()
  ()
  (c
   values
   c
   (c
    (u . "nothing")
    c
    (u . "the loneliest number")
    c
    (u . "company")
    c
    (u . "a crowd"))))
 #""
 #"")
((query-value pgc "select count(*) from the_numbers")
 ((3) 0 () 0 () () (c values c 4))
 #""
 #"")
((query-value pgc "select d from the_numbers where n = 5")
 ((3)
  0
  ()
  0
  ()
  ()
  (c
   exn
   c
   "query-value: query returned wrong number of rows\n  statement: \"select d from the_numbers where n = 5\"\n  expected: 1\n  got: 0"))
 #""
 #"")
((query-maybe-value pgc "select d from the_numbers where n = 5")
 ((3) 0 () 0 () () (c values c #f))
 #""
 #"")
((for
  (((n d) (in-query pgc "select * from the_numbers where n < 4")))
  (printf "~a: ~a\n" n d))
 ((3) 0 () 0 () () (c values c (void)))
 #"0: nothing\n1: the loneliest number\n2: company\n3: a crowd\n"
 #"")
((for/fold
  ((sum 0))
  ((n (in-query pgc "select n from the_numbers")))
  (+ sum n))
 ((3) 0 () 0 () () (c values c 6))
 #""
 #"")
((begin
   (with-handlers
    ((exn:fail? (lambda (e) (printf "~a~n" (exn-message e)))))
    (query-value pgc "select NoSuchField from NoSuchTable"))
   (query-value pgc "select 'okay to proceed!'"))
 ((3) 0 () 0 () () (c values c (u . "okay to proceed!")))
 #"query-value: relation \"nosuchtable\" does not exist\n  SQLSTATE: 42P01\n"
 #"")
((query-value pgc "select d from the_numbers where n = $1" 2)
 ((3) 0 () 0 () () (c values c (u . "company")))
 #""
 #"")
((query-list pgc "select n from the_numbers where n > $1 and n < $2" 0 3)
 ((3) 0 () 0 () () (c values c (c 1 c 2)))
 #""
 #"")
((define get-less-than-pst
   (prepare pgc "select n from the_numbers where n < $1"))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((query-list pgc get-less-than-pst 1)
 ((3) 0 () 0 () () (c values c (c 0)))
 #""
 #"")
((query-list pgc (bind-prepared-statement get-less-than-pst '(2)))
 ((3) 0 () 0 () () (c values c (c 0 c 1)))
 #""
 #"")
((void) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((define pool
   (connection-pool
    (lambda ()
      (displayln "connecting!")
      (sqlite3-connect #:database 'memory))))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define c1 (connection-pool-lease pool))
 ((3) 0 () 0 () () (c values c (void)))
 #"connecting!\n"
 #"")
((define c2 (connection-pool-lease pool))
 ((3) 0 () 0 () () (c values c (void)))
 #"connecting!\n"
 #"")
((disconnect c1) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((define c3 (connection-pool-lease pool))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define c
   (virtual-connection
    (lambda () (printf "connecting!\n") (dsn-connect 'db-scribble-env))))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((connected? c) ((3) 0 () 0 () () (c values c #f)) #"" #"")
((query-value c "select 1")
 ((3) 0 () 0 () () (c values c 1))
 #"connecting!\n"
 #"")
((connected? c) ((3) 0 () 0 () () (c values c #t)) #"" #"")
((void (thread (lambda () (displayln (query-value c "select 2")))))
 ((3) 0 () 0 () () (c values c (void)))
 #"connecting!\n2\n"
 #"")
((disconnect c) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((connected? c) ((3) 0 () 0 () () (c values c #f)) #"" #"")
((query-value c "select 3")
 ((3) 0 () 0 () () (c values c 3))
 #"connecting!\n"
 #"")
((prepare c "select 2 + $1")
 ((3)
  0
  ()
  0
  ()
  ()
  (c exn c "prepare: cannot prepare statement with virtual connection"))
 #""
 #"")
((query-value c "select 2 + $1" 2) ((3) 0 () 0 () () (c values c 4)) #"" #"")
((define pst (virtual-statement "select 2 + $1"))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((query-value c pst 3) ((3) 0 () 0 () () (c values c 5)) #"" #"")
((begin (set! c #f) (set! pst #f))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define c pgc) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((query-exec pgc "insert into the_numbers values (42, 'the answer')")
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((query-exec pgc "delete from the_numbers where n = $1" 42)
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((query-rows pgc "select * from the_numbers where n = $1" 2)
 ((3) 0 () 0 () () (c values c (c (v! 2 (u . "company")))))
 #""
 #"")
((query-rows c "select 17")
 ((3) 0 () 0 () () (c values c (c (v! 17))))
 #""
 #"")
((query-list c "select n from the_numbers where n < 2")
 ((3) 0 () 0 () () (c values c (c 0 c 1)))
 #""
 #"")
((query-list c "select 'hello'")
 ((3) 0 () 0 () () (c values c (c (u . "hello"))))
 #""
 #"")
((query-row pgc "select * from the_numbers where n = $1" 2)
 ((3) 0 () 0 () () (c values c (v! 2 (u . "company"))))
 #""
 #"")
((query-row pgc "select min(n), max(n) from the_numbers")
 ((3) 0 () 0 () () (c values c (v! 0 3)))
 #""
 #"")
((query-maybe-row pgc "select * from the_numbers where n = $1" 100)
 ((3) 0 () 0 () () (c values c #f))
 #""
 #"")
((query-maybe-row c "select 17")
 ((3) 0 () 0 () () (c values c (v! 17)))
 #""
 #"")
((query-value pgc "select timestamp 'epoch'")
 ((3)
  1
  (((lib "db/private/generic/sql-data.rkt")
    .
    deserialize-info:sql-timestamp-v0))
  0
  ()
  ()
  (c values c (0 1970 1 1 0 0 0 0 #f)))
 #""
 #"")
((query-value pgc "select d from the_numbers where n = $1" 3)
 ((3) 0 () 0 () () (c values c (u . "a crowd")))
 #""
 #"")
((query-value pgc "select d from the_numbers where n = $1" 100)
 ((3)
  0
  ()
  0
  ()
  ()
  (c
   exn
   c
   "query-value: query returned wrong number of rows\n  statement: \"select d from the_numbers where n = $1\"\n  expected: 1\n  got: 0"))
 #""
 #"")
((query-value c "select count(*) from the_numbers")
 ((3) 0 () 0 () () (c values c 4))
 #""
 #"")
((for/list ((n (in-query pgc "select n from the_numbers where n < 2"))) n)
 ((3) 0 () 0 () () (c values c (c 0 c 1)))
 #""
 #"")
((call-with-transaction
  pgc
  (lambda ()
    (for
     (((n d)
       (in-query pgc "select * from the_numbers where n < $1" 4 #:fetch 1)))
     (printf "~a: ~a\n" n d))))
 ((3) 0 () 0 () () (c values c (void)))
 #"0: nothing\n1: the loneliest number\n2: company\n3: a crowd\n"
 #"")
((for ((n (in-query pgc "select * from the_numbers"))) (displayln n))
 ((3)
  0
  ()
  0
  ()
  ()
  (c
   exn
   c
   "in-query: query returned wrong number of columns\n  statement: \"select * from the_numbers\"\n  expected: 1\n  got: 2"))
 #""
 #"")
((define vehicles-result
   (rows-result
    '(((name . "type")) ((name . "maker")) ((name . "model")))
    `(#("car" "honda" "civic")
      #("car" "ford" "focus")
      #("car" "ford" "pinto")
      #("bike" "giant" "boulder")
      #("bike" "schwinn" ,sql-null))))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((group-rows vehicles-result #:group '(#("type")))
 ((3)
  2
  (((lib "db/private/generic/interfaces.rkt")
    .
    deserialize-info:rows-result-v0)
   ((lib "db/private/generic/sql-data.rkt") . deserialize-info:sql-null-v0))
  1
  ("ford")
  ()
  (c
   values
   c
   (0
    (c
     (c (c name . "type"))
     c
     (c
      (c name . "grouped")
      c
      (c grouped c (c (c name . "maker")) c (c (c name . "model")))))
    (c
     (v!
      "car"
      (c (v! "honda" "civic") c (v! (? . 0) "focus") c (v! (? . 0) "pinto")))
     c
     (v! "bike" (c (v! "giant" "boulder") c (v! "schwinn" (1))))))))
 #""
 #"")
((group-rows
  vehicles-result
  #:group
  '(#("type") #("maker"))
  #:group-mode
  '(list))
 ((3)
  1
  (((lib "db/private/generic/interfaces.rkt")
    .
    deserialize-info:rows-result-v0))
  1
  ((c name . "grouped"))
  ()
  (c
   values
   c
   (0
    (c
     (c (c name . "type"))
     c
     (c
      (? . 0)
      c
      (c
       grouped
       c
       (c (c name . "maker"))
       c
       (c (? . 0) c (c grouped c (c (c name . "model")))))))
    (c
     (v!
      "car"
      (c (v! "honda" (c "civic")) c (v! "ford" (c "focus" c "pinto"))))
     c
     (v! "bike" (c (v! "giant" (c "boulder")) c (v! "schwinn" ())))))))
 #""
 #"")
((rows->dict vehicles-result #:key "model" #:value '#("type" "maker"))
 ((3)
  1
  (((lib "db/private/generic/sql-data.rkt") . deserialize-info:sql-null-v0))
  3
  ("car" "ford" "bike")
  ()
  (c
   values
   c
   (h
    -
    (equal)
    ((0) v! (? . 2) "schwinn")
    ("civic" v! (? . 0) "honda")
    ("pinto" v! (? . 0) (? . 1))
    ("focus" v! (? . 0) (? . 1))
    ("boulder" v! (? . 2) "giant"))))
 #""
 #"")
((rows->dict
  vehicles-result
  #:key
  "maker"
  #:value
  "model"
  #:value-mode
  '(list))
 ((3)
  0
  ()
  0
  ()
  ()
  (c
   values
   c
   (h
    -
    (equal)
    ("ford" c "focus" c "pinto")
    ("honda" c "civic")
    ("giant" c "boulder")
    ("schwinn"))))
 #""
 #"")
((let* ((get-name-pst (prepare pgc "select d from the_numbers where n = $1"))
        (get-name2 (bind-prepared-statement get-name-pst (list 2)))
        (get-name3 (bind-prepared-statement get-name-pst (list 3))))
   (list (query-value pgc get-name2) (query-value pgc get-name3)))
 ((3) 0 () 0 () () (c values c (c (u . "company") c (u . "a crowd"))))
 #""
 #"")
((define pst
   (virtual-statement
    (lambda (dbsys)
      (case (dbsystem-name dbsys)
        ((postgresql) "select n from the_numbers where n < $1")
        ((sqlite3) "select n from the_numbers where n < ?")
        (else (error "unknown system"))))))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((query-list pgc pst 3) ((3) 0 () 0 () () (c values c (c 0 c 1 c 2))) #"" #"")
((query-list pgc pst 3) ((3) 0 () 0 () () (c values c (c 0 c 1 c 2))) #"" #"")
((with-handlers
  ((exn:fail:sql? exn:fail:sql-info))
  (query pgc "select * from nosuchtable"))
 ((3)
  0
  ()
  0
  ()
  ()
  (c
   values
   c
   (c
    (c severity u . "ERROR")
    c
    (c code u . "42P01")
    c
    (c message u . "relation \"nosuchtable\" does not exist")
    c
    (c position u . "15")
    c
    (c file u . "parse_relation.c")
    c
    (c line u . "857")
    c
    (c routine u . "parserOpenTable"))))
 #""
 #"")
((query-value pgc "select count(*) from the_numbers")
 ((3) 0 () 0 () () (c values c 4))
 #""
 #"")
((query-value pgc "select false") ((3) 0 () 0 () () (c values c #f)) #"" #"")
((query-value pgc "select 1 + $1" 2) ((3) 0 () 0 () () (c values c 3)) #"" #"")
((query-value pgc "select inet '127.0.0.1'")
 ((3)
  0
  ()
  0
  ()
  ()
  (c exn c "query-value: unsupported type\n  type: inet\n  typeid: 869"))
 #""
 #"")
((query-value pgc "select cast(inet '127.0.0.1' as varchar)")
 ((3) 0 () 0 () () (c values c (u . "127.0.0.1/32")))
 #""
 #"")
((query-value pgc "select real '+Infinity'")
 ((3) 0 () 0 () () (c values c +inf.0))
 #""
 #"")
((query-value pgc "select numeric '12345678901234567890'")
 ((3) 0 () 0 () () (c values c 12345678901234567890))
 #""
 #"")
((query-value pgc "select 1 in (1, 2, 3)")
 ((3) 0 () 0 () () (c values c #t))
 #""
 #"")
((query-value
  pgc
  "select 1 = any ($1::integer[])"
  (list->pg-array (list 1 2 3)))
 ((3) 0 () 0 () () (c values c #t))
 #""
 #"")
((query-value pgc "select 1 = any ($1)" (list 1 2 3))
 ((3) 0 () 0 () () (c values c #t))
 #""
 #"")
((query-value pgc "select $1::integer = any ($2)" 1 (list 1 2 3))
 ((3) 0 () 0 () () (c values c #t))
 #""
 #"")
((query-value pgc "select $1 = any ($2)" 1 (list 1 2 3))
 ((3)
  0
  ()
  0
  ()
  ()
  (c
   exn
   c
   "query-value: cannot convert given value to SQL type\n  given: 1\n  type: string\n  dialect: PostgreSQL"))
 #""
 #"")
((query-value c "select NULL")
 ((3)
  1
  (((lib "db/private/generic/sql-data.rkt") . deserialize-info:sql-null-v0))
  0
  ()
  ()
  (c values c (0)))
 #""
 #"")
((sql-null->false "apple") ((3) 0 () 0 () () (c values c "apple")) #"" #"")
((sql-null->false sql-null) ((3) 0 () 0 () () (c values c #f)) #"" #"")
((sql-null->false #f) ((3) 0 () 0 () () (c values c #f)) #"" #"")
((false->sql-null "apple") ((3) 0 () 0 () () (c values c "apple")) #"" #"")
((false->sql-null #f)
 ((3)
  1
  (((lib "db/private/generic/sql-data.rkt") . deserialize-info:sql-null-v0))
  0
  ()
  ()
  (c values c (0)))
 #""
 #"")
((query-value pgc "select date '25-dec-1980'")
 ((3)
  1
  (((lib "db/private/generic/sql-data.rkt") . deserialize-info:sql-date-v0))
  0
  ()
  ()
  (c values c (0 1980 12 25)))
 #""
 #"")
((query-value pgc "select time '7:30'")
 ((3)
  1
  (((lib "db/private/generic/sql-data.rkt") . deserialize-info:sql-time-v0))
  0
  ()
  ()
  (c values c (0 7 30 0 0 #f)))
 #""
 #"")
((query-value pgc "select timestamp 'epoch'")
 ((3)
  1
  (((lib "db/private/generic/sql-data.rkt")
    .
    deserialize-info:sql-timestamp-v0))
  0
  ()
  ()
  (c values c (0 1970 1 1 0 0 0 0 #f)))
 #""
 #"")
((query-value pgc "select timestamp with time zone 'epoch'")
 ((3)
  1
  (((lib "db/private/generic/sql-data.rkt")
    .
    deserialize-info:sql-timestamp-v0))
  0
  ()
  ()
  (c values c (0 1969 12 31 19 0 0 0 -18000)))
 #""
 #"")
((sql-bits->list (string->sql-bits "1011"))
 ((3) 0 () 0 () () (c values c (c #t c #f c #t c #t)))
 #""
 #"")
((sql-bits->string (query-value pgc "select B'010110111'"))
 ((3) 0 () 0 () () (c values c (u . "010110111")))
 #""
 #"")
((sql-datetime->srfi-date (query-value pgc "select time '7:30'"))
 ((3)
  1
  (((lib "srfi/19/time.rkt") . deserialize-info:tm:date-v0))
  0
  ()
  ()
  (c values c (0 0 0 30 7 0 0 0 0)))
 #""
 #"")
((sql-datetime->srfi-date (query-value pgc "select date '25-dec-1980'"))
 ((3)
  1
  (((lib "srfi/19/time.rkt") . deserialize-info:tm:date-v0))
  0
  ()
  ()
  (c values c (0 0 0 0 0 25 12 1980 0)))
 #""
 #"")
((sql-datetime->srfi-date (query-value pgc "select timestamp 'epoch'"))
 ((3)
  1
  (((lib "srfi/19/time.rkt") . deserialize-info:tm:date-v0))
  0
  ()
  ()
  (c values c (0 0 0 0 0 1 1 1970 0)))
 #""
 #"")
