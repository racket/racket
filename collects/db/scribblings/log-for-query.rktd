;; This file was created by make-log-based-eval
((require racket/class db db/util/postgresql db/util/datetime)
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((begin
   (define pgc (dsn-connect 'db-scribble-env))
   (query-exec
    pgc
    "create temporary table the_numbers (n integer, d varchar(20))")
   (query-exec pgc "insert into the_numbers values (0, 'nothing')")
   (query-exec
    pgc
    "insert into the_numbers values (1, 'the loneliest number')")
   (query-exec pgc "insert into the_numbers values (2, 'company')")
   (query-exec pgc "insert into the_numbers values (3, 'a crowd')"))
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
 ((3) 0 () 0 () () (q values (0 1)))
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
 ((3) 0 () 0 () () (q values #f))
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
((query-maybe-value pgc "select d from the_numbers where n = $1" 100)
 ((3) 0 () 0 () () (q values #f))
 #""
 #"")
((query-maybe-value c "select count(*) from the_numbers")
 ((3) 0 () 0 () () (q values 4))
 #""
 #"")
((for/list ((n (in-query pgc "select n from the_numbers where n < 2"))) n)
 ((3) 0 () 0 () () (q values (0 1)))
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
  (q
   exn
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
    (q
     ((name . "type"))
     ((name . "grouped") (grouped ((name . "maker")) ((name . "model")))))
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
  ((q name . "grouped"))
  ()
  (c
   values
   c
   (0
    (c
     (q (name . "type"))
     c
     (c
      (? . 0)
      c
      (c
       grouped
       c
       (q (name . "maker"))
       c
       (c (? . 0) q (grouped ((name . "model")))))))
    (c
     (v! "car" (c (v! "honda" (q "civic")) c (v! "ford" (q "focus" "pinto"))))
     c
     (v! "bike" (c (v! "giant" (q "boulder")) c (v! "schwinn" ())))))))
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
    ("ford" q "focus" "pinto")
    ("honda" q "civic")
    ("giant" q "boulder")
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
((query-list pgc pst 3) ((3) 0 () 0 () () (q values (0 1 2))) #"" #"")
((query-list pgc pst 3) ((3) 0 () 0 () () (q values (0 1 2))) #"" #"")
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
