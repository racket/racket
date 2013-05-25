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
