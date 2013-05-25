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
((query-value pgc "select count(*) from the_numbers")
 ((3) 0 () 0 () () (q values 4))
 #""
 #"")
((query-value pgc "select false") ((3) 0 () 0 () () (q values #f)) #"" #"")
((query-value pgc "select 1 + $1" 2) ((3) 0 () 0 () () (q values 3)) #"" #"")
((query-value pgc "select inet '127.0.0.1'")
 ((3)
  0
  ()
  0
  ()
  ()
  (q exn "query-value: unsupported type\n  type: inet\n  typeid: 869"))
 #""
 #"")
((query-value pgc "select cast(inet '127.0.0.1' as varchar)")
 ((3) 0 () 0 () () (c values c (u . "127.0.0.1/32")))
 #""
 #"")
((query-value pgc "select real '+Infinity'")
 ((3) 0 () 0 () () (q values +inf.0))
 #""
 #"")
((query-value pgc "select numeric '12345678901234567890'")
 ((3) 0 () 0 () () (q values 12345678901234567890))
 #""
 #"")
((query-value pgc "select 1 in (1, 2, 3)")
 ((3) 0 () 0 () () (q values #t))
 #""
 #"")
((query-value
  pgc
  "select 1 = any ($1::integer[])"
  (list->pg-array (list 1 2 3)))
 ((3) 0 () 0 () () (q values #t))
 #""
 #"")
((query-value pgc "select 1 = any ($1)" (list 1 2 3))
 ((3) 0 () 0 () () (q values #t))
 #""
 #"")
((query-value pgc "select $1::integer = any ($2)" 1 (list 1 2 3))
 ((3) 0 () 0 () () (q values #t))
 #""
 #"")
((query-value pgc "select $1 = any ($2)" 1 (list 1 2 3))
 ((3)
  0
  ()
  0
  ()
  ()
  (q
   exn
   "query-value: cannot convert given value to SQL type\n  given: 1\n  type: string\n  expected: string?\n  dialect: PostgreSQL"))
 #""
 #"")
((query-value pgc "select NULL")
 ((3)
  1
  (((lib "db/private/generic/sql-data.rkt") . deserialize-info:sql-null-v0))
  0
  ()
  ()
  (c values c (0)))
 #""
 #"")
((sql-null->false "apple") ((3) 0 () 0 () () (q values "apple")) #"" #"")
((sql-null->false sql-null) ((3) 0 () 0 () () (q values #f)) #"" #"")
((sql-null->false #f) ((3) 0 () 0 () () (q values #f)) #"" #"")
((false->sql-null "apple") ((3) 0 () 0 () () (q values "apple")) #"" #"")
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
  (c values c (0 1970 1 1 0 0 0 0 0)))
 #""
 #"")
