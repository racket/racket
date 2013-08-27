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
((query-exec pgc "insert into the_numbers values ($1, $2)" (+ 1 1) "company")
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
  (c values c (0 (q (insert-id . #f) (affected-rows . 1)))))
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
     (c (c name u . "n") q (typeid . 23) (type-size . 4) (type-mod . -1))
     c
     (c (c name u . "d") q (typeid . 1043) (type-size . -1) (type-mod . 24)))
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
 ((3) 0 () 0 () () (q values 4))
 #""
 #"")
((query-value pgc "select d from the_numbers where n = 5")
 ((3)
  0
  ()
  0
  ()
  ()
  (q
   exn
   "query-value: query returned wrong number of rows\n  statement: \"select d from the_numbers where n = 5\"\n  expected: 1\n  got: 0"))
 #""
 #"")
((query-maybe-value pgc "select d from the_numbers where n = 5")
 ((3) 0 () 0 () () (q values #f))
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
 ((3) 0 () 0 () () (q values 6))
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
 ((3) 0 () 0 () () (q values (1 2)))
 #""
 #"")
((define get-less-than-pst
   (prepare pgc "select n from the_numbers where n < $1"))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((query-list pgc get-less-than-pst 1)
 ((3) 0 () 0 () () (q values (0)))
 #""
 #"")
((query-list pgc (bind-prepared-statement get-less-than-pst '(2)))
 ((3) 0 () 0 () () (q values (0 1)))
 #""
 #"")
((void) ((3) 0 () 0 () () (c values c (void))) #"" #"")
