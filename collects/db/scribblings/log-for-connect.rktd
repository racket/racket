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
((connected? c) ((3) 0 () 0 () () (q values #f)) #"" #"")
((query-value c "select 1")
 ((3) 0 () 0 () () (q values 1))
 #"connecting!\n"
 #"")
((connected? c) ((3) 0 () 0 () () (q values #t)) #"" #"")
((void (thread (lambda () (displayln (query-value c "select 2")))))
 ((3) 0 () 0 () () (c values c (void)))
 #"connecting!\n2\n"
 #"")
((disconnect c) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((connected? c) ((3) 0 () 0 () () (q values #f)) #"" #"")
((query-value c "select 3")
 ((3) 0 () 0 () () (q values 3))
 #"connecting!\n"
 #"")
((prepare c "select 2 + $1")
 ((3)
  0
  ()
  0
  ()
  ()
  (q exn "prepare: cannot prepare statement with virtual connection"))
 #""
 #"")
((query-value c "select 2 + $1" 2) ((3) 0 () 0 () () (q values 4)) #"" #"")
((define pst (virtual-statement "select 2 + $1"))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((query-value c pst 3) ((3) 0 () 0 () () (q values 5)) #"" #"")
((begin (set! c #f) (set! pst #f))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
