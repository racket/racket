#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          racket/sandbox
          "config.rkt"
          (for-label db
                     web-server/lang/web))

@title[#:tag "introduction"]{Introduction}

This section introduces this library's basic features and discusses
how to build a database-backed web servlet.

@section[#:tag "intro-basic"]{Basic Features}

The following annotated program demonstrates how to connect to a
database and perform simple queries. Some of the SQL syntax used below
is PostgreSQL-specific, such as the syntax of query parameters
(@litchar{$1} rather than @litchar{?}).

@my-interaction[
[(require db)
 (void)]
]

First we create a connection. Replace @racket[_user], @racket[_db],
and @racket[_password] below with the appropriate values for your
configuration (see @secref{creating-connections} for other connection examples):

@my-interaction[
[(define pgc
   (postgresql-connect #:user _user
                       #:database _db
                       #:password _password))
 (void)]
[pgc
 (new connection%)]
]

Use @racket[query-exec] method to execute a SQL statement for effect.

@my-interaction[
[(query-exec pgc
  "create temporary table the_numbers (n integer, d varchar(20))")
 (void)]
[(query-exec pgc
   "insert into the_numbers values (0, 'nothing')")
 (void)]
[(query-exec pgc 
   "insert into the_numbers values (1, 'the loneliest number')")
 (void)]
[(query-exec pgc
   "insert into the_numbers values (2, 'company')")
 (void)]
]

The @racket[query] function is a more general way to execute a
statement. It returns a structure encapsulating information about the
statement's execution. (But some of that information varies from
system to system and is subject to change.)

@my-interaction[
[(query pgc "insert into the_numbers values (3, 'a crowd')")
 (simple-result '((command insert 0 1)))]
[(query pgc "select n, d from the_numbers where n % 2 = 0")
 (rows-result
  (list
   '((name . "n") (typeid . 23))
   '((name . "d") (typeid . 1043)))
  '(#(0 "nothing") #(2 "company")))]
]

When the query is known to return rows and when the field
descriptions are not needed, it is more convenient to use the
@racket[query-rows] function.

@my-interaction[
[(query-rows pgc "select n, d from the_numbers where n % 2 = 0")
 '(#(0 "nothing") #(2 "company"))]
]

Use @racket[query-row] for queries that are known to return exactly
one row.

@my-interaction[
[(query-row pgc "select * from the_numbers where n = 0")
 (vector 0 "nothing")]
]

Similarly, use @racket[query-list] for queries that produce rows of
exactly one column.

@my-interaction[
[(query-list pgc "select d from the_numbers order by n")
 (list "nothing" "the loneliest number" "company" "a crowd")]
]

When a query is known to return a single value (one row and one
column), use @racket[query-value].

@my-interaction[
[(query-value pgc "select count(*) from the_numbers")
 4]
[(query-value pgc "select d from the_numbers where n = 5")
 (error 'query-value
        "query returned zero rows: ~s"
        "select d from the_numbers where n = 5")]
]

When a query may return zero or one rows, as the last example, use
@racket[query-maybe-row] or @racket[query-maybe-value] instead.

@my-interaction[
[(query-maybe-value pgc "select d from the_numbers where n = 5")
 (values #f)]
]

The @racket[in-query] function produces a sequence that can be used
with Racket's iteration forms:

@my-interaction[
[(for ([(n d) (in-query pgc "select * from the_numbers where n < 4")])
   (printf "~a is ~a\n" n d))
 (for-each (lambda (n d) (printf "~a: ~a\n" n d))
           '(0 1 2 3)
           '("nothing" "the loneliest number" "company" "a crowd"))]
[(for/fold ([sum 0]) ([n (in-query pgc "select n from the_numbers")])
   (+ sum n))
 (for/fold ([sum 0]) ([n (in-list '(0 1 2 3))])
   (+ sum n))]
]

Errors in queries generally do not cause the connection to disconnect.

@my-interaction[
[(begin (with-handlers [(exn:fail?
                         (lambda (e) (printf "~a~n" (exn-message e))))]
          (query-value pgc "select NoSuchField from NoSuchTable"))
        (query-value pgc "select 'okay to proceed!'"))
 (begin (display "query-value: relation \"nosuchtable\" does not exist (SQLSTATE 42P01)")
        "okay to proceed!")]
]

Queries may contain parameters. The easiest way to execute a
parameterized query is to provide the parameters ``inline'' after the
SQL statement in the query function call.

@my-interaction[
[(query-value pgc
  "select d from the_numbers where n = $1" 2)
 "company"]
[(query-list pgc
  "select n from the_numbers where n > $1 and n < $2" 0 3)
 (list 1 2)]
]

Alternatively, a parameterized query may be prepared in advance and
executed later. @tech{Prepared statements} can be executed multiple
times with different parameter values.

@my-interaction[
[(define get-less-than-pst
   (prepare pgc "select n from the_numbers where n < $1"))
 (void)]
[(query-list pgc get-less-than-pst 1)
 (list 0)]
[(query-list pgc (bind-prepared-statement get-less-than-pst 2))
 (list 0 1)]
]

When a connection's work is done, it should be disconnected.

@my-interaction[
[(disconnect pgc)
 (void)]
]


@;{============================================================}

@section[#:tag "intro-servlets"]{Databases and Web Servlets}

Using database connections in a web servlet is more complicated than
in a standalone program. A single servlet is potentially used to serve
many requests at once, each in a separate request-handling
thread. Furthermore, the use of @racket[send/suspend],
@racket[send/suspend/dispatch], etc means that there are many places
where a servlet may start and stop executing to service a request.

Why not use a single connection to handle all of a servlet's requests?
That is, create the connection with the servlet instance and never
disconnect it. Such a servlet would look something like the following:

@racketmod[
#:file "bad-servlet.rkt" 
web-server
(require db)
(define db-conn (postgresql-connect ....))
(define (serve req)
  .... db-conn ....)
]

The main problem with using one connection for all requests is that
multiple threads accessing the same connection are not properly
@hyperlink["http://en.wikipedia.org/wiki/Isolation_%28database_systems%29"]{isolated}. For
example, if two threads both attempt to start a new transaction, the
second one will fail, because the first thread has already put the
connection into an ``in transaction'' state. And if one thread is
accessing the connection within a transaction and another thread
issues a query, the second thread may see invalid data or even disrupt
the work of the first thread.

A secondary problem is performance. A connection can only perform a
single query at a time, whereas most database systems are capable of
concurrent query processing.

The proper way to use database connections in a servlet is to create a
connection for each request and disconnect it when the request
is handled. But since a request thread may start and stop executing in
many places (due to @racket[send/suspend], etc), inserting the code to
connect and disconnect at the proper places can be challenging and
messy.

A better solution is to use a @tech{virtual connection}, which creates
a request-specific (that is, thread-specific) ``actual connection'' by
need and disconnects it when the request is handled (that is, when the
thread terminates). Different request-handling threads using the same
virtual connection are assigned different actual connection, so the
threads are properly isolated.

@racketmod[
#:file "better-servlet.rkt" 
web-server
(require db)
(define db-conn
  (virtual-connection
   (lambda () (postgresql-connect ....))))
(define (serve req)
  .... db-conn ....)
]

This solution preserves the simplicity of the naive solution and fixes
the isolation problem but at the cost of creating many short-lived
database connections. That cost can be eliminated by using a
@tech{connection pool}:

@racketmod[
#:file "best-servlet.rkt" 
web-server
(require db)
(define db-conn
  (virtual-connection
   (connection-pool
    (lambda () (postgresql-connect ....)))))
(define (serve req)
  .... db-conn ....)
]

By using a virtual connection backed by a connection pool, a servlet
can achieve simplicity, isolation, and performance.

@;{

TODO:
 - talk about virtual statements, too
 - show actual working servlet code

--

A prepared statement is tied to the connection used to create it;
attempting to use it with another connection results in an
error. Unfortunately, in some scenarios such as web servlets, the
lifetimes of connections are short or difficult to track, making
prepared statements inconvenient. In such cases, a better tool is the
@tech{virtual statement}, which prepares statements on demand and
caches them for future use with the same connection.

@my-interaction[
[(define get-less-than-pst
   (virtual-statement "select n from the_numbers where n < $1"))
 (void)]
[(code:line (query-list pgc1 get-less-than-pst 1) (code:comment "prepares statement for pgc1"))
 (list 0)]
[(code:line (query-list pgc2 get-less-than-pst 2) (code:comment "prepares statement for pgc2"))
 (list 0 1)]
[(code:line (query-list pgc1 get-less-than-pst 3) (code:comment "uses existing prep. stmt."))
 (list 0 1 2)]
]
}
