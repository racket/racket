#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          racket/sandbox
          racket/runtime-path
          "config.rkt"
          (for-label db db/util/testing racket/dict web-server/lang/web))

@(define-runtime-path log-file "log-for-using-db.rktd")
@(define the-eval (make-pg-eval log-file #f))

@title[#:tag "using-db"]{Using Database Connections}

This section introduces this library's basic features and covers some
practical issues with database programming in general and with this
library in particular.


@section[#:tag "intro-basic"]{Introduction to Using Database Connections}

The following annotated program demonstrates how to connect to a
database and perform simple queries. Some of the SQL syntax used below
is PostgreSQL-specific, such as the syntax of query parameters
(@litchar{$1} rather than @litchar{?}).

@interaction[#:eval the-eval
(require db)
]

First we create a connection. Replace @racket[_user], @racket[_db],
and @racket[_password] below with the appropriate values for your
configuration (see @secref{creating-connections} for other connection examples):

@interaction[#:eval the-eval
(eval:alts
 (define pgc
   (postgresql-connect #:user _user
                       #:database _db
                       #:password _password))
 (define pgc (dsn-connect 'db-scribble-env)))
]

Use @racket[query-exec] method to execute a SQL statement for effect.

@;{ Keep these examples in sync with config.rkt }

@interaction[#:eval the-eval
(query-exec pgc
 "create temporary table the_numbers (n integer, d varchar(20))")
(query-exec pgc
  "insert into the_numbers values (0, 'nothing')")
(query-exec pgc 
  "insert into the_numbers values (1, 'the loneliest number')")
]

When a query contains a SQL value that isn't constant, refer to it
through a ``query parameter'' rather than by dynamically computing the
SQL query string (see also @secref["dbsec-sql-injection"]). Just
provide the parameter values after the SQL statement in the query
function call:

@interaction[#:eval the-eval
(query-exec pgc
  "insert into the_numbers values ($1, $2)"
  (+ 1 1)
  "company")
]

Every standard query function accepts query parameters. The SQL syntax
for query parameters depends on the database system (see
@secref["query-statements"]). Other options for running parameterized
queries are discussed below.

The @racket[query] function is a more general way to execute a
statement. It returns a structure encapsulating information about the
statement's execution. (But some of that information varies from
system to system and is subject to change.)

@interaction[#:eval the-eval
(query pgc "insert into the_numbers values (3, 'a crowd')")
(query pgc "select n, d from the_numbers where n % 2 = 0")
]

When the query is known to return rows and when the field
descriptions are not needed, it is more convenient to use the
@racket[query-rows] function.

@interaction[#:eval the-eval
(query-rows pgc "select n, d from the_numbers where n % 2 = 0")
]

Use @racket[query-row] for queries that are known to return exactly
one row.

@interaction[#:eval the-eval
(query-row pgc "select * from the_numbers where n = 0")
]

Similarly, use @racket[query-list] for queries that produce rows of
exactly one column.

@interaction[#:eval the-eval
(query-list pgc "select d from the_numbers order by n")
]

When a query is known to return a single value (one row and one
column), use @racket[query-value].

@interaction[#:eval the-eval
(query-value pgc "select count(*) from the_numbers")
(query-value pgc "select d from the_numbers where n = 5")
]

When a query may return zero or one rows, as the last example, use
@racket[query-maybe-row] or @racket[query-maybe-value] instead.

@interaction[#:eval the-eval
(query-maybe-value pgc "select d from the_numbers where n = 5")
]

The @racket[in-query] function produces a sequence that can be used
with Racket's iteration forms:

@interaction[#:eval the-eval
(for ([(n d) (in-query pgc "select * from the_numbers where n < 4")])
  (printf "~a: ~a\n" n d))
(for/fold ([sum 0]) ([n (in-query pgc "select n from the_numbers")])
  (+ sum n))
]

Errors in queries generally do not cause the connection to disconnect.

@interaction[#:eval the-eval
(begin (with-handlers [(exn:fail?
                       (lambda (e)
                         (printf "~a~n" (exn-message e))))]
         (query-value pgc "select NoSuchField from NoSuchTable"))
       (query-value pgc "select 'okay to proceed!'"))
]

Queries may contain parameters. The easiest way to execute a
parameterized query is to provide the parameters ``inline'' after the
SQL statement in the query function call.

@interaction[#:eval the-eval
(query-value pgc
 "select d from the_numbers where n = $1" 2)
(query-list pgc
 "select n from the_numbers where n > $1 and n < $2" 0 3)
]

Alternatively, a parameterized query may be prepared in advance and
executed later. @tech{Prepared statements} can be executed multiple
times with different parameter values.

@interaction[#:eval the-eval
(define get-less-than-pst
  (prepare pgc "select n from the_numbers where n < $1"))
(query-list pgc get-less-than-pst 1)
(query-list pgc (bind-prepared-statement get-less-than-pst '(2)))
]

When a connection's work is done, it should be disconnected.

@;{ Don't actually disconnect; use this connection for the rest of the examples. }
@interaction[#:eval the-eval
(eval:alts (disconnect pgc) (void))
]


@section[#:tag "dbsec"]{Database Security}

Database security requires both that the database back end be secured
against unauthorized use and that authorized clients are not tricked
or subverted into violating the database's security.

Securing database back ends is mostly beyond the scope of this
manual. In brief: choose sufficiently strong authentication methods
and keep credentials secure, and follow the
@wplink["Principle_of_least_privilege"]{principle of least privilege}:
create and use roles that have the minimum permissions needed.

The following is an incomplete list of security issues related to
database @emph{client} programming.

@;{Add section on db roles and ro/rw access?
   eg, for servlet create two connections: one ro and one rw
   (in pg/my/etc, backed by two roles; in sqlite, connection options) }

@subsection[#:tag "dbsec-sql-injection"]{SQL Injection}

@wplink["SQL_injection"]{SQL injection} happens when part of a SQL
statement that was intended as SQL literal data is instead interpreted
as SQL code---possibly @hyperlink["http://xkcd.com/327/"]{malicious}
SQL code.

Avoid dynamically creating SQL query strings by string concatenation
or interpolation (eg, with @racket[string-append] or
@racket[format]). In most cases, it is possible to use
@tech{parameterized queries} instead. For example, instead of this

@racketblock[
(code:comment "WRONG! DANGER!")
(query-exec c
  (format "UPDATE users SET passwd='~a' WHERE user='~a'"
          user new-passwd))
]
write one of the following instead (depending on SQL dialect):
@racketblock[
(code:comment "for PostgreSQL, SQLite")
(query-exec c "UPDATE users SET passwd=$1 WHERE user=$2" user new-passwd)
(code:comment "for MySQL, SQLite, ODBC")
(query-exec c "UPDATE users SET passwd=? WHERE user=?" user new-passwd)
]

The first form would choke on names like @racket["Patrick O'Connor"].
Worse, it would be susceptible to attack by malicious input like
@racket["me' OR user='root'"], which yields the following SQL
statement:

@(element 'tt "UPDATE users SET passwd='whatever' WHERE user='me' OR user='root'")

In contrast, using a @tech{parameterized query} causes the
parameterized SQL and its arguments to be submitted to the back end
separately; the back end then combines them safely.

Only SQL literal values can be replaced with parameter placeholders; a
SQL statement cannot be parameterized over a column name or a sort
order, for example. In such cases, constructing the query dynamically
may be the only feasible solution. But while the query construction
may be influenced by external input, it should never directly
incorporate external input without validation. That is, don't do the
following:

@racketblock[
(code:comment "WRONG! DANGER!")
(query-rows c
  (format "SELECT name, ~a FROM contestants" column))
(query-list c
  (format "SELECT name FROM contestants ORDER BY score ~a" direction))
]

Instead, select the inserted SQL from known good alternatives:

@racketblock[
(code:comment "BETTER")
(query-rows c
  (format "SELECT name, ~a FROM contestants"
          (cond [(member column '("wins" "losses")) column]
                [else (error ....)])))
(query-list c
  (format "SELECT name FROM contestants ORDER BY score ~a" 
          (if ascending? "ASC" "DESC")))
]

@;{ Discuss dynamic IN comparisons? }


@subsection[#:tag "dbsec-xss"]{Cross-site Scripting (XSS)}

@wplink["Cross-site_scripting"]{Cross-site scripting}---which should
probably be called ``HTML injection'' or ``markup injection''---is
when arbitrary text from an untrusted source is embedded without
escaping into an HTML page. The @emph{unstructured text from the
untrusted source} is reinterpreted as @emph{markup from the web
server}; if the reinterpreted markup contains embedded Javascript
code, it executes with the security privileges associated with the web
server's domain.

This issue has little to do with databases @emph{per se} except that
such text is often stored in a database. This issue is mitigated by
using structured markup representations like SXML or X-expressions
(xexprs), since they automatically escape ``markup'' characters found
in embedded text.


@;{============================================================}

@section[#:tag "dbperf"]{Database Performance}

Achieving good database performance mostly consists of good database
design and intelligent client behavior.

On the database design side, most important are wise use of indexes
and choosing appropriate data representations. As an example of the
latter, a regexp-based search using @tt{LIKE} will probably be slower
than a specialized
@hyperlink["http://www.postgresql.org/docs/9.0/static/textsearch.html"]{full-text
search} feature for large data sets. Consult your database back end's
manual for additional performance advice.

The following sections describe a few client-side aspects of
performance.

@subsection[#:tag "dbperf-n+1"]{The N+1 Selects Problem}

@;{ per comments on http://stackoverflow.com/questions/97197/what-is-the-n1-selects-problem
    Is N+1 actually a problem?
    ie, Is communication overhead with db back end worse than 
    grouping cost?  Should measure to see. }

A common mistake is to fetch a large amount of data by running a query
to get a set of initial records and then running another query inside
a loop with an iteration for each of the initial records. This is
sometimes called the ``n+1 selects problem.'' For example:

@racketblock[
(for/list ([(name id) (in-query c "SELECT name, id FROM contestants")])
  (define wins
    (query-list c "SELECT contest FROM contests WHERE winner = $1" id))
  (make-contestant-record name wins))
]

The same information can be retrieved in a single query by performing
a @tt{LEFT OUTER JOIN} and grouping the results:

@racketblock[
(for/list ([(name id wins)
            (in-query c
             (string-append "SELECT name, id, contest "
                            "FROM contestants LEFT OUTER JOIN contests "
                            "ON contestants.id = contests.winner")
             #:group '(#("name" "id"))
             #:group-mode '(list))])
  (make-contestant-record name wins))
]

The one-query form will perform better when database communication has
high latency. On the other hand, it may duplicate the contents of the
non-key @tt{name} column, using more bandwidth. Another approach is to
perform two queries:

@racketblock[
(let ([id=>name
       (rows->dict #:key "id" #:value "name"
        (query c "SELECT id, name FROM contestants"))])
  (for/list ([(id wins)
              (in-query c
               (string-append "SELECT id, contest "
                              "FROM contestants LEFT OUTER JOIN contests "
                              "ON contestants.id = contests.winner")
               #:group '(#("id"))
               #:group-mode '(list))])
    (make-contestant-record (dict-ref id=>name id) wins)))
]

Compared with the one-query form, the two-query form requires
additional communication, but it avoids duplicating @tt{name} values
in the @tt{OUTER JOIN} results. If additional non-key @tt{contestant}
fields were to be retrieved, the bandwidth savings of this approach
would be even greater.

See also @secref["dbperf-testing"].


@subsection[#:tag "dbperf-update-tx"]{Updates and Transactions}

Using transactions can dramatically improve the performance of bulk
database operations, especially @tt{UPDATE} and @tt{INSERT}
statements. As an extreme example, on commodity hardware in 2012,
SQLite is capable of executing thousands of @tt{INSERT} statements per
second within a transaction, but it is capable of only dozens of
single-@tt{INSERT} transactions per second.


@subsection[#:tag "dbperf-pstcache"]{Statement Caching}

Connections cache implicitly prepared statements (that is, statements
given in string form directly to a query function). The effect of the
cache is to eliminate an extra round-trip to the server (to send the
statement and receive a prepared statement handle), leaving just a
single round-trip (to send parameters and receive results) per
execution.

Currently, prepared statements are only cached within a
transaction. The statement cache is flushed when entering or leaving a
transaction and whenever a DDL statement is executed.

@;{ virtual statements are mostly obsolete }


@subsection[#:tag "dbperf-testing"]{Testing Performance of Database-Backed Programs}

When testing the performance of database-backed programs, remember to
test them in environments with realistic latency and
bandwidth. High-latency environments may be roughly approximated with
the @racket[high-latency-connection] function, but there's no
substitute for the real thing.

@subsection[#:tag "dbperf-concurrency"]{Transactions and Concurrency}

Database systems use
@hyperlink["http://en.wikipedia.org/wiki/Database_transaction"]{transactions}
to guarantee properties such as
@hyperlink["http://en.wikipedia.org/wiki/ACID"]{atomicity and
isolation} while accommodating concurrent reads and writes by the
database's clients. Within a transaction a client is insulated from
the actions of other clients, but the transaction may be aborted and
rolled back if the database system cannot reconcile it with other
concurrent interactions. Some database systems are more adept at
reconciling transactions than others, and most allow reconciliation to
be tuned through the specification of @emph{isolation levels}.

PostgreSQL supports
@hyperlink["http://www.postgresql.org/docs/9.2/static/mvcc.html"]{very
fine-grained reconciliation}: two transactions that both read and
modify the same table concurrently might both be allowed to complete
if they involve disjoint sets of rows. However, clients should be
prepared to retry transactions that fail with a @racket[exn:fail:sql]
exception with SQLSTATE matching @racket[#rx"^40...$"]---typically
@racket["40001"], ``could not serialize access due to concurrent
update.''

MySQL's transaction behavior varies based on the storage drivers in
use. Clients should be prepared to retry transactions that fail with a
@racket[exn:fail:sql] exception with SQLSTATE matching
@racket[#rx"^40...$"].@;{---typically @racket["40001"], ``Deadlock found
when trying to get lock; try restarting transaction.''}

SQLite enforces a
@hyperlink["http://www.sqlite.org/lockingv3.html"]{very coarse-grained
policy}: only one transaction is allowed to write to the database at a
time, and thus concurrent writers are very likely to conflict. Clients
should be prepared to retry transactions that fail with a
@racket[exn:fail:sql] exception with SQLSTATE of @racket['busy].

An alternative to retrying whole SQLite transactions is to start each
transaction with the appropriate locking level, since a transaction
usually fails when it is unable to upgrade its lock level. Start a
transaction that only performs reads in the default mode, and start a
transaction that may perform writes in @racket['immediate] mode (see
@racket[start-transaction]). That converts the problem of retrying
whole transactions into the problem of retrying the initial @tt{BEGIN
TRANSACTION} statment, and this library already automatically retries
individual statements that fail with @racket['busy] errors. Depending
on the length and frequency of the transactions, you may need to
adjust @racket[_busy-retry-limit] (see @racket[sqlite3-connect]).

ODBC's behavior varies depending on the driver and back end. See the
appropriate database system's documentation.


@;{============================================================}

@section[#:tag "intro-servlets"]{Databases and Web Servlets}

Using database connections in a web servlet is more complicated than
in a standalone program. A single servlet potentially serves many
requests at once, each in a separate request-handling
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
@wplink["Isolation_%28database_systems%29"]{isolated}. For example, if
one thread is accessing the connection within a transaction and
another thread issues a query, the second thread may see invalid data
or even disrupt the work of the first thread.

A secondary problem is performance. A connection can only perform a
single query at a time, whereas most database systems are capable of
concurrent query processing.

The proper way to use database connections in a servlet is to create a
connection for each request and disconnect it when the request has
been handled. But since a request thread may start and stop executing
in many places (due to @racket[send/suspend], etc), inserting the code
to connect and disconnect at the proper places can be challenging and
messy.

A better solution is to use a @tech{virtual connection}, which
automatically creates a request-specific (that is, thread-specific)
``actual connection'' by need and disconnects it when the request has
been handled (that is, when the thread terminates). Different
request-handling threads using the same virtual connection are
assigned different actual connections, so the requests are properly
isolated.

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
can achieve simplicity, isolation, and performance all at the same
time.


@(close-eval the-eval)
