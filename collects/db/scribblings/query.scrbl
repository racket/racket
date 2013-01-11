#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          racket/sandbox
          racket/runtime-path
          "config.rkt"
          "tabbing.rkt"
          (for-label db db/util/geometry db/util/postgresql racket/dict))

@(define-runtime-path log-file "log-for-query.rktd")
@(define the-eval (make-pg-eval log-file #t))

@;{ c - misc connection (alias to pgc)
    myc - MySQL connection (???)
    slc - SQLite connection (???)
}
@(the-eval '(define c pgc))

@title[#:tag "query-api"]{Queries}

@declare-exporting[db db/base #:use-sources (db/base)]

This library provides a high-level functional query API,
unlike many other database libraries, which present a stateful,
iteration-based interface to queries. When a query function is
invoked, it either returns a result or, if the query caused an error,
raises an exception. Different query functions impose different
constraints on the query results and offer different mechanisms for
processing the results.

@parheading{Errors} In most cases, a query error does not cause the
connection to be disconnected. Specifically, the following kinds of
errors should never cause a connection to be disconnected:
@itemize[
@item{SQL syntax errors, such as references to undefined tables,
  columns, or operations, etc}
@item{SQL runtime errors, such as integrity constraint violations}
@item{violations of a specialized query function's expectations, such
  as using @racket[query-value] with a query that returns multiple
  columns}
@item{supplying the wrong number or wrong types of parameters to a
  prepared query, executing a prepared query with the wrong
  connection, etc}
]
The following kinds of errors may cause a connection to be
disconnected:
@itemize[
@item{changing communication settings, such as changing the
  connection's character encoding}
@item{communication failures and internal errors in the library}
]
See @secref["transactions"] for information on how errors can affect
the transaction status.

@parheading{Character encoding} This library is designed to interact with
database systems using the UTF-8 character encoding. The connection
functions attempt to negotiate UTF-8 communication at the beginning of
every connection, but some systems also allow the character encoding
to be changed via SQL commands (eg, @tt{SET NAMES}). If this happens,
the client might be unable to reliably communicate with the database,
and data might get corrupted in transmission. Avoid changing a
connection's character encoding. When possible, the connection will
observe the change and automatically disconnect with an error.

@parheading{Synchronization} Connections are internally synchronized:
it is safe to use a connection from different threads
concurrently. Most connections are not kill-safe: killing a thread
that is using a connection may leave the connection locked, causing
future operations to block indefinitely. See also
@secref["kill-safe"].


@section{Statements}

All query functions require both a connection and a
@deftech{statement}, which is one of the following:
@itemlist[
@item{a string containing a single SQL statement}
@item{a @tech{prepared statement} produced by @racket[prepare]}
@item{a @tech{virtual statement} produced by
  @racket[virtual-statement]}
@item{a statement-binding value produced by
  @racket[bind-prepared-statement]}
@item{an instance of a struct type that implements @racket[prop:statement]}
]

A SQL statement may contain parameter placeholders that stand for SQL
scalar values; such statements are called @deftech{parameterized
queries}. The parameter values must be supplied when the statement is
executed; the parameterized statement and parameter values are sent to
the database back end, which combines them correctly and safely.

Use parameters instead of Racket string interpolation (eg,
@racket[format] or @racket[string-append]) to avoid
@secref["dbsec-sql-injection"].

The syntax of placeholders varies depending on the database
system. For example:

@centered{
@tabbing{
PostgreSQL:   @&  @tt{select * from the_numbers where n > $1;} @//
MySQL, ODBC:  @&  @tt{select * from the_numbers where n > ?;} @//
SQLite:       @&  supports both syntaxes (plus others)
}
}

@defproc[(statement? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @tech{statement}, @racket[#f]
  otherwise.
}


@section{Simple Queries}

The simple query API consists of a set of functions specialized to
various types of queries. For example, @racket[query-value] is
specialized to queries that return exactly one row of exactly one
column.

If a statement takes parameters, the parameter values are given as
additional arguments immediately after the SQL statement. Only a
statement given as a string, @tech{prepared statement}, or
@tech{virtual statement} can be given ``inline'' parameters; if the
statement is a statement-binding, no inline parameters are permitted.

The types of parameters and returned fields are described in
@secref["sql-types"].

@defproc[(query-exec [connection connection?]
                     [stmt statement?]
                     [arg any/c] ...)
         void?]{

  Executes a SQL statement for effect.

@examples[#:eval the-eval
(query-exec pgc "insert into the_numbers values (42, 'the answer')")
(query-exec pgc "delete from the_numbers where n = $1" 42)
]
}

@defproc[(query-rows [connection connection?]
                     [stmt statement?]
                     [arg any/c] ...
                     [#:group groupings
                      (let* ([field/c (or/c string? exact-nonnegative-integer?)]
                             [grouping/c (or/c field/c (vectorof field/c))])
                        (or/c grouping/c (listof grouping/c)))
                      null]
                     [#:group-mode group-mode
                      (listof (or/c 'preserve-null 'list))
                      null])
         (listof vector?)]{

  Executes a SQL query, which must produce rows, and returns the list
  of rows (as vectors) from the query.

@examples[#:eval the-eval
(query-rows pgc "select * from the_numbers where n = $1" 2)
(query-rows c "select 17")
]

  If @racket[groupings] is not empty, the result is the same as if
  @racket[group-rows] had been called on the result rows.
}

@defproc[(query-list [connection connection?]
                     [stmt statement?]
                     [arg any/c] ...)
         list?]{

  Executes a SQL query, which must produce rows of exactly one
  column, and returns the list of values from the query.

@examples[#:eval the-eval
(query-list c "select n from the_numbers where n < 2")
(query-list c "select 'hello'")
]
}

@defproc[(query-row [connection connection?]
                    [stmt statement?]
                    [arg any/c] ...)
         vector?]{

  Executes a SQL query, which must produce exactly one row, and
  returns its (single) row result as a vector.

@examples[#:eval the-eval
(query-row pgc "select * from the_numbers where n = $1" 2)
(query-row pgc "select min(n), max(n) from the_numbers")
]
}

@defproc[(query-maybe-row [connection connection?]
                          [stmt statement?]
                          [arg any/c] ...)
         (or/c vector? #f)]{

  Like @racket[query-row], but the query may produce zero rows; in
  that case, @racket[#f] is returned.

@examples[#:eval the-eval
(query-maybe-row pgc "select * from the_numbers where n = $1" 100)
(query-maybe-row c "select 17")
]
}

@defproc[(query-value [connection connection?]
                      [stmt statement?]
                      [arg any/c] ...)
         any/c]{

  Executes a SQL query, which must produce exactly one row of exactly
  one column, and returns its single value result.

@examples[#:eval the-eval
(query-value pgc "select timestamp 'epoch'")
(query-value pgc "select d from the_numbers where n = $1" 3)
]
}

@defproc[(query-maybe-value [connection connection?]
                            [stmt statement?]
                            [arg any/c] ...)
         (or/c any/c #f)]{

  Like @racket[query-value], but the query may produce zero rows; in
  that case, @racket[#f] is returned.

@examples[#:eval the-eval
(query-maybe-value pgc "select d from the_numbers where n = $1" 100)
(query-maybe-value c "select count(*) from the_numbers")
]
}

@defproc[(in-query [connection connection?]
                   [stmt statement?]
                   [arg any/c] ...
                   [#:fetch fetch-size (or/c exact-positive-integer? +inf.0) +inf.0]
                   [#:group groupings
                    (let* ([field/c (or/c string? exact-nonnegative-integer?)]
                           [grouping/c (or/c field/c (vectorof field/c))])
                      (or/c grouping/c (listof grouping/c)))
                    null]
                   [#:group-mode group-mode
                    (listof (or/c 'preserve-null 'list))
                    null])
         sequence?]{

  Executes a SQL query, which must produce rows, and returns a
  sequence. Each step in the sequence produces as many values as the
  rows have columns. 

  If @racket[fetch-size] is @racket[+inf.0], all rows are fetched when
  the sequence is created. If @racket[fetch-size] is finite, a
  @deftech{cursor} is created and @racket[fetch-size] rows are fetched
  at a time, allowing processing to be interleaved with retrieval. On
  some database systems, ending a transaction implicitly closes all
  open cursors; attempting to fetch more rows may fail. On PostgreSQL,
  a cursor can be opened only within a transaction.

  If @racket[groupings] is not empty, the result is the same as
  if @racket[group-rows] had been called on the result rows. If
  @racket[groupings] is not empty, then @racket[fetch-size] must
  be @racket[+inf.0]; otherwise, an exception is raised.

@examples[#:eval the-eval
(for/list ([n (in-query pgc "select n from the_numbers where n < 2")])
  n)
(call-with-transaction pgc
  (lambda ()
    (for ([(n d)
           (in-query pgc "select * from the_numbers where n < $1" 4
                     #:fetch 1)])
      (printf "~a: ~a\n" n d))))
]

An @racket[in-query] application can provide better performance when
it appears directly in a @racket[for] clause. In addition, it may
perform stricter checks on the number of columns returned by the query
based on the number of variables in the clause's left-hand side:

@examples[#:eval the-eval
(for ([n (in-query pgc "select * from the_numbers")])
  (displayln n))
]
}


@section{General Query Support}

A general query result is either a @racket[simple-result] or a
@racket[rows-result].

@defstruct*[simple-result
            ([info (listof (cons/c symbol? any/c))])]{

Represents the result of a SQL statement that does not return a
relation, such as an @tt{INSERT} or @tt{DELETE} statement.  

The @racket[info] field is an association list, but its contents vary
based on database system and may change in future versions of this
library (even new minor versions). The following keys are supported for
multiple database systems:

@itemlist[

@item{@racket['insert-id]: If the value is a positive integer, the
statement was an @tt{INSERT} statement and the value is a
system-specific identifier for the inserted row. For PostgreSQL, the
value is the row's OID, if the table has OIDs (for an alternative, see
the @tt{INSERT ... RETURNING} statement). For MySQL, the value is the
same as the result of
@hyperlink["http://dev.mysql.com/doc/refman/5.0/en/information-functions.html#function_last-insert-id"]{last_insert_id}
function---that is, the value of the row's @tt{AUTO_INCREMENT}
field. If there is no such field, the value is @racket[#f]. For
SQLite, the value is the same as the result of the
@hyperlink["http://www.sqlite.org/lang_corefunc.html#last_insert_rowid"]{last_insert_rowid}
function---that is, the
@hyperlink["http://www.sqlite.org/lang_createtable.html#rowid"]{ROWID}
of the inserted row.}

@item{@racket['affected-rows]: The number (a nonnegative integer) of
rows inserted by an @tt{INSERT} statement, modified by an @tt{UPDATE}
statement, or deleted by a @tt{DELETE} statement. Only directly
affected rows are counted; rows affected because of triggers or
integrity constraint actions are not counted.}
]
}

@defstruct*[rows-result
            ([headers (listof any/c)]
             [rows (listof vector?)])]{

Represents the result of SQL statement that results in a relation,
such as a @tt{SELECT} query.

The @racket[headers] field is a list whose length is the number of
columns in the result rows. Each header is usually an association list
containing information about the column, but do not rely on its
contents; it varies based on the database system and may change in
future version of this library (even new minor versions).
}

@defproc[(query [connection connection?]
                [stmt statement?]
                [arg any/c] ...)
         (or/c simple-result? rows-result?)]{

  Executes a query, returning a structure that describes the
  results. Unlike the more specialized query functions, @racket[query]
  supports both rows-returning and effect-only queries.
}

@defproc[(group-rows [result rows-result?]
                     [#:group groupings
                      (let* ([field/c (or/c string? exact-nonnegative-integer?)]
                             [grouping/c (or/c field/c (vectorof field/c))])
                        (or/c grouping/c (listof grouping/c)))]
                     [#:group-mode group-mode
                      (listof (or/c 'preserve-null 'list))
                      null])
         rows-result?]{

If @racket[groupings] is a vector, the elements must be names of
fields in @racket[result], and @racket[result]'s rows are regrouped
using the given fields. Each grouped row contains N+1 fields; the
first N fields are the @racket[groupings], and the final field
is a list of ``residual rows'' over the rest of the fields. A residual
row of all NULLs is dropped (for convenient processing of @tt{OUTER
JOIN} results) unless @racket[group-mode] includes
@racket['preserve-null]. If @racket[group-mode] contains
@racket['list], there must be exactly one residual field, and its
values are included without a vector wrapper (similar to
@racket[query-list]).

See also @secref["dbperf-n+1"].

@examples[#:eval the-eval
(define vehicles-result
  (rows-result
   '(((name . "type")) ((name . "maker")) ((name . "model")))
   `(#("car"  "honda"   "civic")
     #("car"  "ford"    "focus")
     #("car"  "ford"    "pinto")
     #("bike" "giant"   "boulder")
     #("bike" "schwinn" ,sql-null))))
(group-rows vehicles-result
            #:group '(#("type")))
]

The grouped final column is given the name @racket["grouped"].

The @racket[groupings] argument may also be a list of vectors;
in that case, the grouping process is repeated for each set of
grouping fields. The grouping fields must be distinct.

@examples[#:eval the-eval
(group-rows vehicles-result
            #:group '(#("type") #("maker"))
            #:group-mode '(list))
]
}

@defproc[(rows->dict [result rows-result?]
                     [#:key key-field/s
                      (let ([field/c (or/c string? exact-nonnegative-integer?)])
                        (or/c field/c (vectorof field/c)))]
                     [#:value value-field/s
                      (let ([field/c (or/c string? exact-nonnegative-integer?)])
                        (or/c field/c (vectorof field/c)))]
                     [#:value-mode value-mode
                      (listof (or/c 'list 'preserve-null))
                      null])
         dict?]{
         
Creates a dictionary mapping @racket[key-field/s] to
@racket[value-field/s]. If @racket[key-field/s] is a single field name
or index, the keys are the field values; if @racket[key-field/s] is a
vector, the keys are vectors of the field values. Likewise for
@racket[value-field/s].

If @racket[value-mode] contains @racket['list], a list of values is
accumulated for each key; otherwise, there must be at most one value
for each key. Values consisting of all @racket[sql-null?] values are
dropped unless @racket[value-mode] contains
@racket['preserve-null].

@examples[#:eval the-eval
(rows->dict vehicles-result 
            #:key "model" #:value '#("type" "maker"))
(rows->dict vehicles-result
            #:key "maker" #:value "model" #:value-mode '(list))
]
}


@section{Prepared Statements}

A @deftech{prepared statement} is the result of a call to
@racket[prepare].

Any server-side or native-library resources associated with a prepared
statement are released when the prepared statement is
garbage-collected or when the connection that owns it is closed;
prepared statements do not need to be (and cannot be) explicitly
closed.

@defproc[(prepare [connection connection?]
                  [stmt (or/c string? virtual-statement?)])
         prepared-statement?]{

  Prepares a statement. The resulting @tech{prepared statement} is
  tied to the connection that prepared it; attempting to execute it
  with another connection will trigger an exception. The prepared
  statement holds its connection link weakly; a reference to a
  prepared statement will not keep a connection from being garbage
  collected.
}

@defproc[(prepared-statement? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @tech{prepared statement}
  created by @racket[prepare], @racket[#f] otherwise.
}

@defproc[(prepared-statement-parameter-types [pst prepared-statement?])
         (listof (list/c boolean? (or/c symbol? #f) any/c))]{

  Returns a list with one element for each of the prepared statement's
  parameters. Each element is itself a list of the following form:

  @racketblock[(list _supported? _type _typeid)]

  The @racket[_supported?] field indicates whether the type is
  supported by this library; the @racket[_type] field is a symbol
  corresponding to an entry in one of the tables in
  @secref["db-types"]; and the @racket[_typeid] field is a
  system-specific type identifier. The type description list format
  may be extended with additional information in future versions of
  this library.
}

@defproc[(prepared-statement-result-types [pst prepared-statement?])
         (listof (list/c boolean? (or/c symbol? #f) any/c))]{

  If @racket[pst] is a rows-returning statement (eg, @tt{SELECT}),
  returns a list of type descriptions as described above, identifying
  the SQL types (or pseudo-types) of the result columns. If
  @racket[pst] is not a rows-returning statement, the function returns
  the empty list.
}

@defproc[(bind-prepared-statement
            [pst prepared-statement?]
            [params (listof any/c)])
         statement-binding?]{

  Creates a statement-binding value pairing @racket[pst] with
  @racket[params], a list of parameter arguments. The result can be
  executed with @racket[query] or any of the other query functions,
  but it must be used with the same connection that created
  @racket[pst].

  @examples[#:eval the-eval
    (let* ([get-name-pst
            (prepare pgc "select d from the_numbers where n = $1")]
           [get-name2
            (bind-prepared-statement get-name-pst (list 2))]
           [get-name3
            (bind-prepared-statement get-name-pst (list 3))])
      (list (query-value pgc get-name2)
            (query-value pgc get-name3)))
  ]

  Most query functions perform the binding step implicitly.
}

@defproc[(statement-binding? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a statement created by
  @racket[bind-prepared-statement], @racket[#f] otherwise.
}

@defproc[(virtual-statement [gen (or/c string? (-> dbsystem? string?))])
         virtual-statement?]{

  Creates a @deftech{virtual statement}, @racket[_stmt], which
  encapsulates a weak mapping of connections to prepared
  statements. When a query function is called with @racket[_stmt] and
  a connection, the weak hash is consulted to see if the statement has
  already been prepared for that connection. If so, the prepared
  statement is used; otherwise, the statement is prepared and stored
  in the table.

  The @racket[gen] argument must be either a SQL string or a function
  that accepts a databse system object and produces a SQL string. The
  function variant allows the SQL syntax to be dynamically customized
  for the database system in use.

@examples[#:eval the-eval
(define pst
  (virtual-statement
   (lambda (dbsys)
     (case (dbsystem-name dbsys)
       ((postgresql) "select n from the_numbers where n < $1")
       ((sqlite3) "select n from the_numbers where n < ?")
       (else (error "unknown system"))))))
(query-list pgc pst 3)
(eval:alts (query-list slc pst 3)
           (query-list pgc pst 3))
]
}

@defproc[(virtual-statement? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @tech{virtual statement}
  created by @racket[virtual-statement], @racket[#f] otherwise.
}


@section[#:tag "transactions"]{Transactions}

The functions described in this section provide a consistent interface
to transactions.

A @deftech{managed transaction} is one created via either
@racket[start-transaction] or @racket[call-with-transaction]. In
contrast, an @deftech{unmanaged transaction} is one created by
evaluating a SQL statement such as @tt{START TRANSACTION}. A
@deftech{nested transaction} is a transaction created within the
extent of an existing transaction. If a nested transaction is
committed, its changes are promoted to the enclosing transaction,
which may itself be committed or rolled back. If a nested transaction
is rolled back, its changes are discarded, but the enclosing
transaction remains open. Nested transactions are implemented via SQL
@tt{SAVEPOINT}, @tt{RELEASE SAVEPOINT}, and @tt{ROLLBACK TO
SAVEPOINT}.

ODBC connections must use @tech{managed transactions} exclusively;
using transaction-changing SQL may cause these functions to behave
incorrectly and may cause additional problems in the ODBC driver. ODBC
connections do not support @tech{nested transactions}.

PostgreSQL, MySQL, and SQLite connections must not mix @tech[#:key
"managed transaction"]{managed} and @tech[#:key "unmanaged
transaction"]{unmanaged} transactions. For example, calling
@racket[start-transaction] and then executing a @tt{ROLLBACK}
statement is not allowed. Note that in MySQL, some SQL statements have
@hyperlink["http://dev.mysql.com/doc/refman/5.0/en/implicit-commit.html"]{implicit
transaction effects}. For example, in MySQL a @tt{CREATE TABLE}
statement implicitly commits the current transaction. These statements
also must not be used within @tech{managed transactions}. (In
contrast, PostgreSQL and SQLite both support transactional DDL.)

@parheading{Errors} Query errors may affect an open transaction in one of
three ways:
@itemlist[#:style 'ordered
@item{the transaction remains open and unchanged}
@item{the transaction is automatically rolled back}
@item{the transaction becomes an @deftech{invalid transaction}; all
subsequent queries will fail until the transaction is rolled back}
]
To avoid the silent loss of information, this library attempts to
avoid behavior (2) completely by marking transactions as invalid
instead (3). Invalid transactions can be identified using the
@racket[needs-rollback?] function. The following list is a rough guide
to what errors cause which behaviors:
@itemlist[
@item{All errors raised by checks performed by this library, such as
  parameter arity and type errors, leave the transaction open and
  unchanged (1).}
@item{All errors originating from PostgreSQL cause the transaction to
  become @tech[#:key "invalid transaction"]{invalid} (3).}
@item{Most errors originating from MySQL leave the transaction open
  and unchanged (1), but a few cause the transaction to become
  @tech[#:key "invalid transaction"]{invalid} (3). In the latter
  cases, the underlying behavior of MySQL is to roll back the
  transaction but @emph{leave it open} (see
  @hyperlink["http://dev.mysql.com/doc/refman/5.1/en/innodb-error-handling.html"]{the
  MySQL documentation}). This library detects those cases and marks
  the transaction @tech[#:key "invalid transaction"]{invalid}
  instead.}
@item{Most errors originating from SQLite leave the transaction open
  and unchanged (1), but a few cause the transaction to become
  @tech[#:key "invalid transaction"]{invalid} (3). In the latter
  cases, the underlying behavior of SQLite is to roll back the
  transaction (see
  @hyperlink["http://www.sqlite.org/lang_transaction.html"]{the SQLite
  documentation}). This library detects those cases and marks the
  transaction @tech[#:key "invalid transaction"]{invalid} instead.}
@item{All errors originating from an ODBC driver cause the transaction
  to become @tech[#:key "invalid transaction"]{invalid} (3). The
  underlying behavior of ODBC drivers varies widely, and ODBC provides
  no mechanism to detect when an existing transaction has been rolled
  back, so this library intercepts all errors and marks the
  transaction @tech[#:key "invalid transaction"]{invalid} instead.}
]
If a nested transaction marked @tech[#:key "invalid
transaction"]{invalid} is rolled back, the enclosing transaction is
typically still valid.

If a transaction is open when a connection is disconnected, it is
implicitly rolled back.

@defproc[(start-transaction [c connection?]
                            [#:isolation isolation-level
                             (or/c 'serializable
                                   'repeatable-read
                                   'read-committed
                                   'read-uncommitted
                                   #f)
                             #f]
                            [#:option option any/c #f])
         void?]{

  Starts a transaction with isolation @racket[isolation-level]. If
  @racket[isolation-level] is @racket[#f], the isolation is
  database-dependent; it may be a default isolation level or it may be
  the isolation level of the previous transaction.

  The behavior of @racket[option] depends on the database system:
  @itemlist[
  @item{PostgreSQL supports @racket['read-only] and @racket['read-write]
    for the @hyperlink["http://www.postgresql.org/docs/9.0/static/sql-set-transaction.html"]{corresponding
    transaction options}.}
  @item{SQLite supports @racket['deferred], @racket['immediate], and
    @racket['exclusive] for the @hyperlink["http://www.sqlite.org/lang_transaction.html"]{corresponding
    locking modes}.} 
  @item{MySQL and ODBC no not support any options.}
  ]
  If @racket[option] is not supported, an exception is raised.

  If @racket[c] is already in a transaction, @racket[isolation-level]
  and @racket[option] must both be @racket[#f], and a @tech{nested
  transaction} is opened.

  See also @secref["dbperf-concurrency"].
}

@defproc[(commit-transaction [c connection?]) void?]{

  Attempts to commit the current transaction, if one is open. If the
  transaction cannot be commited (for example, if it is @tech[#:key
  "invalid transaction"]{invalid}), an exception is raised.

  If the current transaction is a @tech{nested transaction}, the
  nested transaction is closed, its changes are incorporated into the
  enclosing transaction, and the enclosing transaction is resumed.

  If no transaction is open, this function has no effect.
}

@defproc[(rollback-transaction [c connection?]) void?]{

  Rolls back the current transaction, if one is open.

  If the current transaction is a @tech{nested transaction}, the
  nested transaction is closed, its changes are abandoned, and the
  enclosing transaction is resumed.

  If no transaction is open, this function has no effect.
}

@defproc[(in-transaction? [c connection?])
         boolean?]{

  Returns @racket[#t] if @racket[c] has an open transaction
  (@tech[#:key "managed transaction"]{managed} or @tech[#:key
  "unmanaged transaction"]{unmanaged}), @racket[#f] otherwise.
}

@defproc[(needs-rollback? [c connection?]) boolean?]{

  Returns @racket[#t] if @racket[c] is in an @tech{invalid
  transaction}. All queries executed using @racket[c] will fail until
  the transaction is rolled back (either using
  @racket[rollback-transaction], if the transaction was created with
  @racket[start-transaction], or when the procedure passed to
  @racket[call-with-transaction] returns).
}

@defproc[(call-with-transaction [c connection?]
                                [proc (-> any)]
                                [#:isolation isolation-level
                                 (or/c 'serializable
                                       'repeatable-read
                                       'read-committed
                                       'read-uncommitted
                                       #f)
                                 #f]
                                [#:option option any/c #f])
         any]{

  Calls @racket[proc] in the context of a new transaction with
  isolation level @racket[isolation-level]. If @racket[proc] completes
  normally, the transaction is committed and @racket[proc]'s results
  are returned. If @racket[proc] raises an exception (or if the
  implicit commit at the end raises an exception), the transaction is
  rolled back and the exception is re-raised.

  If @racket[call-with-transaction] is called within a transaction,
  @racket[isolation-level] must be @racket[#f], and it creates a
  @tech{nested transaction}. Within the extent of a call to
  @racket[call-with-transaction], transactions must be properly
  nested. In particular:
  @itemlist[
  @item{Calling either @racket[commit-transaction] or
  @racket[rollback-transaction] when the open transaction was
  created by @racket[call-with-transaction] causes an exception to be
  raised.}
  @item{If a further nested transaction is open when @racket[proc]
  completes (that is, created by an unmatched
  @racket[start-transaction] call), an exception is raised and the
  nested transaction created by @racket[call-with-transaction] is
  rolled back.}
  ]
}

@section{SQL Errors}

SQL errors are represented by the @racket[exn:fail:sql] exception
type.

@defstruct[(exn:fail:sql exn:fail)
           ([sqlstate (or/c string? symbol?)]
            [info (listof (cons/c symbol? any/c))])]{

  Represents a SQL error originating from the database server or
  native library. The @racket[sqlstate] field contains the SQLSTATE
  code (a five-character string) of the error for PostgreSQL, MySQL,
  or ODBC connections or a symbol for SQLite connections. Refer to the
  database system's documentation for the definitions of error codes:

  @itemlist[
  @item{@hyperlink["http://www.postgresql.org/docs/9.0/static/errcodes-appendix.html"]{
    PostgreSQL SQLSTATE codes}}
  @item{@hyperlink["http://dev.mysql.com/doc/refman/5.5/en/error-messages-server.html"]{
    MySQL SQLSTATE codes}}
  @item{@hyperlink["http://www.sqlite.org/c3ref/c_abort.html"]{
    SQLite error codes}; errors are represented as a symbol based on
  the error constant's name, such as @racket['busy] for @tt{SQLITE_BUSY}}
  @item{ODBC: see the database system's documentation}
  ]

  The @racket[info] field contains all information available about the
  error as an association list. The available keys vary, but the
  @racket['message] key is typically present; its value is a string
  containing the error message.

@examples[#:eval the-eval
(with-handlers ([exn:fail:sql? exn:fail:sql-info])
  (query pgc "select * from nosuchtable"))
]

  Errors originating from the @racketmodname[db] library, such as
  arity and contract errors, type conversion errors, etc, are not
  represented by @racket[exn:fail:sql]. 
}

@section{Database Catalog Information}

@defproc[(list-tables [c connection?]
                      [#:schema schema
                       (or/c 'search-or-current 'search 'current)
                       'search-or-current])
         (listof string?)]{

Returns a list of unqualified names of tables (and views) defined in
the current database.

If @racket[schema] is @racket['search], the list contains all tables
in the current schema search path (with the possible exception of
system tables); if the search path cannot be determined, an exception
is raised. If @racket[schema] is @racket['current], the list contains
all tables in the current schema. If @racket[schema] is
@racket['search-or-current] (the default), the search path is used if
it can be determined; otherwise the current schema is used.
The schema search path cannot be determined for ODBC-based
connections.
}

@defproc[(table-exists? [c connection?]
                        [table-name string?]
                        [#:schema schema
                         (or/c 'search-or-current 'search 'current)
                         'search-or-current]
                        [#:case-sensitive? case-sensitive? any/c #f])
         boolean?]{

Indicates whether a table (or view) named @racket[table-name]
exists. The meaning of the @racket[schema] argument is the same as for
@racket[list-tables], and the @racket[case-sensitive?] argument
controls how table names are compared.
}


@section{Creating New Kinds of Statements}

@defthing[prop:statement (struct-type-property/c
                          (-> any/c connection? statement?))]{

A struct type property for creating new kinds of statements. The
property value is applied to the struct instance and a connection, and
it must return a @tech{statement}.
}


@(close-eval the-eval)
