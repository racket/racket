#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          racket/sandbox
          racket/runtime-path
          "config.rkt"
          (for-label db
                     openssl))


@(define-runtime-path log-file "log-for-connect.rktd")
@(define the-eval (make-pg-eval log-file #t))

@title[#:tag "connect"]{Connections}

This section describes functions for creating connections as well as
administrative functions for managing connections.

@section[#:tag "creating-connections"]{Base Connections}

@declare-exporting[db]

There are four kinds of base connection, and they are divided into two
groups: @deftech{wire-based connections} and @deftech{FFI-based
connections}. PostgreSQL and MySQL connections are wire-based, and
SQLite and ODBC connections are FFI-based. See also
@secref["ffi-concurrency"].

Base connections are made using the following functions.

@defproc[(postgresql-connect [#:user user string?]
                  [#:database database string?]
                  [#:server server string? "localhost"]
                  [#:port port exact-positive-integer? 5432]
                  [#:socket socket (or/c path-string? 'guess #f) #f]
                  [#:password password (or/c string? #f) #f]
                  [#:allow-cleartext-password? allow-cleartext-password?
                   boolean? #f]
                  [#:ssl ssl (or/c 'yes 'optional 'no) 'no]
                  [#:ssl-context ssl-context ssl-client-context?
                   (ssl-make-client-context 'sslv3)]
                  [#:notice-handler notice-handler
                   (or/c 'output 'error output-port?
                         (-> string? string? any))
                   void]
                  [#:notification-handler notification-handler
                   (or/c 'output 'error output-port?
                         (-> string? any))
                   void])
         connection?]{

  Creates a connection to a PostgreSQL server. Only the
  @racket[database] and @racket[user] arguments are mandatory.

  By default, the connection is made via TCP to @racket["localhost"]
  at port @racket[5432]. To make a different TCP connection, provide
  one or both of the @racket[server] and @racket[port] arguments.

  To connect via a local socket, specify the socket path as the
  @racket[socket] argument. You must not supply the @racket[socket]
  argument if you have also supplied either of the TCP arguments. See
  also @secref{connecting-to-server} for notes on socket
  paths. Supplying a @racket[socket] argument of @racket['guess] is
  the same as supplying @racket[(postgresql-guess-socket-path)].
  Sockets are only available under Linux (x86) and Mac OS X.

  If the server requests password authentication, the
  @racket[password] argument must be present; otherwise an exception
  is raised. If the server does not request password authentication,
  the @racket[password] argument is ignored and may be omitted.  A
  connection normally only sends password hashes (using the @tt{md5}
  authentication method). If the server requests a password sent as
  cleartext (un-hashed), the connection is aborted unless
  @racket[allow-cleartext-password?] is true.

  If the @racket[ssl] argument is either @racket['yes] or
  @racket['optional], the connection attempts to negotiate an SSL
  connection. If the server refuses SSL, the connection raises an
  exception if @racket[ssl] was set to @racket['yes] or continues with
  an unencrypted connection if @racket[ssl] was set to
  @racket['optional]. By default, SSL provides encryption but does not
  verify the identity of the server (see
  @hyperlink["http://www.postgresql.org/docs/9.0/static/libpq-ssl.html"]{this
  explanation}). Host verification can be required via the
  @racket[ssl-context] argument; see @racket[ssl-set-verify!]. Some
  servers use SSL certificates to authenticate clients; see
  @racket[ssl-load-certificate-chain!] and
  @racket[ssl-load-private-key!]. SSL may only be used with TCP
  connections, not with local sockets.

  The @racket[notice-handler] is called on notice messages
  received asynchronously from the server. A common example is notice
  of an index created automatically for a table's primary key. The
  @racket[notice-handler] function takes two string arguments: the
  condition's SQLSTATE and a message. The
  @racket[notification-handler] is called in response to an event
  notification (see the @tt{LISTEN} and @tt{NOTIFY} statements); its
  argument is the name of the event as a string. An output port may be
  supplied instead of a procedure, in which case a message is printed
  to the given port. Finally, the symbol @racket['output] causes the
  message to be printed to the current output port, and
  @racket['error] causes the message to be printed to the current
  error port.

  If the connection cannot be made, an exception is raised.

  @fake-examples[
    [(postgresql-connect #:server "db.mysite.com"
                         #:port 5432
                         #:database "webappdb"
                         #:user "webapp"
                         #:password "ultra5ecret")
     (new connection%)]
    [(postgresql-connect #:user "me"
                         #:database "me"
                         #:password "icecream")
     (new connection%)]
    [(postgresql-connect @code:comment{Typical socket path}
                         #:socket "/var/run/postgresql/.s.PGSQL.5432"
                         #:user "me"
                         #:database "me")
     (new connection%)]
    [(postgresql-connect #:socket 'guess (code:comment "or (postgresql-guess-socket-path)")
                         #:user "me"
                         #:database "me")
     (new connection%)]]
}

@defproc[(postgresql-guess-socket-path)
         path-string?]{

  Attempts to guess the path for the socket based on conventional
  locations. This function returns the first such path that exists in
  the filesystem. It does not check that the path is a socket file,
  nor that the path is connected to a PostgreSQL server.

  If none of the attempted paths exist, an exception is raised.
}

@defproc[(mysql-connect [#:user user string?]
                  [#:database database (or/c string? #f) #f]
                  [#:server server string? "localhost"]
                  [#:port port exact-positive-integer? 3306]
                  [#:socket socket (or/c path-string? #f) #f]
                  [#:ssl ssl (or/c 'yes 'optional 'no) 'no]
                  [#:ssl-context ssl-context ssl-client-context?
                   (ssl-make-client-context 'tls)]
                  [#:password password (or/c string? #f) #f]
                  [#:notice-handler notice-handler
                   (or/c 'output 'error output-port?
                         (-> exact-nonnegative-integer? string? any))
                   void])
         connection?]{

  Creates a connection to a MySQL server. If @racket[database] is
  @racket[#f], the connection is established without setting the
  current database; it should be subsequently set with the @tt{USE}
  SQL command.

  The meaning of the other keyword arguments is similar to those of
  the @racket[postgresql-connect] function, except that the first
  argument to a @racket[notice-handler] function is a MySQL-specific
  integer code rather than a SQLSTATE string, and a @racket[socket]
  argument of @racket['guess] is the same as supplying
  @racket[(mysql-guess-socket-path)].

  If the connection cannot be made, an exception is raised.

  @fake-examples[
    [(mysql-connect #:server "db.mysite.com"
                    #:port 3306
                    #:database "webappdb"
                    #:user "webapp"
                    #:password "ultra5ecret")
     (new connection%)]
    [(mysql-connect #:user "me"
                    #:database "me"
                    #:password "icecream")
     (new connection%)]
    [(mysql-connect @code:comment{Typical socket path}
                    #:socket "/var/run/mysqld/mysqld.sock"
                    #:user "me"
                    #:database "me")
     (new connection%)]
    [(mysql-connect #:socket (mysql-guess-socket-path)
                    #:user "me"
                    #:database "me")
     (new connection%)]]
}

@defproc[(mysql-guess-socket-path)
         path-string?]{

  Attempts to guess the path for the socket based on conventional
  locations. This function returns the first such path that exists in
  the filesystem. It does not check that the path is a socket file,
  nor that the path is connected to a MySQL server.

  If none of the attempted paths exist, an exception is raised.
}

@defproc[(sqlite3-connect
                [#:database database (or/c path-string? 'memory 'temporary)]
                [#:mode mode (or/c 'read-only 'read/write 'create) 'read/write]
                [#:busy-retry-limit busy-retry-limit 
                 (or/c exact-nonnegative-integer? +inf.0) 10]
                [#:busy-retry-delay busy-retry-delay
                 (and/c rational? (not/c negative?)) 0.1]
                [#:use-place use-place boolean? #f])
         connection?]{

  Opens the SQLite database at the file named by @racket[database], if
  @racket[database] is a string or path. If @racket[database] is
  @racket['temporary], a private disk-based database is created. If
  @racket[database] is @racket['memory], a private memory-based
  database is created.

  If @racket[mode] is @racket['read-only], the database is opened in
  read-only mode. If @racket[mode] is @racket['read/write] (the
  default), the database is opened for reading and writing (if
  filesystem permissions permit). The @racket['create] mode is like
  @racket['read/write], except that if the given file does not exist,
  it is created as a new database.

  SQLite uses @hyperlink["http://www.sqlite.org/lockingv3.html"]{coarse-grained
  locking}, and many internal operations fail with the
  @tt{SQLITE_BUSY} condition when a lock cannot be acquired. When an
  internal operation fails because the database is busy, the
  connection sleeps for @racket[busy-retry-delay] seconds and retries
  the operation, up to @racket[busy-retry-limit] additional times. If
  @racket[busy-retry-limit] is @racket[0], the operation is only
  attempted once. If after @racket[busy-retry-limit] retries the
  operation still does not succeed, an exception is raised.

  If @racket[use-place] is true, the actual connection is created in
  a distinct @tech/reference{place} for database connections and a
  proxy is returned; see @secref["ffi-concurrency"].

  If the connection cannot be made, an exception is raised.

  @fake-examples[
    [(sqlite3-connect #:database "/path/to/my.db")
     (new connection%)]
    [(sqlite3-connect #:database "relpath/to/my.db"
                      #:mode 'create)
     (new connection%)]]
}


@defproc[(sqlite3-available?) boolean?]{

Reports whether the SQLite native library is found, in which case
@racket[sqlite3-connect] works, otherwise it raises an exception.}


@defproc[(odbc-connect [#:dsn dsn string?]
                       [#:user user (or/c string? #f) #f]
                       [#:password password (or/c string? #f) #f]
                       [#:notice-handler notice-handler
                        (or/c output-port? 'output 'error 
                              (-> string? string? any))
                        void]
                       [#:strict-parameter-types? strict-parameter-types? boolean? #f]
                       [#:character-mode character-mode
                        (or/c 'wchar 'utf-8 'latin-1)
                        'wchar]
                       [#:use-place use-place boolean? #f])
         connection?]{

  Creates a connection to the ODBC Data Source named @racket[dsn]. The
  @racket[user] and @racket[password] arguments are optional, since
  that information may be incorporated into the data source
  definition, or it might not be relevant to the data source's driver.
  The @racket[notice-handler] argument behaves the same as in
  @racket[postgresql-connect].

  If @racket[strict-parameter-types?] is true, then the connection
  attempts to determine and enforce specific types for query
  parameters. See @secref["odbc-types"] for more details.

  By default, connections use ODBC's @tt{SQL_C_WCHAR}-based character
  encoding (as UTF-16) to send and receive Unicode character
  data. Unfortunately, some drivers' support for this method is
  buggy. To use @tt{SQL_C_CHAR} instead, set @racket[character-mode]
  to @racket['utf-8] or @racket['latin-1], depending on which encoding
  the driver uses.

  See @secref["odbc-status"] for notes on specific ODBC drivers and
  recommendations for connection options.

  If @racket[use-place] is true, the actual connection is created in
  a distinct @tech/reference{place} for database connections and a
  proxy is returned; see @secref["ffi-concurrency"].

  If the connection cannot be made, an exception is raised.
}

@defproc[(odbc-driver-connect [connection-string string?]
                              [#:notice-handler notice-handler
                               (or/c output-port? 'output 'error
                                     (-> string? string? any))
                               void]
                              [#:strict-parameter-types? strict-parameter-types? boolean? #f]
                              [#:character-mode character-mode
                               (or/c 'wchar 'utf-8 'latin-1)
                               'wchar]
                              [#:use-place use-place boolean? #f])
         connection?]{

  Creates a connection using an ODBC connection string containing a
  sequence of keyword and value connection parameters. The syntax of
  connection strings is described in
  @hyperlink["http://msdn.microsoft.com/en-us/library/ms715433%28v=VS.85%29.aspx"]{SQLDriverConnect}
  (see Comments section); supported attributes depend on the
  driver. The other arguments are the same as in @racket[odbc-connect].

  If the connection cannot be made, an exception is raised.
}

@defproc[(odbc-data-sources)
         (listof (list/c string? string?))]{

  Returns a list of known ODBC Data Sources. Each data souce is
  represented by a list of two strings; the first string is the name
  of the data source, and the second is the name of its associated
  driver.
}

@defproc[(odbc-drivers)
         (listof (cons/c string? any/c))]{

  Returns a list of known ODBC Drivers. Each driver is represented by
  a list, the first element of which is the name of the driver. The
  contents of the rest of each entry is currently undefined.
}


@;{============================================================}

@section{Connection Pooling}

@declare-exporting[db db/base #:use-sources (db/base)]

Creating a database connection can be a costly operation; it may
involve steps such as process creation and SSL negotiation. A
@deftech{connection pool} helps reduce connection costs by reusing
connections.

@defproc[(connection-pool
             [connect (-> connection?)]
             [#:max-connections max-connections (or/c (integer-in 1 10000) +inf.0) +inf.0]
             [#:max-idle-connections max-idle-connections (or/c (integer-in 1 10000) +inf.0) 10])
         connection-pool?]{

Creates a @tech{connection pool}. The pool consists of up to
@racket[max-connections], divided between leased connections and up to
@racket[max-idle-connections] idle connections. The pool uses
@racket[connect] to create new connections when needed. The
@racket[connect] function is called with the same
@racket[current-custodian] value as when the connection pool was
created, and it must return a fresh connection each time it is called.

@examples[#:eval the-eval
(eval:alts
 (define pool
   (connection-pool
    (lambda () (displayln "connecting!") (sqlite3-connect ....))
    #:max-idle-connections 1))
 (define pool
   (connection-pool
    (lambda () (displayln "connecting!") (sqlite3-connect #:database 'memory)))))
(define c1 (connection-pool-lease pool))
(define c2 (connection-pool-lease pool))
(disconnect c1)
(code:line (define c3 (connection-pool-lease pool)) (code:comment "reuses actual conn. from c1"))
]

See also @racket[virtual-connection] for a mechanism that eliminates
the need to explicitly call @racket[connection-pool-lease] and
@racket[disconnect].
}

@defproc[(connection-pool? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is a connection pool, @racket[#f]
otherwise.
}

@defproc[(connection-pool-lease
             [pool connection-pool?]
             [release (or/c evt? custodian?) (current-thread)])
         connection?]{

Obtains a connection from the connection pool, using an existing idle
connection in @racket[pool] if one is available. If no idle connection
is available and the pool contains fewer than its maximum allowed
connections, a new connection is created; otherwise an exception is
raised.

Calling @racket[disconnect] on the connection obtained causes the
connection to be released back to the connection pool. The connection
is also released if @racket[release] becomes available, if it is a
synchronizable event, or if @racket[release] is shutdown, if it is a
custodian. The default for @racket[release] is the current thread, so
the resulting connection is released when the thread that requested it
terminates.

When a connection is released, it is kept as an idle connection if
@racket[pool]'s idle connection limit would not be exceeded;
otherwise, it is disconnected. In either case, if the connection is in
a transaction, the transaction is rolled back.
}


@;{========================================}

@section{Virtual Connections}

@declare-exporting[db db/base #:use-sources (db/base)]

A @deftech{virtual connection} creates actual connections on demand and
automatically releases them when they are no longer needed.

@defproc[(virtual-connection
             [connect (or/c (-> connection?) connection-pool?)]
             #| [#:timeout timeout (and/c real? positive?) +inf.0] |#)
         connection?]{

Creates a @tech{virtual connection} that creates actual connections on
demand using the @racket[connect] function, or by calling
@racket[(connection-pool-lease connect)] if @racket[connect] is a
@tech{connection pool}. If @racket[connect] is a function, it is
called with the same @racket[current-custodian] value as when the
virtual connection was created.

A virtual connection encapsulates a mapping
of threads to actual connections. When a query function is called with
a virtual connection, the current thread's associated actual
connection is used to execute the query. If there is no actual
connection associated with the current thread, one is obtained by
calling @racket[connect]. An actual connection is disconnected when
its associated thread dies.

Virtual connections are especially useful in contexts such as web
servlets (see @secref["intro-servlets"]), where each request is
handled in a fresh thread. A single global virtual connection can be
defined, freeing each servlet request from explicitly opening and
closing its own connections. In particular, a @tech{virtual
connection} backed by a @tech{connection pool} combines convenience
with efficiency:

@racketblock[
(define the-connection
  (virtual-connection (connection-pool (lambda () ....))))
]

The resulting virtual connection leases a connection from the pool on
demand for each servlet request thread and releases it when the thread
terminates (that is, when the request has been handled).

When given a connection produced by @racket[virtual-connection],
@racket[connected?] indicates whether there is an actual connection
associated with the current thread. Likewise, @racket[disconnect]
causes the current actual connection associated with the thread (if
there is one) to be disconnected, but the connection will be recreated
if a query function is executed.

@examples[#:eval the-eval
(eval:alts
 (define c
   (virtual-connection
    (lambda ()
      (printf "connecting!\n")
      (postgresql-connect ....))))
 (define c
   (virtual-connection
    (lambda ()
      (printf "connecting!\n")
      (dsn-connect 'db-scribble-env)))))
(connected? c)
(query-value c "select 1")
(connected? c)
(void (thread (lambda () (displayln (query-value c "select 2")))))
(disconnect c)
(connected? c)
(query-value c "select 3")
]

Connections produced by @racket[virtual-connection] may not be used
with the @racket[prepare] function. However, they may still be used to
execute parameterized queries expressed as strings or encapsulated via
@racket[virtual-statement].

@examples[#:eval the-eval
(prepare c "select 2 + $1")
(query-value c "select 2 + $1" 2)
(define pst (virtual-statement "select 2 + $1"))
(query-value c pst 3)
]
}

@(the-eval '(begin (set! c #f) (set! pst #f)))

@;{========================================}

@section[#:tag "kill-safe"]{Kill-safe Connections}

@declare-exporting[db db/base #:use-sources (db/base)]

@defproc[(kill-safe-connection [c connection?]) 
         connection?]{

Creates a proxy for connection @racket[c]. All queries performed
through the proxy are kill-safe; that is, if a thread is killed during
a call to a query function such as @racket[query], the connection will
not become locked or damaged. (Connections are normally thread-safe but
not kill-safe.)

Note: A kill-safe connection whose underlying connection uses ports to
communicate with a database server is not protected from a custodian
shutting down its ports.
}

@;{========================================}

@section{Data Source Names}

@declare-exporting[db db/base #:use-sources (db/base)]

A DSN (data source name) is a symbol associated with a connection
specification in a DSN file. They are inspired by, but distinct from,
ODBC's DSNs.

@defstruct*[data-source
              ([connector (or/c 'postgresql 'mysql 'sqlite3 'odbc)]
               [args list?]
               [extensions (listof (list/c symbol? any/c))])
            #:mutable]{

  Represents a data source. The @racket[connector] field determines
  which connection function is used to create the connection. The
  @racket[args] field is a partial list of arguments passed to the
  connection function; additional arguments may be added when
  @racket[dsn-connect] is called. The @racket[extensions] field
  contains additional information about a connection; for example,
  this library's testing framework uses it to store SQL dialect
  flags.

  Data sources can also be created using the
  @racket[postgresql-data-source], etc auxiliary functions.
}

@defproc[(dsn-connect [dsn (or/c symbol? data-source?)]
                      [#:dsn-file dsn-file path-string? (current-dsn-file)]
                      [arg any/c] ...
                      [#:<kw> kw-arg any/c] ...)
         connection?]{

  Makes a connection using the connection information associated with
  @racket[dsn] in @racket[dsn-file]. The given @racket[arg]s and
  @racket[kw-arg]s are added to those specified by @racket[dsn] to
  form the complete arguments supplied to the connect function.

  If @racket[dsn-file] does not exist, or if it contains no entry
  for @racket[dsn], an exception is raised. If @racket[dsn] is a
  @racket[data-source], then @racket[dsn-file] is ignored.

@fake-examples[
[(put-dsn 'pg
          (postgresql-data-source #:user "me"
                                  #:database "mydb" 
                                  #:password "icecream"))
 (void)]
[(dsn-connect 'pg)
 (new connection%)]
[(dsn-connect 'pg #:notice-handler (lambda (code msg) ....))
 (new connection%)]
]
}

@defparam[current-dsn-file x path-string?]{

  A parameter holding the location of the default DSN file. The
  initial value is a file located immediately within
  @racket[(find-system-path 'prefs-dir)].
}

@defproc[(get-dsn [dsn symbol?]
                  [default any/c #f]
                  [#:dsn-file dsn-file path-string? (current-dsn-file)])
         (or/c data-source? any/c)]{

  Returns the @racket[data-source] associated with @racket[dsn] in
  @racket[dsn-file].

  If @racket[dsn-file] does not exist, an exception is raised. If
  @racket[dsn-file] does not have an entry for @racket[dsn],
  @racket[default] is called if it is a function or returned
  otherwise.
}

@defproc[(put-dsn [dsn symbol?]
                  [ds (or/c data-source? #f)]
                  [#:dsn-file dsn-file path-string? (current-dsn-file)])
         void?]{

  Associates @racket[dsn] with the given data source @racket[ds] in
  @racket[dsn-file], replacing the previous association, if one
  exists.
}

@(define absent @italic{absent})

@deftogether[[
@defproc[(postgresql-data-source
           [#:user user string? @#,absent]
           [#:database database string? @#,absent]
           [#:server server string? @#,absent]
           [#:port port exact-positive-integer? @#,absent]
           [#:socket socket (or/c path-string? 'guess #f) @#,absent]
           [#:password password (or/c string? #f) @#,absent]
           [#:allow-cleartext-password? allow-cleartext-password? boolean? @#,absent]
           [#:ssl ssl (or/c 'yes 'optional 'no) @#,absent]
           [#:notice-handler notice-handler (or/c 'output 'error) @#,absent]
           [#:notification-handler notification-handler (or/c 'output 'error) @#,absent])
         data-source?]
@defproc[(mysql-data-source
           [#:user user string? @#,absent]
           [#:database database (or/c string? #f) @#,absent]
           [#:server server string? @#,absent]
           [#:port port exact-positive-integer? @#,absent]
           [#:socket socket (or/c path-string? 'guess #f) @#,absent]
           [#:ssl ssl (or/c 'yes 'optional 'no) @#,absent]
           [#:password password (or/c string? #f) @#,absent]
           [#:notice-handler notice-handler (or/c 'output 'error) @#,absent])
         data-source?]
@defproc[(sqlite3-data-source
           [#:database database (or/c path-string? 'memory 'temporary) @#,absent]
           [#:mode mode (or/c 'read-only 'read/write 'create) @#,absent]
           [#:busy-retry-limit busy-retry-limit 
            (or/c exact-nonnegative-integer? +inf.0) @#,absent]
           [#:busy-retry-delay busy-retry-delay
            (and/c rational? (not/c negative?)) @#,absent]
           [#:use-place use-place boolean? @#,absent])
         data-source?]
@defproc[(odbc-data-source
           [#:dsn dsn (or/c string? #f) @#,absent]
           [#:database database (or/c string? #f) @#,absent]
           [#:user user (or/c string? #f) @#,absent]
           [#:password password (or/c string? #f) @#,absent]
           [#:notice-handler notice-handler (or/c 'output 'error) @#,absent]
           [#:strict-parameter-types? strict-parameter-types? boolean? @#,absent]
           [#:character-mode character-mode (or/c 'wchar 'utf-8 'latin-1) @#,absent])
         data-source?]]]{

  Analogues of @racket[postgresql-connect], @racket[mysql-connect],
  @racket[sqlite3-connect], and @racket[odbc-connect], respectively,
  that return a @racket[data-source] describing the (partial)
  connection information. All arguments are optional, even those that
  are mandatory in the corresponding connection function; the missing
  arguments must be supplied when @racket[dsn-connect] is called.
}


@;{============================================================}

@section[#:tag "managing-connections"]{Managing Connections}

@declare-exporting[db db/base #:use-sources (db/base)]

@defproc[(connection? [x any/c])
         boolean?]{

Returns @racket[#t] if @racket[x] is a connection, @racket[#f] otherwise.
}

@defproc[(disconnect [connection connection?])
         void?]{
Closes the connection.
}

@defproc[(connected? [connection connection?])
         boolean?]{

Returns @racket[#t] if @racket[connection] is connected, @racket[#f]
otherwise.
}

@defproc[(connection-dbsystem [connection connection?])
         dbsystem?]{

Gets an object encapsulating information about the database system of
@racket[connection].
}

@defproc[(dbsystem? [x any/c])
         boolean?]{

Predicate for objects representing database systems.
}

@defproc[(dbsystem-name [sys dbsystem?])
         symbol?]{

Returns a symbol that identifies the database system. Currently one of the
following:
@itemize[
@item[@racket['postgresql]]
@item[@racket['mysql]]
@item[@racket['sqlite3]]
@item[@racket['odbc]]
]
}

@defproc[(dbsystem-supported-types [sys dbsystem?])
         (listof symbol?)]{

Returns a list of symbols identifying types supported by the database
system. See @secref["db-types"].
}


@section{System-specific Modules}

The @racketmodname[db] module exports all of the
functions listed in this manual except those described in
@secref["util"]. The database system-specific connection modules are
loaded lazily to avoid unnecessary dependencies on foreign libraries.

The following modules provide subsets of the bindings described in
this manual.

@defmodule*/no-declare[(db/base)]

Provides all generic connection operations (those described in
@secref{managing-connections} and @secref{query-api}) and SQL data
support (@secref{sql-types}).

@defmodule*/no-declare[(db/postgresql)]

Provides only @racket[postgresql-connect] and
@racket[postgresql-guess-socket-path].

@defmodule*/no-declare[(db/mysql)]

Provides only @racket[mysql-connect] and
@racket[mysql-guess-socket-path].

@defmodule*/no-declare[(db/sqlite3)]

Provides @racket[sqlite3-connect] plus @racket[sqlite3-available?]. When
the SQLite native library cannot be found, @racket[sqlite3-connect]
raises an exception.

@defmodule*/no-declare[(db/odbc)]

Provides only @racket[odbc-connect], @racket[odbc-driver-connect],
@racket[odbc-data-sources], and @racket[odbc-drivers]. In contrast to
@racketmodname[db], this module immediately attempts to
load the ODBC native library when required, and it raises an exception
if it cannot be found.


@(close-eval the-eval)
