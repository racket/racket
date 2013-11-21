#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          racket/sandbox
          "config.rkt"
          (for-label db
                     setup/dirs))

@title[#:tag "notes"]{Notes}

This section discusses issues related to specific database systems.


@section[#:tag "connecting-to-server"]{Local Sockets for PostgreSQL and MySQL Servers}

PostgreSQL and MySQL servers are sometimes configured by default to
listen only on local sockets (also called ``unix domain
sockets''). This library provides support for communication over local
sockets on Linux (x86 and x86-64) and Mac OS X. If local socket
communication is not available, the server must be reconfigured to
listen on a TCP port.

The socket file for a PostgreSQL server is located in the directory
specified by the @tt{unix_socket_directory} variable in the
@tt{postgresql.conf} server configuration file.  For example, on
Ubuntu 11.04 running PostgreSQL 8.4, the socket directory is
@tt{/var/run/postgresql} and the socket file is
@tt{/var/run/postgresql/.s.PGSQL.5432}. Common socket paths may be
searched automatically using the @racket[postgresql-guess-socket-path]
function.

The socket file for a MySQL server is located at the path specified by
the @tt{socket} variable in the @tt{my.cnf} configuration file. For
example, on Ubuntu 11.04 running MySQL 5.1, the socket is located at
@tt{/var/run/mysqld/mysqld.sock}. Common socket paths for MySQL can be
searched using the @racket[mysql-guess-socket-path] function.


@section{PostgreSQL Database Character Encoding}

In most cases, a database's character encoding is irrelevant, since
the connect function always requests translation to Unicode (UTF-8)
when creating a connection. If a PostgreSQL database's character
encoding is @tt{SQL_ASCII}, however, PostgreSQL will not honor the
connection encoding; it will instead send untranslated octets, which
will cause corrupt data or internal errors in the client connection.

To convert a PostgreSQL database from @tt{SQL_ASCII} to something
sensible, @tt{pg_dump} the database, recode the dump file (using a
utility such as @tt{iconv}), create a new database with the desired
encoding, and @tt{pg_restore} from the recoded dump file.


@section{PostgreSQL Authentication}

PostgreSQL supports a large variety of
@hyperlink["http://www.postgresql.org/docs/8.4/static/auth-pg-hba-conf.html"]{authentication
mechanisms}, controlled by the @tt{pg_hba.conf} server configuration
file. This library currently supports only cleartext and md5-hashed
passwords, and it does not send cleartext passwords unless explicitly
ordered to (see @racket[postgresql-connect]). These correspond to the
@tt{md5} and @tt{password} authentication methods in the parlance of
@tt{pg_hba.conf}, respectively. On Linux and Mac OS X, @tt{ident}
authentication is automatically supported for local sockets (as of
PostgreSQL 9.1, this authentication method has been renamed
@tt{peer}). The @tt{gss}, @tt{sspi}, @tt{krb5}, @tt{pam}, and
@tt{ldap} methods are not supported.

@section[#:tag "postgresql-timestamp-tz"]{PostgreSQL Timestamps and Time Zones}

PostgreSQL's @tt{timestamp with time zone} type is inconsistent with
the SQL standard (probably), inconsistent with @tt{time with time
zone}, and potentially confusing to PostgreSQL newcomers.

A @tt{time with time zone} is essentially a @tt{time} structure with
an additional field storing a time zone offset. In contrast, a
@tt{timestamp with time zone} has no fields beyond those of
@tt{timestamp}. Rather, it indicates that its datetime fields should
be interpreted as a UTC time. Thus it represents an absolute point in
time, unlike @tt{timestamp without time zone}, which represents local
date and time in some unknown time zone (possibly---hopefully---known
the the database designer, but unknown to PostgreSQL).

When a @tt{timestamp with time zone} is created from a source without
time zone information, the session's @tt{TIME ZONE} setting is used to
adjust the source to UTC time. When the source contains time zone
information, it is used to adjust the timestamp to UTC time. In either
case, the time zone information is @emph{discarded} and only the UTC
timestamp is stored. When a @tt{timestamp with time zone} is rendered
as text, it is first adjusted to the time zone specified by the
@tt{TIME ZONE} setting (or by
@hyperlink["http://www.postgresql.org/docs/8.0/static/functions-datetime.html#FUNCTIONS-DATETIME-ZONECONVERT"]{@tt{AT
TIME ZONE}}) and that offset is included in the rendered text.

This library receives timestamps in binary format, so the time zone
adjustment is not applied, nor is the session's @tt{TIME ZONE} offset
included; thus all @racket[sql-timestamp] values in a query result
have a @racket[tz] field of @racket[0] (for @tt{timestamp with time
zone}) or @racket[#f] (for @tt{timestamp without time
zone}). (Previous versions of this library sent and received
timestamps as text, so they received timestamps with adjusted time
zones.)


@section{MySQL Authentication}

As of version 5.5.7, MySQL supports
@hyperlink["http://dev.mysql.com/doc/mysql-security-excerpt/5.5/en/pluggable-authentication.html"]{authentication
plugins}. The only plugins currently supported by this library are
@tt{mysql_native_password} (the default) and @tt{mysql_old_password},
which corresponds to the password authentication mechanisms used since
version 4.1 and before 4.1, respectively.


@section[#:tag "sqlite3-requirements"]{SQLite Requirements}

SQLite support requires the appropriate native library.

@itemlist[

@item{On Windows, the library is @tt{sqlite3.dll}. It is included in
the Racket distribution.}

@item{On Mac OS X, the library is @tt{libsqlite3.0.dylib}, which is
included (in @tt{/usr/lib}) in Mac OS X version 10.4 onwards.}

@item{On Linux, the library is @tt{libsqlite3.so.0}. It is included in
the @tt{libsqlite3-0} package in Debian/Ubuntu and in the @tt{sqlite}
package in Red Hat.}
]


@section[#:tag "ffi-concurrency"]{FFI-Based Connections and Concurrency}

@tech{Wire-based connections} communicate using
@tech/reference{ports}, which do not cause other Racket threads to
block. In contrast, an FFI call causes all Racket threads to block
until it completes, so @tech{FFI-based connections} can degrade the
interactivity of a Racket program, particularly if long-running
queries are performed using the connection. This problem can be
avoided by creating the FFI-based connection in a separate
@tech/reference{place} using the @racket[#:use-place] keyword
argument. Such a connection will not block all Racket threads during
queries; the disadvantage is the cost of creating and communicating
with a separate @tech/reference{place}.


@section[#:tag "odbc-requirements"]{ODBC Requirements}

ODBC requires the appropriate driver manager native library as well as
driver native libraries for each database system you want use ODBC to
connect to.

@itemlist[

@item{On Windows, the driver manager is @tt{odbc32.dll}, which is
included automatically with Windows.}

@item{On Mac OS X, the driver manager is @tt{libiodbc.2.dylib}
(@hyperlink["http://www.iodbc.org"]{iODBC}), which is included (in
@tt{/usr/lib}) in Mac OS X version 10.2 onwards.}

@item{On Linux, the driver manager is @tt{libodbc.so.1}
(@hyperlink["http://www.unixodbc.org"]{unixODBC}---iODBC is not
supported). It is available from the @tt{unixodbc} package in
Debian/Ubuntu and in the @tt{unixODBC} package in Red Hat.}
]

In addition, you must install the appropriate ODBC Drivers and
configure Data Sources. Refer to the ODBC documentation for the
specific database system for more information.


@section[#:tag "odbc-status"]{ODBC Status}

ODBC support is experimental. The behavior of ODBC connections can
vary widely depending on the driver in use and even the configuration
of a particular data source.

The following sections describe the configurations that this library
has been tested with.

Reports of success or failure on other platforms or with other drivers
would be appreciated.

@;{
** There's no reason to actually use the following drivers. They're just
** useful for testing ODBC support.

@subsection{PostgreSQL ODBC Driver}

The PostgreSQL ODBC driver version 09.00.0300 has been tested on
@bold{win32} and @bold{linux}. 

To get specific parameter type information, set the following Data
Source options: @tt{Protocol = 7.4} and @tt{UserServerSidePrepare =
1}, and use the @racket[#:strict-parameter-types?] connection option.

Older versions of the driver, including version 08.03.0200, provided
by Ubuntu 11.04, seem to have a bug in the character mode this library
uses by default; use the @racket[#:character-mode 'utf-8] connection
option as a workaround.

@subsection{MySQL ODBC Driver}

The MySQL ODBC driver version 5.1.6-1 has been tested on @bold{win32}
and @bold{linux}.

Avoid using the @racket[#:strict-parameter-types?] connection option,
as the driver assigns all parameters the type @tt{varchar}. 

@subsection{SQLite3 ODBC Driver}

Avoid using the @racket[#:strict-parameter-types?] connection option,
as the driver assigns all parameters the type @tt{longvarchar}.
Furthermore, this driver interprets the declared types of columns
strictly, replacing nonconforming values in query results with
@tt{NULL}. All computed columns, even those with explicit @tt{CAST}s,
seem to be returned as @tt{text}.
}

@subsection{DB2 ODBC Driver}

The driver from IBM DB2 Express-C v9.7 has been tested on Ubuntu 11.04
(32-bit only).

For a typical installation where the instance resides at
@tt{/home/db2inst1}, set the following option in the Driver
configuration: @tt{Driver =
/home/db2inst1/sqllib/lib32/libdb2.so}. (The path would presumably be
different for a 64-bit installation.)

The DB2 driver does not seem to accept a separate argument for the
database to connect to; it must be the same as the Data Source name.

@subsection{Oracle ODBC Driver}

The driver from Oracle Database 10g Release 2 Express Edition has been
tested on Ubuntu 11.04 (32-bit only).

It seems the @tt{ORACLE_HOME} and @tt{LD_LIBRARY_PATH} environment
variables must be set according to the @tt{oracle_env.{csh,sh}} script
for the driver to work. 

Columns of type @tt{TIME} can cause a memory error (ie, Racket
crashes). This seems to be due to a
@hyperlink["http://forums.oracle.com/forums/thread.jspa?threadID=572661"]{bug}
in Oracle's ODBC driver, but I do not yet have a workaround.

@;{
Maybe Oracle bug? See:
  http://forums.oracle.com/forums/thread.jspa?threadID=572661
  http://stackoverflow.com/questions/38435/
  http://forums.oracle.com/forums/thread.jspa?threadID=856713
}

@subsection{SQL Server ODBC Driver}

Basic SQL Server support has been verified on Windows,
but the automated test suite has not yet been adapted and run.

The ``SQL Server'' driver refuses to accept @tt{NUMERIC} or
@tt{DECIMAL} parameters, producing the error ``Invalid precision value
(SQLSTATE: HY104).'' If possible, use the ``Native SQL Server''
driver instead.
