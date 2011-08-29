#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          racket/sandbox
          "config.rkt"
          (for-label db))

@title[#:tag "notes"]{Notes}

This section describes miscellaneous issues.

@section[#:tag "connecting-to-server"]{Local Sockets for PostgreSQL and MySQL Servers}

PostgreSQL and MySQL servers are sometimes configured by default to
listen only on local sockets (also called ``unix domain
sockets''). This library provides support for communication over local
sockets, but only on Linux (x86 and x86-64) and Mac OS X. If local
socket communication is not available, the server must be reconfigured
to listen on a TCP port.

The socket file for a PostgreSQL server is located in the directory
specified by the @tt{unix_socket_directory} variable in the
@tt{postgresql.conf} server configuration file.  For example, on
Ubuntu 10.10 running PostgreSQL 8.4, the socket directory is
@tt{/var/run/postgresql} and the socket file is
@tt{/var/run/postgresql/.s.PGSQL.5432}. Common socket paths may be
searched automatically using the @racket[postgresql-guess-socket-path]
function.

The socket file for a MySQL server is located at the path specified by
the @tt{socket} variable in the @tt{my.cnf} configuration file. For
example, on Ubuntu 10.10 running MySQL 5.1, the socket is located at
@tt{/var/run/mysqld/mysqld.sock}. Common socket paths for MySQL can be
searched using the @racket[mysql-guess-socket-path] function.


@section{Database Character Encodings}

In most cases, a PostgreSQL or MySQL database's character encoding is
irrelevant, since the connect function always requests translation to
Unicode (UTF-8) when creating a connection. If a PostgreSQL database's
character encoding is @tt{SQL_ASCII}, however, PostgreSQL will not
honor the connection encoding; it will instead send untranslated
octets, which will cause corrupt data or internal errors in the client
connection.

To convert a PostgreSQL database from @tt{SQL_ASCII} to something
sensible, @tt{pg_dump} the database, recode the dump file (using a
utility such as @tt{iconv}), create a new database with the desired
encoding, and @tt{pg_restore} from the recoded dump file.


@section{Prepared Query Parameter Types}

Different database systems vary in their handling of query parameter
types. For example, consider the following parameterized SQL
statement:

@tt{SELECT 1 + ?;}

PostgreSQL reports an expected type of @tt{integer} for the parameter and
will not accept other types. MySQL and SQLite, in contrast, report no
useful parameter type information, and ODBC connections vary in
behavior based on the driver, the data source configuration, and the
connection parameters (see @secref["odbc-status"] for specific notes).


@section{PostgreSQL Authentication}

PostgreSQL supports a large variety of authentication mechanisms,
controlled by the @tt{pg_hba.conf} server configuration file. This
library currently supports only cleartext and md5-hashed passwords,
and it does not send cleartext passwords unless explicitly ordered to
(see @racket[postgresql-connect]). These correspond to the @tt{md5}
and @tt{password} authentication methods in the parlance of
@tt{pg_hba.conf}, respectively. On Linux, @tt{ident} authentication is
automatically supported for unix domain sockets (but not TCP). The
@tt{gss}, @tt{sspi}, @tt{krb5}, @tt{pam}, and @tt{ldap} methods are
not supported.


@section{MySQL Authentication}

As of version 5.5.7, MySQL supports
@hyperlink["http://dev.mysql.com/doc/mysql-security-excerpt/5.5/en/pluggable-authentication.html"]{authentication
plugins}. The only plugin currently supported by this library is
@tt{mysql_native_password} (the default), which corresponds to the
password authentication mechanism used since version 4.1.


@section[#:tag "sqlite3-native-libs"]{SQLite Native Library}

SQLite support requires the appropriate native library, specifically
@tt{libsqlite3.so.0} on Unix or @tt{sqlite3.dll} on Windows.


@section[#:tag "odbc-native-libs"]{ODBC Native Libraries}

ODBC support requires the appropriate native library, specifically
@tt{libodbc.so.1} (from unixODBC; iODBC is not supported) on Unix or
@tt{odbc32.dll} on Windows. In addition, the appropriate ODBC Drivers
must be installed and any Data Sources configured.


@section[#:tag "odbc-status"]{ODBC Support Status}

ODBC support is experimental. This library is compatible only with
ODBC 3.x Driver Managers. The behavior of ODBC connections can vary
widely depending on the driver in use and even the configuration of a
particular data source.

The following sections describe the configurations that this library
has been tested with. The platform @bold{win32} means Windows Vista on
a 32-bit processor and @bold{linux} means Ubuntu 11.04 and unixODBC on
both x86 (32-bit) and x86-64 processors, unless otherwise
specified. The iODBC Driver Manager is not supported.

Reports of success or failure on other platforms or with other drivers
would be appreciated.

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

@subsection{DB2 ODBC Driver}

The driver from IBM DB2 Express-C v9.7 has been tested on @bold{linux}
(32-bit only).

For a typical installation where the instance resides at
@tt{/home/db2inst1}, set the following option in the Driver
configuration: @tt{Driver = /home/db2inst1/sqllib/lib32/libdb2.so}.

The DB2 driver does not seem to accept a separate argument for the
database to connect to; it must be the same as the Data Source name.

@subsection{Oracle ODBC Driver}

The driver from Oracle Database 10g Release 2 Express Edition has been
tested on @bold{linux} (32-bit only).

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

Basic SQL Server support has been verified on @bold{win32}, but the
automated test suite has not yet been adapted and run.
