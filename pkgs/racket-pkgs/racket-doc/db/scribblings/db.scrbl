#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/eval
          "config.rkt")

@title{DB: Database Connectivity}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@section-index["database"]

@centered{@bold{@italic{A database interface for functional programmers.}}}

@defmodule[db]

This library provides a high-level interface to several database
systems. The following database systems are currently supported:
@itemlist[

@item{@bold{@as-index{@hyperlink["http://www.postgresql.org"]{PostgreSQL}}
versions 7.4 and later.} This library implements the PostgreSQL wire
protocol, so no native client library is required.}

@item{@bold{@as-index{@hyperlink["http://www.mysql.com"]{MySQL}} versions 5 and
later.} This library implements the MySQL wire protocol, so no
native client library is required.}

@item{@bold{@as-index{@hyperlink["http://www.sqlite.org"]{SQLite}} version
3.} The SQLite native client library is required; see
@secref["sqlite3-requirements"].}

@item{@bold{@as-index{ODBC}.} An ODBC Driver Manager and appropriate
ODBC drivers are required; see @secref["odbc-requirements"]. The
following database systems are known to work with this library via
ODBC (see @secref["odbc-status"] for details):
@bold{@as-index{@hyperlink["http://www.ibm.com/software/data/db2/"]{DB2}}},
@bold{@as-index{@hyperlink["http://www.oracle.com"]{Oracle}}}, and
@bold{@as-index{@hyperlink["http://www.microsoft.com/sqlserver/"]{SQL Server}}}.}
]

The query operations are functional in spirit: queries return results
or raise exceptions rather than stashing their state into a cursor
object for later navigation and retrieval. Query parameters and result
fields are automatically translated to and from appropriate Racket
values. Resources are managed automatically by the garbage collector
and via custodians. Connections are internally synchronized, so
multiple threads can use a connection simultaneously.

@bold{Acknowledgments} Thanks to Dave Gurnell, Noel Welsh, Mike Burns,
and Doug Orleans for contributions to @tt{spgsql}, the PostgreSQL-only
predecessor of this library. The SQLite support is based in part on
code from Jay McCarthy's @tt{sqlite} package.

@include-section["using-db.scrbl"]
@include-section["connect.scrbl"]
@include-section["query.scrbl"]
@include-section["sql-types.scrbl"]
@include-section["util.scrbl"]
@include-section["notes.scrbl"]

@(close-eval fake-eval)
