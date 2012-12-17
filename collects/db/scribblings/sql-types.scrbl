#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          racket/sandbox
          racket/runtime-path
          "config.rkt"
          "tabbing.rkt"
          (for-label (prefix-in srfi: srfi/19)
                     db db/util/geometry db/util/postgresql
                     json))

@(define-runtime-path log-file "log-for-sql-types.rktd")
@(define the-eval (make-pg-eval log-file #t))

@title[#:tag "sql-types"]{SQL Types and Conversions}

@declare-exporting[db db/base #:use-sources (db/base)]

Connections automatically convert query results to appropriate Racket
types. Likewise, query parameters are accepted as Racket values and
converted to the appropriate SQL type.

@examples[#:eval the-eval
(query-value pgc "select count(*) from the_numbers")
(query-value pgc "select false")
(query-value pgc "select 1 + $1" 2)
]

If a query result contains a column with a SQL type not supported by
this library, an exception is raised. As a workaround, cast the column
to a supported type:

@examples[#:eval the-eval
(query-value pgc "select inet '127.0.0.1'")
(query-value pgc "select cast(inet '127.0.0.1' as varchar)")
]

The exception for unsupported types in result columns is raised when
the query is executed, not when it is prepared; for parameters it is
raised when the parameter values are supplied. Thus even unexecutable
prepared statements can be inspected using
@racket[prepared-statement-parameter-types] and
@racket[prepared-statement-result-types].


@section[#:tag "db-types"]{SQL Type Conversions}

This section describes the correspondences between SQL types and
Racket types for the supported database systems. 

@subsection[#:tag "postgresql-types"]{PostgreSQL Types}

This section applies to connections created with
@racket[postgresql-connect]. 

The following table lists the PostgreSQL types known to this library,
along with their corresponding Racket representations.

@centered{
@tabbing{
  @bold{PostgreSQL type}  @& @bold{pg_type.typname}  @& @bold{Racket type} @//
  @racket['boolean]       @& @tt{bool}               @& @racket[boolean?] @//
  @racket['char1]         @& @tt{char}               @& @racket[char?] @//
  @racket['smallint]      @& @tt{int2}               @& @racket[exact-integer?] @//
  @racket['integer]       @& @tt{int4}               @& @racket[exact-integer?] @//
  @racket['bigint]        @& @tt{int8}               @& @racket[exact-integer?] @//
  @racket['real]          @& @tt{float4}             @& @racket[real?] @//
  @racket['double]        @& @tt{float8}             @& @racket[real?] @//
  @racket['decimal]       @& @tt{numeric}            @& @racket[rational?] or @racket[+nan.0] @//
  @racket['character]     @& @tt{bpchar}             @& @racket[string?] @//
  @racket['varchar]       @& @tt{varchar}            @& @racket[string?] @//
  @racket['text]          @& @tt{text}               @& @racket[string?] @//
  @racket['bytea]         @& @tt{bytea}              @& @racket[bytes?] @//
  @racket['date]          @& @tt{date}               @& @racket[sql-date?] @//
  @racket['time]          @& @tt{time}               @& @racket[sql-time?] @//
  @racket['timetz]        @& @tt{timetz}             @& @racket[sql-time?] @//
  @racket['timestamp]     @& @tt{timestamp}          @& @racket[sql-timestamp?]
                                                        or @racket[-inf.0] or @racket[+inf.0] @//
  @racket['timestamptz]   @& @tt{timestamptz}        @& @racket[sql-timestamp?] 
                                                        or @racket[-inf.0] or @racket[+inf.0] @//
  @racket['interval]      @& @tt{interval}           @& @racket[sql-interval?] @//
  @racket['bit]           @& @tt{bit}                @& @racket[bit-vector?] @//
  @racket['varbit]        @& @tt{varbit}             @& @racket[bit-vector?] @//

  @racket['json]          @& @tt{json}               @& @racket[jsexpr?] @//
  @racket['int4range]     @& @tt{int4range}          @& @racket[pg-range-or-empty?] @//
  @racket['int8range]     @& @tt{int8range}          @& @racket[pg-range-or-empty?] @//
  @racket['numrange]      @& @tt{numrange}           @& @racket[pg-range-or-empty?] @//
  @racket['tsrange]       @& @tt{tsrange}            @& @racket[pg-range-or-empty?] @//
  @racket['tstzrange]     @& @tt{tstzrange}          @& @racket[pg-range-or-empty?] @//
  @racket['daterange]     @& @tt{daterange}          @& @racket[pg-range-or-empty?] @//

  @racket['point]         @& @tt{point}              @& @racket[point?] @//
  @racket['lseg]          @& @tt{lseg}               @& @racket[line?] @//
  @racket['path]          @& @tt{path}               @& @racket[pg-path?] @//
  @racket['box]           @& @tt{box}                @& @racket[pg-box?] @//
  @racket['polygon]       @& @tt{polygon}            @& @racket[polygon?] @//
  @racket['circle]        @& @tt{circle}             @& @racket[pg-circle?]
}
}

The @racket['char1] type, written @tt{"char"} in PostgreSQL's SQL
syntax (the quotation marks are significant), is one byte, essentially
a tiny integer written as a character.

A SQL value of type @tt{decimal} is converted to either an exact
rational or @racket[+nan.0]. When converting Racket values to SQL
@tt{decimal}, exact rational values representable by finite decimal
strings are converted without loss of precision. (Precision may be
lost, of course, if the value is then stored in a database field of
lower precision.) Other real values are converted to decimals with a
loss of precision. In PostgreSQL, @tt{numeric} and @tt{decimal} refer
to the same type.

@examples[#:eval the-eval
(query-value pgc "select real '+Infinity'")
(query-value pgc "select numeric '12345678901234567890'")
]

A SQL @tt{timestamp with time zone} is converted to a Racket
@racket[sql-timestamp] in UTC---that is, with a @racket[tz] field of
@racket[0]. If a Racket @racket[sql-timestamp] without a time zone
(@racket[tz] is @racket[#f]) is given for a parameter of type
@tt{timestamp with time zone}, it is treated as a timestamp in
UTC. See also @secref["postgresql-timestamp-tz"].

The geometric types such as @racket['point] are represented by
structures defined in the @racketmodname[db/util/geometry] and
@racketmodname[db/util/postgresql] modules.

PostgreSQL user-defined @emph{domains} are supported in query results
if the underlying type is supported. Recordset headers and
@racket[prepared-statement-result-types] report them in terms of the
underlying type. Parameters with user-defined domain types are not
currently supported. As a workaround, cast the parameter to the
underlying type. For example, if the type of @tt{$1} is a domain whose
underlying type is @tt{integer}, then replace @tt{$1} with
@tt{($1::integer)}.

For each type in the table above, the corresponding
@hyperlink["http://www.postgresql.org/docs/8.2/static/arrays.html"]{array
type} is also supported, using the @racket[pg-array] structure. Use
the
@hyperlink["http://www.postgresql.org/docs/8.2/static/functions-comparisons.html#AEN14122"]{
@tt{= ANY}} syntax with an array parameter instead of dynamically
constructing a SQL @tt{IN} expression:

@examples[#:eval the-eval
(query-value pgc "select 1 in (1, 2, 3)")
(query-value pgc "select 1 = any ($1::integer[])"
             (list->pg-array (list 1 2 3)))
]

A list may be provided for an array parameter, in which case it is
automatically converted using @racket[list->pg-array]. The type
annotation can be dropped when the array type can be inferred from the
left-hand side.

@examples[#:eval the-eval
(query-value pgc "select 1 = any ($1)" (list 1 2 3))
(query-value pgc "select $1::integer = any ($2)"
             1 (list 1 2 3))
(query-value pgc "select $1 = any ($2)" (code:comment "what type are we using?")
             1 (list 1 2 3))
]

PostgreSQL defines many other types, such as network addresses and row
types. These are currently not supported, but support may be added in
future versions of this library.


@subsection[#:tag "mysql-types"]{MySQL Types}

This section applies to connections created with
@racket[mysql-connect].

The following table lists the MySQL types known to this library, along
with their corresponding Racket representations.

@centered{
@tabbing[#:spacing 8]{
  @bold{MySQL type}            @& @bold{Racket type} @//
  @racket['integer]            @& @racket[exact-integer?] @//
  @racket['tinyint]            @& @racket[exact-integer?] @//
  @racket['smallint]           @& @racket[exact-integer?] @//
  @racket['mediumint]          @& @racket[exact-integer?] @//
  @racket['bigint]             @& @racket[exact-integer?] @//
  @racket['real]               @& @racket[real?] @//
  @racket['double]             @& @racket[real?] @//
  @racket['decimal]            @& @racket[exact?] @//
  @racket['varchar]            @& @racket[string?] @//
  @racket['date]               @& @racket[sql-date?] @//
  @racket['time]               @& @racket[sql-time?] or @racket[sql-day-time-interval?] @//
  @racket['datetime]           @& @racket[sql-timestamp?] @//

  @racket['var-string]         @& @racket[string?] @//
  @racket['text]               @& @racket[string?] @//

  @racket['var-binary]         @& @racket[bytes?] @//
  @racket['blob]               @& @racket[bytes?] @//

  @racket['bit]                @& @racket[bit-vector?] @//
  @racket['geometry]           @& @racket[geometry2d?]
}
}

MySQL does not report specific parameter types for prepared queries,
so they are instead assigned the pseudo-type @racket['any]. Conversion
of Racket values to parameters accepts strings, numbers
(@racket[rational?]---no infinities or NaN), bytes, SQL date/time
structures (@racket[sql-date?], @racket[sql-time?],
@racket[sql-timestamp?], and @racket[sql-day-time-interval?]), bits
(@racket[bit-vector?]), and geometric values
(@racket[geometry2d?]). Numbers are sent as 64-bit signed integers, if
possible, or as double-precision floating point numbers otherwise.

Fields of type @tt{CHAR} or @tt{VARCHAR} are typically reported as
@racket['var-string], and fields of type @tt{BINARY} or @tt{VARBINARY}
are typically reported as @racket['var-binary].

The MySQL @tt{time} type represents time intervals, which may not
correspond to times of day (for example, the interval may be negative
or larger than 24 hours). In conversion from MySQL results to Racket
values, those @tt{time} values that represent times of day are
converted to @racket[sql-time] values; the rest are represented by
@racket[sql-interval] values.

The MySQL @tt{enum} and @tt{set} types are not supported. As a
workaround, cast them to/from either integers or strings.


@subsection[#:tag "sqlite-types"]{SQLite Types}

This section applies to connections created with
@racket[sqlite3-connect].

The following table lists the SQLite types known to this library,
along with their corresponding Racket representations.

Unlike PostgreSQL and MySQL, SQLite does not enforce declared type
constraints (with the exception of @tt{integer primary key}) on
@emph{columns}. Rather, every SQLite @emph{value} has an associated
``storage class''.

@centered{
@tabbing{
  @bold{SQLite storage class}  @& @bold{Racket type} @//
  @tt{integer}                 @& @racket[exact-integer?] @//
  @tt{real}                    @& @racket[real?] @//
  @tt{text}                    @& @racket[string?] @//
  @tt{blob}                    @& @racket[bytes?]
}
}

SQLite does not report specific parameter and result types for
prepared queries. Instead, they are assigned the pseudo-type
@racket['any]. Conversion of Racket values to parameters accepts
strings, bytes, and real numbers.

An exact integer that cannot be represented as a 64-bit signed integer
is converted as @tt{real}, not @tt{integer}.

@fake-examples[
[(expt 2 80)
 (expt 2 80)]
[(query-value slc "select ?" (expt 2 80))
 1.2089258196146292e+24]
]


@subsection[#:tag "odbc-types"]{ODBC Types}

This section applies to connections created with @racket[odbc-connect]
or @racket[odbc-driver-connect].

The following table lists the ODBC types known to this library,
along with their corresponding Racket representations.

@centered{
@tabbing[#:spacing 8]{
  @bold{ODBC type}        @& @bold{Racket type} @//
  @racket['character]     @& @racket[string?] @//
  @racket['varchar]       @& @racket[string?] @//
  @racket['longvarchar]   @& @racket[string?] @//
  @racket['numeric]       @& @racket[rational?] @//
  @racket['decimal]       @& @racket[rational?] @//
  @racket['integer]       @& @racket[exact-integer?] @//
  @racket['tinyint]       @& @racket[exact-integer?] @//
  @racket['smallint]      @& @racket[exact-integer?] @//
  @racket['bigint]        @& @racket[exact-integer?] @//
  @racket['float]         @& @racket[real?] @//
  @racket['real]          @& @racket[real?] @//
  @racket['double]        @& @racket[real?] @//
  @racket['date]          @& @racket[sql-date?] @//
  @racket['time]          @& @racket[sql-time?] @//
  @racket['datetime]      @& @racket[sql-timestamp?] @//
  @racket['timestamp]     @& @racket[sql-timestamp?] @//
  @racket['binary]        @& @racket[bytes?] @//
  @racket['varbinary]     @& @racket[bytes?] @//
  @racket['longvarbinary] @& @racket[bytes?] @//
  @racket['bit1]          @& @racket[boolean?]
}
}

Not all ODBC drivers provide specific parameter type information for
prepared queries. Some omit parameter type information entirely or,
worse, assign all parameters a single type such as @tt{varchar}. To
avoid enforcing irrelevant type constraints in the last case,
connections only attempt to fetch and enforce parameter types when the
connection is made using the @racket[#:strict-parameter-type?]
option. Otherwise, the connection assigns all parameters the type
@racket['unknown]. (The @racket['unknown] type is also used when
specific parameter types are requested but are not available.)
Conversion of Racket values to @racket['unknown] parameters accepts
strings, bytes, numbers (@racket[rational?]---no infinities or NaN),
booleans, and SQL date/time structures (@racket[sql-date?],
@racket[sql-time?], and @racket[sql-timestamp?]).

The ODBC type @racket['bit1] represents a single bit, unlike the
standard SQL @tt{bit(N)} type.

Interval types are not currently supported on ODBC.


@;{----------------------------------------}

@section[#:tag "sql-data"]{SQL Data}

This section describes data types for representing various SQL types
that have no existing appropriate counterpart in Racket.

@subsection{SQL NULL}

SQL @tt{NULL} is translated into the unique @racket[sql-null] value.

@defthing[sql-null sql-null?]{

  A special value used to represent @tt{NULL} values in query
  results. The @racket[sql-null] value may be recognized using
  @racket[eq?].

@examples[#:eval the-eval
(query-value pgc "select NULL")
]
}

@defproc[(sql-null? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is @racket[sql-null]; @racket[#f]
  otherwise.
}

@defproc[(sql-null->false [x any/c]) any/c]{

  Returns @racket[#f] if @racket[x] is @racket[sql-null]; otherwise
  returns @racket[x].

@examples[#:eval the-eval
(sql-null->false "apple")
(sql-null->false sql-null)
(sql-null->false #f)
]
}

@defproc[(false->sql-null [x any/c]) any/c]{

  Returns @racket[sql-null] if @racket[x] is @racket[#f]; otherwise
  returns @racket[x].

@examples[#:eval the-eval
(false->sql-null "apple")
(false->sql-null #f)
]
}


@subsection{Dates and Times}

The @tt{DATE}, @tt{TIME} (@tt{WITH TIME ZONE} and without),
@tt{TIMESTAMP} (@tt{WITH TIME ZONE} and without), and @tt{INTERVAL}
SQL types are represented by the following structures.

See also @secref["datetime-util"] for more functions on datetime
values.

@defstruct*[sql-date
            ([year exact-integer?]
             [month (integer-in 0 12)]
             [day (integer-in 0 31)])]{

  Represents a SQL date.

  Dates with zero-valued @racket[month] or @racket[day] components are
  a MySQL extension.
}

@defstruct*[sql-time
            ([hour exact-nonnegative-integer?]
             [minute exact-nonnegative-integer?]
             [second exact-nonnegative-integer?]
             [nanosecond exact-nonnegative-integer?]
             [tz (or/c exact-integer? #f)])]
@defstruct*[sql-timestamp
            ([year exact-nonnegative-integer?]
             [month exact-nonnegative-integer?]
             [day exact-nonnegative-integer?]
             [hour exact-nonnegative-integer?]
             [minute exact-nonnegative-integer?]
             [second exact-nonnegative-integer?]
             [nanosecond exact-nonnegative-integer?]
             [tz (or/c exact-integer? #f)])]{

  Represents SQL times and timestamps.

  The @racket[tz] field indicates the time zone offset as the number
  of seconds east of GMT (as in SRFI 19). If @racket[tz] is
  @racket[#f], the time or timestamp does not carry time zone
  information.

  The @racket[sql-time] and @racket[sql-timestamp] structures store
  fractional seconds to nanosecond precision for compatibility with
  SRFI 19. Note, however, that database systems generally do not
  support nanosecond precision; PostgreSQL, for example, only supports
  microsecond precision.

@examples[#:eval the-eval
(query-value pgc "select date '25-dec-1980'")
(query-value pgc "select time '7:30'")
(query-value pgc "select timestamp 'epoch'")
(query-value pgc "select timestamp with time zone 'epoch'")
]
}

@defstruct*[sql-interval
            ([years exact-integer?]
             [months exact-integer?]
             [days exact-integer?]
             [hours exact-integer?]
             [minutes exact-integer?]
             [seconds exact-integer?]
             [nanoseconds exact-integer?])]{

  Represents lengths of time. Intervals are normalized to satisfy the
  following constraints:
  @itemlist[
  @item{@racket[years] and @racket[months] have the same sign}
  @item{@racket[months] ranges from @racket[-11] to @racket[11]}
  @item{@racket[days], @racket[hours], @racket[minutes],
    @racket[seconds], and @racket[nanoseconds] all have the same sign}
  @item{@racket[hours] ranges from @racket[-23] to @racket[23]}
  @item{@racket[minutes] and @racket[seconds] range from @racket[-59]
    to @racket[59]} 
  @item{@racket[nanoseconds] ranges from
    @racket[(- (sub1 #, @racketvalfont{#e1e9}))] to
    @racket[(sub1 #, @racketvalfont{#e1e9})]}
  ]

  That is, an interval consists of two groups of components:
  year-month and day-time, and normalization is done only within
  groups. In fact, the SQL standard recognizes those two types of
  intervals separately (see @racket[sql-year-month-interval?] and
  @racket[sql-day-time-interval?], below), and does not permit
  combining them. Intervals such as @tt{1 month 3 days} are a
  PostgreSQL extension.
}

@defproc[(sql-year-month-interval? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @racket[sql-interval] value
  where the @racket[days], @racket[hours], @racket[minutes],
  @racket[seconds], and @racket[nanoseconds] fields are zero.
}

@defproc[(sql-day-time-interval? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @racket[sql-interval] value
  where the @racket[years] and @racket[months] fields are zero.
}

@defproc[(sql-interval->sql-time [interval sql-interval?]
                                 [failure any/c (lambda () (error ....))])
         any]{

  If @racket[interval] is a day-time interval that
  represents a time of day, returns the corresponding
  @racket[sql-time] value. In particular, the following must be true:
  @itemlist[
  @item{@racket[hours], @racket[minutes], @racket[seconds], and
    @racket[nanoseconds] must all be non-negative}
  @item{@racket[hours] must be between @racket[0] and @racket[23]}
  ]
  The corresponding constraints on @racket[minutes], etc are
  enforced by the constructor.

  If @racket[interval] is out of range, the @racket[failure] value is
  called, if it is a procedure, or returned, otherwise.
}

@defproc[(sql-time->sql-interval [time sql-time?])
         sql-day-time-interval?]{

  Converts @racket[time] to an interval. If @racket[time] has
  time-zone information, it is ignored.
}


@subsection{Bits}

The @tt{BIT} and @tt{BIT VARYING} (@tt{VARBIT}) SQL types are
represented by bit-vectors (@racketmodname[data/bit-vector]).

The following functions are provided for backwards compatibility. They
are deprecated and will be removed in a future release of Racket.

@deftogether[[
@defproc[(make-sql-bits [len exact-nonnegative-integer?]) 
         sql-bits?]
@defproc[(sql-bits? [v any/c]) boolean?]
@defproc[(sql-bits-length [b sql-bits?])
         exact-nonnegative-integer?]
@defproc[(sql-bits-ref [b sql-bits?] [i exact-nonnegative-integer?])
         boolean?]
@defproc[(sql-bits-set! [b sql-bits?] 
                        [i exact-nonnegative-integer?] 
                        [v boolean?])
         void?]
@defproc[(sql-bits->list [b sql-bits?]) (listof boolean?)]
@defproc[(sql-bits->string [b sql-bits?]) string?]
@defproc[(list->sql-bits [lst (listof boolean?)]) sql-bits?]
@defproc[(string->sql-bits [s string?]) sql-bits?]]]{

Deprecated; use @racketmodname[data/bit-vector] instead.

}

@(close-eval the-eval)
