#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/struct
          racket/sandbox
          "config.rkt"
          (for-label db db/util/datetime db/util/geometry db/util/postgresql))

@title[#:tag "util"]{Utilities}

The bindings described in this section are provided by the specific
modules below, not by @racketmodname[db] or @racketmodname[db/base].

@;{========================================}

@section[#:tag "datetime-util"]{Datetime Type Utilities}

@defmodule[db/util/datetime]

@deftogether[[
@defproc[(sql-datetime->srfi-date [t (or/c sql-date? sql-time? sql-timestamp?)])
         srfi:date?]
@defproc[(srfi-date->sql-date [d srfi:date?])
         sql-date?]
@defproc[(srfi-date->sql-time [d srfi:date?])
         sql-time?]
@defproc[(srfi-date->sql-time-tz [d srfi:date?])
         sql-time?]
@defproc[(srfi-date->sql-timestamp [d srfi:date?])
         sql-timestamp?]
@defproc[(srfi-date->sql-timestamp-tz [d srfi:date?])
         sql-timestamp?]]]{

  Converts between this library's date and time values and SRFI 19's
  date values (see @racketmodname[srfi/19]). SRFI dates store more
  information than SQL dates and times, so converting a SQL time to a
  SRFI date, for example, puts zeroes in the year, month, and day
  fields.

@(examples/results
  [(sql-datetime->srfi-date
    (query-value pgc "select time '7:30'"))
   (sql-datetime->srfi-date (make-sql-time 7 30 0 0 #f))]
  [(sql-datetime->srfi-date
    (query-value pgc "select date '25-dec-1980'"))
   (sql-datetime->srfi-date
    (make-sql-date 1980 12 25))]
  [(sql-datetime->srfi-date
    (query-value pgc "select timestamp 'epoch'"))
   (sql-datetime->srfi-date (make-sql-timestamp 1970 1 1 0 0 0 0 #f))])
}

@defproc[(sql-day-time-interval->seconds [interval sql-day-time-interval?])
         rational?]{

  Returns the length of @racket[interval] in seconds.
}


@;{========================================}

@section[#:tag "geometry"]{Geometric Types}

@defmodule[db/util/geometry]

The following structures and functions deal with geometric values
based on the OpenGIS (ISO 19125) model.

@section-index{PostGIS}

Note: Geometric columns defined using the PostGIS extension to
PostgreSQL are not directly supported. Instead, data should be
exchanged in the Well-Known Binary format; conversion of the following
structures to and from WKB format is supported by the
@racket[wkb->geometry] and @racket[geometry->wkb] functions.

@defstruct*[point
            ([x real?] [y real?])]{
  Represents an OpenGIS @tt{Point}.
}
@defstruct*[line-string
            ([points (listof point?)])]{
  Represents an OpenGIS @tt{LineString}.
}
@defstruct*[polygon
            ([exterior linear-ring?]
             [interior (listof linear-ring?)])]{
  Represents an OpenGIS @tt{Polygon}.
}
@defstruct*[multi-point ([elements (listof point?)])]{
  Represents an OpenGIS @tt{MultiPoint}, a collection of points.
}
@defstruct*[multi-line-string ([elements (listof line-string?)])]{
  Represents an OpenGIS @tt{MultiLineString}, a collection of line-strings.
}
@defstruct*[multi-polygon ([elements (listof polygon?)])]{
  Represents an OpenGIS @tt{MultiPolygon}, a collection of polygons.
}
@defstruct*[geometry-collection ([elements (listof geometry2d?)])]{
  Represents an OpenGIS @tt{GeometryCollection}, a collection of
  arbitrary geometric values.
}

@defproc[(geometry2d? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @racket[point],
  @racket[line-string], @racket[polygon], @racket[multi-point],
  @racket[multi-line-string], @racket[multi-polygon], or
  @racket[geometry-collection]; @racket[#f] othewise.
}

@defproc[(line? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @racket[line-string]
  consisting of exactly two points (cf OpenGIS @tt{Line}); @racket[#f]
  otherwise.
}

@defproc[(linear-ring? [x any/c]) boolean?]{

  Returns @racket[#t] if @racket[x] is a @racket[line-string] whose
  first and last points are equal (cf OpenGIS @tt{LinearRing});
  @racket[#f] otherwise.
}

@defproc[(geometry->wkb [g geometry2d?]
                        [#:big-endian? big-endian? (system-big-endian?)])
         bytes?]{

  Returns the Well-Known Binary (WKB) encoding of the geometric value
  @racket[g]. The @racket[big-endian?] argument determines the byte
  order used (the WKB format includes byte-order markers, so a robust
  client should accept either encoding).
}

@defproc[(wkb->geometry [b bytes?])
         geometry2d?]{

  Decodes the Well-Known Binary (WKB) representation of a geometric
  value.
}

@;{========================================}

@section[#:tag "postgresql-ext"]{PostgreSQL-specific Types}

@defmodule[db/util/postgresql]

@defstruct*[pg-array
            ([dimensions exact-nonnegative-integer?]
             [dimension-lengths (listof exact-positive-integer?)]
             [dimension-lower-bounds (listof exact-integer?)]
             [contents vector?])]{

Represents a PostrgreSQL array. The @racket[dimension-lengths] and
@racket[dimension-lower-bounds] fields are both lists of
@racket[dimensions] elements. By default, PostgreSQL array indexes
start with 1 (not 0), so @racket[dimension-lower-bounds] is typically
a list of @racket[1]s.
}

@defproc[(pg-array-ref [arr pg-array?] [index exact-integer?] ...+)
         any/c]{

Returns the element of @racket[arr] at the given position. There must
be as many @racket[index] arguments as the dimension of
@racket[arr]. Recall that PostgreSQL array indexes usually start with
1, not 0.
}

@defproc[(pg-array->list [arr pg-array?])
         list?]{

Returns a list of @racket[arr]'s contents. The dimension of
@racket[arr] must be 1; otherwise an error is raised.
}

@defproc[(list->pg-array [lst list?])
         pg-array?]{

Returns a @racket[pg-array] of dimension 1 with the contents of
@racket[lst].
}

@deftogether[[
@defstruct*[pg-box
            ([ne point?] [sw point?])]

@defstruct*[pg-path
            ([closed? boolean?] [points (listof point?)])]

@defstruct*[pg-circle
            ([center point?] [radius real?])]
]]{

These structures represent certain of PostgreSQL's built-in geometric
types that have no appropriate analogue in the OpenGIS model:
@tt{box}, @tt{path}, and @tt{circle}. The @tt{point}, @tt{lseg}, and
@tt{polygon} PostgreSQL built-in types are represented using
@racket[point], @racket[line-string] (@racket[line?]), and
@racket[polygon] structures.

Note: PostgreSQL's built-in geometric types are distinct from those
provided by the PostGIS extension library (see @secref["geometry"]).
}
