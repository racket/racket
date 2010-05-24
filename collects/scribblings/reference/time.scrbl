#lang scribble/doc
@(require "mz.ss"
          (for-label racket/date))

@title[#:tag "time"]{Time}
 
@defproc[(current-seconds) exact-integer?]{

Returns the current time in seconds. This time is always an exact
integer based on a platform-specific starting date (with a
platform-specific minimum and maximum value).

The value of @racket[(current-seconds)] increases as time passes
(increasing by 1 for each second that passes). The current time in
seconds can be compared with a time returned by
@racket[file-or-directory-modify-seconds].}

@defproc[(seconds->date [secs-n exact-integer?]) date?]{

Takes @racket[secs-n], a platform-specific time in seconds returned by
@racket[current-seconds] or @racket[file-or-directory-modify-seconds],
and returns an instance of the @racket[date] structure type.  If
@racket[secs-n] is too small or large, the @exnraise[exn:fail].

The value returned by @racket[current-seconds] or
@racket[file-or-directory-modify-seconds] is not portable among
platforms. Convert a time in seconds using @racket[seconds->date] when
portability is needed.}

@defstruct[date ([second (integer-in 0 61)]
                 [minute (integer-in 0 59)]
                 [hour (integer-in 0 23)]
                 [day (integer-in 1 31)]
                 [month (integer-in 1 12)]
                 [year exact-integer?]
                 [week-day (integer-in 0 6)]
                 [year-day (integer-in 0 365)]
                 [dst? boolean?]
                 [time-zone-offset exact-integer?])
                #:inspector #f]{

Represents a date. For the @racket[second] field, values of
@racket[60] and @racket[61] are for unusual, but possible for
leap-seconds. The @racket[year-day] field reaches @racket[365] only in
leap years.

The @racket[time-zone-offset] field reports the number of seconds east
of GMT for the current time zone (e.g., Pacific Standard Time is
@racket[-28800]), an exact integer.

The value produced for the @racket[time-zone-offset] field tends to be
sensitive to the value of the @envvar{TZ} environment variable,
especially on Unix platforms; consult the system documentation
(usually under @tt{tzset}) for details.

See also the @racketmodname[racket/date] library.}


@defproc[(current-milliseconds) exact-integer?]{

Returns the current ``time'' in @tech{fixnum} milliseconds (possibly
negative). This time is based on a platform-specific starting date or
on the machine's startup time. Since the result is a @tech{fixnum},
the value increases only over a limited (though reasonably long)
time.}


@defproc[(current-inexact-milliseconds) real?]{

Returns the current time in milliseconds since midnight UTC, January
1, 1970.}


@defproc[(current-process-milliseconds [thread (or/c thread? #f)]) 
         exact-integer?]{

Returns an amount of processor time in @tech{fixnum} milliseconds
that has been consumed by the Racket process on the underlying
operating system. (Under @|AllUnix|, this includes both user and
system time.)  If @racket[thread] is @racket[#f], the reported time
is for all Racket threads, otherwise the result is specific to the
time while @racket[thread] ran.
The precision of the result is platform-specific, and
since the result is a @tech{fixnum}, the value increases only over a
limited (though reasonably long) time.}


@defproc[(current-gc-milliseconds) exact-integer?]{

Returns the amount of processor time in @tech{fixnum} milliseconds
that has been consumed by Racket's garbage collection so far. This
time is a portion of the time reported by
@racket[(current-process-milliseconds)], and is similarly limited.}


@defproc[(time-apply [proc procedure?]
                     [lst list?])
         (values list?
                 exact-integer?
                 exact-integer?
                 exact-integer?)]{

Collects timing information for a procedure application.

Four values are returned: a list containing the result(s) of applying
@racket[proc] to the arguments in @racket[lst], the number of milliseconds of
CPU time required to obtain this result, the number of ``real'' milliseconds
required for the result, and the number of milliseconds of CPU time (included
in the first result) spent on garbage collection.

The reliability of the timing numbers depends on the platform. If
multiple Racket threads are running, then the reported time may
include work performed by other threads.}

@defform[(time expr)]{

Reports @racket[time-apply]-style timing information for the
evaluation of @racket[expr] directly to the current output port.  The
result is the result of @racket[expr].}

@; ----------------------------------------------------------------------

@section[#:tag "date-string"]{Date Utilities}

@note-lib-only[racket/date]

@defproc[(current-date) date?]{
                         
An abbreviation for @racket[(seconds->date (current-seconds))].}

@defproc[(date->string [date date?] [time? any/c #f]) string?]{

Converts a date to a string. The returned string contains the time of
day only if @racket[time?]. See also @racket[date-display-format].}


@defparam[date-display-format format (or/c 'american
                                           'chinese
                                           'german
                                           'indian
                                           'irish
                                           'iso-8601
                                           'rfc2822
                                           'julian)]{

Parameter that determines the date string format. The initial format
is @racket['american].}

@defproc[(date->seconds [date date?]) exact-integer?]{
Finds the representation of a date in platform-specific seconds.  If
the platform cannot represent the specified date, an error is
signaled, otherwise an integer is returned. }

@defproc[(find-seconds [second (integer-in 0 61)]
                       [minute (integer-in 0 59)]
                       [hour (integer-in 0 23)]
                       [day (integer-in 1 31)]
                       [month (integer-in 1 12)]
                       [year exact-nonnegative-integer?])
         exact-integer?]{

Finds the representation of a date in platform-specific seconds. The
arguments correspond to the fields of the @racket[date] structure. If
the platform cannot represent the specified date, an error is
signaled, otherwise an integer is returned.}


@defproc[(date->julian/scalinger [date date?]) exact-integer?]{

Converts a date structure (up to 2099 BCE Gregorian) into a Julian
date number. The returned value is not a strict Julian number, but
rather Scalinger's version, which is off by one for easier
calculations.}


@defproc[(julian/scalinger->string [date-number exact-integer?])
         string?]{

Converts a Julian number (Scalinger's off-by-one version) into a
string.}
