#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "mz:time"]{Time}
 

@defproc[(current-seconds) exact-integer?]{

Returns the current time in seconds. This time is always an exact
integer based on a platform-specific starting date (with a
platform-specific minimum and maximum value).

The value of @scheme[(current-seconds)] increases as time passes
(increasing by 1 for each second that passes). The current time in
seconds can be compared with a time returned by
@scheme[file-or-directory-modify-seconds].}


@defproc[(seconds->date [secs-n exact-integer?]) date?]{

Takes @scheme[secs-n], a platform-specific time in seconds returned by
@scheme[current-seconds] or @scheme[file-or-directory-modify-seconds],
and returns an instance of the @scheme[date] structure type.  If
@scheme[secs-n] is too small or large, the @exnraise[exn:fail].

The value returned by @scheme[current-seconds] or
@scheme[file-or-directory-modify-seconds] is not portable among
platforms. Convert a time in seconds using @scheme[seconds->date] when
portability is needed.}

@defstruct[date ([second (integer-in 0 61)]
                 [minute (integer-in 0 59)]
                 [hour (integer-in 0 23)]
                 [day (integer-in 1 31)]
                 [month (integer-in 1 12)]
                 [year nonnegative-exact-integer?]
                 [week-day (integer-in 0 6)]
                 [year-day (integer-in 0 365)]
                 [dst? boolean?]
                 [time-zone-offset exact-integer?])
                #:inspector #f]{

Represents a date. For the @scheme[second] field, values of
@scheme[60] and @scheme[61] are for unusual, but possible for
leap-seconds. The @scheme[year-day] field reaches @scheme[365] only in
leap years.

The @scheme[time-zone-offset] field reports the number of seconds east
of GMT for the current time zone (e.g., Pacific Standard Time is
@scheme[-28800]), an exact integer.

The value produced for the @scheme[time-zone-offset] field tends to be
sensitive to the value of the @envvar{TZ} environment variable,
especially on Unix platforms; consult the system documentation
(usually under @tt{tzset}) for details.}


@defproc[(current-milliseconds) exact-integer?]{

Returns the current ``time'' in fixnum milliseconds (possibly
negative). This time is based on a platform-specific starting date or
on the machine's startup time. Since the result is a fixnum, the value
increases only over a limited (though reasonably long) time.}


@defproc[(current-inexact-milliseconds) real?]{

Returns the current ``time'' in positive milliseconds, not necessarily
an integer. This time is based on a platform-specific starting date or
on the machine's startup time, but it never decreases (until the
machine is turned off).}


@defproc[(current-process-milliseconds) exact-integer?]{

Returns the amount of processor time in fixnum milliseconds that has
been consumed by the Scheme process on the underlying operating
system. (Under @|AllUnix|, this includes both user and system time.)
The precision of the result is platform-specific, and since the result
is a fixnum, the value increases only over a limited (though
reasonably long) time.}


@defproc[(current-gc-milliseconds) exact-integer?]{

Returns the amount of processor time in fixnum milliseconds that has
been consumed by Scheme's garbage collection so far. This time is a
portion of the time reported by
@scheme[(current-process-milliseconds)].}


@defproc[(time-apply [proc procedure?]
                     [arg any/c] ...)
         (values exact-integer?
                 exact-integer?
                 exact-integer?
                 list?)]{

Collects timing information for a procedure application.

Four values are returned: a list containing the result(s) of applying
@scheme[proc], the number of milliseconds of CPU time required to
obtain this result, the number of ``real'' milliseconds required for
the result, and the number of milliseconds of CPU time (included in
the first result) spent on garbage collection.

The reliability of the timing numbers depends on the platform. If
multiple MzScheme threads are running, then the reported time may
include work performed by other threads.}

@defform[(time expr)]{

Reports @scheme[time-apply]-style timing information for the
evaluation of @scheme[expr] directly to the current output port.  The
result is the result of @scheme[expr].}
