#lang scribble/doc
@(require "mz.rkt" (for-label racket/cmdline))

@title[#:tag "logging"]{Logging}

A @deftech{logger} accepts events that contain information to be
logged for interested parties. A @deftech{log receiver} represents an
interested party that receives logged events asynchronously. Each
event has a level of importance, and a @tech{log receiver} subscribes to
logging events at a certain level of importance and higher. The
levels, in decreasing order of importance, are @racket['fatal],
@racket['error], @racket['warning], @racket['info], and
@racket['debug].

To help organize logged events, @tech{loggers} can be named and
hierarchical. Every event reported to a logger is also propagated to
its parent (if any), but the event message is prefixed with the name
(if any) of the logger to which is was originally reported. A logger
is not required to have a parent or name.

On start-up, Racket creates an initial logger that is used to
record events from the core run-time system. For example, an
@racket['info] event is reported for each garbage collection (see
@secref["gc-model"]). For this initial logger, two log receivers are
also created: one that writes events to the process's original error
output port, and one that writes events to the system log. The level
of written events in each case is system-specific, and the default can
be changed through command-line flags (see @secref["mz-cmdline"]) or
through environment variables:

@itemize[

 @item{If the @indexed-envvar{PLTSTDERR} environment variable is
       defined and is not overridden by a command-line flag, it
       determines the level of the @tech{log receiver} that propagates
       events to the original error port. The environment variable's value
       should be @racket["none"], @racket["fatal"], @racket["error"],
       @racket["warning"], @racket["info"], or @racket["debug"].

       The default is @racket["error"].}

 @item{If the @indexed-envvar{PLTSYSLOG} environment variable is
       defined and is not overridden by a command-line flag, it
       determines the level of the @tech{log receiver} that propagates
       events to the system log. The possible values are the
       same as for @envvar{PLTSTDERR}.

       The default is @racket["none"] for Unix or @racket["error"] for
       Windows and Mac OS X.}

]

The @racket[current-logger] @tech{parameter} determines the
@deftech{current logger} that is used by forms such as
@racket[log-warning]. On start-up, the initial value of this parameter
is the initial logger. The run-time system sometimes uses the current
logger to report events. For example, the bytecode compiler sometimes
reports @racket['warning] events when it detects an expression that
would produce a run-time error if evaluated.

@; ----------------------------------------
@section{Creating Loggers}

@defproc[(logger? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{logger}, @racket[#f]
otherwise.}


@defproc[(make-logger [name (or/c symbol? #f) #f]
                      [parent (or/c logger? #f) #f])
         logger?]{

Creates a new logger with an optional name and parent.}


@defproc[(logger-name [logger logger?]) (or/c symbol? #f)]{

Reports @racket[logger]'s name, if any.}

@defparam[current-logger logger logger?]{

A @tech{parameter} that determines the @tech{current logger}.}


@; ----------------------------------------
@section{Logging Events}

@defproc[(log-message [logger logger?]
                      [level (or/c 'fatal 'error 'warning 'info 'debug)]
                      [message string?]
                      [data any/c])
          void?]{

Reports an event to @racket[logger], which in turn distributes the
information to any @tech{log receivers} attached to @racket[logger] or
its ancestors that are interested in events at @racket[level] or
higher.

If @racket[logger] has a name, then @racket[message] is prefixed with
the logger's name followed by @racket[": "] before it is sent to
receivers.}


@defproc[(log-level? [logger logger?]
                     [level (or/c 'fatal 'error 'warning 'info 'debug)])
         boolean?]{

Reports whether any @tech{log receiver} attached to @racket[logger] or
one of its ancestors is interested in @racket[level] events (or
potentially lower). Use this function to avoid work generating an
event for @racket[log-message] if no receiver is interested in the
information; this shortcut is built into @racket[log-fatal],
@racket[log-error], @racket[log-warning], @racket[log-info], and
@racket[log-debug], however, so it should not be used with those
forms.

The result of this function can change if a garbage collection
determines that a log receiver is no longer accessible (and therefore
that any event information it receives will never become accessible).}

@deftogether[(
@defform[(log-fatal string-expr)]
@defform[(log-error string-expr)]
@defform[(log-warning string-expr)]
@defform[(log-info string-expr)]
@defform[(log-debug string-expr)]
)]{

Log an event with the @tech{current logger}, evaluating
@racket[string-expr] only if the logger has receivers that are
interested in the event. In addition, the current continuation's
@tech{continuation marks} are sent to the logger with the message
string.

For each @racketkeywordfont{log-}@racket[_level],

@racketblock[
(@#,racketkeywordfont{log-}_level string-expr)
]

is equivalent to

@racketblock[
(let ([l (current-logger)])
  (when (log-level? l '@#,racket[_level])
    (log-message l '@#,racket[_level] string-expr 
                 (current-continuation-marks))))
]}

@; ----------------------------------------
@section[#:tag "receiving-logged-events"]{Receiving Logged Events}

@defproc[(log-receiver? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{log receiver}, @racket[#f]
otherwise.}

@defproc[(make-log-receiver [logger logger?]
                            [level (or/c 'fatal 'error 'warning 'info 'debug)])
         log-receiver?]{

Creates a @tech{log receiver} to receive events of importance
@racket[level] and higher as reported to @racket[logger] and its
descendants.

A @tech{log receiver} is a @tech{synchronizable event}. It becomes
ready as an @tech{synchronizable event} when a logging event is
received, so use @racket[sync] to receive an logged event. The
@tech{log receiver}'s synchronization value is a vector containing
three values: the level of the event as a symbol, an immutable string
for the event message, and an arbitrary value that was supplied as the
last argument to @racket[log-message] when the event was logged.}
