#lang scribble/doc
@(require "mz.ss"
          (for-label racket/cmdline))

@title[#:tag "logging"]{Logging}

A @deftech{logger} accepts events that contain information to be
logged for interested parties. A @deftech{log receiver} represents an
interested party that receives logged events asynchronously. Each
event has a level of importance, and a @tech{log receiver} subscribes to
logging events at a certain level of importance and higher. The
levels, in decreasing order of importance, are @scheme['fatal],
@scheme['error], @scheme['warning], @scheme['info], and
@scheme['debug].

To help organize logged events, @tech{loggers} can be named and
hierarchical. Every event reported to a logger is also propagated to
its parent (if any), but the event message is prefixed with the name
(if any) of the logger to which is was originally reported. A logger
is not required to have a parent or name.

On start-up, PLT Scheme creates an initial logger that is used to
record events from the core run-time system. For example, an
@scheme['info] event is reported for each garbage collection (see
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
       should be @scheme["none"], @scheme["fatal"], @scheme["error"],
       @scheme["warning"], @scheme["info"], or @scheme["debug"].

       The default is @scheme["error"].}

 @item{If the @indexed-envvar{PLTSYSLOG} environment variable is
       defined and is not overridden by a command-line flag, it
       determines the level of the @tech{log receiver} that propagates
       events to the system log. The possible values are the
       same as for @envvar{PLTSYSLOG}.

       The default is @scheme["none"] for Unix or @scheme["error"] for
       Windows and Mac OS X.}

]

The @scheme[current-logger] @tech{parameter} determines the
@deftech{current logger} that is used by forms such as
@scheme[log-warning]. On start-up, the initial value of this parameter
is the initial logger. The run-time system sometimes uses the current
logger to report events. For example, the bytecode compiler sometimes
reports @scheme['warning] events when it detects an expression that
would produce a run-time error if evaluated.

@; ----------------------------------------
@section{Creating Loggers}

@defproc[(logger? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{logger}, @scheme[#f]
otherwise.}


@defproc[(make-logger [name (or/c symbol? #f) #f]
                      [parent (or/c logger? #f) #f])
         logger?]{

Creates a new logger with an optional name and parent.}


@defproc[(logger-name [logger logger?]) (or/c symbol? #f)]{

Reports @scheme[logger]'s name, if any.}

@defparam[current-logger logger logger?]{

A @tech{parameter} that determines the @tech{current logger}.}


@; ----------------------------------------
@section{Logging Events}

@defproc[(log-message [logger logger?]
                      [level (or/c 'fatal 'error 'warning 'info 'debug)]
                      [message string?]
                      [data any/c])
          void?]{

Reports an event to @scheme[logger], which in turn distributes the
information to any @tech{log receivers} attached to @scheme[logger] or
its ancestors that are interested in events at @scheme[level] or
higher.

If @scheme[logger] has a name, then @scheme[message] is prefixed with
the logger's name followed by @scheme[": "] before it is sent to
receivers.}


@defproc[(log-level? [logger logger?]
                     [level (or/c 'fatal 'error 'warning 'info 'debug)])
         boolean?]{

Reports whether any @tech{log receiver} attached to @scheme[logger] or
one of its ancestors is interested in @scheme[level] events (or
potentially lower). Use this function to avoid work generating an
event for @scheme[log-message] if no receiver is interested in the
information; this shortcut is built into @scheme[log-fatal],
@scheme[log-error], @scheme[log-warning], @scheme[log-info], and
@scheme[log-debug], however, so it should not be used with those
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
@scheme[string-expr] only if the logger has receivers that are
interested in the event. In addition, the current continuation's
@tech{continuation marks} are sent to the logger with the message
string.

For each @schemekeywordfont{log-}@scheme[_level],

@schemeblock[
(@#,schemekeywordfont{log-}_level string-expr)
]

is equivalent to

@schemeblock[
(let ([l (current-logger)])
  (when (log-level? l '@#,scheme[_level])
   (log-message l '@#,scheme[_level] string-expr 
                (current-continuation-marks))))
]}

@; ----------------------------------------
@section{Receiving Logged Events}

@defproc[(log-receiver? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{log receiver}, @scheme[#f]
otherwise.}

@defproc[(make-log-receiver [logger logger?]
                            [level (or/c 'fatal 'error 'warning 'info 'debug)])
         log-receiver?]{

Creates a @tech{log receiver} to receive events of importance
@scheme[level] and higher as reported to @scheme[logger] and its
descendants.

A @tech{log receiver} is a @tech{synchronizable event}. It becomes
ready as an @tech{synchronizable event} when a logging event is
received, so use @scheme[sync] to receive an logged event. The
@tech{log receiver}'s synchronization value is a vector containing
three values: the level of the event as a symbol, an immutable string
for the event message, and an arbitrary value that was supplied as the
last argument to @scheme[log-message] when the event was logged.}
