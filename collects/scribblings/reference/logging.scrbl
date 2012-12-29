#lang scribble/doc
@(require "mz.rkt" 
          scribble/bnf
          (for-label racket/cmdline))

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
its parent (if any), but the event message is prefixed with a name (if
any) that is typically the name of the logger to which is was
originally reported. A logger is not required to have a parent or
name.

On start-up, Racket creates an initial logger that is used to
record events from the core run-time system. For example, an
@racket['debug] event is reported for each garbage collection (see
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
       events to the original error port.

       The environment variable's value can be a @nonterm{level}:
       @litchar{none}, @litchar{fatal}, @litchar{error},
       @litchar{warning}, @litchar{info}, or @litchar{debug}; all
       events the corresponding level of higher are printed. After an
       initial @nonterm{level}, the value can contain space-separated
       specifications of the form
       @nonterm{level}@litchar["@"]@nonterm{name}, which prints events
       whose names match @nonterm{name} only at the given
       @nonterm{level} or higher (where a @nonterm{name} contains any
       character other than a space or @litchar["@"]). For example,
       the value @racket["error debug@GC"] prints all events at the
       @racket['error] level and higher, but prints events
       named @racket['GC] at the @racket['debug] level and
       higher (which includes all levels).

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
                      [parent (or/c logger? #f) #f]
                      [notify-callback (vector? . -> . any/c)])
         logger?]{

Creates a new @tech{logger} with an optional name and parent.

If @racket[notify-callback] is provided, then it is called (under a
@tech{continuation barrier}) whenever an event is logged to the result
@tech{logger} or one of its descendants, but only if some @tech{log
receiver} is inteested in the event in the same sense as
@racket[log-level?]. The event is not propagated to any @tech{log
receivers} until @racket[notify-callback] returns.}


@defproc[(logger-name [logger logger?]) (or/c symbol? #f)]{

Reports @racket[logger]'s name, if any.}

@defparam[current-logger logger logger?]{

A @tech{parameter} that determines the @tech{current logger}.}

@defform[(define-logger id)]{

Defines @racketkeywordfont{log-}@racket[id]@racketkeywordfont{-fatal},
@racketkeywordfont{log-}@racket[id]@racketkeywordfont{-error},
@racketkeywordfont{log-}@racket[id]@racketkeywordfont{-warning},
@racketkeywordfont{log-}@racket[id]@racketkeywordfont{-info}, and
@racketkeywordfont{log-}@racket[id]@racketkeywordfont{-debug} as forms
like @racket[log-fatal], @racket[log-error],@racket[log-warning],
@racket[log-info], and @racket[log-debug]. The @racket[define-logger]
form also defines @racket[id]@racketidfont{-logger}, which is a logger named
@racket['@#,racket[id]] that is a child of @racket[(current-logger)];
the @racketkeywordfont{log-}@racket[id]@racketkeywordfont{-fatal},
@|etc| forms use this new logger. The new logger is
created when @racket[define-logger] is evaluated.}

@; ----------------------------------------
@section{Logging Events}

@defproc[(log-message [logger logger?]
                      [level (or/c 'fatal 'error 'warning 'info 'debug)]
                      [name (or/c symbol? #f) (object-name logger)]
                      [message string?]
                      [data any/c])
          void?]{

Reports an event to @racket[logger], which in turn distributes the
information to any @tech{log receivers} attached to @racket[logger] or
its ancestors that are interested in events at @racket[level] or
higher.

@tech{Log receivers} can filter events based on @racket[name].  In
addition, if @racket[name] is not @racket[#f], then @racket[message]
is prefixed with the name followed by @racket[": "] before it is sent
to receivers.}


@defproc[(log-level? [logger logger?]
                     [level (or/c 'fatal 'error 'warning 'info 'debug)])
         boolean?]{

Reports whether any @tech{log receiver} attached to @racket[logger] or
one of its ancestors is interested in @racket[level] events (or
potentially lower). Use this function to avoid work generating an
event for @racket[log-message] if no receiver is interested in the
information; this shortcut is built into @racket[log-fatal],
@racket[log-error], @racket[log-warning], @racket[log-info],
@racket[log-debug], and forms bound by @racket[define-logger],
however, so it should not be used with those forms.

The result of this function can change if a garbage collection
determines that a log receiver is no longer accessible (and therefore
that any event information it receives will never become accessible).}


@defproc[(log-max-level [logger logger?])
         (or/c #f 'fatal 'error 'warning 'info 'debug)]{

Similar to @racket[log-level?], but reports the maximum level of logging for
which @racket[log-level?] on @racket[logger] returns @racket[#t]. The
result is @racket[#f] if @racket[log-level?] with @racket[logger]
currently returns @racket[#f] for all levels.}


@deftogether[(
@defform*[[(log-fatal string-expr)
           (log-fatal format-string-expr v ...)]]
@defform*[[(log-error string-expr)
           (log-error format-string-expr v ...)]]
@defform*[[(log-warning string-expr)
           (log-warning format-string-expr v ...)]]
@defform*[[(log-info string-expr)
           (log-info format-string-expr v ...)]]
@defform*[[(log-debug string-expr)
           (log-debug format-string-expr v ...)]]
)]{

Log an event with the @tech{current logger}, evaluating
@racket[string-expr] or @racket[(format format-string-expr v ...)]
only if the logger has receivers that are interested in the event. In
addition, the current continuation's @tech{continuation marks} are
sent to the logger with the message string.

These form are convenient for using the current logger, but libraries
should generally use a specifically named logger---typically through
similar convenience forms generated by @racket[define-logger].

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
]

while

@racketblock[
(@#,racketkeywordfont{log-}_level format-string-expr v ...)
]

is equivalent to

@racketblock[
(@#,racketkeywordfont{log-}_level (format format-string-expr v ...))
]}

@; ----------------------------------------
@section[#:tag "receiving-logged-events"]{Receiving Logged Events}

@defproc[(log-receiver? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{log receiver}, @racket[#f]
otherwise.}

@defproc[(make-log-receiver [logger logger?]
                            [level (or/c 'none 'fatal 'error 'warning 'info 'debug)]
                            [name (or/c #f symbol?) #f]
                            ... ...)
         log-receiver?]{

Creates a @tech{log receiver} to receive events of importance
@racket[level] and higher as reported to @racket[logger] and its
descendants, as long as either @racket[name] is @racket[#f] or the
event's name matches @racket[name].

A @tech{log receiver} is a @tech{synchronizable event}. It becomes
@tech{ready for synchronization} when a logging event is
received, so use @racket[sync] to receive an logged event. The
@tech{log receiver}'s @tech{synchronization result} is an immutable vector containing
four values: the level of the event as a symbol, an immutable string
for the event message, an arbitrary value that was supplied as the
last argument to @racket[log-message] when the event was logged, and a
symbol or @racket[#f] for the event name (where a symbol is usually
the name of the original logger for the event).

Multiple pairs of @racket[level] and @racket[name] can be provided to
indicate different specific @racket[level]s for different
@racket[name]s (where @racket[name] defaults to @racket[#f] only for
the last given @racket[level]). A @racket[level] for a @racket[#f]
@racket[name] applies only to events whose names do not match any other
provided @racket[name]. If the same @racket[name] is provided multiple
times, the @racket[level] provided with the last instance in the
argument list takes precedence.}

