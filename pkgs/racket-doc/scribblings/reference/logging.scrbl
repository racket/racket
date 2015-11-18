#lang scribble/doc
@(require "mz.rkt" 
          scribble/bnf
          (for-label racket/cmdline))

@title[#:tag "logging"]{Logging}

A @deftech{logger} accepts events that contain information to be
logged for interested parties. A @deftech{log receiver} represents an
interested party that receives logged events asynchronously. Each
event has a topic and level of detail, and a @tech{log receiver} subscribes to
logging events at a certain level of detail (and higher) for a specific topic or for all topics. The
levels, in increasing order of detail, are @racket['fatal],
@racket['error], @racket['warning], @racket['info], and
@racket['debug].

To help organize logged events, a @tech{logger} can have a default topic and/or
a parent logger. Every event reported to a logger is also propagated to
its parent (if any), while the event message is prefixed with the logger's topic (if
any) if the message doesn't already have a topic. Furthermore, events that are propagated
from a logger to its parent can be filtered by level and topic.

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
       @nonterm{level}@litchar["@"]@nonterm{topic}, which prints events
       whose topics match @nonterm{topic} only at the given
       @nonterm{level} or higher (where a @nonterm{topic} contains any
       character other than a space or @litchar["@"]). For example,
       the value @racket["error debug@GC"] prints all events at the
       @racket['error] level and higher, but prints events for the topic
       topic @racket['GC] at the @racket['debug] level and
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


@defproc[(make-logger [topic (or/c symbol? #f) #f]
                      [parent (or/c logger? #f) #f]
                      [propagate-level log-level/c 'debug]
                      [propagate-topic (or/c #f symbol?) #f]
                      ... ...)
         logger?]{

Creates a new @tech{logger} with an optional topic and parent.

The optional @racket[propagate-level] and @racket[propagate-topic]
arguments constrain the events that are propagated from the new logger
to @racket[parent] (when @racket[parent] is not @racket[#f]) in the
same way that events are described for a log receiver in
@racket[make-log-receiver]. By default, all events are propagated to
@racket[parent].

@history[#:changed "6.1.1.3" @elem{Removed an optional argument to
                                   specify a notification callback,
                                   and added @racket[propagate-level] and
                                   @racket[propagate-topic] constraints for
                                   events to propagate.}]}


@defproc[(logger-name [logger logger?]) (or/c symbol? #f)]{

Reports @racket[logger]'s default topic, if any.}


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
form also defines @racket[id]@racketidfont{-logger}, which is a logger with
default topic @racket['@#,racket[id]] that is a child of @racket[(current-logger)];
the @racketkeywordfont{log-}@racket[id]@racketkeywordfont{-fatal},
@|etc| forms use this new logger. The new logger is
created when @racket[define-logger] is evaluated.}

@; ----------------------------------------
@section{Logging Events}

@defproc[(log-message [logger logger?]
                      [level log-level/c]
                      [topic (or/c symbol? #f) (logger-name logger)]
                      [message string?]
                      [data any/c]
                      [prefix-message? any/c #t])
          void?]{

Reports an event to @racket[logger], which in turn distributes the
information to any @tech{log receivers} attached to @racket[logger] or
its ancestors that are interested in events at @racket[level] or
higher.

@tech{Log receivers} can filter events based on @racket[topic].  In
addition, if @racket[topic] and @racket[prefix-message?] are not
@racket[#f], then @racket[message] is prefixed with the topic followed
by @racket[": "] before it is sent to receivers.

@history[#:changed "6.0.1.10" @elem{Added the @racket[prefix-message?] argument.}]}


@defproc[(log-level? [logger logger?]
                     [level log-level/c]
                     [topic (or/c symbol? #f) #f])
         boolean?]{

Reports whether any @tech{log receiver} attached to @racket[logger] or
one of its ancestors is interested in @racket[level] events (or
potentially lower) for @racket[topic]. If @racket[topic] is @racket[#f],
the result indicates whether a @tech{log receiver} is interested in
events at @racket[level] for any topic.

Use this function to avoid work generating an
event for @racket[log-message] if no receiver is interested in the
information; this shortcut is built into @racket[log-fatal],
@racket[log-error], @racket[log-warning], @racket[log-info],
@racket[log-debug], and forms bound by @racket[define-logger],
however, so it should not be used with those forms.

The result of this function can change if a garbage collection
determines that a log receiver is no longer accessible (and therefore
that any event information it receives will never become accessible).

@history[#:changed "6.1.1.3" @elem{Added the @racket[topic] argument.}]}

@defproc[(log-max-level [logger logger?]
                        [topic (or/c symbol? #f) #f])
         (or/c log-level/c #f)]{

Similar to @racket[log-level?], but reports the maximum-detail level of logging for
which @racket[log-level?] on @racket[logger] and @racket[topic] returns @racket[#t]. The
result is @racket[#f] if @racket[log-level?] with @racket[logger] and @racket[topic]
currently returns @racket[#f] for all levels.

@history[#:changed "6.1.1.3" @elem{Added the @racket[topic] argument.}]}


@defproc[(log-all-levels [logger logger?])
         (list/c (or/c #f log-level/c)
                 (or/c #f symbol?)
                 ... ...)]{

Summarizes the possible results of @racket[log-max-level] on all
possible @tech{interned} symbols. The result list contains a sequence
of symbols and @racket[#f], where the first, third, etc., list element
corresponds to a level, and the second, fourth, etc., list element
indicates a corresponding topic. The level is the result that
@racket[log-max-level] would produce for the topic, where the level for
the @racket[#f] topic (which is always present in the result list)
indicates the result for any @tech{interned}-symbol topic that does not
appear in the list.

The result is suitable as a sequence of arguments to
@racket[make-log-receiver] (after a @tech{logger} argument) to create
a new receiver for events that currently have receivers in @racket[logger].

@history[#:added "6.1.1.4"]}


@defproc[(log-level-evt [logger logger?]) evt?]{

Creates a @tech{synchronizable event} that is @tech{ready for
synchronization} when the result of @racket[log-level?],
@racket[log-max-level], or @racket[log-all-levels] can be different
than before @racket[log-level-evt] was called. The event's
@tech{synchronization result} is the event itself.

The condition reported by the event is a conservative approximation:
the event can become @tech{ready for synchronization} even if the
results of @racket[log-level?], @racket[log-max-level], and
@racket[log-all-levels] are unchanged. Nevertheless, the expectation
is that events produced by @racket[log-level-evt] become ready infrequently,
because they are triggered by the creation of a log receiver.

@history[#:added "6.1.1.4"]}


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
should generally use a logger for a specific topic---typically through
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
                            [level log-level/c]
                            [topic (or/c #f symbol?) #f]
                            ... ...)
         log-receiver?]{

Creates a @tech{log receiver} to receive events of detail
@racket[level] and lower as reported to @racket[logger] and its
descendants, as long as either @racket[topic] is @racket[#f] or the
event's topic matches @racket[topic].

A @tech{log receiver} is a @tech{synchronizable event}. It becomes
@tech{ready for synchronization} when a logging event is
received, so use @racket[sync] to receive a logged event. The
@tech{log receiver}'s @tech{synchronization result} is an immutable vector containing
four values: the level of the event as a symbol, an immutable string
for the event message, an arbitrary value that was supplied as the
last argument to @racket[log-message] when the event was logged, and a
symbol or @racket[#f] for the event topic.

Multiple pairs of @racket[level] and @racket[topic] can be provided to
indicate different specific @racket[level]s for different
@racket[topic]s (where @racket[topic] defaults to @racket[#f] only for
the last given @racket[level]). A @racket[level] for a @racket[#f]
@racket[topic] applies only to events whose topic does not match any other
provided @racket[topic]. If the same @racket[topic] is provided multiple
times, the @racket[level] provided with the last instance in the
argument list takes precedence.}


@; ----------------------------------------
@section{Additional Logging Functions}

@note-lib[racket/logging]
@(require (for-label racket/logging))
@(define log-eval (make-base-eval))
@(interaction-eval #:eval log-eval
                   (require racket/logging))

@defproc[(log-level/c [v any/c])
         boolean?]{
Returns @racket[#t] if @racket[v] is a valid logging level (@racket['none],
@racket['fatal], @racket['error], @racket['warning], @racket['info], or
@racket['debug]), @racket[#f] otherwise.

@history[#:added "6.3"]{}
}

@defproc[(with-intercepted-logging
           [interceptor (-> (vector/c
                              log-level/c
                              string?
                              any/c
                              (or/c symbol? #f))
                             any)]
           [proc (-> any)]
           [level log-level/c]
           [topic (or/c #f symbol?) #f]
           ... ...)
         any]{

Runs @racket[proc], calling @racket[interceptor] on any log event that would
be received by @racket[(make-log-receiver (current-logger) level topic ... ...)].
Returns whatever @racket[proc] returns.

@defexamples[
#:eval log-eval
(let ([warning-counter 0])
  (with-intercepted-logging
    (lambda (l)
      (when (eq? (vector-ref l 0) ; actual level
                 'warning)
        (set! warning-counter (add1 warning-counter))))
    (lambda ()
      (log-warning "Warning!")
      (log-warning "Warning again!")
      (+ 2 2))
    'warning)
  warning-counter)]

@history[#:added "6.3"]{}}

@defproc[(with-logging-to-port
           [port output-port?] [proc (-> any)]
           [level log-level/c]
           [topic (or/c #f symbol?) #f]
           ... ...)
         any]{

Runs @racket[proc], outputting any logging that would be received by
@racket[(make-log-receiver (current-logger) level topic ... ...)] to @racket[port].
Returns whatever @racket[proc] returns.

@defexamples[
#:eval log-eval
(let ([my-log (open-output-string)])
  (with-logging-to-port my-log
    (lambda ()
      (log-warning "Warning World!")
      (+ 2 2))
    'warning)
  (get-output-string my-log))]

@history[#:added "6.3"]{}}
