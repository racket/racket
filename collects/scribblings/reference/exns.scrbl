#lang scribble/doc
@(require scribble/bnf "mz.rkt" (for-label racket/fixnum))

@title[#:tag "exns"]{Exceptions}

@guideintro["exns"]{exceptions}

See @secref["exn-model"] for information on the Racket exception
model. It is based on a proposal by Friedman, Haynes, and Dybvig
@cite["Friedman95"].

Whenever a primitive error occurs in Racket, an exception is
raised.  The value that is passed to the current @tech{exception
handler} for a primitive error is always an instance of the
@racket[exn] structure type. Every @racket[exn] structure value has a
@racket[message] field that is a string, the primitive error message.
The default exception handler recognizes exception values with the
@racket[exn?] predicate and passes the error message to the current
@tech{error display handler} (see @racket[error-display-handler]).

Primitive procedures that accept a procedure argument with a
particular required arity (e.g., @racket[call-with-input-file],
@racket[call/cc]) check the argument's arity immediately, raising
@racket[exn:fail:contract] if the arity is incorrect.

@;----------------------------------------------------------------------
@section[#:tag "err-msg-conventions"]{Error Message Conventions}

Racket's @deftech{error message convention} is to produce error
messages with the following shape:

@racketblock[
  @#,nonterm{srcloc}: @#,nonterm{name}: @#,nonterm{message}@#,tt{;}
   @#,nonterm{continued-message} ...
    @#,nonterm{field}: @#,nonterm{detail}
    ...
]

The message starts with an optional source location, @nonterm{srcloc},
which is followed by a colon and space when present. The message
continues with an optional @nonterm{name} that usually identifies the
complaining function, syntactic form, or other entity, but may also
refer to an entity being complained about; the @nonterm{name} is also
followed by a colon and space when present.

The @nonterm{message} should be relatively short, and it should be
largely independent of specific values that triggered the error. More
detailed explanation that requires multiple lines should continue with
each line indented by a single space, in which case @nonterm{message}
should end in a semi-colon (but the semi-colon should be omitted if
@nonterm{continued-message} is not present). Message text should be
lowercase---using semi-colons to separate sentences if needed,
although long explanations may be better deferred to extra fields.

Specific values that triggered the error or other helpful information
should appear in separate @nonterm{field} lines, each of which is
indented by two spaces. If a @nonterm{detail} is especially long or
takes multiple lines, it should start on its own line after the
@nonterm{field} label, and each of its lines should be indented by
three spaces. Field names should be all lowercase.

A @nonterm{field} name should end with @litchar{...} if the field
provides relatively detailed information that might be distracting in
common cases but useful in others. For example, when a contract
failure is reported for a particular argument of a function, other
arguments to the function might be shown in an ``other arguments...''
field. The intent is that fields whose names end in @litchar{...}
might be hidden by default in an environment such as DrRacket.

Make @nonterm{field} names as short as possible, relying on
@nonterm{message} or @nonterm{continued message} text to clarify the
meaning for a field. For example, prefer ``given'' to ``given turtle''
as a field name, where @nonterm{message} is something like ``given
turtle is too sleepy'' to clarify that ``given'' refers to a turtle.

@;------------------------------------------------------------------------
@section[#:tag "errorproc"]{Raising Exceptions}

@defproc[(raise [v any/c] [barrier? any/c #t]) any]{

Raises an exception, where @racket[v] represents the exception being
raised. The @racket[v] argument can be anything; it is passed to the
current @tech{exception handler}.

If @racket[barrier?] is true, then the call to the @tech{exception
handler} is protected by a @tech{continuation barrier}, so that
multiple returns/escapes are impossible. All exceptions raised by
@racketmodname[racket] functions effectively use @racket[raise] with a
@racket[#t] value for @racket[barrier?].

Breaks are disabled from the time the exception is raised until the
exception handler obtains control, and the handler itself is
@racket[parameterize-break]ed to disable breaks initially; see
@secref["breakhandler"] for more information on breaks.

@examples[
(with-handlers ([number? (lambda (n)
                           (+ n 5))])
  (raise 18 #t))
(define-struct (my-exception exn:fail:user) ())
(with-handlers ([my-exception? (lambda (e)
                                 #f)])
  (+ 5 (raise (make-my-exception
                "failed"
                (current-continuation-marks)))))
(raise 'failed #t)
]}

@defproc*[([(error [sym symbol?]) any]
           [(error [msg string?] [v any/c] ...) any]
           [(error [src symbol?] [format string?] [v any/c] ...) any])]{

Raises the exception @racket[exn:fail], which contains an error
string. The different forms produce the error string in different
ways:

@itemize[

 @item{@racket[(error sym)] creates a message string by concatenating
  @racket["error: "] with the string form of @racket[sym]. Use this
  form sparingly.}

 @item{@racket[(error msg v ...)] creates a message string by
 concatenating @racket[msg] with string versions of the @racket[v]s
 (as produced by the current error value conversion handler; see
 @racket[error-value->string-handler]). A space is inserted before
 each @racket[v]. Use this form sparingly, because it does not conform
 well to Racket's @tech{error message conventions}; consider
 @racket[raise-arguments-error], instead. }

 @item{@racket[(error src frmat v ...)] creates a
 message string equivalent to the string created by

  @racketblock[
  (format (string-append "~s: " frmat) src v ...)
  ]

 When possible, use functions such as @racket[raise-argument-error],
 instead, which construct messages that follow Racket's @tech{error message
 conventions}.}

]

In all cases, the constructed message string is passed to
@racket[make-exn:fail], and the resulting exception is raised.

@examples[
(error 'failed)
(error "failed" 23 'pizza (list 1 2 3))
(error 'method-a "failed because ~a" "no argument supplied")
]}


@defproc*[([(raise-user-error [sym symbol?]) any]
           [(raise-user-error [msg string?] [v any/c] ...) any]
           [(raise-user-error [src symbol?] [format string?] [v any/c] ...) any])]{

Like @racket[error], but constructs an exception with
@racket[make-exn:fail:user] instead of @racket[make-exn:fail]. The
default @tech{error display handler} does not show a ``stack trace'' for
@racket[exn:fail:user] exceptions (see @secref["contmarks"]), so
@racket[raise-user-error] should be used for errors that are intended
for end users.}


@defproc*[([(raise-argument-error [name symbol?] [expected string?] [v any/c]) any]
           [(raise-argument-error [name symbol?] [expected string?] [bad-pos exact-nonnegative-integer?] [v any/c] ...) any])]{

Creates an @racket[exn:fail:contract] value and @racket[raise]s it as
an exception.  The @racket[name] argument is used as the source
procedure's name in the error message. The @racket[expected] argument
is used as a description of the expected contract (i.e., as a string,
but the string is intended to contain a contract expression).

In the first form, @racket[v] is the value received by the procedure
that does not have the expected type.

In the second form, the bad argument is indicated by an index
@racket[bad-pos] (counting from @math{0}), and all of the original
arguments @racket[v] are provided (in order). The resulting error
message names the bad argument and also lists the other arguments. If
@racket[bad-pos] is not less than the number of @racket[v]s, the
@exnraise[exn:fail:contract].

@examples[
(define (feed-machine bits)
  (if (not (integer? bits))
    (raise-argument-error 'feed-machine "integer?" bits)
    "fed the machine"))
(feed-machine 'turkey)
(define (feed-cow animal)
  (if (not (eq? animal 'cow))
    (raise-argument-error 'feed-cow "'cow" animal)
    "fed the cow"))
(feed-cow 'turkey)
(define (feed-animals cow sheep goose cat)
  (if (not (eq? goose 'goose))
    (raise-argument-error 'feed-animals "'goose" 2 cow sheep goose cat)
    "fed the animals"))
(feed-animals 'cow 'sheep 'dog 'cat)
]}


@defproc*[([(raise-result-error [name symbol?] [expected string?] [v any/c]) any]
           [(raise-result-error [name symbol?] [expected string?] [bad-pos exact-nonnegative-integer?] [v any/c] ...) any])]{

Like @racket[raise-argument-error], but the error message describe @racket[v]
as a ``result'' instead of an ``argument.''}


@defproc[(raise-arguments-error [name symbol?] [message string?]
                                [field string?] [v any/c] 
                                ... ...) any]{

Creates an @racket[exn:fail:contract] value and @racket[raise]s it as
an exception.  The @racket[name] is used as the source procedure's
name in the error message. The @racket[message] is the error
message; if @racket[message] contains newline characters, each extra line should be
suitably indented (with one extra space at the start of each line), but it should not end with a newline character.
Each @racket[field] must have a corresponding @racket[v],
and the two are rendered on their own
line in the error message, with each @racket[v] formatted 
using the error value conversion handler (see
@racket[error-value->string-handler]).

@examples[
  (raise-arguments-error 'eat 
                         "fish is smaller than its given meal"
                         "fish" 12
                         "meal" 13)
]}


@defproc[(raise-range-error [name symbol?] [type-description string?] [index-prefix string?]
                            [index exact-integer?] [in-value any/c]
                            [lower-bound exact-integer?] [upper-bound exact-integer?]
                            [alt-lower-bound (or/c #f exact-integer?)])
         any]{

Creates an @racket[exn:fail:contract] value and @racket[raise]s it as
an exception to report an out-of-range error. The @racket[type-description]
string describes the value for which the index is meant to select an element,
and @racket[index-prefix] is a prefix for the word ``index.'' The @racket[index]
argument is the rejected index. The @racket[in-value] argument is the value
for which the index was meant. The @racket[lower-bound] and @racket[upper-bound]
arguments specify the valid range of indices, inclusive; if @racket[upper-bound]
is below @racket[lower-bound], the value is characterized as ``empty.'' If @racket[alt-lower-bound]
is not @racket[#f], and if @racket[index] is between @racket[alt-lower-bound]
and @racket[upper-bound], then the error is report as @racket[index] being less
than the ``starting'' index @racket[lower-bound].

Since @racket[upper-bound] is inclusive, a typical value is @emph{one
less than} the size of a collection---for example, @racket[(sub1
(vector-length _vec))], @racket[(sub1 (length _lst))], and so on.

@examples[
(raise-range-error 'vector-ref "vector" "starting " 5 #(1 2 3 4) 0 3)
(raise-range-error 'vector-ref "vector" "ending " 5 #(1 2 3 4) 0 3)
(raise-range-error 'vector-ref "vector" "" 3 #() 0 -1)
(raise-range-error 'vector-ref "vector" "ending " 1 #(1 2 3 4) 2 3 0)
]}



@defproc*[([(raise-type-error [name symbol?] [expected string?] [v any/c]) any]
           [(raise-type-error [name symbol?] [expected string?] [bad-pos exact-nonnegative-integer?] [v any/c] ...) any])]{

Like @racket[raise-argument-error], but with Racket's old formatting
conventions, and where @racket[expected] is used as a ``type''
description instead of a contract expression. Use @racket[raise-argument-error]
or @racket[raise-result-error], instead.}


@defproc[(raise-mismatch-error [name symbol?] [message string?] [v any/c] 
                               ...+ ...+) any]{

Similar to @racket[raise-arguments-error], but using Racket's old
formatting conventions, with a required @racket[v] immediately
after the first @racket[message] string, and with further @racket[message]
strings that are spliced into the message without line breaks or
space. Use @racket[raise-arguments-error], instead.}


@defproc[(raise-arity-error [name (or/c symbol? procedure?)]
                            [arity-v (or/c exact-nonnegative-integer?
                                           arity-at-least?
                                           (listof
                                            (or/c exact-nonnegative-integer?
                                                  arity-at-least?)))]
                            [arg-v any/c #f] ...)
         any]{

Creates an @racket[exn:fail:contract:arity] value and @racket[raise]s
it as an exception.  The @racket[name] is used for the source
procedure's name in the error message. 

The @racket[arity-v] value must
be a possible result from @racket[procedure-arity], except
that it does not have to be normalized (see @racket[procedure-arity?] for
the details of normalized arities); @racket[raise-arity-error] 
will normalize the arity and use the normalized form in the error message.
If @racket[name] is a procedure, its actual arity is
ignored.  

The @racket[arg-v] arguments are the actual supplied
arguments, which are shown in the error message (using the error value
conversion handler; see @racket[error-value->string-handler]); also,
the number of supplied @racket[arg-v]s is explicitly mentioned in the
message.}

@defproc[(raise-syntax-error [name (or/c symbol? #f)]
                             [message string?]
                             [expr any/c #f]
                             [sub-expr any/c #f]
                             [extra-sources (listof syntax?) null])
         any]{

Creates an @racket[exn:fail:syntax] value and @racket[raise]s it as an
exception.  Macros use this procedure to report syntax errors.

The @racket[name] argument is usually @racket[#f] when @racket[expr]
is provided; it is described in more detail below. The
@racket[message] is used as the main body of the error message; if
@racket[message] contains newline characters, each new line should be
suitably indented (with one space at the start), and it should not end with a newline character.

The optional @racket[expr] argument is the erroneous source syntax
object or S-expression (but the expression @racket[#f] cannot be
represented by itself; it must be wrapped as a @tech{syntax
object}). The optional @racket[sub-expr] argument is a syntax object
or S-expression (again, @racket[#f] cannot represent itself) within
@racket[expr] that more precisely locates the error.  Both may appear
in the generated error-message text if
@racket[error-print-source-location] is @racket[#t]. Source location
information in the error-message text is similarly extracted from
@racket[sub-expr] or @racket[expr] when at least one is a syntax
object and @racket[error-print-source-location] is @racket[#t].

If @racket[sub-expr] is provided and not @racket[#f], it is used (in
syntax form) for the @racket[exprs] field of the generated exception
record, else the @racket[expr] is used if provided and not
@racket[#f]. In either case, the syntax object is @racket[cons]ed onto
@racket[extra-sources] to produce the @racket[exprs] field, or
@racket[extra-sources] is used directly for @racket[exprs] if neither
@racket[expr] nor @racket[sub-expr] is provided and not @racket[#f].

The form name used in the generated error message is determined
through a combination of the @racket[name], @racket[expr], and
@racket[sub-expr] arguments:

@itemize[

  @item{When @racket[name] is @racket[#f], and when @racket[expr] is
  either an identifier or a syntax pair containing an identifier as
  its first element, then the form name from the error message is the
  identifier's symbol.}

 @item{When @racket[name] is @racket[#f] and when @racket[expr] is not
  an identifier or a syntax pair containing an identifier as its
  first element, then the form name in the error message is
  @racket["?"].}

 @item{@racket[symbol]: When @racket[name] is a symbol, then the symbol
  is used as the form name in the generated error message.}

]}

@;------------------------------------------------------------------------
@section{Handling Exceptions}

@defproc[(call-with-exception-handler [f (any/c . -> . any)] [thunk (-> any)]) any]{

Installs @racket[f] as the @tech{exception handler} for the
@tech{dynamic extent} of the call to @racket[thunk]. If an exception
is raised during the evaluation of @racket[thunk] (in an extension of
the current continuation that does not have its own exception
handler), then @racket[f] is applied to the @racket[raise]d value in
the continuation of the @racket[raise] call (but the continuation is 
normally extended
with a @tech{continuation barrier}; see @secref["prompt-model"] and
@racket[raise]).

Any procedure that takes one argument can be an exception handler.
Normally, an exception handler escapes from the context of the
@racket[raise] call via @racket[abort-current-continuation] or some other escape
mechanism. To propagate an exception to the ``previous'' exception
handler---that is, the exception handler associated with the rest of
the continuation after the point where the called exception handler
was associated with the continuation---an exception handler can simply
return a result instead of escaping, in which case the @racket[raise] call
propagates the value to the previous exception handler (still in the
dynamic extent of the call to @racket[raise], and under the same
barrier, if any). If an exception handler returns a result and no
previous handler is available, the @tech{uncaught-exception handler}
is used.

A call to an exception handler is @racket[parameterize-break]ed to
disable breaks, and it is wrapped with
@racket[call-with-exception-handler] to install an exception handler
that reports both the original and newly raised exceptions via the
@tech{error display handler} and then escapes via the @tech{error
escape handler}.}


@defparam[uncaught-exception-handler f (any/c . -> . any)]{

A @tech{parameter} that determines an @deftech{uncaught-exception handler} used by
@racket[raise] when the relevant continuation has no exception handler
installed with @racket[call-with-exception-handler] or
@racket[with-handlers]. Unlike exception handlers installed with
@racket[call-with-exception-handler], the uncaught-exception
handler must not return a value when called by @racket[raise]; if
it returns, an exception is raised (to be handled by an exception
handler that reports both the original and newly raised exception).

The default uncaught-exception handler prints an error message using
the current @tech{error display handler} (see @racket[error-display-handler]),
unless the argument to the handler is an instance of @racket[exn:break:hang-up].
If the argument to the handler is an instance of @racket[exn:break:hang-up]
or @racket[exn:break:terminate], the default uncaught-exception handler
then calls the @tech{exit handler} with @racket[1], which normally exits
or escapes. For any argument, the default uncaught-exception handler
then escapes by calling the current @tech{error escape handler} (see
@racket[error-escape-handler]). The call to each handler is
@racket[parameterize]d to set @racket[error-display-handler] to the
default @tech{error display handler}, and it is @racket[parameterize-break]ed
to disable breaks. The call to the @tech{error escape handler} is further
parameterized to set @racket[error-escape-handler] to the default
@tech{error escape handler}; if the @tech{error escape handler} returns, then
the default @tech{error escape handler} is called.

When the current @tech{error display handler} is the default handler, then the
error-display call is parameterized to install an emergency error
display handler that logs an error (see @racket[log-error]) and never
fails.}


@defform[(with-handlers ([pred-expr handler-expr] ...)
           body ...+)]{

Evaluates each @racket[pred-expr] and @racket[handler-expr] in the
order that they are specified, and then evaluates the @racket[body]s
with a new exception handler during its dynamic extent.

The new exception handler processes an exception only if one of the
@racket[pred-expr] procedures returns a true value when applied to the
exception, otherwise the exception handler is invoked from the
continuation of the @racket[with-handlers] expression (by raising the
exception again). If an exception is handled by one of the
@racket[handler-expr] procedures, the result of the entire
@racket[with-handlers] expression is the return value of the handler.

When an exception is raised during the evaluation of @racket[body]s,
each predicate procedure @racket[pred-expr] is applied to the
exception value; if a predicate returns a true value, the
corresponding @racket[handler-expr] procedure is invoked with the
exception as an argument. The predicates are tried in the order that
they are specified.

Before any predicate or handler procedure is invoked, the continuation
of the entire @racket[with-handlers] expression is restored, but also
@racket[parameterize-break]ed to disable breaks. Thus, breaks are
disabled by default during the predicate and handler procedures (see
@secref["breakhandler"]), and the exception handler is the one from
the continuation of the @racket[with-handlers] expression.

The @racket[exn:fail?] procedure is useful as a handler predicate to
catch all error exceptions. Avoid using @racket[(lambda (x) #t)] as a
predicate, because the @racket[exn:break] exception typically should
not be caught (unless it will be re-raised to cooperatively
break). Beware, also, of catching and discarding exceptions, because
discarding an error message can make debugging unnecessarily
difficult; instead of discarding an error message, consider logging it
via @racket[log-error] or a logging form created by
@racket[define-logger].}

@defform[(with-handlers* ([pred-expr handler-expr] ...)
           body ...+)]{

Like @racket[with-handlers], but if a @racket[handler-expr] procedure
is called, breaks are not explicitly disabled, and the handler call is
in tail position with respect to the @racket[with-handlers*] form.}

@;------------------------------------------------------------------------
@section{Configuring Default Handling}

@defparam[error-escape-handler proc (-> any)]{

A parameter for the @deftech{error escape handler}, which takes no
arguments and escapes from the dynamic context of an exception.  The
default error escape handler escapes using
@racket[(abort-current-continuation (default-continuation-prompt-tag)
void)].

The error escape handler is normally called directly by an exception
handler, in a @tech{parameterization} that sets the @tech{error
display handler} and @tech{error escape handler} to the default
handlers, and it is normally @racket[parameterize-break]ed to disable
breaks. To escape from a run-time error in a different context, use
@racket[raise] or @racket[error].

Due to a @tech{continuation barrier} around exception-handling calls,
an error escape handler cannot invoke a full continuation that was
created prior to the exception, but it can abort to a prompt (see
@racket[call-with-continuation-prompt]) or invoke an escape
continuation (see @racket[call-with-escape-continuation]).}

@defparam[error-display-handler proc (string? any/c . -> . any)]{

A parameter for the @deftech{error display handler}, which is called
by the default exception handler with an error message and the
exception value. More generally, the handler's first argument is a
string to print as an error message, and the second is a value
representing a raised exception.

The default error display handler @racket[display]s its first argument
to the current error port (determined by the
@racket[current-error-port] parameter) and extracts a stack trace (see
@racket[continuation-mark-set->context]) to display from the second
argument if it is an @racket[exn] value but not an
@racket[exn:fail:user] value.

@margin-note{The default error display handler in DrRacket also uses
the second argument to highlight source locations.}

To report a run-time error, use @racket[raise] or procedures like
@racket[error], instead of calling the error display handler
directly.}

@defparam[error-print-width width (and/c exact-integer? (>=/c 3))]{

A parameter whose value is used as the maximum number of characters
used to print a Racket value that is embedded in a primitive error
message.}

@defparam[error-print-context-length cnt exact-nonnegative-integer?]{

A parameter whose value is used by the default @tech{error display handler}
as the maximum number of lines of context (or ``stack trace'') to
print; a single ``...'' line is printed if more lines are available
after the first @racket[cnt] lines. A @racket[0] value for
@racket[cnt] disables context printing entirely.}

@defparam[error-value->string-handler proc (any/c exact-nonnegative-integer?
                                                  . -> .
                                                  string?)]{

A @tech{parameter} that determines the @deftech{error value conversion
handler}, which is used to print a Racket value that is embedded in a
primitive error message.

The integer argument to the handler specifies the maximum number of
characters that should be used to represent the value in the resulting
string.  The default error value conversion handler @racket[print]s
the value into a string (using the current @tech{global port print
handler}; see @racket[global-port-print-handler]). If the printed form
is too long, the printed form is truncated and the last three
characters of the return string are set to ``...''.

If the string returned by an error value conversion handler is longer
than requested, the string is destructively ``truncated'' by setting
the first extra position in the string to the null character. If a
non-string is returned, then the string @racket["..."] is used. If a
primitive error string needs to be generated before the handler has
returned, the default error value conversion handler is used.

Calls to an error value conversion handler are @racket[parameterize]d
to re-install the default error value conversion handler, and to
enable printing of unreadable values (see @racket[print-unreadable]).}

@defboolparam[error-print-source-location include?]{

A @tech{parameter} that controls whether read and syntax error messages
include source information, such as the source line and column or the
expression.  This parameter also controls the error message when a
module-defined variable is accessed before its definition is executed;
the parameter determines whether the message includes a module
name. Only the message field of an @racket[exn:fail:read],
@racket[exn:fail:syntax], or @racket[exn:fail:contract:variable]
structure is affected by the parameter. The default is @racket[#t].}

@;------------------------------------------------------------------------
@section{Built-in Exception Types}

@defstruct[exn ([message string?]
                [continuation-marks continuation-mark-set?])
           #:inspector #f]{

The base @tech{structure type} for exceptions. The @racket[message]
field contains an error message, and the @racket[continuation-marks]
field contains the value produced by @racket[(current-continuation-marks)]
immediately before the exception was raised.

Exceptions raised by Racket form a hierarchy under @racket[exn]:

@racketblock[
exn
  exn:fail
    exn:fail:contract
      exn:fail:contract:arity
      exn:fail:contract:divide-by-zero
      exn:fail:contract:non-fixnum-result
      exn:fail:contract:continuation
      exn:fail:contract:variable
    exn:fail:syntax
      exn:fail:syntax:unbound
    exn:fail:read
      exn:fail:read:eof
      exn:fail:read:non-char
    exn:fail:filesystem
      exn:fail:filesystem:exists
      exn:fail:filesystem:version
      exn:fail:filesystem:errno
    exn:fail:network
      exn:fail:network:errno
    exn:fail:out-of-memory
    exn:fail:unsupported
    exn:fail:user
  exn:break
    exn:break:hang-up
    exn:break:terminate
]}

@defstruct[(exn:fail exn) ()
           #:inspector #f]{

Raised for exceptions that represent errors, as opposed to
@racket[exn:break].}


@defstruct[(exn:fail:contract exn:fail) ()
           #:inspector #f]{

Raised for errors from the inappropriate run-time use of a function or
syntactic form.}

@defstruct[(exn:fail:contract:arity exn:fail:contract) ()
           #:inspector #f]{

Raised when a procedure is applied to the wrong number of arguments.}

@defstruct[(exn:fail:contract:divide-by-zero exn:fail:contract) ()
           #:inspector #f]{

Raised for division by exact zero.}

@defstruct[(exn:fail:contract:non-fixnum-result exn:fail:contract) ()
           #:inspector #f]{

Raised by functions like @racket[fx+] when the result would not be a fixnum.}

@defstruct[(exn:fail:contract:continuation exn:fail:contract) ()
           #:inspector #f]{

Raised when a continuation is applied where the jump would cross a
continuation barrier.}

@defstruct[(exn:fail:contract:variable exn:fail:contract) ([id symbol?])
           #:inspector #f]{

Raised for a reference to a not-yet-defined @tech{top-level variable}
or @tech{module-level variable}.}

@defstruct[(exn:fail:syntax exn:fail) ([exprs (listof syntax?)])
           #:inspector #f]{

Raised for a syntax error that is not a @racket[read] error. The
@racket[exprs] indicate the relevant source expressions,
least-specific to most-specific.}

@defstruct[(exn:fail:syntax:unbound exn:fail:syntax) ()
           #:inspector #f]{

Raised by @racket[#%top] or @racket[set!] for an
unbound identifier within a module.}


@defstruct[(exn:fail:read exn:fail) ([srclocs (listof srcloc?)])
           #:inspector #f]{

Raised for a @racket[read] error. The @racket[srclocs] indicate the
relevant source expressions.}

@defstruct[(exn:fail:read:eof exn:fail:read) ()
           #:inspector #f]{

Raised for a @racket[read] error, specifically when the error is due
to an unexpected end-of-file.}

@defstruct[(exn:fail:read:non-char exn:fail:read) ()
           #:inspector #f]{

Raised for a @racket[read] error, specifically when the error is due
to an unexpected non-character (i.e., ``special'') element in the
input stream.}

@defstruct[(exn:fail:filesystem exn:fail) ()
           #:inspector #f]{

Raised for an error related to the filesystem (such as a file not
found).}

@defstruct[(exn:fail:filesystem:exists exn:fail:filesystem) ()
           #:inspector #f]{

Raised for an error when attempting to create a file that exists
already.}

@defstruct[(exn:fail:filesystem:version exn:fail:filesystem) ()
           #:inspector #f]{

Raised for a version-mismatch error when loading an extension.}

@defstruct[(exn:fail:filesystem:errno exn:fail:filesystem) ([errno (cons/c exact-integer? (or/c 'posix 'windows 'gai))])
           #:inspector #f]{

Raised for a filesystem error for which a system error code is
available. The symbol part of an @racket[errno] field indicates the
category of the error code: @racket['posix] indicates a C/Posix
@tt{errno} value, @racket['windows] indicates a Windows system error
code (under Windows, only), and @racket['gai] indicates a
@tt{getaddrinfo} error code (which shows up only in
@racket[exn:fail:network:errno] exceptions for operations that resolve
hostnames, but it allowed in @racket[exn:fail:filesystem:errno]
instances for consistency).}

@defstruct[(exn:fail:network exn:fail) ()
           #:inspector #f]{

Raised for TCP and UDP errors.}

@defstruct[(exn:fail:network:errno exn:fail:network) ([errno (cons/c exact-integer? (or/c 'posix 'windows 'gai))])
           #:inspector #f]{

Raised for a TCP or UDP error for which a system error code is
available, where the @racket[errno] field is as for
@racket[exn:fail:filesystem:errno].}


@defstruct[(exn:fail:out-of-memory exn:fail) ()
           #:inspector #f]{

Raised for an error due to insufficient memory, in cases where sufficient
memory is at least available for raising the exception.}

@defstruct[(exn:fail:unsupported exn:fail) ()
           #:inspector #f]{

Raised for an error due to an unsupported feature on the current
platform or configuration.}

@defstruct[(exn:fail:user exn:fail) ()
           #:inspector #f]{

Raised for errors that are intended to be seen by end users. In
particular, the default error printer does not show the program
context when printing the error message.}

@defstruct[(exn:break exn) ([continuation continuation?])
           #:inspector #f]{

Raised asynchronously (when enabled) in response to a break request.
The @racket[continuation] field can be used by a handler to resume the
interrupted computation.}

@defstruct[(exn:break:hang-up exn:break) ()
           #:inspector #f]{

Raised asynchronously for hang-up breaks. The default
 @tech{uncaught-exception handler} reacts to this exception type by
 calling the @tech{exit handler}.}

@defstruct[(exn:break:terminate exn:break) ()
           #:inspector #f]{

Raised asynchronously for termination-request breaks. The default
 @tech{uncaught-exception handler} reacts to this exception type by
 calling the @tech{exit handler}.}


@defthing[prop:exn:srclocs struct-type-property?]{

A property that identifies structure types that provide a list of
@racket[srcloc] values. The property is normally attached to structure
types used to represent exception information.

The property value must be a procedure that accepts a single
value---the structure type instance from which to extract source
locations---and returns a list of @racket[srcloc]s. Some @tech{error
display handlers} use only the first returned location.}

As an example,
@codeblock|{
#lang racket

;; We create a structure that supports the
;; prop:exn:srcloc protocol.  It carries
;; with it the location of the syntax that
;; is guilty.
(define-struct (exn:fail:he-who-shall-not-be-named
                exn:fail)
  (a-srcloc)
  #:property prop:exn:srclocs
  (lambda (a-struct)
    (match a-struct
      [(struct exn:fail:he-who-shall-not-be-named
         (msg marks a-srcloc))
       (list a-srcloc)])))

;; We can play with this by creating a form that
;; looks at identifiers, and only flags specific ones.
(define-syntax (skeeterize stx)
  (syntax-case stx ()
    [(_ expr)
     (cond
       [(and (identifier? #'expr)
             (eq? (syntax-e #'expr) 'voldemort))
        (quasisyntax/loc stx
          (raise (make-exn:fail:he-who-shall-not-be-named
                  "oh dear don't say his name"
                  (current-continuation-marks)
                  (srcloc '#,(syntax-source #'expr)
                          '#,(syntax-line #'expr)
                          '#,(syntax-column #'expr)
                          '#,(syntax-position #'expr)
                          '#,(syntax-span #'expr)))))]
       [else
        ;; Otherwise, leave the expression alone.
        #'expr])]))

(define (f x)
  (* (skeeterize x) x))

(define (g voldemort)
  (* (skeeterize voldemort) voldemort))

;; Examples:
(f 7)
(g 7)  
;; The error should highlight the use
;; of the one-who-shall-not-be-named
;; in g.
}|

@defproc[(exn:srclocs? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] has the @racket[prop:exn:srclocs]
property, @racket[#f] otherwise.}


@defproc[(exn:srclocs-accessor [v exn:srclocs?])
         (exn:srclocs? . -> . (listof srcloc))]{

Returns the @racket[srcloc]-getting procedure associated with @racket[v].}


@defstruct[srcloc ([source any/c]
                   [line (or/c exact-positive-integer? #f)]
                   [column (or/c exact-nonnegative-integer? #f)]
                   [position (or/c exact-positive-integer? #f)]
                   [span (or/c exact-nonnegative-integer? #f)])
                  #:inspector #f]{

The fields of a @racket[srcloc] instance are as follows:

@itemize[

 @item{@racket[source] --- An arbitrary value identifying the source,
 often a path (see @secref["pathutils"]).}

 @item{@racket[line] --- The line number (counts from 1) or
 @racket[#f] (unknown).}

 @item{@racket[column] --- The column number (counts from 0) or
 @racket[#f] (unknown).}

 @item{@racket[position] --- The starting position (counts from 1) or
 @racket[#f] (unknown).}

 @item{@racket[span] --- The number of covered positions (counts from
 0) or @racket[#f] (unknown).}

]}
