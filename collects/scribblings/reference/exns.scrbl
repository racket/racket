#lang scribble/doc
@(require scribble/bnf
          "mz.ss"
          (for-label racket/fixnum))

@title[#:tag "exns"]{Exceptions}

See @secref["exn-model"] for information on the PLT Scheme exception
model. It is based on a proposal by Friedman, Haynes, and Dybvig
@cite["Friedman95"].

Whenever a primitive error occurs in PLT Scheme, an exception is
raised.  The value that is passed to the current @tech{exception
handler} for a primitive error is always an instance of the
@scheme[exn] structure type. Every @scheme[exn] structure value has a
@scheme[message] field that is a string, the primitive error message.
The default exception handler recognizes exception values with the
@scheme[exn?] predicate and passes the error message to the current
@tech{error display handler} (see @scheme[error-display-handler]).

Primitive procedures that accept a procedure argument with a
particular required arity (e.g., @scheme[call-with-input-file],
@scheme[call/cc]) check the argument's arity immediately, raising
@scheme[exn:fail:contract] if the arity is incorrect.

@;------------------------------------------------------------------------
@section[#:tag "errorproc"]{Raising Exceptions}

@defproc[(raise [v any/c][barrier? any/c #t]) any]{

Raises an exception, where @scheme[v] represents the exception being
raised. The @scheme[v] argument can be anything; it is passed to the
current @tech{exception handler}.

If @scheme[barrier?] is true, then the call to the @tech{exception
handler} is protected by a @tech{continuation barrier}, so that
multiple returns/escapes are impossible. All exceptions raised by
@schememodname[scheme] functions effectively use @scheme[raise] with a
@scheme[#t] value for @scheme[barrier?].

Breaks are disabled from the time the exception is raised until the
exception handler obtains control, and the handler itself is
@scheme[parameterize-break]ed to disable breaks initially; see
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
           [(error [msg string?][v any/c] ...) any]
           [(error [src symbol?][frmat string?][v any/c] ...) any])]{

Raises the exception @scheme[exn:fail], which contains an error
string. The different forms produce the error string in different
ways:

@itemize[

 @item{@scheme[(error sym)] creates a message string by concatenating
  @scheme["error: "] with the string form of @scheme[sym].}

 @item{@scheme[(error msg v ...)] creates a message string by
 concatenating @scheme[msg] with string versions of the @scheme[v]s
 (as produced by the current error value conversion handler; see
 @scheme[error-value->string-handler]). A space is inserted before
 each @scheme[v].}

 @item{@scheme[(error src frmat v ...)] creates a
 message string equivalent to the string created by

  @schemeblock[
  (format (string-append "~s: " frmat) src v ...)
  ]}

]

In all cases, the constructed message string is passed to
@scheme[make-exn:fail], and the resulting exception is raised.

@examples[
(error 'failed)
(error "failed" 23 'pizza (list 1 2 3))
(error 'failed "~a failed because ~a" 'method-a "no argument supplied")
]}

@defproc*[([(raise-user-error [sym symbol?]) any]
           [(raise-user-error [msg string?][v any/c] ...) any]
           [(raise-user-error [src symbol?][format string?][v any/c] ...) any])]{

Like @scheme[error], but constructs an exception with
@scheme[make-exn:fail:user] instead of @scheme[make-exn:fail]. The
default @tech{error display handler} does not show a ``stack trace'' for
@scheme[exn:fail:user] exceptions (see @secref["contmarks"]), so
@scheme[raise-user-error] should be used for errors that are intended
for end users.

@examples[
(raise-user-error 'failed)
(raise-user-error "failed" 23 'pizza (list 1 2 3))
(raise-user-error 'failed "~a failed because ~a" 'method-a "no argument supplied")
]}


@defproc*[([(raise-type-error [name symbol?][expected string?][v any/c]) any]
           [(raise-type-error [name symbol?][expected string?][bad-pos exact-nonnegative-integer?][v any/c] ...) any])]{

Creates an @scheme[exn:fail:contract] value and @scheme[raise]s it as
an exception.  The @scheme[name] argument is used as the source
procedure's name in the error message. The @scheme[expected] argument
is used as a description of the expected type.

In the first form, @scheme[v] is the value received by the procedure
that does not have the expected type.

In the second form, the bad argument is indicated by an index
@scheme[bad-pos] (counting from @math{0}), and all of the original
arguments @scheme[v] are provided (in order). The resulting error
message names the bad argument and also lists the other arguments. If
@scheme[bad-pos] is not less than the number of @scheme[v]s, the
@exnraise[exn:fail:contract].

@examples[
(define (feed-cow animal)
  (if (not (eq? animal 'cow))
    (raise-type-error 'feed-cow "cow" animal)
    "fed the cow"))
(feed-cow 'turkey)
(define (feed-animals cow sheep goose cat)
  (if (not (eq? goose 'goose))
    (raise-type-error 'feed-animals "goose" 2 cow sheep goose cat)
    "fed the animals"))
(feed-animals 'cow 'sheep 'dog 'cat)
]}

@defproc[(raise-mismatch-error [name symbol?][message string?][v any/c]) any]{

Creates an @scheme[exn:fail:contract] value and @scheme[raise]s it as
an exception.  The @scheme[name] is used as the source procedure's
name in the error message. The @scheme[message] is the error
message. The @scheme[v] argument is the improper argument received by
the procedure. The printed form of @scheme[v] is appended to
@scheme[message] (using the error value conversion handler; see
@scheme[error-value->string-handler]).}

@defproc[(raise-arity-error [name (or/c symbol? procedure?)]
                            [arity-v (or/c exact-nonnegative-integer?
                                           arity-at-least?
                                           (listof
                                            (or/c exact-nonnegative-integer?
                                                  arity-at-least?)))]
                            [arg-v any/c #f] ...)
         any]{

Creates an @scheme[exn:fail:contract:arity] value and @scheme[raise]s
it as an exception.  The @scheme[name] is used for the source
procedure's name in the error message. 

The @scheme[arity-v] value must
be a possible result from @scheme[procedure-arity], except
that it does not have to be normalized (see @scheme[procedure-arity?] for
the details of normalized arities); @scheme[raise-arity-error] 
will normalize the arity and used the normalized form in the error message.
If @scheme[name-symbol-or-procedure] is a procedure, its actual arity is
ignored.  

The @scheme[arg-v] arguments are the actual supplied
arguments, which are shown in the error message (using the error value
conversion handler; see @scheme[error-value->string-handler]); also,
the number of supplied @scheme[arg-v]s is explicitly mentioned in the
message.}

@defproc[(raise-syntax-error [name (or/c symbol? #f)]
                             [message string?]
                             [expr any/c #f]
                             [sub-expr any/c #f]
                             [extra-sources (listof syntax?) null])
         any]{

Creates an @scheme[exn:fail:syntax] value and @scheme[raise]s it as an
exception.  Macros use this procedure to report syntax errors.

The @scheme[name] argument is usually @scheme[#f] when @scheme[expr]
is provided; it is described in more detail below. The
@scheme[message] is used as the main body of the error message.

The optional @scheme[expr] argument is the erroneous source syntax
object or S-expression (but the expression @scheme[#f] cannot be
represented by itself; it must be wrapped as a @tech{syntax
object}). The optional @scheme[sub-expr] argument is a syntax object
or S-expression (again, @scheme[#f] cannot represent itself) within
@scheme[expr] that more precisely locates the error.  Both may appear
in the generated error-message text if
@scheme[error-print-source-location] is @scheme[#t]. Source location
information in the error-message text is similarly extracted from
@scheme[sub-expr] or @scheme[expr] when at least one is a syntax
object and @scheme[error-print-source-location] is @scheme[#t].

If @scheme[sub-expr] is provided and not @scheme[#f], it is used (in
syntax form) for the @scheme[exprs] field of the generated exception
record, else the @scheme[expr] is used if provided and not
@scheme[#f]. In either case, the syntax object is @scheme[cons]ed onto
@scheme[extra-sources] to produce the @scheme[exprs] field, or
@scheme[extra-sources] is used directly for @scheme[exprs] if neither
@scheme[expr] nor @scheme[sub-expr] is provided and not @scheme[#f].

The form name used in the generated error message is determined
through a combination of the @scheme[name], @scheme[expr], and
@scheme[sub-expr] arguments:

@itemize[

  @item{When @scheme[name] is @scheme[#f], and when @scheme[expr] is
  either an identifier or a syntax pair containing an identifier as
  its first element, then the form name from the error message is the
  identifier's symbol.}

 @item{When @scheme[name] is @scheme[#f] and when @scheme[expr] is not
  an identifier or a syntax pair containing and identifier as its
  first element, then the form name in the error message is
  @scheme["?"].}

 @item{@scheme[symbol]: When @scheme[name] is a symbol, then the symbol
  is used as the form name in the generated error message.}

]}

@;------------------------------------------------------------------------
@section{Handling Exceptions}

@defproc[(call-with-exception-handler [f (any/c . -> . any)][thunk (-> any)]) any]{

Installs @scheme[f] as the @tech{exception handler} for the
@tech{dynamic extent} of the call to @scheme[thunk]. If an exception
is raised during the evaluation of @scheme[thunk] (in an extension of
the current continuation that does not have its own exception
handler), then @scheme[f] is applied to the @scheme[raise]d value in
the continuation of the @scheme[raise] call (but normally extended
with a @tech{continuation barrier}; see @secref["prompt-model"] and
@scheme[raise]).

Any procedure that takes one argument can be an exception handler.  If
the exception handler returns a value when invoked by @scheme[raise],
then @scheme[raise] propagates the value to the ``previous'' exception
handler (still in the dynamic extent of the call to @scheme[raise],
and under the same barrier, if any). The previous exception handler is
the exception handler associated with the rest of the continuation
after the point where the called exception handler was associated with
the continuation; if no previous handler is available, the
uncaught-exception handler is used (see below). In all cases, a call
to an exception handler is @scheme[parameterize-break]ed to disable
breaks, and it is wrapped with @scheme[call-with-exception-handler] to
install the an exception handler that reports both the original and
newly raised exceptions.}

@defparam[uncaught-exception-handler f (any/c . -> . any)]{

A @tech{parameter} that determines an exception handler used by
@scheme[raise] when the relevant continuation has no exception handler
installed with @scheme[call-with-exception-handler] or
@scheme[with-handlers]. Unlike exception handlers installed with
@scheme[call-with-exception-handler], the handler for uncaught
exceptions must not return a value when called by @scheme[raise]; if
it returns, an exception is raised (to be handled by an exception
handler that reports both the original and newly raised exception).

The default uncaught-exception handler prints an error message using
the current @tech{error display handler} (see @scheme[error-display-handler])
and then escapes by calling the current error escape handler (see
@scheme[error-escape-handler]). The call to each handler is
@scheme[parameterize]d to set @scheme[error-display-handler] to the
default @tech{error display handler}, and it is @scheme[parameterize-break]ed
to disable breaks. The call to the error escape handler is further
parameterized to set @scheme[error-escape-handler] to the default
error escape handler.

When the current @tech{error display handler} is the default handler, then the
error-display call is parameterized to install an emergency error
display handler that attempts to print directly to a console and never
fails.}

@defform[(with-handlers ([pred-expr handler-expr] ...)
           body ...+)]{

Evaluates each @scheme[pred-expr] and and @scheme[handler-expr] in the
order that they are specified, and then evaluates the @scheme[body]s
with a new exception handler during the its dynamic extent.

The new exception handler processes an exception only if one of the
@scheme[pred-expr] procedures returns a true value when applied to the
exception, otherwise the exception handler is invoked from the
continuation of the @scheme[with-handlers] expression (by raising the
exception again). If an exception is handled by one of the
@scheme[handler-expr] procedures, the result of the entire
@scheme[with-handlers] expression is the return value of the handler.

When an exception is raised during the evaluation of @scheme[body]s,
each predicate procedure @scheme[pred-expr] is applied to the
exception value; if a predicate returns a true value, the
corresponding @scheme[handler-expr] procedure is invoked with the
exception as an argument. The predicates are tried in the order that
they are specified.

Before any predicate or handler procedure is invoked, the continuation
of the entire @scheme[with-handlers] expression is restored, but also
@scheme[parameterize-break]ed to disable breaks. Thus, breaks are
disabled by default during the predicate and handler procedures (see
@secref["breakhandler"]), and the exception handler is the one from
the continuation of the @scheme[with-handlers] expression.

The @scheme[exn:fail?] procedure is useful as a handler predicate to
catch all error exceptions. Avoid using @scheme[(lambda (x) #t)] as a
predicate, because the @scheme[exn:break] exception typically should
not be caught (unless it will be re-raised to cooperatively
break). Beware, also, of catching and discarding exceptions, because
discarding an error message can make debugging unnecessarily
difficult.}

@defform[(with-handlers* ([pred-expr handler-expr] ...)
           body ...+)]{

Like @scheme[with-handlers], but if a @scheme[handler-expr] procedure
is called, breaks are not explicitly disabled, and the handler call is
in tail position with respect to the @scheme[with-handlers*] form.}

@;------------------------------------------------------------------------
@section{Configuring Default Handling}

@defparam[error-escape-handler proc (-> any)]{

A parameter for the @deftech{error escape handler}, which takes no
arguments and escapes from the dynamic context of an exception.  The
default error escape handler escapes using
@scheme[(abort-current-continuation (default-continuation-prompt-tag)
void)].

The error escape handler is normally called directly by an exception
handler, in a @tech{parameterization} that sets the @tech{error
display handler} and @tech{error escape handler} to the default
handlers, and it is normally @scheme[parameterize-break]ed to disable
breaks. To escape from a run-time error in a different context, use
@scheme[raise] or @scheme[error].

Due to a @tech{continuation barrier} around exception-handling calls,
an error escape handler cannot invoke a full continuation that was
created prior to the exception, but it can abort to a prompt (see
@scheme[call-with-continuation-prompt]) or invoke an escape
continuation (see @scheme[call-with-escape-continuation]).}

@defparam[error-display-handler proc (string? any/c . -> . any)]{

A parameter for the @deftech{error display handler}, which is called
by the default exception handler with an error message and the
exception value. More generally, the handler's first argument is a
string to print as an error message, and the second is a value
representing a raised exception.

The default error display handler @scheme[display]s its first argument
to the current error port (determined by the
@scheme[current-error-port] parameter) and extracts a stack trace (see
@scheme[continuation-mark-set->context]) to display from the second
argument if it is an @scheme[exn] value but not an
@scheme[exn:fail:user] value.

@margin-note{The default error display handler in DrScheme also uses
the second argument to highlight source locations.}

To report a run-time error, use @scheme[raise] or procedures like
@scheme[error], instead of calling the error display handler
directly.}

@defparam[error-print-width width (and exact-integer? (>=/c 3))]{

A parameter whose value is used as the maximum number of characters
used to print a Scheme value that is embedded in a primitive error
message.}

@defparam[error-print-context-length cnt exact-nonnegative-integer?]{

A parameter whose value is used by the default @tech{error display handler}
as the maximum number of lines of context (or ``stack trace'') to
print; a single ``...'' line is printed if more lines are available
after the first @scheme[cnt] lines. A @scheme[0] value for
@scheme[cnt] disables context printing entirely.}

@defparam[error-value->string-handler proc (any/c exact-nonnegative-integer?
                                                  . -> .
                                                  string?)]{

A parameter that determines the @deftech{error value conversion
handler}, which is used to print a Scheme value that is embedded in a
primitive error message.

The integer argument to the handler specifies the maximum number of
characters that should be used to represent the value in the resulting
string.  The default error value conversion handler @scheme[print]s
the value into a string (using the current @tech{global port print
handler}; see @scheme[global-port-print-handler]). If the printed form
is too long, the printed form is truncated and the last three
characters of the return string are set to ``...''.

If the string returned by an error value conversion handler is longer
than requested, the string is destructively ``truncated'' by setting
the first extra position in the string to the null character. If a
non-string is returned, then the string @scheme["..."] is used. If a
primitive error string needs to be generated before the handler has
returned, the default error value conversion handler is used.

Call to an error value conversion handler are @scheme[parameterized]
to re-install the default error value conversion handler, and to
enable printing of unreadable values (see @scheme[print-unreadable]).}

@defboolparam[error-print-source-location include?]{

A parameter that controls whether read and syntax error messages
include source information, such as the source line and column or the
expression.  This parameter also controls the error message when a
module-defined variable is accessed before its definition is executed;
the parameter determines whether the message includes a module
name. Only the message field of an @scheme[exn:fail:read],
@scheme[exn:fail:syntax], or @scheme[exn:fail:contract:variable]
structure is affected by the parameter. The default is @scheme[#t].}

@;------------------------------------------------------------------------
@section{Built-in Exception Types}

@defstruct[exn ([message string?]
                [continuation-marks continuation-mark-set?])
           #:inspector #f]{

The base @tech{structure type} for exceptions. The @scheme[message]
field contains an error message, and the @scheme[continuation-marks]
field contains the value produced by @scheme[(current-continuation-marks)]
immediately before the exception was raised.}

@defstruct[(exn:fail exn) ()
           #:inspector #f]{

Raised for exceptions that represent errors, as opposed to
@scheme[exn:break].}


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

Raised by functions like @scheme[fx+] when the result would not be a fixnum.}

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

Raised for a syntax error that is not a @scheme[read] error. The
@scheme[exprs] indicate the relevant source expressions,
least-specific to most-specific.}


@defstruct[(exn:fail:read exn:fail) ([srclocs (listof srcloc?)])
           #:inspector #f]{

Raised for a @scheme[read] error. The @scheme[srclocs] indicate the
relevant source expressions.}

@defstruct[(exn:fail:read:eof exn:fail:read) ()
           #:inspector #f]{

Raised for a @scheme[read] error, specifically when the error is due
to an unexpected end-of-file.}

@defstruct[(exn:fail:read:non-char exn:fail:read) ()
           #:inspector #f]{

Raised for a @scheme[read] error, specifically when the error is due
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


@defstruct[(exn:fail:network exn:fail) ()
           #:inspector #f]{

Raised for TCP and UDP errors.}


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

Raised for errors that are intended to be seen by end-users. In
particular, the default error printer does not show the program
context when printing the error message.}

@defstruct[(exn:break exn) ([continuation continuation?])
           #:inspector #f]{

Raised asynchronously (when enabled) in response to a break request.
The @scheme[continuation] field can be used by a handler to resume the
interrupted computation.}


@defthing[prop:exn:srclocs struct-type-property?]{

A property that identifies structure types that provide a list of
@scheme[srcloc] values. The property is normally attached to structure
types used to represent exception information.

The property value must be a procedure that accepts a single
value---the structure type instance from which to extract source
locations---and returns a list of @scheme[srcloc]s. Some @tech{error
display handlers} use only the first returned location.}


@defproc[(exn:srclocs? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] has the @scheme[prop:exn:srclocs]
property, @scheme[#f] otherwise.}


@defproc[(exn:srclocs-accessor [v exn:srclocs?])
         (exn:srclocs? . -> . (listof srcloc))]{

Returns the @scheme[srcloc]-getting procedure associated with @scheme[v].}


@defstruct[srcloc ([source any/c]
                   [line (or/c exact-positive-integer? #f)]
                   [column (or/c exact-nonnegative-integer? #f)]
                   [position (or/c exact-positive-integer? #f)]
                   [span (or/c exact-nonnegative-integer? #f)])
                  #:inspector #f]{

The fields of an @scheme[srcloc] instance are as follows:

@itemize[

 @item{@scheme[source] --- An arbitrary value identifying the source,
 often a path (see @secref["pathutils"]).}

 @item{@scheme[line] --- The line number (counts from 1) or
 @scheme[#f] (unknown).}

 @item{@scheme[column] --- The column number (counts from 0) or
 @scheme[#f] (unknown).}

 @item{@scheme[position] --- The starting position (counts from 1) or
 @scheme[#f] (unknown).}

 @item{@scheme[span] --- The number of covered positions (counts from
 0) or @scheme[#f] (unknown).}

]}
