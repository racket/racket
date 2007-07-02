#reader(lib "docreader.ss" "scribble")
@require[(lib "bnf.ss" "scribble")]
@require["mz.ss"]

@title[#:tag "mz:exns"]{Exceptions}

See @secref["mz:exn-model"] for information on the PLT Scheme
exception model. It is based on @cite[#:key "friedman-exns" #:title
"Exception system proposal" #:author "Daniel P. Friedman and
C. T. Haynes and R. Kent Dybvig" #:location
"http://www.cs.indiana.edu/scheme-repository/doc.proposals.exceptions.html"
#:date ""].

Whenever a primitive error occurs in PLT Scheme, an exception is
raised.  The value that is passed to the current @tech{exception
handler} is always an instance of the @scheme[exn] structure
type. Every @scheme[exn] structure value has a @scheme[message] field
that is a string, the primitive error message.  The default exception
handler recognizes exception values with the @scheme[exn?] predicate
and passes the error message to the current error display handler (see
@scheme[error-display-handler]).

Primitive procedures that accept a procedure argument with a
particular required arity (e.g., @scheme[call-with-input-file],
@scheme[call/cc]) check the argument's arity immediately, raising
@scheme[exn:fail:contract] if the arity is incorrect.

@;------------------------------------------------------------------------
@section[#:tag "mz:errorproc"]{Raising Exceptions}

@defproc[(raise [v any/c]) any]{

Raises an exception, where @scheme[v] represents the exception being
raised. The @scheme[v] argument can be anything; it is passed to the
current @deftech{exception handler}.  Breaks are disabled from the
time the exception is raised until the exception handler obtains
control, and the handler itself is @scheme[parameterize-break]ed to
disable breaks initially; see @secref["mz:breakhandler"] for more
information on breaks.}


@defproc*[([(error [sym symbol?]) any]
           [(error [msg string?][v any/c] ...) any]
           [(error [src symbol?][format string?][v any/c] ...) any])]{

Raises the exception @scheme[exn:fail], which contains an error
string. The different forms produce the error string in different
ways:

@itemize{

 @item{@scheme[(error sym)] creates a message string by concatenating
  @scheme["error: "] with the string form of @scheme[sym].}

 @item{@scheme[(error msg v ...)] creates a message string by
 concatenating @scheme[msg] with string versions of the @scheme[v]s
 (as produced by the current error value conversion handler; see
 @scheme[error-value->string-handler]). A space is inserted before
 each @scheme[v].}

 @item{@scheme[(error src format v ...)] creates a
 message string equivalent to the string created by

  @schemeblock[
  (format (string-append "~s: " format) src v ...)
  ]}

}

In all cases, the constructed message string is passed to
@scheme[make-exn:fail], and the resulting exception is raised.}

@defproc*[([(raise-user-error [sym symbol?]) any]
           [(raise-user-error [msg string?][v any/c] ...) any]
           [(raise-user-error [src symbol?][format string?][v any/c] ...) any])]{

Like @scheme[error], but constructs an exception with
@scheme[make-exn:fail:user] instead of @scheme[make-exn:fail]. The
default error display handler does not show a ``stack trace'' for
@scheme[exn:fail:user] exceptions (see @secref["mz:contmarks"]), so
@scheme[raise-user-error] should be used for errors that are intended
for end users.}


@defproc*[([(raise-type-error [name symbol?][expected string?][v any/c]) any]
           [(raise-type-error [name symbol?][expected string?][bad-pos nonnegative-exact-integer?][v any/c]) any])]{

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
@exnraise[exn:fail:contract].}

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
procedure's name in the error message. The @scheme[arity-v] value must
be a possible result from @scheme[procedure-arity], and it is used for
the procedure's arity in the error message; if
@scheme[name-symbol-or-procedure] is a procedure, its actual arity is
ignored.  The @scheme[arg-v] arguments are the actual supplied
arguments, which are shown in the error message (using the error value
conversion handler; see @scheme[error-value->string-handler]); also,
the number of supplied @scheme[arg-v]s is explicitly mentioned in the
message.}

@defproc[(raise-syntax-error [name (or/c symbol? false/c)]
                             [message string?]
                             [expr any/c #f]
                             [sub-expr any/c #f])
         any]{

Creates an @scheme[exn:fail:syntax] value and @scheme[raise]s it as an
exception.  Macros use this procedure to report syntax errors.

The @scheme[name] argument is usually @scheme[#f] when @scheme[expr]
is provided; it is described in more detail below. The
@scheme[message] is used as the main body of the error message.

The optional @scheme[expr] argument is the erroneous source syntax
object or S-expression. The optional @scheme[sub-expr] argument is a
syntax object or S-expression within @scheme[expr] that more precisely
locates the error. If @scheme[sub-expr] is provided, it is used (in
syntax form) for the @scheme[exprs] field of the generated exception
record, else the @scheme[expr] is used if provided, otherwise the
@scheme[exprs] field is the empty list. Source location information in
the error-message text is similarly extracted from @scheme[sub-expr]
or @scheme[expr], when at least one is a syntax object.

The form name used in the generated error message is determined
through a combination of the @scheme[name], @scheme[expr], and
@scheme[sub-expr] arguments:

@itemize{

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

}

See also @scheme[error-print-source-location].}

@;------------------------------------------------------------------------
@section{Handling Exceptions}

@defproc[(call-with-exception-handler [f (any/c . -> . any)][thunk (-> any)]) any]{

Installs @scheme[f] as the @tech{exception handler} for the current
continuation---i.e., for the dynamic extent of a call to
@scheme[thunk].  The @scheme[thunk] is called in tail position with
respect to the call to @scheme[call-with-exception-handler]. If an
exception is raised during the evaluation of @scheme[thunk] (in an
extension of the current continuation that does not have its own
exception handler), then @scheme[f] is applied to the @scheme[raise]d
value in the continuation of the @scheme[raise] call (but extended
with a continuation barrier; see @secref["mz:continuations"]).

Any procedure that takes one argument can be an exception handler.  If
the exception handler returns a value when invoked by @scheme[raise],
then @scheme[raise] propagates the value to the ``previous'' exception
handler (still in the dynamic extent of the call to
@scheme[raise]). The previous exception handler is the exception
handler associated with the rest of the continuation after the point
where the called exception handler was associated with the
continuation; if no previous handler is available, the
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
the current error display handler (see @scheme[error-display-handler])
and then escapes by calling the current error escape handler (see
@scheme[error-escape-handler]). The call to each handler is
@scheme[parameterize]d to set @scheme[error-display-handler] to the
default error display handler, and it is @scheme[parameterize-break]ed
to disable breaks. The call to the error escape handler is further
parameterized to set @scheme[error-escape-handler] to the default
error escape handler.

When the current error display handler is the default handler, then the
error-display call is parameterized to install an emergency error
display handler that attempts to print directly to a console and never
fails.}

@defform[(with-handlers ((pred-expr handler-expr))
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
@secref["mz:breakhandler"]), and the exception handler is the one from
the continuation of the @scheme[with-handlers] expression.

The @scheme[exn:fail?] procedure is useful as a handler predicate to
catch all error exceptions. Avoid using @scheme[(lambda (x) #t)] as a
predicate, because the @scheme[exn:break] exception typically should
not be caught (unless it will be re-raised to cooperatively
break). Beware, also, of catching and discarding exceptions, because
discarding an error message can make debugging unnecessarily
difficult.}

@defform[(with-handlers* ((pred-expr handler-expr))
           body ...+)]{

Like @scheme[with-handlers], but if a @scheme[handler-expr] procedure
is called, breaks are not explicitly disabled, and the call is in tail
position with respect to the @scheme[with-handlers*] form.}

@;------------------------------------------------------------------------
@section{Built-in Exception Types}

@defstruct[exn ([message string?]
                [continuation-marks continuation-mark-set?])
           #:immutable]{

The base @tech{structure type} for exceptions. The @scheme[message]
field contains an error message, and the @scheme[continuation-marks]
field contains the value produced by @scheme[(current-continuation-marks)]
immediately before the exception was raised.}

@defstruct[(exn:fail exn) ()]{

Raised for exceptions that represent errors, as opposed to
@scheme[exn:break].}


@defstruct[(exn:fail:contract exn:fail) ()]{

Raised for errors from the inappropriate run-time use of a function or
syntactic form.}

@defstruct[(exn:fail:contract:arity exn:fail:contract) ()]{

Raised when a procedure is applied to the wrong number of arguments.}

@defstruct[(exn:fail:contract:divide-by-zero exn:fail:contract) ()]{

Raised for division by exact zero.}

@defstruct[(exn:fail:contract:continuation exn:fail:contract) ()]{

Raised when a continuation is applied where the jump would cross a
continuation barrier.}

@defstruct[(exn:fail:contract:variable exn:fail:contract) ([id symbol?])
           #:immutable]{

Raised for a reference to a not-yet-defined @tech{top-level variable}
or @tech{module-level variable}.}

@defstruct[(exn:fail:syntax exn:fail) ([exprs (listof syntax?)])
           #:immutable]{

Raised for a syntax error that is not a @scheme[read] error. The
@scheme[exprs] indicate the relevant source expressions,
least-specific to most-specific.}


@defstruct[(exn:fail:read exn:fail) ([srclocs (listof srcloc?)])
           #:immutable]{

Raised for a @scheme[read] error. The @scheme[srclocs] indicate the
relevant source expressions.}

@defstruct[(exn:fail:read:eof exn:fail:read) ()]{

Raised for a @scheme[read] error, specifically when the error is due
to an unexpected end-of-file.}

@defstruct[(exn:fail:read:non-char exn:fail:read) ()]{

Raised for a @scheme[read] error, specifically when the error is due
to an unexpected non-character (i.e., ``special'') element in the
input stream.}

@defstruct[(exn:fail:filesystem exn:fail) ()]{

Raised for an error related to the filesystem (such as a file not
found).}

@defstruct[(exn:fail:filesystem:exists exn:fail:filesystem) ()]{

Raised for an error when attempting to create a file that exists
already.}

@defstruct[(exn:fail:filesystem:version exn:fail:filesystem) ()]{

Raised for a version-mismatch error when loading an extension.}


@defstruct[(exn:fail:network exn:fail) ()]{

Raised for TCP and UDP errors.}


@defstruct[(exn:fail:out-of-memory exn:fail) ()]{

Raised for an error due to insufficient memory, in cases where sufficient
memory is at least available for raising the exception.}

@defstruct[(exn:fail:unsupported exn:fail) ()]{

Raised for an error due to an unsupported feature on the current
platform or configuration.}

@defstruct[(exn:fail:user exn:fail) ()]{

Raised for errors that are intended to be seen by end-users. In
particular, the default error printer does not show the program
context when printing the error message.}

@defstruct[(exn:break exn) ([continuation continuation?])
           #:immutable]{

Raised asynchronously (when enabled) in response to a break request.
The @scheme[continuation] field can be used by a handler to resume the
interrupted computation.}
