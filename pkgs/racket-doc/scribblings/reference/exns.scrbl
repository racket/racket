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
(struct my-exception exn:fail:user ())
(with-handlers ([my-exception? (lambda (e)
                                 #f)])
  (+ 5 (raise (my-exception
               "failed"
               (current-continuation-marks)))))
(eval:error (raise 'failed #t))
]}

@defproc*[([(error [message-sym symbol?]) any]
           [(error [message-str string?] [v any/c] ...) any]
           [(error [who-sym symbol?] [format-str string?] [v any/c] ...) any])]{

Raises the exception @racket[exn:fail], which contains an error
string. The different forms produce the error string in different
ways:

@itemize[

 @item{@racket[(error message-sym)] creates a message string by concatenating
  @racket["error: "] with the string form of @racket[message-sym]. Use this
  form sparingly.}

 @item{@racket[(error message-str v ...)] creates a message string by
 concatenating @racket[message-str] with string versions of the @racket[v]s
 (as produced by the current error value conversion handler; see
 @racket[error-value->string-handler]). A space is inserted before
 each @racket[v]. Use this form sparingly, because it does not conform
 well to Racket's @tech{error message conventions}; consider
 @racket[raise-arguments-error], instead. }

 @item{@racket[(error who-sym format-str v ...)] creates a
 message string equivalent to the string created by

  @racketblock[
  (format (string-append "~s: " format-str) who-sym v ...)
  ]

 When possible, use functions such as @racket[raise-argument-error],
 instead, which construct messages that follow Racket's @tech{error message
 conventions}.}

]

In all cases, the constructed message string is passed to
@racket[make-exn:fail], and the resulting exception is raised.

@examples[
(eval:error (error 'failed))
(eval:error (error "failed" 23 'pizza (list 1 2 3)))
(eval:error (error 'method-a "failed because ~a" "no argument supplied"))
]}


@defproc*[([(raise-user-error [message-sym symbol?]) any]
           [(raise-user-error [message-str string?] [v any/c] ...) any]
           [(raise-user-error [who-sym symbol?] [format-str string?] [v any/c] ...) any])]{

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

The error message generated by @racket[raise-argument-error] is
adjusted via @racket[error-contract->adjusted-string] and then
@racket[error-message->adjusted-string] using the default
@racket['racket] realm.

@examples[
(define (feed-machine bits)
  (if (not (integer? bits))
    (raise-argument-error 'feed-machine "integer?" bits)
    "fed the machine"))
(eval:error (feed-machine 'turkey))
(define (feed-cow animal)
  (if (not (eq? animal 'cow))
    (raise-argument-error 'feed-cow "'cow" animal)
    "fed the cow"))
(eval:error (feed-cow 'turkey))
(define (feed-animals cow sheep goose cat)
  (if (not (eq? goose 'goose))
    (raise-argument-error 'feed-animals "'goose" 2 cow sheep goose cat)
    "fed the animals"))
(eval:error (feed-animals 'cow 'sheep 'dog 'cat))
]}

@defproc*[([(raise-argument-error* [name symbol?] [realm symbol?] [expected string?] [v any/c]) any]
           [(raise-argument-error* [name symbol?] [realm symbol?] [expected string?] [bad-pos exact-nonnegative-integer?] [v any/c] ...) any])]{

Like @racket[raise-argument-error], but using the given @racket[realm]
for error-message adjustments.

@history[#:added "8.4.0.2"]}


@defproc*[([(raise-result-error [name symbol?] [expected string?] [v any/c]) any]
           [(raise-result-error [name symbol?] [expected string?] [bad-pos exact-nonnegative-integer?] [v any/c] ...) any])]{

Like @racket[raise-argument-error], but the error message describe @racket[v]
as a ``result'' instead of an ``argument.''}

@defproc*[([(raise-result-error* [name symbol?] [realm symbol?] [expected string?] [v any/c]) any]
           [(raise-result-error* [name symbol?] [realm symbol?] [expected string?] [bad-pos exact-nonnegative-integer?] [v any/c] ...) any])]{

Like @racket[raise-result-error], but using the given @racket[realm]
for error-message adjustments.

@history[#:added "8.4.0.2"]}

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
line in the error message; each @racket[v] is formatted 
using the error value conversion handler (see
@racket[error-value->string-handler]), unless @racket[v] is a
@tech{unquoted-printing string}, in which case the string content is
@racket[display]ed without using the error value conversion handler.

The error message generated by @racket[raise-result-error] is adjusted
via @racket[error-contract->adjusted-string] and then
@racket[error-message->adjusted-string] using the default
@racket['racket] realm.

@examples[
 (eval:error
  (raise-arguments-error 'eat 
                         "fish is smaller than its given meal"
                         "fish" 12
                         "meal" 13))
]}

@defproc[(raise-arguments-error* [name symbol?] [realm symbol?] [message string?]
                                 [field string?] [v any/c] 
                                 ... ...) any]{

Like @racket[raise-arguments-error], but using the given @racket[realm]
for error-message adjustments.

@history[#:added "8.4.0.2"]}


@defproc[(raise-range-error [name symbol?] [type-description string?] [index-prefix string?]
                            [index exact-integer?] [in-value any/c]
                            [lower-bound exact-integer?] [upper-bound exact-integer?]
                            [alt-lower-bound (or/c #f exact-integer?) #f])
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

The error message generated by @racket[raise-range-error] is adjusted
via @racket[error-message->adjusted-string] using the default
@racket['racket] realm.

@examples[
(eval:error (raise-range-error 'vector-ref "vector" "starting " 5 #(1 2 3 4) 0 3))
(eval:error (raise-range-error 'vector-ref "vector" "ending " 5 #(1 2 3 4) 0 3))
(eval:error (raise-range-error 'vector-ref "vector" "" 3 #() 0 -1))
(eval:error (raise-range-error 'vector-ref "vector" "ending " 1 #(1 2 3 4) 2 3 0))
]}

@defproc[(raise-range-error* [name symbol?] [realm symbol?] [type-description string?] [index-prefix string?]
                             [index exact-integer?] [in-value any/c]
                             [lower-bound exact-integer?] [upper-bound exact-integer?]
                             [alt-lower-bound (or/c #f exact-integer?) #f])
         any]{

Like @racket[raise-range-error], but using the given @racket[realm]
for error-message adjustments.

@history[#:added "8.4.0.2"]}


@defproc*[([(raise-type-error [name symbol?] [expected string?] [v any/c]) any]
           [(raise-type-error [name symbol?] [expected string?] [bad-pos exact-nonnegative-integer?] [v any/c] ...) any])]{

Like @racket[raise-argument-error], but with Racket's old formatting
conventions, and where @racket[expected] is used as a ``type''
description instead of a contract expression. Use @racket[raise-argument-error]
or @racket[raise-result-error], instead.

The error message generated by @racket[raise-type-error] is adjusted
via @racket[error-message->adjusted-string] using the default
@racket['racket] realm.}


@defproc[(raise-mismatch-error [name symbol?] [message string?] [v any/c] 
                               ...+ ...+) any]{

Similar to @racket[raise-arguments-error], but using Racket's old
formatting conventions, with a required @racket[v] immediately
after the first @racket[message] string, and with further @racket[message]
strings that are spliced into the message without line breaks or
space. Use @racket[raise-arguments-error], instead.

The error message generated by @racket[raise-mismatch-error] is adjusted
via @racket[error-message->adjusted-string] using the default
@racket['racket] realm.}


@defproc[(raise-arity-error [name (or/c symbol? procedure?)]
                            [arity-v (or/c exact-nonnegative-integer?
                                           arity-at-least?
                                           (listof
                                            (or/c exact-nonnegative-integer?
                                                  arity-at-least?)))]
                            [arg-v any/c] ...)
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
message.

The error message generated by @racket[raise-arity-error] is adjusted
via @racket[error-message->adjusted-string] using the default
@racket['racket] realm.

@examples[
(eval:error (raise-arity-error 'unite (arity-at-least 13) "Virginia" "Maryland"))
]}


@defproc[(raise-arity-error* [name (or/c symbol? procedure?)]
                             [realm symbol?]
                             [arity-v (or/c exact-nonnegative-integer?
                                            arity-at-least?
                                            (listof
                                             (or/c exact-nonnegative-integer?
                                                   arity-at-least?)))]
                             [arg-v any/c] ...)
         any]{


Like @racket[raise-arity-error], but using the given @racket[realm]
for error-message adjustments.

@history[#:added "8.4.0.2"]}


@defproc[(raise-arity-mask-error [name (or/c symbol? procedure?)]
                                 [mask exact-integer?]
                                 [arg-v any/c] ...)
         any]{

The same as @racket[raise-arity-error], but using the arity representation
described with @racket[procedure-arity-mask].

@history[#:added "7.0.0.11"]}

@defproc[(raise-arity-mask-error* [name (or/c symbol? procedure?)]
                                  [realm symbol?]
                                  [mask exact-integer?]
                                  [arg-v any/c] ...)
         any]{

Like @racket[raise-arity-mask-error], but using the given @racket[realm]
for error-message adjustments.

@history[#:added "8.4.0.2"]}


@defproc[(raise-result-arity-error [name (or/c symbol? #f)]
                                   [arity-v exact-nonnegative-integer?]
                                   [detail-str (or/c string? #f)]
                                   [result-v any/c] ...)
         any]{

Like @racket[raise-arity-error], but reports a ``result'' mismatch
instead of an ``argument'' mismatch. The @racket[name] argument can be
@racket[#f] to omit an initial source for the error. The
@racket[detail-str] argument, if non-@racket[#f], should be a string
that starts with a newline, since it is added near the end of the
generated error message.

The error message generated by @racket[raise-result-arity-error] is
adjusted via @racket[error-message->adjusted-string] using the default
@racket['racket] realm.

@examples[
(eval:error (raise-result-arity-error 'let-values 2 "\n  in: example" 'a 2.0 "three"))
]

@history[#:added "6.90.0.26"]}


@defproc[(raise-result-arity-error* [name (or/c symbol? #f)]
                                    [realm symbol?]
                                    [arity-v exact-nonnegative-integer?]
                                    [detail-str (or/c string? #f)]
                                    [result-v any/c] ...)
         any]{

Like @racket[raise-result-arity-error], but using the given @racket[realm]
for error-message adjustments.

@history[#:added "8.4.0.2"]}


@defproc[(raise-syntax-error [name (or/c symbol? #f)]
                             [message string?]
                             [expr any/c #f]
                             [sub-expr any/c #f]
                             [extra-sources (listof syntax?) null]
                             [message-suffix string? ""]
                             [#:exn exn
                              (-> string?
                                  continuation-mark-set?
                                  (listof syntax?)
                                  exn:fail:syntax?)
                              exn:fail:syntax])
         any]{

Creates an @racket[exn:fail:syntax?] value and @racket[raise]s it as an
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
The @racket[extra-sources] argument is also used directly for
@racket[exprs] in the unusual case that the @racket[sub-expr] or
@racket[expr] that would be included in @racket[exprs] cannot be
converted to a syntax object (because it contains a cycle).

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

 @item{When @racket[name] is a symbol, then the symbol
  is used as the form name in the generated error message.}

]

The @racket[message-suffix] string is appended to the end of the error
message. If not @racket[""], it should normally start with a newline
and two spaces to add extra fields to the message (see
@secref["err-msg-conventions"]).

If specified, @racket[exn] should be a constructor or function that
has the same signature as the @racket[exn:fail:syntax] constructor.

@examples[
  (eval:error (raise-syntax-error #f "bad syntax" '(bad syntax)))
  (eval:error (raise-syntax-error #f "unbound identifier" 'unbound-id #:exn exn:fail:syntax:unbound))
]

@history[#:changed "6.90.0.18" @elem{Added the @racket[message-suffix] optional argument.}
         #:changed "8.4.0.6" @elem{Added the @racket[exn] optional argument.}]}


@deftogether[(
@defproc[(unquoted-printing-string? [v any/c]) boolean?]
@defproc[(unquoted-printing-string [s string?]) unquoted-printing-string?]
@defproc[(unquoted-printing-string-value [ups unquoted-printing-string?]) string?]
)]{

An @deftech{unquoted-printing string} wraps a string and
@racket[print]s, @racket[write]s, and @racket[display]s the same way
that the string @racket[display]s. An @tech{unquoted-printing string}
is especially useful with @racket[raise-arguments-error] to serve as a
field ``value'' that causes literal text to be printed as the field
content.

The @racket[unquoted-printing-string?] procedure returns @racket[#t]
if @racket[v] is a @tech{unquoted-printing string}, @racket[#f]
otherwise. The @racket[unquoted-printing-string] creates a
@tech{unquoted-printing string} value that encapsulates the string
@racket[s], and @racket[unquoted-printing-string-value] returns the
string within a @tech{unquoted-printing string}.

@history[#:added "6.10.0.2"]}

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
@racket[define-logger].

@examples[
  (with-handlers ([exn:fail:syntax?
                   (λ (e) (displayln "got a syntax error"))])
    (raise-syntax-error #f "a syntax error"))
  (with-handlers ([exn:fail:syntax?
                   (λ (e) (displayln "got a syntax error"))]
                  [exn:fail?
                   (λ (e) (displayln "fallback clause"))])
    (raise-syntax-error #f "a syntax error"))
]}

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
representing a raised exception. An error display handler can
print errors in different ways, but it should always print to the
current error port.

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


@defboolparam[error-print-source-location include?]{

A @tech{parameter} that controls whether read and syntax error messages
include source information, such as the source line and column or the
expression.  This parameter also controls the error message when a
module-defined variable is accessed before its definition is executed;
the parameter determines whether the message includes a module
name. Only the message field of an @racket[exn:fail:read],
@racket[exn:fail:syntax], or @racket[exn:fail:contract:variable]
structure is affected by the parameter. The default is @racket[#t].}


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


@defparam[error-syntax->string-handler proc (any/c (or/c exact-nonnegative-integer? #f)
                                                  . -> .
                                                  string?)]{

A @tech{parameter} that determines the @deftech{error syntax
conversion handler}, which is used to print a syntax form that is
embedded in an error message, such as from @racket[raise-syntax-error]
when @racket[error-print-source-location] is @racket[#t].

The arguments to the handler are analogous to the arguments for a
@tech{error value conversion handler} as configured with
@racket[error-value->string-handler], except that @racket[#f] can be
provided instead of an integer for the length, meaning that the
printed form should not be truncated. The first argument is normally a
@tech{syntax object}, but in the same way that
@racket[raise-syntax-error] accepts other S-expressions, the error
syntax conversion handler must also handle representations that are
not syntax objects.

@history[#:added "8.2.0.8"]}


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
      exn:fail:syntax:missing-module
    exn:fail:read
      exn:fail:read:eof
      exn:fail:read:non-char
    exn:fail:filesystem
      exn:fail:filesystem:exists
      exn:fail:filesystem:version
      exn:fail:filesystem:errno
      exn:fail:filesystem:missing-module
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
least-specific to most-specific.

This structure type implements the @racket[prop:exn:srclocs] property.}

@defstruct[(exn:fail:syntax:unbound exn:fail:syntax) ()
           #:inspector #f]{

Raised by @racket[#%top] or @racket[set!] for an
unbound identifier within a module.}

@defstruct[(exn:fail:syntax:missing-module exn:fail:syntax) ([path module-path?])
           #:inspector #f]{

Raised by the default @tech{module name resolver} or default
@tech{load handler} to report a module path---a reported in the
@racket[path] field---whose implementation file cannot be
found.

The default @tech{module name resolver} raises this exception only
when it is given a syntax object as its second argument, and the
default @tech{load handler} raises this exception only when the value
of @racket[current-module-path-for-load] is a syntax object (in which
case both the @racket[exprs] field and the @racket[path] field
are determined by the syntax object).

This structure type implements the @racket[prop:exn:missing-module] property.}

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
hostnames, but is allowed in @racket[exn:fail:filesystem:errno]
instances for consistency).}

@defstruct[(exn:fail:filesystem:missing-module exn:fail:filesystem) ([path module-path?])
           #:inspector #f]{

Raised by the default @tech{module name resolver} or default
@tech{load handler} to report a module path---a reported in the
@racket[path] field---whose implementation file cannot be
found.

The default @tech{module name resolver} raises this exception only
when it is @emph{not} given a syntax object as its second argument, and the
default @tech{load handler} raises this exception only when the value
of @racket[current-module-path-for-load] is @emph{not} a syntax object.

This structure type implements the @racket[prop:exn:missing-module] property.}

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
The @racket[continuation] field can be used to resume the interrupted
computation in the @tech{uncaught-exception handler} or
@racket[call-with-exception-handler] (but @emph{not}
@racket[with-handlers] because it escapes from the exception context
before evaluating any predicates or handlers).}

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
(struct exn:fail:he-who-shall-not-be-named exn:fail
  (a-srcloc)
  #:property prop:exn:srclocs
  (lambda (a-struct)
    (match a-struct
      [(exn:fail:he-who-shall-not-be-named msg marks a-srcloc)
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
          (raise (exn:fail:he-who-shall-not-be-named
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
;; of voldemort in g.
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

A @deftech{source location} is most frequently represented by a
@racket[srcloc] structure. More generally, a source location has the
same information as a @racket[srcloc] structure, but potentially
represented or accessed differently. For example, source-location
information is accessed from a @tech{syntax object} with functions
like @racket[syntax-source] and @racket[syntax-line], while
@racket[datum->syntax] accepts a source location as a list, vector, or
another syntax object. For ports, a combination of
@racket[object-name] and @racket[port-next-location] provides location
information, especially in a port for which counting has been enabled
through @racket[port-count-lines!].

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

]

See @secref["print-compiled"] for information about the treatment of
@racket[srcloc] values that are embedded in compiled code.}


@defproc[(srcloc->string [srcloc srcloc?]) (or/c string? #f)]{

Formats @racket[srcloc] as a string suitable for error reporting.  A
path source in @racket[srcloc] is shown relative to the value of
@racket[current-directory-for-user]. The result is @racket[#f] if
@racket[srcloc] does not contain enough information to format a
string.}


@defthing[prop:exn:missing-module struct-type-property?]{

A property that identifies structure types that provide a module path
for a load that fails because a module is not found.

The property value must be a procedure that accepts a single
value---the structure type instance from which to extract source
locations---and returns a @tech{module path}.}

@defproc[(exn:missing-module? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] has the @racket[prop:exn:missing-module]
property, @racket[#f] otherwise.}


@defproc[(exn:missing-module-accessor [v exn:srclocs?])
         (exn:missing-module? . -> . module-path?)]{

Returns the @tech{module path}-getting procedure associated with @racket[v].}

@;------------------------------------------------------------------------
@section{Additional Exception Functions}

@note-lib-only[racket/exn]

@history[#:added "6.3"]

@defproc[(exn->string [exn (or/c exn? any/c)]) string?]{

Formats @racket[exn] as a string. If @racket[exn] is an @racket[exn?],
collects and returns the output from the current
@racket[(error-display-handler)]; otherwise, simply converts
@racket[exn] to a string using @racket[(format "~s\n" exn)].}

@;----------------------------------------------------------------------
@section[#:tag "err-realm"]{Realms and Error Message Adjusters}

A @deftech{realm} identifies a convention for naming functions and
specifying contracts for function arguments and results. Realms are
intended to help improve layering and interoperability among languages
that are implemented on top of Racket.

Realms primarily enable a language to recognize and rewrite error
messages that are generated by lower layers of an implementation. For
example, a language's implementation of ``arrays'' might use Racket
vectors directly, but when an object-type or primitive bounds check
fails for a vector, the generated error message mentions ``vector''
and possibly a contract like @racket[vector?] and a function name like
@racket[vector-ref]. Since these error messages are identified as
being from the @racket['racket/primitive] realm, a language
implementation can look for @racket['racket/primitive] to detect and
rewrite error messages with minimal danger of mangling error messages
from other parts of an application (possibly implemented in the new
language) that happen to use the word ``vector.''

Each procedure and each module also has a realm. A procedure's realm
is relevant, for example, when it is applied to the wrong number of
arguments; in that case, the arity-error message itself is from the
@racket['racket/primitive] realm, but the error message also should
include the name of the procedure, which can be from some different
realm. Along similar lines, @racket[continuation-mark-set->context]
can report the realm associated with (the procedure for) each frame in
a continuation, which might be useful to identify boundary crossings.

The construction of an error message must cooperate explicitly with
error-message adjusting. The most basic may to cooperate is through
functions like @racket[error-message->adjusted-string] and
@racket[error-contract->adjusted-string], which run error-message
adjusters via the @racket[current-error-message-adjuster] parameter
and other adjusters associated with the current continuation using
@racket[error-message-adjuster-key] as a @tech{continuation-mark} key.
Functions like @racket[raise-argument-error] and
@racket[raise-arity-error] use @racket[error-message->adjusted-string]
and @racket[error-contract->adjusted-string] with the default realm,
@racket['racket]. Functions like @racket[raise-argument-error*] and
@racket[raise-arity-error*] accept an explicit realm argument.

Not all error functions automatically cooperate with error-message
adjusting. For example, the @racket[raise-reader-error] and
@racket[raise-syntax-error] functions do not call adjusters, because
they report errors that are intimately tied to syntax (and, along
those lines, errors of a more static nature).

@defproc[(error-message->adjusted-string [name (or/c symbol? #f)]
                                         [name-realm symbol?]
                                         [message string?]
                                         [message-realm symbol?])
         string?]{

Combines @racket[name] (if it is not @racket[#f]) with @racket[": "]
and then @racket[message] to generate an error-message string, but
first giving error-message adjusters a chance to adjust @racket[name]
and/or @racket[message].

Any adjuster functions associated with the current continuation as a
@tech{continuation mark} with @racket[error-message-adjuster-key] are
run first; the adjusters are run in order from shallowest to deepest.
Then, the adjuster value of @racket[current-error-message-adjuster] is
used.

Each adjuster is tried with the @racket['message] protocol, first. If
the adjuster responds with @racket[#f] for @racket['message], then the
@racket['name] protocol is tried. See
@racket[current-error-message-adjuster] for information on the
protocols. An adjuster that responds with @racket[#f] for both is
skipped, as is any value associated as continuation mark using
@racket[error-message-adjuster-key] where the value is not a procedure
that accepts one argument. In addition, the @racket['name] protocol is
skipped if the (possibly adjusted) @racket[name] is @racket[#f].

@history[#:added "8.4.0.2"]}


@defproc[(error-contract->adjusted-string [contract-str string?]
                                          [contract-realm symbol?])
         string?]{

Analogous to @racket[error-message->adjusted-string], but for just the
contract part of an error message. The result string is typically
incorporated into a larger error message that may then be adjusted
further.

Adjustment of contract string uses the @racket['contract] protocol as
described for @racket[current-error-message-adjuster].

@history[#:added "8.4.0.2"]}


@defparam[current-error-message-adjuster proc (symbol? . -> . (or/c procedure? #f))]{

A @tech{parameter} that determines an error-message adjuster that is
applied after any adjusters associated to the current continuation via
@racket[error-message-adjuster-key].

An adjuster procedure receives a symbol identifying a protocol, and it
must return either @racket[#f] or a procedure for performing
adjustments through that protocol. The following protocols are
currently defined, but more may be added in the future:

@itemlist[

 @item{@racket['name]: the procedure receives two arguments, a name
       symbol and a realm symbol; it returns an adjusted name symbol
       and an adjusted realm symbol.}

 @item{@racket['message]: the procedure receives four arguments: a
       name symbol or @racket[#f] (which means that no name will be
       prefixed on the message), a name-realm symbol, an message
       string, and a message-realm symbol; it returns four adjusted
       values.}

 @item{@racket['contract]: the procedure receives two arguments, a
       contract string and a realm symbol; it returns an adjusted
       contract string and an adjusted realm symbol.}

]

A new library or language can introduce additional mode symbols, too.
To avoid conflicts, prefix the mode symbol with a collection or
library name followed by @litchar{/}.

If an adjuster procedure returns @racket[#f] for a protocol, it's the
same as returning a function that performs no adjustment and returns
its arguments. The default value of this parameter returns @racket[#f]
for any symbol argument except the protocols listed above, for which
it returns a procedure that checks its arguments and returns them with
no adjustment.

@history[#:added "8.4.0.2"]}


@defthing[error-message-adjuster-key symbol?]{

An @tech{uninterned} symbol intended for use as a @tech{continuation
mark} key with an error-adjuster procedure value. An error adjuster
associated with the key should follow the same protocol as a value of
@racket[current-error-message-adjuster].

See @racket[error-message->adjusted-string] for a description of how
marks using this key are can adjust error messages.

@history[#:added "8.4.0.2"]}
