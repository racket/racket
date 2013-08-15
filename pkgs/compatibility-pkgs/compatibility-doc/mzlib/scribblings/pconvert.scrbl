#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/pconvert
                     mzlib/pconvert-prop
                     racket/contract
                     racket/pretty))

@mzlib[#:mode title pconvert]

The @racketmodname[mzlib/pconvert] library defines routines for
printing Racket values as @racket[eval]uable S-expressions. Racket's
default printing mode also prints values as expressions (in contrast
to the Lisp and Racket tradition of printing @racket[read]able
S-expressions), but @racketmodname[mzlib/pconvert] is more
configurable and approximates expressions for a wider range of
values. For example, procedures print using @racketresultfont{lambda}
instead of @racketresultfont{#<procedure>}.

The @racket[print-convert] procedure does not print values; rather, it
converts a Racket value into another Racket value such that the new
value @racket[pretty-write]s as a Racket expression that evaluates to
the original value. For example, @racket[(pretty-write (print-convert
`(9 ,(box 5) #(6 7))))] prints the literal expression
@racketresult[(list 9 (box 5) (vector 6 7))] to the current output
port.

To install print converting into the read-eval-print loop, require
@racketmodname[mzlib/pconvert] and call the procedure
@racket[install-converting-printer].

In addition to @racket[print-convert], this library provides
@racket[print-convert], @racket[build-share], @racket[get-shared], 
and @racket[print-convert-expr].  The last three are used to convert
sub-expressions of a larger expression (potentially with shared
structure).

See also @racket[prop:print-convert-constructor-name].

@defboolparam[abbreviate-cons-as-list abbreviate?]{

A parameter that controls how lists are represented with
constructor-style conversion. If the parameter's value is @racket[#t],
lists are represented using @racket[list]. Otherwise, lists are
represented using @racket[cons]. The initial value of the parameter is
@racket[#t].}


@defboolparam[booleans-as-true/false use-name?]{

A parameter that controls how @racket[#t] and @racket[#f] are
represented. If the parameter's value is @racket[#t], then @racket[#t]
is represented as @racket[true] and @racket[#f] is represented as
@racket[false].  The initial value of the parameter is @racket[#t].}


@defparam[use-named/undefined-handler use-handler (any/c . -> . any/c)]{

A parameter that controls how values that have inferred names are
represented. The procedure is passed a value. If the procedure returns
true, the procedure associated with @racket[named/undefined-handler]
is invoked to render that value. Only values that have inferred names
but are not defined at the top-level are used with this handler.

The initial value of the parameter is @racket[(lambda (x) #f)].}


@defparam[named/undefined-handler use-handler (any/c . -> . any/c)]{

Parameter for a procedure that controls how values that have inferred
names are represented. The procedure is called only if
@racket[use-named/undefined-handler] returns true for some value. In
that case, the procedure is passed that same value, and the result of
the parameter is used as the representation for the value.

The initial value of the parameter is @racket[(lambda (x) #f)].}


@defboolparam[add-make-prefix-to-constructor add-prefix?]{

A parameter that controls whether a @racketidfont{make-} prefix is
added to a constructor name for a structure instance.  The initial
value of the parameter is @racket[#f].}



@defproc[(build-share [v any/c]) ....]{

Takes a value and computes sharing information used for representing
the value as an expression. The return value is an opaque structure
that can be passed back into @racket[get-shared] or
@racket[print-convert-expr].}


@defboolparam[constructor-style-printing use-constructors?]{

Parameter that controls how values are represented after conversion.
If this parameter's value is @racket[#t], then constructors are used;
e.g., pair containing @racket[1] and @racket[2] is represented as
@racket[(cons 1 2)].  Otherwise, @racket[quasiquote]-style syntax is
used; e.g., the pair containing @racket[1] and @racket[2] is
represented as @racket[`(1 . 2)]. The initial value of the parameter
is @racket[#f].

The constructor used for mutable pairs is @racketidfont{mcons}, unless
@racket[print-mpair-curly-braces] is set to @racket[#f], in which case
@racketidfont{cons} and @racketidfont{list} are used. Similarly, when
using @racket[quasiquote] style and @racket[print-mpair-curly-braces]
is set to @racket[#f], mutable pair constructions are represented
using @racketidfont{quote}, @racketidfont{quasiquote}, etc.

See also @racket[quasi-read-style-printing] and
@racket[prop:print-convert-constructor-name].}


@defparam[current-build-share-hook 
          hook
          (any/c (any/c . -> . void?)
                 (any/c . -> . void?) . -> . any)]{

Parameter that sets a procedure used by @racket[print-convert] and
@racket[build-share] to assemble sharing information. The procedure
@racket[hook] takes three arguments: a value @racket[_v], a procedure
@racket[_basic-share], and a procedure @racket[_sub-share]; the return
value is ignored. The @racket[basic-share] procedure takes @racket[_v]
and performs the built-in sharing analysis, while the
@racket[_sub-share] procedure takes a component of @racket[_v] ands
analyzes it. Sharing information is accumulated as values are passed
to @racket[basic-share] and @racket[sub-share].

A @racket[current-build-share-hook] procedure usually works together
with a @racket[current-print-convert-hook] procedure.}


@defparam[current-build-share-name-hook hook (any/c . -> . (or/c symbol? false/c))]{

Parameter that sets a procedure used by @racket[print-convert] and
@racket[build-share] to generate a new name for a shared value. The
@racket[hook] procedure takes a single value and returns a symbol for
the value's name. If @racket[hook] returns @racket[#f], a name is
generated using the form
``@racketidfont{-}@racket[_n]@racketidfont{-}, where @racket[n] is an
integer.}


@defparam[current-print-convert-hook
          hook
          (any/c (any/c . -> . any/c)
                 (any/c . -> . any/c)
                 . -> . any/c)]{

Parameter that sets a procedure used by @racket[print-convert] and
@racket[print-convert-expr] to convert values. The procedure
@racket[hook] takes three arguments---a value @racket[_v], a procedure
@racket[_basic-convert], and a procedure @racket[_sub-convert]---and
returns the converted representation of @racket[_v]. The
@racket[_basic-convert] procedure takes @racket[_v] and returns the
default conversion, while the @racket[_sub-convert] procedure takes a
component of @racket[_v] and returns its conversion.

A @racket[current-print-convert-hook] procedure usually works together
with a @racket[current-build-share-hook] procedure.}


@defparam[current-read-eval-convert-print-prompt str string?]{

Parameter that sets the prompt used by
@racket[install-converting-printer].
The initial value is @racket["|- "].}


@defproc[(get-shared [share-info ....]
                     [cycles-only? any/c #f])
         (list-of (cons/c symbol? any/c))]{

The @racket[shared-info] value must be a result from @racket[build-share].
The procedure returns a list matching variables to shared values
within the value passed to @racket[build-share].

The default value for @racket[cycles-only?] is @racket[#f];
if it is not @racket[#f], @racket[get-shared] returns only information
about cycles.

For example,

@racketblock[
(get-shared (build-share (shared ([a (cons 1 b)]
                                  [b (cons 2 a)]) 
                           a)))
]

might return the list 

@racketblock[
'((-1- (cons 1 -2-)) (-2- (cons 2 -1-)))
]}


@defproc[(install-converting-printer) void?]{

Sets the current print handler to print values using
@racket[print-convert] and sets @racket[print-as-expression] to
@racket[#f] (since the conversion of a value is meant to be printed in
@racket[read]able form rather than @racket[eval]uable form). The
current read handler is also set to use the prompt returned by
@racket[current-read-eval-convert-print-prompt].}


@defproc[(print-convert [v any/c][cycles-only? any/c (show-sharing)]) any/c]{

Converts the value @racket[v]. If @racket[cycles-only?] is not
@racket[#f], then only circular objects are included in the
output.}


@defproc[(print-convert-expr [share-info ....]
                             [v any/c]
                             [unroll-once? any/c]) any/c]{

Converts the value @racket[v] using sharing information
@racket[share-info], which was previously returned by
@racket[build-share] for a value containing @racket[v]. If the most
recent call to @racket[get-shared] with @racket[share-info] requested
information only for cycles, then @racket[print-convert-expr] will
only display sharing among values for cycles, rather than showing all
value sharing.

The @racket[unroll-once?] argument is used if @racket[v] is a shared
value in @racket[share-info]. In this case, if @racket[unroll-once?]
is @racket[#f], then the return value will be a shared-value
identifier; otherwise, the returned value shows the internal structure
of @racket[v] (using shared value identifiers within @racket[v]'s
immediate structure as appropriate).}


@defboolparam[quasi-read-style-printing on?]{

Parameter that controls how vectors and boxes are represented after
conversion when the value of @racket[constructor-style-printing] is
@racket[#f]. If @racket[quasi-read-style-printing] is set to
@racket[#f], then boxes and vectors are unquoted and represented using
constructors. For example, the list of a box containing the number 1
and a vector containing the number 1 is represented as @racketresult[`(,(box
1) ,(vector 1))]. If the parameter's value is @racket[#t], then
@racket[#&....]  and @racket[#(....)] are used, e.g., @racket[`(#&1
#(1))].  The initial value of the parameter is @racket[#t].}


@defboolparam[show-sharing show?]{

Parameter that determines whether sub-value sharing is conserved (and
shown) in the converted output by default. The initial value of the
parameter is @racket[#t].}


@defboolparam[whole/fractional-exact-numbers whole-frac?]{

Parameter that controls how exact, non-integer numbers are converted
when the numerator is greater than the denominator. If the parameter's
value is @racket[#t], the number is converted to the form @racket[(+
_integer _fraction)] (i.e., a list containing @racket['+], an exact
integer, and an exact rational less than @racket[1] and greater than
@racket[-1]). The initial value of the parameter is @racket[#f].}

