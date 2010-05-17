#lang scribble/doc
@(require "common.rkt"
          (for-label mzlib/pconvert
                     mzlib/pconvert-prop
                     scheme/pretty))

@mzlib[#:mode title pconvert]

The @schememodname[mzlib/pconvert] library defines routines for
printing Scheme values as @scheme[eval]uable S-expressions, rather
than @scheme[read]able S-expressions.

The @scheme[print-convert] procedure does not print values; rather, it
converts a Scheme value into another Scheme value such that the new
value pretty-prints as a Scheme expression that evaluates to the
original value. For example, @scheme[(pretty-print (print-convert `(9
,(box 5) #(6 7))))] prints the literal expression @schemeresult[(list
9 (box 5) (vector 6 7))] to the current output port.

To install print converting into the read-eval-print loop, require
@scheme[mzlib/pconvert] and call the procedure
@scheme[install-converting-printer].

In addition to @scheme[print-convert], this library provides
@scheme[print-convert], @scheme[build-share], @scheme[get-shared], 
and @scheme[print-convert-expr].  The last three are used to convert
sub-expressions of a larger expression (potentially with shared
structure).

See also @scheme[prop:print-convert-constructor-name].

@defboolparam[abbreviate-cons-as-list abbreviate?]{

A parameter that controls how lists are represented with
constructor-style conversion. If the parameter's value is @scheme[#t],
lists are represented using @scheme[list]. Otherwise, lists are
represented using @scheme[cons]. The initial value of the parameter is
@scheme[#t].}


@defboolparam[booleans-as-true/false use-name?]{

A parameter that controls how @scheme[#t] and @scheme[#f] are
represented. If the parameter's value is @scheme[#t], then @scheme[#t]
is represented as @scheme[true] and @scheme[#f] is represented as
@scheme[false].  The initial value of the parameter is @scheme[#t].}


@defparam[use-named/undefined-handler use-handler (any/c . -> . any/c)]{

A parameter that controls how values that have inferred names are
represented. The procedure is passed a value. If the procedure returns
true, the procedure associated with @scheme[named/undefined-handler]
is invoked to render that value. Only values that have inferred names
but are not defined at the top-level are used with this handler.

The initial value of the parameter is @scheme[(lambda (x) #f)].}


@defparam[named/undefined-handler use-handler (any/c . -> . any/c)]{

Parameter for a procedure that controls how values that have inferred
names are represented. The procedure is called only if
@scheme[use-named/undefined-handler] returns true for some value. In
that case, the procedure is passed that same value, and the result of
the parameter is used as the representation for the value.

The initial value of the parameter is @scheme[(lambda (x) #f)].}


@defproc[(build-share [v any/c]) ....]{

Takes a value and computes sharing information used for representing
the value as an expression. The return value is an opaque structure
that can be passed back into @scheme[get-shared] or
@scheme[print-convert-expr].}


@defboolparam[constructor-style-printing use-constructors?]{

Parameter that controls how values are represented after conversion.
If this parameter's value is @scheme[#t], then constructors are used;
e.g., pair containing @scheme[1] and @scheme[2] is represented as
@scheme[(cons 1 2)].  Otherwise, @scheme[quasiquote]-style syntax is
used; e.g., the pair containing @scheme[1] and @scheme[2] is
represented as @scheme[`(1 . 2)]. The initial value of the parameter
is @scheme[#f].

The constructor used for mutable pairs is @schemeidfont{mcons}, unless
@scheme[print-mpair-curly-braces] is set to @scheme[#f], in which case
@schemeidfont{cons} and @schemeidfont{list} are used. Similarly, when
using @scheme[quasiquote] style and @scheme[print-mpair-curly-braces]
is set to @scheme[#f], mutable pair constructions are represented
using @schemeidfont{quote}, @schemeidfont{quasiquote}, etc.

See also @scheme[quasi-read-style-printing] and
@scheme[prop:print-convert-constructor-name].}


@defparam[current-build-share-hook 
          hook
          (any/c (any/c . -> . void?)
                 (any/c . -> . void?) . -> . any)]{

Parameter that sets a procedure used by @scheme[print-convert] and
@scheme[build-share] to assemble sharing information. The procedure
@scheme[hook] takes three arguments: a value @scheme[_v], a procedure
@scheme[_basic-share], and a procedure @scheme[_sub-share]; the return
value is ignored. The @scheme[basic-share] procedure takes @scheme[_v]
and performs the built-in sharing analysis, while the
@scheme[_sub-share] procedure takes a component of @scheme[_v] ands
analyzes it. Sharing information is accumulated as values are passed
to @scheme[basic-share] and @scheme[sub-share].

A @scheme[current-build-share-hook] procedure usually works together
with a @scheme[current-print-convert-hook] procedure.}


@defparam[current-build-share-name-hook hook (any/c . -> . (or/c symbol? false/c))]{

Parameter that sets a procedure used by @scheme[print-convert] and
@scheme[build-share] to generate a new name for a shared value. The
@scheme[hook] procedure takes a single value and returns a symbol for
the value's name. If @scheme[hook] returns @scheme[#f], a name is
generated using the form
``@schemeidfont{-}@scheme[_n]@schemeidfont{-}, where @scheme[n] is an
integer.}


@defparam[current-print-convert-hook
          hook
          (any/c/ (any/c . -> . any/c)
                  (any/c . -> . any/c))]{

Parameter that sets a procedure used by @scheme[print-convert] and
@scheme[print-convert-expr] to convert values. The procedure
@scheme[hook] takes three arguments---a value @scheme[_v], a procedure
@scheme[_basic-convert], and a procedure @scheme[_sub-convert]---and
returns the converted representation of @scheme[_v]. The
@scheme[_basic-convert] procedure takes @scheme[_v] and returns the
default conversion, while the @scheme[_sub-convert] procedure takes a
component of @scheme[_v] and returns its conversion.

A @scheme[current-print-convert-hook] procedure usually works together
with a @scheme[current-build-share-hook] procedure.}


@defparam[current-read-eval-convert-print-prompt str string?]{

Parameter that sets the prompt used by
@scheme[install-converting-printer].
The initial value is @scheme["|- "].}


@defproc[(get-shared [share-info ....]
                     [cycles-only? any/c #f])
         (list-of (cons/c symbol? any/c))]{

The @scheme[shared-info] value must be a result from @scheme[build-share].
The procedure returns a list matching variables to shared values
within the value passed to @scheme[build-share].

The default value for @scheme[cycles-only?] is @scheme[#f];
if it is not @scheme[#f], @scheme[get-shared] returns only information
about cycles.

For example,

@schemeblock[
(get-shared (build-share (shared ([a (cons 1 b)]
                                  [b (cons 2 a)]) 
                           a)))
]

might return the list 

@schemeblock[
'((-1- (cons 1 -2-)) (-2- (cons 2 -1-)))
]}


@defproc[(install-converting-printer) void?]{

Sets the current print handler to print values using
@scheme[print-convert]. The current read handler is also set to use
the prompt returned by
@scheme[current-read-eval-convert-print-prompt].}


@defproc[(print-convert [v any/c][cycles-only? any/c (show-sharing)]) any/c]{

Converts the value @scheme[v]. If @scheme[cycles-only?] is not
@scheme[#f], then only circular objects are included in the
output.}


@defproc[(print-convert-expr [share-info ....]
                             [v any/c]
                             [unroll-once? any/c]) any/c]{

Converts the value @scheme[v] using sharing information
@scheme[share-info], which was previously returned by
@scheme[build-share] for a value containing @scheme[v]. If the most
recent call to @scheme[get-shared] with @scheme[share-info] requested
information only for cycles, then @scheme[print-convert-expr] will
only display sharing among values for cycles, rather than showing all
value sharing.

The @scheme[unroll-once?] argument is used if @scheme[v] is a shared
value in @scheme[share-info]. In this case, if @scheme[unroll-once?]
is @scheme[#f], then the return value will be a shared-value
identifier; otherwise, the returned value shows the internal structure
of @scheme[v] (using shared value identifiers within @scheme[v]'s
immediate structure as appropriate).}


@defboolparam[quasi-read-style-printing on?]{

Parameter that controls how vectors and boxes are represented after
conversion when the value of @scheme[constructor-style-printing] is
@scheme[#f]. If @scheme[quasi-read-style-printing] is set to
@scheme[#f], then boxes and vectors are unquoted and represented using
constructors. For example, the list of a box containing the number 1
and a vector containing the number 1 is represented as @scheme[`(,(box
1) ,(vector 1))]. If the parameter's value is @scheme[#t], then
@scheme[#&....]  and @scheme[#(....)] are used, e.g., @scheme[`(#&1
#(1))].  The initial value of the parameter is @scheme[#t].}


@defboolparam[show-sharing show?]{

Parameter that determines whether sub-value sharing is conserved (and
shown) in the converted output by default. The initial value of the
parameter is @scheme[#t].}


@defboolparam[whole/fractional-exact-numbers whole-frac?]{

Parameter that controls how exact, non-integer numbers are converted
when the numerator is greater than the denominator. If the parameter's
value is @scheme[#t], the number is converted to the form @scheme[(+
_integer _fraction)] (i.e., a list containing @scheme['+], an exact
integer, and an exact rational less than @scheme[1] and greater than
@scheme[-1]). The initial value of the parameter is @scheme[#f].}

