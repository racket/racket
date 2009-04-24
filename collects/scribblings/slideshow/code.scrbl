#lang scribble/doc
@(require "ss.ss"
          (for-label slideshow/code
                     scheme/gui/base))

@(define stx-obj
  (tech #:doc '(lib "scribblings/reference/reference.scrbl") "syntax object"))

@title{Typesetting Scheme Code}

@defmodule[slideshow/code]{The @scheme[slideshow/code] library
provides utilities for typesetting Scheme code as a pict.}

@defproc[(typeset-code [stx syntax?]) pict?]{

Produces a pict for code in the given @|stx-obj|. The
source-location information of the syntax object determines the line
breaks, line indenting, and space within a row.  Empty rows are
ignored. 

Beware that if you use @scheme[read-syntax] on a file port, you may
have to turn on line counting via @scheme[port-count-lines!] for the
code to typeset properly. Also beware that when a source file
containing a @scheme[syntax] or @scheme[quote-syntax] form is
compiled, source location information is omitted from the compiled
@|stx-obj|.

Normally, @scheme[typeset-code] is used through the @scheme[code]
syntactic form, which works properly with compilation, and that
escapes to pict-producing code via @scheme[unsyntax]. See also
@scheme[define-code].

Embedded picts within @scheme[stx] are used directly. Row elements are
combined using and operator like @scheme[htl-append], so use
@scheme[code-align] (see below) as necessary to add an ascent to
ascentless picts. More precisely, creation of a line of code uses
@scheme[pict-last] to determine the end point of the element most
recently added to a line; the main effect is that closing parentheses
are attached in the right place when a multi-line pict is embedded in
@scheme[stx].

An identifier that starts with @litchar{_} is italicized in the pict,
and the @litchar{_} is dropped, unless the
@scheme[code-italic-underscore-enabled] parameter is set to
@scheme[#f]. Also, unless @scheme[code-scripts-enabled] is set to
@scheme[#f], @litchar{_} and @litchar{^} in the middle of a word
create superscripts and subscripts, respectively (like TeX); for
example @schemeidfont{foo^4_ok} is displayed as the identifier
@schemeidfont{foo} with a @schemeidfont{4} superscript and an
@schemeidfont{ok} subscript.

Further, uses of certain identifiers in @scheme[stx] typeset
specially:

@itemize[

 @item{@as-index{@schemeidfont{code:blank}} --- produces a space.}

 @item{@scheme[(#,(as-index (schemeidfont "code:comment")) _s ...)]
 --- produces a comment block, with each @scheme[_s] on its own line,
 where each @scheme[_s] must be a string or a pict.}

 @item{@scheme[(#,(as-index (schemeidfont "code:line")) _datum ...)]
 --- typesets the @scheme[_datum] sequence, which is mostly useful for
 the top-level sequence, since @scheme[typeset-code] accepts only one
 argument.}

 @item{@scheme[(#,(as-index (schemeidfont "code:contract")) _datum
 ...)]  --- like @schemeidfont{code:line}, but every @scheme[_datum]
 is colored as a comment, and a @litchar{;} is prefixed to every line.}

 @item{@scheme[(#,(as-index (schemeidfont "code:template")) _datum
 ...)]  --- like @schemeidfont{code:line}, but a @litchar{;} is
 prefixed to every line.}

 @item{@schemeidfont{$} --- typesets as a vertical bar (for no
 particularly good reason).}

]}


@defform[(code datum ...)]{

The macro form of @scheme[typeset-code]. Within a @scheme[datum],
@scheme[unsyntax] can be used to escape to an expression.

For more information, see @scheme[typeset-code] and
@scheme[define-code], since @scheme[code] is defined as

@schemeblock[
(define-code code typeset-code)
]}


@defparam[current-code-font style text-style/c]{

Parameter for a base font used to typeset text. The default is
@scheme[`(bold . modern)]. For even more control, see
@scheme[current-code-tt].}


@defparam[current-code-tt proc (string? . -> . pict?)]{

Parameter for a one-argument procedure to turn a
  string into a pict, used to typeset text. The default is

@schemeblock[
(lambda (s) (text s (current-code-font) (current-font-size)))
]

This procedure is not used to typeset subscripts or other items that
require font changes, where @scheme[current-code-font] is used
directly.}


@defparam[current-code-line-sep amt real?]{

A parameter that determines the spacing between lines of typeset code.
The default is @scheme[2].}


@defparam[current-comment-color color (or/c string? (is-a?/c color%))]{

A parameter for the color of comments.}


@defparam[current-keyword-color color (or/c string? (is-a?/c color%))]{

A parameter for the color of syntactic-form names. See
@scheme[current-keyword-list].}


@defparam[current-id-color color (or/c string? (is-a?/c color%))]{

A parameter for the color of identifiers that are not syntactic form
names or constants.}


@defparam[current-literal-color color (or/c string? (is-a?/c color%))]{

A parameter for the color of literal values, such as strings and
numbers.  See also @scheme[current-literal-list]}


@defparam[current-const-color color (or/c string? (is-a?/c color%))]{

A parameter for the color of constant names. See
@scheme[current-const-list].}


@defparam[current-base-color color (or/c string? (is-a?/c color%))]{

A parameter for the color of everything else.}


@defparam[current-reader-forms syms (listof symbol?)]{

Parameter for a list of symbols indicating which built-in reader forms
should be used. The default is @scheme['(quote quasiquote unquote
unquote-splicing syntax quasisyntax unsyntax unsyntax-splicing)].
Remove a symbol to suppress the corresponding reader output.}


@defproc[(code-align [pict pict?]) pict?]{

Adjusts the ascent of @scheme[pict] so that its bottom aligns with the
baseline for text; use this function when @scheme[pict] has no
ascent.}


@defparam[current-keyword-list names (listof string?)]{

A list of strings to color as syntactic-form names. The default
includes most of the forms provided by @scheme[scheme/base].}


@defparam[current-const-list names (listof string?)]{

A list of strings to color as constant names. The default is
@scheme[null].}


@defparam[current-literal-list names (listof string?)]{

A list of strings to color as literals, in addition to literals such
as strings. The default is @scheme[null].}

@defthing[mzscheme-const-list (listof string?)]{

A list of strings that could be used to initialize the
@scheme[current-const-list] parameter.}

@defboolparam[code-colorize-enabled on?]{

A parameter to enable or disable all code coloring. The default is
@scheme[#t].}


@defboolparam[code-colorize-quote-enabled on?]{

A parameter to control whether under a @scheme[quote] is colorized as
a literal (as in this documentation). The default is @scheme[#t].}


@defboolparam[code-italic-underscore-enabled on?]{

A parameter to control whether @litchar{_}-prefixed identifiers are
italicized (dropping the @litchar{_}). The default is @scheme[#t].}

@defboolparam[code-scripts-enabled on?]{

A parameter to control whether TeX-style subscripts and subscripts are
recognized in an identifier.}

@defform*[[(define-code code-id typeset-code-id) 
           (define-code code-id typeset-code-id escape-id)]]{

Defines @scheme[code-id] as a macro that uses
@scheme[typeset-code-id], which is a function with the same input as
@scheme[typeset-code]. The @scheme[escape-id] form defaults to
@scheme[unsyntax].

The resulting @scheme[code-id] syntactic form takes a sequence of
@scheme[_datum]s:

@schemeblock[
(code-id _datum ...)
]

It produces a pict that typesets the sequence. Source-location
information for the @scheme[_datum] determines the layout of code in
the resulting pict. The @scheme[code-id] is expanded in such a way
that source location is preserved during compilation (so
@scheme[typeset-code-id] receives a syntax object with source
locations intact).

If a @scheme[_datum] contains @scheme[(escape-id _expr)]---perhaps as
@SCHEME[#,_expr] when @scheme[escape-id] is @scheme[unsyntax]---then
the @scheme[_expr] is evaluated and the result datum is spliced in
place of the @scheme[escape-id] form in @scheme[_datum]. If the result
is not a syntax object, it is given the source location of the
@scheme[escape-id] form. A pict value intected this way as a
@scheme[_datum] is rendered as itself.}


@defform[(define-exec-code (pict-id runnable-id string-id)
           datum ...)]{

Binds @scheme[pict-id] to the result of @scheme[(code datum ...)],
except that if an identifier @schemeidfont{_} appears anywhere in a
@scheme[datum], then the identifier and the following expression are
not included for @scheme[code].

Meanwhile, @scheme[runnable-id] is bound to a @|stx-obj| that wraps
the @scheme[datum]s in a @scheme[begin]. In this case,
@schemeidfont{_}s are removed from the @scheme[datum]s, but not the
following expression. Thus, an @schemeidfont{_} identifier is used to
comment out an expression from the pict, but have it present in the
@|stx-obj| for evaluation.

The @scheme[string-id] is bound to a string representation of the code
that is in the pict. This string is useful for copying to the
clipboard with @scheme[(send the-clipboard set-clipboard-string
string-id 0)].}


@defform[(define-exec-code/scale scale-expr (pict-id runnable-id string-id)
           datum ...)]{

Like @scheme[define-exec-code], but with a scale to use via
@scheme[scale/improve-new-text] when generating the pict.}


@deftogether[(
@defthing[comment-color (or/c string? (is-a?/c color%))]
@defthing[keyword-color (or/c string? (is-a?/c color%))]
@defthing[id-color (or/c string? (is-a?/c color%))]
@defthing[literal-color (or/c string? (is-a?/c color%))]
)]{

For backward compatibility, the default values for
@scheme[current-comment-color], etc.}

@defproc[(code-pict-bottom-line-pict [pict pict?])
         (or/c pict? #f)]{

The same as @scheme[pict-last], provided for backward compatibility.}

@defproc[(pict->code-pict [pict pict?] [bl-pict (or/c pict? #f)]) pict?]{

Mainly for backward compatibility: returns @scheme[(if bl-pict
(use-last pict (or (pict-last bl-pict) bl-pict)))].}

