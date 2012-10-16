#lang scribble/doc
@(require "ss.rkt" 
          scribble/eval
          (for-label slideshow/code
                     racket/gui/base))

@(define stx-obj
  (tech #:doc '(lib "scribblings/reference/reference.scrbl") "syntax object"))

@(define ss-eval (make-base-eval))
@(interaction-eval #:eval ss-eval 
  (begin
   (require slideshow/code-pict
            slideshow/pict
            (for-syntax racket/base))
   (current-code-tt (lambda (s) (text s "monospace" 14)))
   (define-code code typeset-code)))

@title{Typesetting Racket Code}

@defmodule*[(slideshow/code-pict slideshow/code)]{
The @racketmodname[slideshow/code-pict] library
provides utilities for typesetting Racket code as a pict.
The @racketmodname[slideshow/code] library initializes
@racket[get-current-code-font-size] to @racket[current-font-size].}

@defproc[(typeset-code [stx syntax?]) pict?]{

Produces a pict for code in the given @|stx-obj|. The
source-location information of the syntax object determines the line
breaks, line indenting, and space within a row.  Empty rows are
ignored. 

Beware that if you use @racket[read-syntax] on a file port, you may
have to turn on line counting via @racket[port-count-lines!] for the
code to typeset properly. Also beware that when a source file
containing a @racket[syntax] or @racket[quote-syntax] form is
compiled, source location information is omitted from the compiled
@|stx-obj|.

Normally, @racket[typeset-code] is used through the @racket[code]
syntactic form, which works properly with compilation, and that
escapes to pict-producing code via @racket[unsyntax]. See also
@racket[define-code].

Embedded picts within @racket[stx] are used directly. Row elements are
combined using and operator like @racket[htl-append], so use
@racket[code-align] (see below) as necessary to add an ascent to
ascentless picts. More precisely, creation of a line of code uses
@racket[pict-last] to determine the end point of the element most
recently added to a line; the main effect is that closing parentheses
are attached in the right place when a multi-line pict is embedded in
@racket[stx].

An identifier that starts with @litchar{_} is italicized in the pict,
and the @litchar{_} is dropped, unless the
@racket[code-italic-underscore-enabled] parameter is set to
@racket[#f]. Also, unless @racket[code-scripts-enabled] is set to
@racket[#f], @litchar{_} and @litchar{^} in the middle of a word
create superscripts and subscripts, respectively (like TeX); for
example @racketidfont{foo^4_ok} is displayed as the identifier
@racketidfont{foo} with a @racketidfont{4} superscript and an
@racketidfont{ok} subscript.

Further, uses of certain identifiers in @racket[stx] typeset
specially:

@itemize[

 @item{@as-index{@racketidfont{code:blank}} --- produces a space.}

 @item{@racket[(#,(as-index (racketidfont "code:comment")) _s ...)]
 --- produces a comment block, with each @racket[_s] on its own line,
 where each @racket[_s] must be a string or a pict.}

 @item{@racket[(#,(as-index (racketidfont "code:line")) _datum ...)]
 --- typesets the @racket[_datum] sequence, which is mostly useful for
 the top-level sequence, since @racket[typeset-code] accepts only one
 argument.}

 @item{@racket[(#,(as-index (racketidfont "code:contract")) _datum
 ...)]  --- like @racketidfont{code:line}, but every @racket[_datum]
 is colored as a comment, and a @litchar{;} is prefixed to every line.}

 @item{@racket[(#,(as-index (racketidfont "code:template")) _datum
 ...)]  --- like @racketidfont{code:line}, but a @litchar{;} is
 prefixed to every line.}

 @item{@racketidfont{$} --- typesets as a vertical bar (for no
 particularly good reason).}

]}


@defform[(code datum ...)]{

The macro form of @racket[typeset-code]. Within a @racket[datum],
@racket[unsyntax] can be used to escape to an expression, and
identifiers bound as syntax to @tech{code transformer}s trigger
transformations.

For more information, see @racket[typeset-code] and
@racket[define-code], since @racket[code] is defined as

@racketblock[
(define-code code typeset-code)
]

@defexamples[#:eval ss-eval #:escape potato
  (code (+ 1 2))
  (code (+ 1 #,(+ 1 1)))
  (code (+ 1 #,(frame (code 2))))
  (define-syntax two (make-code-transformer #'(code 2)))
  (code (+ 1 two))
]}


@defparam[current-code-font style text-style/c]{

Parameter for a base font used to typeset text. The default is
@racket[`(bold . modern)]. For even more control, see
@racket[current-code-tt].}


@defparam[current-code-tt proc (string? . -> . pict?)]{

Parameter for a one-argument procedure to turn a
  string into a pict, used to typeset text. The default is

@racketblock[
(lambda (s) (text s (current-code-font) ((get-current-code-font-size))))
]

This procedure is not used to typeset subscripts or other items that
require font changes, where @racket[current-code-font] is used
directly.}


@defparam[get-current-code-font-size proc (-> exact-nonnegative-integer?)]{

A parameter used to access the default font size. The
@racketmodname[slideshow/code] library initializes this parameter to
@racket[current-font-size].}


@defparam[current-code-line-sep amt real?]{

A parameter that determines the spacing between lines of typeset code.
The default is @racket[2].}


@defparam[current-comment-color color (or/c string? (is-a?/c color%))]{

A parameter for the color of comments.}


@defparam[current-keyword-color color (or/c string? (is-a?/c color%))]{

A parameter for the color of syntactic-form names. See
@racket[current-keyword-list].}


@defparam[current-id-color color (or/c string? (is-a?/c color%))]{

A parameter for the color of identifiers that are not syntactic form
names or constants.}


@defparam[current-literal-color color (or/c string? (is-a?/c color%))]{

A parameter for the color of literal values, such as strings and
numbers.  See also @racket[current-literal-list]}


@defparam[current-const-color color (or/c string? (is-a?/c color%))]{

A parameter for the color of constant names. See
@racket[current-const-list].}


@defparam[current-base-color color (or/c string? (is-a?/c color%))]{

A parameter for the color of everything else.}


@defparam[current-reader-forms syms (listof symbol?)]{

Parameter for a list of symbols indicating which built-in reader forms
should be used. The default is @racket['(quote quasiquote unquote
unquote-splicing syntax quasisyntax unsyntax unsyntax-splicing)].
Remove a symbol to suppress the corresponding reader output.}


@defproc[(code-align [pict pict?]) pict?]{

Adjusts the ascent of @racket[pict] so that its bottom aligns with the
baseline for text; use this function when @racket[pict] has no
ascent.}


@defparam[current-keyword-list names (listof string?)]{

A list of strings to color as syntactic-form names. The default
includes all of the forms provided by @racketmodname[racket/base] 
and all of the forms provided by @racketmodname[mzscheme].}


@defparam[current-const-list names (listof string?)]{

A list of strings to color as constant names. The default is
@racket[null].}


@defparam[current-literal-list names (listof string?)]{

A list of strings to color as literals, in addition to literals such
as strings. The default is @racket[null].}

@defthing[racket/base-const-list (listof string?)]{

A list of strings that could be used to initialize the
@racket[current-const-list] parameter.}

@defthing[mzscheme-const-list (listof string?)]{

A list of strings that could be used to initialize the
@racket[current-const-list] parameter.}

@defboolparam[code-colorize-enabled on?]{

A parameter to enable or disable all code coloring. The default is
@racket[#t].}


@defboolparam[code-colorize-quote-enabled on?]{

A parameter to control whether under a @racket[quote] is colorized as
a literal (as in this documentation). The default is @racket[#t].}


@defboolparam[code-italic-underscore-enabled on?]{

A parameter to control whether @litchar{_}-prefixed identifiers are
italicized (dropping the @litchar{_}). The default is @racket[#t].}

@defboolparam[code-scripts-enabled on?]{

A parameter to control whether TeX-style subscripts and subscripts are
recognized in an identifier.}

@defform*[[(define-code code-id typeset-code-id) 
           (define-code code-id typeset-code-id escape-id)]]{

Defines @racket[code-id] as a macro that uses
@racket[typeset-code-id], which is a function with the same input as
@racket[typeset-code]. The @racket[escape-id] form defaults to
@racket[unsyntax].

The resulting @racket[code-id] syntactic form takes a sequence of
@racket[_datum]s:

@racketblock[
(code-id _datum ...)
]

It produces a pict that typesets the sequence. Source-location
information for the @racket[_datum] determines the layout of code in
the resulting pict. The @racket[code-id] is expanded in such a way
that source location is preserved during compilation (so
@racket[typeset-code-id] receives a syntax object with source
locations intact).

If a @racket[_datum] contains @racket[(escape-id _expr)]---perhaps as
@RACKET[#,_expr] when @racket[escape-id] is @racket[unsyntax]---then
the @racket[_expr] is evaluated and the result datum is spliced in
place of the @racket[escape-id] form in @racket[_datum]. If the result
is not a syntax object, it is given the source location of the
@racket[escape-id] form. A pict value intected this way as a
@racket[_datum] is rendered as itself.

If a @racket[_datum] contains @racket[(transform-id _datum ...)] or
@racket[transform-id] for a @racket[transform-id] that is bound as syntax to a
@tech{code transformer}, then the @racket[(transform-id _datum ...)]
or @racket[transform-id] may be replaced with an escaped expression,
depending on the @tech{code transformer}'s result.}

@deftogether[(
@defproc[(make-code-transformer [proc-or-stx (or/c (syntax? . -> . (or/c syntax? #f))
                                                   syntax?)])
         code-transformer?]
@defthing[prop:code-transformer struct-type-property?]
@defproc[(code-transformer? [v any/c]) boolean?]
)]{

Exported @racket[for-syntax] for creating @deftech{code transformers}.

For @tech{code transformer} created with
@racket[(make-code-transformer _proc)], @racket[proc] takes a syntax
object representing the use of an identifier bound to the transformer,
and it produces an expression whose value replaces the identifier use
within a @racket[code] form or a form defined via
@racket[define-code].  Like a macro transformer, a code transformer is
triggered either by a use of the bound identifier in an
``application'' position, in which case the transformer receives the
entire ``application'' form, or the identifier by itself can also
trigger the transformer. The @tech{code transformer}'s @racket[_proc]
can return @racket[#f], in which case the use of the identifier is
left untransformed; if the identifier was used in an ``application''
position, the transformer @racket[_proc] will be called again for the
identifier use by itself.

A @tech{code transformer} produced by @racket[(make-code-transformer _stx)]
is equivalent to 

@racketblock[
(make-code-transformer (lambda (use-stx) 
                         (if (identifier? use-stx)
                             _stx
                             #f)))
]

A structure type with the @racket[prop:code-transformer] property
implements a @tech{code transformer}. The property value must be a
procedure of one argument, which receives the structure and returns a
procedure that is like a @racket[_proc] passed to
@racket[make-code-transformer], except that the property value takes
the structure instance as an argument before the syntax object to
transform.

The @racket[code-transformer?] predicate returns @racket[#t] for a
value produced by @racket[make-code-transformer] or for an instance of
a structure type with the @racket[prop:code-transformer] property,
@racket[#f] otherwise.

@examples[
#:eval ss-eval
(let-syntax ([bag (make-code-transformer #'(code hat))]
             [copy (make-code-transformer (syntax-rules ()
                                           [(_ c) (code (* 2 c))]))])
  (inset (frame (code ((copy cat) in the bag))) 2))
]}


@defform[(define-exec-code (pict-id runnable-id string-id)
           datum ...)]{

Binds @racket[pict-id] to the result of @racket[(code datum ...)],
except that if an identifier @racketidfont{_} appears anywhere in a
@racket[datum], then the identifier and the following expression are
not included for @racket[code].

Meanwhile, @racket[runnable-id] is bound to a @|stx-obj| that wraps
the @racket[datum]s in a @racket[begin]. In this case,
@racketidfont{_}s are removed from the @racket[datum]s, but not the
following expression. Thus, an @racketidfont{_} identifier is used to
comment out an expression from the pict, but have it present in the
@|stx-obj| for evaluation.

The @racket[string-id] is bound to a string representation of the code
that is in the pict. This string is useful for copying to the
clipboard with @racket[(send the-clipboard set-clipboard-string
string-id 0)].}


@defform[(define-exec-code/scale scale-expr (pict-id runnable-id string-id)
           datum ...)]{

Like @racket[define-exec-code], but with a scale to use via
@racket[scale/improve-new-text] when generating the pict.}


@deftogether[(
@defthing[comment-color (or/c string? (is-a?/c color%))]
@defthing[keyword-color (or/c string? (is-a?/c color%))]
@defthing[id-color (or/c string? (is-a?/c color%))]
@defthing[literal-color (or/c string? (is-a?/c color%))]
)]{

For backward compatibility, the default values for
@racket[current-comment-color], etc.}

@defproc[(code-pict-bottom-line-pict [pict pict?])
         (or/c pict? #f)]{

The same as @racket[pict-last], provided for backward compatibility.}

@defproc[(pict->code-pict [pict pict?] [bl-pict (or/c pict? #f)]) pict?]{

Mainly for backward compatibility: returns @racket[(if bl-pict
(use-last pict (or (pict-last bl-pict) bl-pict)))].}

@; ----------------------------------------

@close-eval[ss-eval]
