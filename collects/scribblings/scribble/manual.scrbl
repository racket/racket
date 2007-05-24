#reader"../docreader.ss"
@require["../manual.ss"]
@require["utils.ss"]
@require-for-syntax[mzscheme]

@title[#:tag "manual"]{PLT Manual Forms}

The @file{manual.ss} module provides all of @file{basic.ss}, and
more...

@; ------------------------------------------------------------------------
@section{Typesetting Code}

@defform[(schemeblock datum ...)]{

Typesets the @scheme[datum] sequence as a table of Scheme code inset
by two spaces. The source locations of the @scheme[datum]s determines
the generated layout. For example,

@schemeblock[
(schemeblock
 (define (loop x)
   (loop (not x))))
]

produces the output

@schemeblock[
(define (loop x)
  (loop (not x)))
]

with the @scheme[(loop (not x))] indented under @scheme[define],
because that's the way it is idented the use of @scheme[schemeblock].
Furthermore, @scheme[define] is typeset as a keyword (bold and black)
and as a hyperlink to @scheme[define]'s definition in the reference
manual, because this document was built using information about the
MzScheme manual. Similarly, @scheme[not] is a hyperlink to the its
definition in the reference manual.

Use @scheme[unsyntax] to escape back to an expression that produces an
@scheme[element]. For example,

@let[([unsyntax #f])
(schemeblock
 (schemeblock
   (+ 1 (unsyntax (elem (scheme x) (subscript "2"))))))
]

produces

@schemeblock[
(+ 1 (unsyntax (elem (scheme x) (subscript "2"))))
]

The @scheme[unsyntax] form is regonized via
@scheme[module-identifier=?], so if you want to typeset code that
includes @scheme[unsyntax], you can simply hide the usual binding:

@SCHEMEBLOCK[
(schemeblock
  (let ([(UNSYNTAX (scheme unsyntax)) #f])
    (schemeblock
      (syntax (+ 1 (unsyntax x))))))
]

Or use @scheme[SCHEMEBLOCK], whose escape form is @scheme[UNSYNTAX]
instead of @scheme[unsyntax]. See also @scheme[define-code] from
@file{scheme.ss}.

A few other escapes are recognized symbolically:

@itemize{

 @item{@scheme[(#,(scheme code:line) datum ...)] typesets as the
       sequence of @scheme[datum]s (i.e., without the
       @scheme[code:line] wrapper.}

 @item{@scheme[(#,(scheme code:comment) content-expr)] typesets as a
       comment whose content (i.e., sequence of elements) is produced
       by @scheme[content-expr].}

 @item{@schemeidfont{code:blank} typesets as a blank line.}

}

}

@defform[(SCHEMEBLOCK datum ...)]{Like @scheme[schemeblock], but with
the expression escape @scheme[UNSYNTAX] instead of @scheme[unsyntax].}

@defform[(schemeblock0 datum ...)]{Like @scheme[schemeblock], but
without insetting the code.}

@defform[(SCHEMEBLOCK0 datum ...)]{Like @scheme[SCHEMEBLOCK], but
without insetting the code.}

@defform[(schemeinput datum ...)]{Like @scheme[schemeblock], but the
@scheme[datum] are typeset after a prompt representing a REPL.}

@defform[(schememod lang datum ...)]{Like @scheme[schemeblock], but
the @scheme[datum] are typeset inside a @schemefont{#module}-form
module whose language is @scheme[lang].}

@defform[(scheme datum ...)]{Like @scheme[schemeblock], but typeset on
a single line and wrapped with its enclosing paragraph, independent of
the formatting of @scheme[datum].}

@defform[(schemeresult datum ...)]{Like @scheme[scheme], but typeset
as a REPL value (i.e., a single color with no hperlinks).}

@defform[(schemeid datum ...)]{Like @scheme[scheme], but typeset
as an unbound identifier (i.e., no coloring or hyperlink).}

@defform[(schememodname datum ...)]{Like @scheme[scheme], but typeset
as a @schemefont{#module} language name.}

@defproc[(litchar [str string?]) element?]{Typesets @scheme[str] as a
representation of literal text. Use this when you have to talk about
the individual characters in a stream of text, as as when documenting
a reader extension.}

@defproc[(verbatim [str string?]) flow-element?]{Typesets @scheme[str]
as a table/paragraph in typewriter font with the linebreaks specified
by newline characters in @scheme[str]. ``Here strings'' are often
useful with @scheme[verbatim].}

@defproc[(schemefont [pre-content any/c] ...0) element?]{Typesets the given
content as uncolored, unhyperlinked Scheme. This procedure is useful
for typesetting thngs like @scheme{#module}, which are not
@scheme[read]able by themselves.}

@defproc[(schemevalfont [pre-content any/c] ...0) element?]{Like
@scheme[schemefont], but colored as a value.}

@defproc[(schemeresultfont [pre-content any/c] ...0) element?]{Like
@scheme[schemefont], but colored as a REPL result.}

@defproc[(schemeidfont [pre-content any/c] ...0) element?]{Like
@scheme[schemefont], but colored as an identifier.}

@defproc[(schemekeywordfont [pre-content any/c] ...0) element?]{Like
@scheme[schemefont], but colored as a syntactic form name.}

@defproc[(procedure [pre-content any/c] ...0) element?]{Typesets the given
content as a procedure name in a REPL result (e.g., in typewrite font
with a @schemefont{#<procedure:} prefix and @schemefont{>} suffix.).}

@defform[(var datum)]{Typesets @scheme[var] as an identifier that is
an argument or sub-form in a procedure being
documented. Normally, the @scheme[defproc] and @scheme[defform]
arrange for @scheme[scheme] to format such identifiers automatically
in the description of the procedure, but use @scheme[var] if that
cannot work for some reason.}

@defform[(svar datum)]{Like @scheme[var], but for subform non-terminals
in a form definition.}

@; ------------------------------------------------------------------------
@section{Definition Reference}

@defform[(defproc (identifier arg-spec ...) result-contract-expr-datum pre-flow ...)]{Produces 
a sequence of flow elements (encaptured in a @scheme[splice]) to
document a procedure named @scheme[identifier]. The
@scheme[identifier] is registered so that @scheme[scheme]-typeset uses
of the identifier are hyperlinked to this documentation.

Each @scheme[arg-spec] must have one of the following forms:

@itemize{

 @item{@specsubform/inline[(arg-identifier contract-expr-datum)]{---
       an argument whose contract is specified by
       @scheme[contract-expr-datum] which is typeset via
       @scheme[scheme].}}

 @item{@specsubform/inline[(arg-identifier contract-expr-datum
       default-expr)]{ --- like the previous case, but with a default
       value. All arguments with a default value must be grouped
       together, but they can be in the middle of required
       arguments.}}

 @item{@specsubform/inline[(keyword arg-identifier
       contract-expr-datum)]{ --- like the first case, but for a
       keyword-based argument.}}

 @item{@specsubform/inline[(keyword arg-identifier contract-expr-datum
       default-expr)]{ --- like the previous case, but with a default
       value.}}

 @item{@scheme[...0] --- any number of the preceding argument
      (normally at the end)}

 @item{@scheme[...1] --- one or more of the preceding argument
       (normally at the end)}

}

The @scheme[result-contract-expr-datum] is typeset via
@scheme[scheme], and it represents a contract on the procedure's
result.

The @scheme[pre-flow]s list is parsed as a flow that documents the
procedure. In this description, references to @svar[arg-identifier]s
are typeset as procedure arguments.

The typesetting of all data before the @scheme[pre-flow]s ignores the
source layout.}


@defform[(defproc* (((identifier arg-spec ...) result-contract-expr-datum) ...) pre-flow ...)]{Like
@scheme[defproc], but for multiple cases with the same @scheme[identifier].
}


@defform[(defform (identifier . datum) pre-flow ...)]{Produces a 
a sequence of flow elements (encaptured in a @scheme[splice]) to
document a syntaic form named by @scheme[identifier]. The
@scheme[identifier] is registered so that @scheme[scheme]-typeset uses
of the identifier are hyperlinked to this documentation.

The @scheme[pre-flow]s list is parsed as a flow that documents the
procedure. In this description, a reference to any identifier in
@scheme[datum] is typeset as a sub-form non-terminal.

The typesetting of @scheme[(identifier . datum)] preserves the source
layout, like @scheme[scheme], and unlike @scheme[defproc].}


@defform[(specsubform/inline datum pre-flow ...)]{Similar to
@scheme[defform], but without any specific identifier being defined,
without the output format that highlights a definition, and with
@scheme[datum] as an element rather than a table. This form is
intended for use when refining the syntax of a non-terminal used in a
@scheme[defform], @scheme[specsubform], or other
@scheme[specsubform/inline]. For example, it is used in the
documentation for @scheme[defproc] in the itemization of possible
shapes for @svar[arg-spec].

The @scheme[pre-flow]s list is parsed as a flow that documents the
procedure. In this description, a reference to any identifier in
@scheme[datum] is typeset as a sub-form non-terminal.}


@defform[(specsubform datum pre-flow ...)]{Like
@scheme[specsubform/inline], but the @scheme[datum] is typeset in the
resulting flow as a table instead of an element.}


@defform[(defthing identifier contract-expr-datum pre-flow ...)]{Like
@scheme[defproc], but for a non-procedure binding.}

@defform[(defstruct struct-name ([field-name contract-expr-datum] ...) pre-flow ...)]{Similar 
to @scheme[defform], but for a structure definition.

The @scheme[struct-name] can be either of the following:

@itemize{

 @item{@specsubform/inline[identifier]{--- a structure type with no
       specified supertype.}}

 @item{@specsubform/inline[(identifier super-identifier)]{ --- a structure
 type with indicated supertype.}}

}}


@; ------------------------------------------------------------------------
@section{Various String Forms}

@defproc[(defterm [pre-content any/c] ...0) element?]{Typesets the given
content as a defined term (e.g., in italic).}

@defproc[(onscreen [pre-content any/c] ...0) element?]{ Typesets the given
content as a string that appears in a GUI, such as the name of a
button.}

@defproc[(menuitem [menu-name string?] [item-name string?]) element?]{
Typesets the given combination of a GUI's menu and item name.}

@defproc[(file [pre-content any/c] ...0) element?]{Typesets the given content
as a file name (e.g., in typewriter font and in in quotes).}

@defproc[(exec [pre-content any/c] ...0) element?]{Typesets the given content
as a command line (e.g., in typewriter font).}

@; ------------------------------------------------------------------------
@section{Section Links}

@defproc[(secref [tag string?]) element?]{Inserts the hyperlinked
title of the section tagged @scheme[tag].}

@defproc[(seclink [tag string?] [pre-content any/c] ...0) element?]{The content from
@scheme[pre-content] is hyperlinked to the section tagged @scheme[tag].}

@defproc[(schemelink [id symbol?] [pre-content any/c] ...0) element?]{The content from
@scheme[pre-content] is hyperlinked to the definition of @scheme[id].}



@; ------------------------------------------------------------------------
@section{Indexing}

@defproc[(idefterm [pre-content any/c] ...0) element?]{Combines
@scheme[as-index] and @scheme[defterm]. The content normally should be
plurarl, rather than singular.}

@defproc[(pidefterm [pre-content any/c] ...0) element?]{Like
@scheme[idefterm], but plural: adds an ``s'' on the end of the content
for the index entry.}

@; ------------------------------------------------------------------------
@section{Miscellaneous}

@defthing[PLaneT string?]{@scheme["PLaneT"] (to help make sure you get
the letters in the right case).}

@defproc[(void-const) any/c]{Returns @scheme["void"], as opposed to
@scheme[(scheme void)]---but we may eventually find a clearer way to
refer to @void-const in prose.}

@defproc[(centerline [pre-flow any/c] ...0) table?]{Produces a
centered table with the @scheme[pre-flow] parsed by
@scheme[decode-flow].}

@defproc[(commandline [pre-content any/c] ...0) paragraph?]{Produces a
an inset command-line example (e.g., in typewriter font).}
