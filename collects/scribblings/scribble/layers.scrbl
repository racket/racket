#lang scribble/doc
@(require scribble/manual scribble/bnf "utils.rkt")

@title[#:tag "layers"]{Scribble Layers}

Scribble is made of independently usable parts.  For example, the
Scribble reader can be used in any situation that requires lots of
free-form text. You can also skip Scribble's special reader support,
and instead use the document-generation structure directly.

@; ----------------------------------------------------------------------

@section{Typical Composition}

A Scribble document normally starts

@racketmod[
scribble/manual
]

but it could also start

@racketmod[
scribble/base
]

or

@racketmod[
scribble/doc
]

The last one introduces the smallest number of typesetting bindings in
the document body. Using @racketmodname[scribble/base] after
@hash-lang[] is the same as using @racketmodname[scribble/doc] plus
@racket[(require scribble/base)], and using
@racketmodname[scribble/manual] after @hash-lang[] is the same as using
@racketmodname[scribble/doc] plus @racket[(require scribble/manual)].

Besides making the file a module, each of the @hash-lang[]
declarations selects the Scribble reader (instead of the usual Racket
reader), and it starts the body of the file in ``text'' mode. The
reader layer mostly leaves text alone, but @litchar["@"] forms escape
to S-expression mode.

A module written as

@verbatim[#:indent 2]|{
  #lang scribble/doc
  @(require scribble/manual)

  @(define to-be "To Be")

  @title{@|to-be| or Not @|to-be|}

  @bold{That} is the question.
  Whether 'tis nobler...
}|

reads as

@racketblock[
(module #,(nonterm "name") scribble/doc
  (require scribble/manual)
  "\n"
  (define to-be "To Be") "\n"
  "\n"
  (title to-be " or Not " to-be) "\n"
  "\n"
  (bold "That") " is the question." "\n"
  "Whether 'tis nobler..." "\n")
]

As shown in this example, the read result is a module whose content
mingles text and definitions. The @racketmodname[scribble/doc]
language lifts definitions, @racket[require]s, and @racket[provide]s
to the beginning of the module, while everything else is collected
into a document bound to the provided identifier @racket[doc].  That
is, the module is transformed to something like this:

@racketblock[
(module #,(nonterm "name") racket/base
  (require scribble/decode
           scribble/manual)
  (define to-be "To Be")
  (define doc
    (decode
     "\n" "\n" "\n"
     (title to-be " or Not " to-be) "\n"
     "\n"
     (bold "That") " is the question." "\n"
     "Whether 'tis nobler..." "\n"))
  (provide doc))
]

The @racket[decode] function produces a @racket[part] structure
instance that represents the document. To build the @racket[part]
instance, it inspects its arguments to find a @racket[title-decl]
value created by @racket[title] to name the part, @racket[part-start]
values created by @racket[section] to designate sub-parts, etc.

A @racket[part] is the input to a rendering back-end, such as the HTML
renderer. All renderers recognize a fixed structure hierarchy: the
content of a part is a @defterm{flow}, which is a sequence of
@defterm{flow elements}, such as paragraphs and tables; a table, in
turn, consists of a list of list of flows; a paragraph is a list of
@defterm{elements}, which can be instances of the @racket[element]
structure type, plain strings, or certain special symbols.

The value bound to @racket[doc] in the example above is something like

@racketblock[
(make-part ....
           (list "To Be" " or Not " "To Be") (code:comment "title")
           ....
           (make-flow
             (list
              (make-paragraph
               (list (make-element 'bold (list "That"))
                     " is the question." "\n"
                     "Whether " 'rsquo "tis nobler..."))))
           ....)
]

Notice that the @litchar{'} in the input's @litchar{'tis} has turned
into @racket['rsquo] (rendered as a curly apostrophe). The conversion to use
@racket['rsquo] was performed by @racket[decode] via
@racket[decode-flow] via @racket[decode-paragraph] via
@racket[decode-content] via @racket[decode-string].

In contrast, @racket[(make-element 'bold (list "That"))] was produced
by the @racket[bold] function.  The @racket[decode] operation is a
function, not a syntactic form, and so @racket[bold] has control over
its argument before @racket[decode] sees the result. Also, decoding
traverses only immediate string arguments.

As it turns out, @racket[bold] also decodes its argument, because the
@racket[bold] function is implemented as

@racketblock[
(define (bold . strs)
  (make-element 'bold (decode-content strs)))
]

The @racket[verbatim] function, however, does not decode its content,
and instead typesets its text arguments directly.

A document module can construct elements directly using
@racket[make-element], but normally functions like @racket[bold] and
@racket[verbatim] are used to construct them. In particular, the
@racketmodname[scribble/manual] library provides many functions and
forms to typeset elements and flow elements.

The @racket[part] structure hierarchy includes built-in element types
for setting hyperlink targets and references. Again, this machinery is
normally packaged into higher-level functions and forms, such as
@racket[secref], @racket[defproc], and @racket[racket].

@; ----------------------------------------------------------------------

@section{Layer Roadmap}

Working roughly from the bottom up, the Scribble layers are:

@itemize[

 @item{@racketmodname[scribble/reader]: A reader that extends the
       syntax of Racket with @"@"-forms for conveniently embedding a
       mixin of text and escapes. See @secref["reader"].}

 @item{@racketmodname[scribble/core]: A set of document datatypes
       and utilities that define the basic layout and processing of a
       document. For example, the @racket[part] datatype is defined in
       this layer. See @secref["core"].}

 @item{@racketmodname[scribble/base-render] with
       @racketmodname[scribble/html-render],
       @racketmodname[scribble/latex-render], or
       @racketmodname[scribble/text-render]: A base renderer and
       mixins that generate documents in various formats from
       instances of the @racketmodname[scribble/struct] datatypes. See
       @secref["renderer"].}

 @item{@racketmodname[scribble/decode]: Processes a stream of text,
       section-start markers, etc. to produce instances of the
       @racketmodname[scribble/core] datatypes. See
       @secref["decode"].}

 @item{@racketmodname[scribble/doclang]: A language to be used for the
       initial import of a module; processes the module top level
       through @racketmodname[scribble/decode], and otherwise provides
       all of @racketmodname[racket/base].  See @secref["doclang"].}

 @item{@racketmodname[scribble/doc]: A language that combines
       @racketmodname[scribble/reader] with
       @racketmodname[scribble/doclang]. See @secref["docreader"].}

 @item{@racketmodname[scribble/base]: A library of basic document
       operators---such as @racket[title], @racket[section], and
       @racket[secref]---for use with @racketmodname[scribble/decode]
       and a renderer. This library name also can be used as a
       language, where it combines @racketmodname[scribble/doc] with
       the exports of @racketmodname[scribble/base]. See
       @secref["base"].}

 @item{@racketmodname[scribble/racket]: A library of functions for
       typesetting Racket code. See @secref["scheme"]. These functions
       are not normally used directly, but instead used through
       @racketmodname[scribble/manual].}

 @item{@racketmodname[scribble/manual]: A library of functions for
       writing Racket documentation; re-exports
       @racketmodname[scribble/base]. Also, the
       @racketmodname[scribble/manual-struct] library provides types
       for index-entry descriptions created by functions in
       @racketmodname[scribble/manual]. See @secref["manual"].}

 @item{@racketmodname[scribble/eval]: A library of functions for
       evaluating code at document-build time, especially for showing
       examples. See @secref["eval"].}

 @item{@racketmodname[scribble/bnf]: A library of support functions
       for writing grammars. See @secref["bnf"].}

 @item{@racketmodname[scribble/xref]: A library of support functions
       for using cross-reference information, typically after a
       document is rendered (e.g., to search). See @secref["xref"].}

 @item{@racketmodname[scribble/text]: A language that uses
       @racketmodname[scribble/reader] preprocessing text files.}

]

The @exec{scribble} command-line utility generates output with a
specified renderer. More specifically, the executable installs a
renderer, loads the modules specified on the command line, extracts
the @racket[doc] export of each module (which must be an instance of
@racket[part]), and renders each---potentially with links that span
documents.
