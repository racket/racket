#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          "utils.ss")

@title[#:tag "layers"]{Scribble Layers}

Scribble is made of independently usable parts.  For example, the
Scribble reader can be used in any situation that requires lots of
free-form text. You can also skip Scribble's special reader support,
and instead use the document-generation structure directly.

@; ----------------------------------------------------------------------

@section{Typical Composition}

A Scribble document normally starts

@schememod[
scribble/manual
]

but it could also start

@schememod[
scribble/base
]

or

@schememod[
scribble/doc
]

The last one introduces the smallest number of typesetting bindings in
the document body. Using @schememodname[scribble/base] after
@hash-lang[] is the same as using @schememodname[scribble/doc] plus
@scheme[(require scribble/base)], and using
@schememodname[scribble/manual] after @hash-lang[] is the same as using
@schememodname[scribble/doc] plus @scheme[(require scribble/manual)].

Besides making the file a module, each of the @hash-lang[]
declarations selects the Scribble reader (instead of the usual Scheme
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

@schemeblock[
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
mingles text and definitions. The @schememodname[scribble/doc]
language lifts definitions, @scheme[require]s, and @scheme[provide]s
to the beginning of the module, while everything else is collected
into a document bound to the provided identifier @scheme[doc].  That
is, the module is transformed to something like this:

@schemeblock[
(module #,(nonterm "name") scheme/base
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

The @scheme[decode] function produces a @scheme[part] structure
instance that represents the document. To build the @scheme[part]
instance, it inspects its arguments to find a @scheme[title-decl]
value created by @scheme[title] to name the part, @scheme[part-start]
values created by @scheme[section] to designate sub-parts, etc.

A @scheme[part] is the input to a rendering back-end, such as the HTML
renderer. All renderers recognize a fixed structure hierarchy: the
content of a part is a @defterm{flow}, which is a sequence of
@defterm{flow elements}, such as paragraphs and tables; a table, in
turn, consists of a list of list of flows; a paragraph is a list of
@defterm{elements}, which can be instances of the @scheme[element]
structure type, plain strings, or certain special symbols.

The value bound to @scheme[doc] in the example above is something like

@schemeblock[
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
into @scheme['rsquo] (rendered as a curly apostrophe). The conversion to use
@scheme['rsquo] was performed by @scheme[decode] via
@scheme[decode-flow] via @scheme[decode-paragraph] via
@scheme[decode-content] via @scheme[decode-string].

In contrast, @scheme[(make-element 'bold (list "That"))] was produced
by the @scheme[bold] function.  The @scheme[decode] operation is a
function, not a syntactic form, and so @scheme[bold] has control over
its argument before @scheme[decode] sees the result. Also, decoding
traverses only immediate string arguments.

As it turns out, @scheme[bold] also decodes its argument, because the
@scheme[bold] function is implemented as

@schemeblock[
(define (bold . strs)
  (make-element 'bold (decode-content strs)))
]

The @scheme[verbatim] function, however, does not decode its content,
and instead typesets its text arguments directly.

A document module can construct elements directly using
@scheme[make-element], but normally functions like @scheme[bold] and
@scheme[verbatim] are used to construct them. In particular, the
@schememodname[scribble/manual] library provides many functions and
forms to typeset elements and flow elements.

The @scheme[part] structure hierarchy includes built-in element types
for setting hyperlink targets and references. Again, this machinery is
normally packaged into higher-level functions and forms, such as
@scheme[secref], @scheme[defproc], and @scheme[scheme].

@; ----------------------------------------------------------------------

@section{Layer Roadmap}

Working roughly from the bottom up, the Scribble layers are:

@itemize[

 @item{@schememodname[scribble/reader]: A reader that extends the
       syntax of Scheme with @"@"-forms for conveniently embedding a
       mixin of text and escapes. See @secref["reader"].}

 @item{@schememodname[scribble/core]: A set of document datatypes
       and utilities that define the basic layout and processing of a
       document. For example, the @scheme[part] datatype is defined in
       this layer. See @secref["core"].}

 @item{@schememodname[scribble/base-render] with
       @schememodname[scribble/html-render],
       @schememodname[scribble/latex-render], or
       @schememodname[scribble/text-render]: A base renderer and
       mixins that generate documents in various formats from
       instances of the @schememodname[scribble/struct] datatypes. See
       @secref["renderer"].}

 @item{@schememodname[scribble/decode]: Processes a stream of text,
       section-start markers, etc. to produce instances of the
       @schememodname[scribble/core] datatypes. See
       @secref["decode"].}

 @item{@schememodname[scribble/doclang]: A language to be used for the
       initial import of a module; processes the module top level
       through @schememodname[scribble/decode], and otherwise provides
       all of @schememodname[scheme/base].  See @secref["doclang"].}

 @item{@schememodname[scribble/doc]: A language that combines
       @schememodname[scribble/reader] with
       @schememodname[scribble/doclang]. See @secref["docreader"].}

 @item{@schememodname[scribble/base]: A library of basic document
       operators---such as @scheme[title], @scheme[section], and
       @scheme[secref]---for use with @schememodname[scribble/decode]
       and a renderer. This library name also can be used as a
       language, where it combines @schememodname[scribble/doc] with
       the exports of @schememodname[scribble/base]. See
       @secref["base"].}

 @item{@schememodname[scribble/scheme]: A library of functions for
       typesetting Scheme code. See @secref["scheme"]. These functions
       are not normally used directly, but instead used through
       @schememodname[scribble/manual].}

 @item{@schememodname[scribble/manual]: A library of functions for
       writing PLT Scheme documentation; re-exports
       @schememodname[scribble/base]. Also, the
       @schememodname[scribble/manual-struct] library provides types
       for index-entry descriptions created by functions in
       @schememodname[scribble/manual]. See @secref["manual"].}

 @item{@schememodname[scribble/eval]: A library of functions for
       evaluating code at document-build time, especially for showing
       examples. See @secref["eval"].}

 @item{@schememodname[scribble/bnf]: A library of support functions
       for writing grammars. See @secref["bnf"].}

 @item{@schememodname[scribble/xref]: A library of support functions
       for using cross-reference information, typically after a
       document is rendered (e.g., to search). See @secref["xref"].}

 @item{@schememodname[scribble/text]: A language that uses
       @schememodname[scribble/reader] preprocessing text files.}

]

The @exec{scribble} command-line utility generates output with a
specified renderer. More specifically, the executable installs a
renderer, loads the modules specified on the command line, extracts
the @scheme[doc] export of each module (which must be an instance of
@scheme[part]), and renders each---potentially with links that span
documents.
