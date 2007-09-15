#lang scribble/doc
@require[(lib "manual.ss" "scribble")
         (lib "bnf.ss" "scribble")]
@require["utils.ss"]

@title{How to Scribble Documentation}

@;----------------------------------------
@section[#:tag "getting-started"]{Getting Started}

To document a collection or @|PLaneT| package:

@itemize{

 @item{Create a file in your collection or planet package with the
       file extension @file{.scrbl}. The remainder of these
       instructions assume that the file is called @file{manual.scrbl}.}

 @item{Start @file{manual.scrbl} like this:
@verbatim[#<<EOS
  #lang scribble/doc
  @begin[(require (lib "manual.ss" "scribble"))]

  @title{My Library}

  Welcome to my documentation: @scheme[(list 'testing 1 2 3)].
EOS
]

        The first line starts the file in ``text'' mode, and
        introduces the @litchar["@"] syntax to use Scheme bindings.
        The second line introduces bindings like @scheme[title] and
        @scheme[scheme] for writing PLT Scheme documentation. The
        @scheme[title] call (using @litchar["@"]) produces a title
        declaration in the text stream.}

  @item{Add the following entry to your collect or package's
        @file{info.ss}:

        @schemeblock[
          (define scribblings '(("manual.scrbl" ())))
        ]

        The @scheme[()] above is a list of options. When your document
        gets large enough that you want it split into multiple pages,
        add the @scheme['multi-page] option (omitting the quote, since
        the whole right-hand side of the definition is already
        quoted).}

  @item{Run @exec{setup-plt} to build your documentation. For a
        collection, optionally supply @Flag{l} followed by the
        collection name to limit the build process to the collection.}

  @item{The generated documentation is
        @file{compiled/doc/manual/index.html} within the collection or
        @|PLaneT| package directory.}

}

@; ----------------------------------------
@section{Document Syntax}

Whether in ``text'' mode or Scheme mode, @litchar["@"] in a document
provides an escape to Scheme mode. The syntax of @litchar["@"] is

@schemeblock[
 #, @BNF-seq[@litchar["@"]
             @nonterm{cmd}
             @litchar{[} @kleenestar{@nonterm{datum}} @litchar{]}
             @litchar["{"] @nonterm{text-body} @litchar["}"]]
]

where all three parts after @litchar["@"] are optional, but at least
one must be present. No spaces are allowed between

@itemize{

 @item{@litchar["@"] and @nonterm{cmd}, @litchar["["], or @litchar["{"]}

 @item{@nonterm{cmd} and @litchar["["] or @litchar["{"]; or}

 @item{@litchar["]"] and @litchar["{"].}

}

A @nonterm{cmd} or @nonterm{datum} is a Scheme datum, while a
@nonterm{text-body} is itself in text mode.

The expansion of a @litchar["@"] form into Scheme code is

@schemeblock[
  (#, @nonterm{cmd} #, @kleenestar{@nonterm{datum}} #, @kleenestar{@nonterm{parsed-body}})
]

where @kleenestar{@nonterm{parsed-body}} is the parse result of the
@nonterm{text-body}. It often turns out to be a sequence of Scheme
strings.

In practice, the @nonterm{cmd} is normally a Scheme identifier that is
bound to a procedure or syntactic form. If the procedure or form
expects further text to typeset, then @litchar["{"] @litchar["}"]
supplies the text. If the form expects other data, typically
@litchar["["] @litchar["]"] is used to surround Scheme arguments,
instead. Sometimes, both @litchar["["] @litchar["]"] and @litchar["{"]
@litchar["}"] are used, where the former surround Scheme arguments
that precede text to typeset.

Thus,

@verbatim[#<<EOS
  @title{My Library}
  @scheme[(list 'testing 1 2 3)]
  @section[#:tag "here"]{You Are Here}
EOS
]

means

@schemeblock[
(title "My Library")
(scheme (list 'testing 1 2 3))
(section #:tag "here" "You Are Here")
]

For more information on the syntax of @litchar["@"], see
@secref["reader"].

In a document that starts @tt{#lang scribble/doc},
the top level is a text-mode sequence. The parsed sequence is further
decoded to turn it into a hierarchy of sections and paragraphs. For
example, a linear sequence of @scheme[section] declarations with
interleaved text is turned into a list of @scheme[part] instances with
all text assigned to a particular part. See @secref["decode"] for more
information on the decoding process.

@; ----------------------------------------
@section[#:tag "scheme-hyperlinks"]{Scheme Typesetting and Hyperlinks}

With the document source in @secref["getting-started"], the Scheme
expression @scheme[(#,(schemeidfont "list") 'testing 1 2 3)] is
typeset properly, but the @schemeidfont{list} identifier is not
hyperlinked to the usual definition. To cause @schemeidfont{list} to
be hyperlinked, add the following to the @tt["@begin"] body:

@schemeblock[
(require-for-label (lib "big.ss" "lang"))
]

This @scheme[require-for-label] declaration introduces a document-time
binding for each export of the @scheme[(lib "big.ss" "lang")]
module. When the document is built, the @scheme[scheme] form detects
the binding for @scheme[list], and so it generates a reference to the
specification of @scheme[list]. The setup process detects the
reference, and it finds the matching specification in the existing
documentation, and it ultimately directs the hyperlink to that
specification.

Hyperlinks based on @scheme[require-for-label] and @scheme[scheme] are
the preferred mechanism for linking to information outside of a single
document. Such links require no information about where and how a
binding is documented elsewhere:

@verbatim[#<<EOS
  #lang scribble/doc
  @begin[(require (lib "manual.ss" "scribble"))
         (require-for-label (lib "lang.ss" "big"))]

  @title{My Library}

  See also @scheme[list].
EOS
]

The @scheme[scheme] form typesets a Scheme expression for inline text,
so it ignores the source formatting of the expression. The
@scheme[schemeblock] form, in contrast, typesets inset Scheme code,
and it preserves the expression's formatting from the document source.

@verbatim[#<<EOS
  #lang scribble/doc
  @begin[(require (lib "manual.ss" "scribble"))
         (require-for-label (lib "lang.ss" "big"))]

  @title{My Library}

  Some example Scheme code:

  @schemeblock[
  (define (nobody-understands-me what)
    (list "When I think of all the"
          what
           "I've tried so hard to explain!"))
  (nobody-understands-me "glorble snop")
  ]
EOS
]


@; ----------------------------------------
@section[#:tag "section-hyperlinks"]{Section Hyperlinks}

A @scheme[section] declaration in a document can include a
@scheme[#:tag] argument that declares a hyperlink-target tag. The
@scheme[secref] function generates a hyperlink, using the section name
as the text of the hyperlink. Use @scheme[seclink] to create a
hyperlink with text other than the section title.

The following example illustrates section hyperlinks:

@verbatim[#<<EOS
  #lang scribble/doc
  @begin[(require (lib "manual.ss" "scribble"))
         (require-for-label (lib "lang.ss" "big"))]


  @title{My Library}

  Welcome to my documentation: @scheme[(list 'testing 1 2 3)].

  @table-of-contents[]


  @section[#:tag "chickens"]{Philadelphia Chickens}
  
  Dancing tonight!


  @section{Reprise}

  See @secref{chickens}.
EOS
]

Since the page is so short, it the hyperlinks are more effective if
 you change the @file{info.ss} file to add the @scheme['multi-file]
 flag:

@schemeblock[
(define scribblings '(("manual.scrbl" (multi-page))))
]

A section can have a @techlink{tag prefix} that applies to all tags as
seen from outside the section. Such a prefix is automatically given to
each top-level document as processed by @exec{setup-plt}. Thus,
referencing a section tag in a different document requires using a
prefix, which is based on the target document's main source file.  The
following example links to a section in the PLT Scheme reference
manual:

@verbatim[#<<EOS
  #lang scribble/doc
  @begin[(require (lib "manual.ss" "scribble"))
         (require-for-label (lib "lang.ss" "big"))
         (define ref-src
           '(lib "reference.scrbl" "scribblings" "reference"))]

  @title{My Library}

  See also @italic{@secref[#:doc reference-src]{pairs}}.
EOS
]

As mentioned in @secref{scheme-hyperlinks}, however, cross-document
references based on @scheme[require-for-label] and @scheme[scheme] are
usually better than to cross-document references using
@scheme[secref].

@; ----------------------------------------
@section{Defining Scheme Bindings}

Use @scheme[defproc] to document a procedure, @scheme[defform] to
document a syntactic form, @scheme[defstruct] to document a structure
type, etc.  These forms provide consistent formatting of definitions,
and they declare hyperlink targets for @scheme[scheme]-based
hyperlinks.

To document a @scheme[my-helper] procedure that is exported by
@file{helper.ss} in the collection that contains @file{manual.scrbl},
first use @scheme[require-for-label] to import the binding information
of @file{helper.ss}. Then use @scheme[defproc] to document the
procedure:

@verbatim[#<<EOS
  #lang scribble/doc
  @begin[(require (lib "manual.ss" "scribble"))
         (require-for-label (lib "lang.ss" "big")
                            "helper.ss")]

  @title{My Library}

  @defproc[(my-helper [lst list?])
           (listof 
            (not/c (one-of/c 'cow)))]{

   Replaces each @scheme['cow] in @scheme[lst] with
   @scheme['aardvark].}
EOS
]

In @scheme[defproc], a contract is specified with each argument to the
procedure. In this example, the contract for the @scheme[_lst]
argument is @scheme[list?], which is the contract for a list. After
the closing parenthesis that ends the argument sequence, the contract
of the result must be given; in this case, @scheme[my-helper]
guarantees a result that is a list where none of the elements are
@scheme['cow].

Some things to notice in this example and the documentation that it
generates:

@itemize{

 @item{The @scheme[list?], @scheme[listof], @|etc| elements of
       contracts are hyperlinked to their documentation.}

 @item{The result contract is formatted in the generated documentation
       in the same way as in the source. That is, the source layout of
       contracts is preserved. (In this case, putting the contract all
       on one line would be better.)}

 @item{In the prose that documents @scheme[my-helper], @scheme[_lst]
       is automatically typeset in italic, matching the typesetting in
       the blue box. The @scheme[scheme] form essentially knows that
       it's used in the scope of a procedure with argument
       @scheme[_lst].}

 @item{If you use @scheme[my-helper] in any documentation now, as long
       as that documentation source also has a
       @scheme[require-for-label] of @file{my-helper.ss}, then the
       reference is hyperlinked to the definition above.}

}

See @scheme[defproc*], @scheme[defform], @|etc| for more information
on forms to document Scheme bindings.

@; ----------------------------------------
@section{Showing Scheme Examples}

The @scheme[examples] form from @scheme[(lib "eval.ss" "scribble")]
helps you generate examples in your documentation. @bold{Warning:} the
@scheme[examples] form is especially likely to change or be replaced.

To use @scheme[examples], the procedures to document must be suitable
for use at documentation time; in fact, @scheme[examples] uses
bindings introduced into the document source by
@scheme[require]. Thus, to generate examples using @scheme[my-helper]
from the previous section, then @file{helper.ss} must be imported both
via @scheme[require-for-label] and @scheme[require]:

@verbatim[#<<EOS
  #lang scribble/doc
  @begin[(require (lib "manual.ss" "scribble")
                  (lib "eval.ss" "scribble")  ; <--- added
                  "helper.ss")                ; <--- added
         (require-for-label (lib "lang.ss" "big")
                            "helper.ss")]

  @title{My Library}

  @defproc[(my-helper [lst list?])
           (listof (not/c (one-of/c 'cow)))]{

   Replaces each @scheme['cow] in @scheme[lst] with
   @scheme['aardvark].

   @examples[
     (my-helper '())
     (my-helper '(cows such remarkable cows))
   ]}
EOS
]

@;----------------------------------------
@section{Splitting the Document Source}

In general, a @file{.scrbl} file produces a @techlink{part}. A part
produced by a document's main source (as specified in the
@scheme{info.ss} file) represents the whole document. The
@scheme[include-section] procedure can be used to incorporate a part
as a sub-part of the enclosing part.

In @file{manual.scrbl}:

@verbatim[#<<EOS
  #lang scribble/doc
  @begin[(require (lib "manual.ss" "scribble"))]

  @title{My Library}

  @include-section["cows.scrbl"]
  @include-section["aardvarks.scrbl"]
EOS
]

In @file{cows.scrbl}:

@verbatim[#<<EOS
  #lang scribble/doc
  @begin[(require (lib "manual.ss" "scribble"))]

  @title{Cows}

  Wherever they go, it's a quite a show.
EOS
]

In @file{aardvarks.scrbl}:

@verbatim[#<<EOS
  #lang scribble/doc
  @begin[(require (lib "manual.ss" "scribble"))
         (require-for-label (lib "lang.ss" "big")
                            "helper.ss")]

  @title{Aardvarks}

  @defproc[(my-helper [lst list?])
           (listof (not/c (one-of/c 'cow)))]{

   Replaces each @scheme['cow] in @scheme[lst] with
   @scheme['aardvark].}
EOS
]


@;----------------------------------------
@section{Multi-Page Sections}

Setting the @scheme['multi-page] option (see
@secref["section-hyperlinks"]) causes each top-level section of a
document to be rendered as a separate HTML page.

To push sub-sections onto separate pages, use the @scheme['toc] style
for the enclosing section (as started by @scheme[title],
@scheme[section], @scheme[subsection], etc.) and use
@scheme[local-table-of-contents] to generate hyperlinks to the
sub-sections.

Revising @file{cows.scrbl} from the previous section:

@verbatim[#<<EOS
  #lang scribble/doc
  @begin[(require (lib "manual.ss" "scribble"))]

  @title[#:style '(toc)]{Cows}

  @local-table-of-contents[]

  @section[#:tag "singing"]{Singing}
  Wherever they go, it's a quite a show.

  @section{Dancing}
  See @secref["singing"].
EOS
]

To run this example, remember to change @file{info.ss} to add the
@scheme['multi-page] style. You may also want to add a call to
@scheme[table-of-contents] in @file{manual.scrbl}.

The difference between @scheme[table-of-contents] and
@scheme[local-table-of-contents] is that the latter is ignored for
Latex output.

When using @scheme[local-table-of-contents], often it makes sense to
include introductory text before the call of
@scheme[local-table-of-contents]. When the introductory text is less
important and when when local table of contents is short, putting the
introductory text after the call of @scheme[local-table-of-contents]
make be appropriate.

@;----------------------------------------
@include-section["style.scrbl"]
