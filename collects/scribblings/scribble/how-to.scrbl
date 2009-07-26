#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          "utils.ss")

@title[#:tag "how-to-doc"]{Starting Documentation}

Although the @exec{scribble} command-line utility generates output
from a Scribble document, documentation of PLT Scheme libraries is
normally built by @exec{setup-plt}. This chapter emphasizes the
@exec{setup-plt} approach, which more automatically supports links
across documents.

@margin-note{See @secref["how-to-paper"] for information on using the
             @exec{scribble} command-line utility.}

@;----------------------------------------
@section[#:tag "setting-up"]{Setting Up Documentation}

To document a collection or @|PLaneT| package:

@itemize[

  @item{Create a file in your collection or planet package with the
        file extension @filepath{.scrbl}. Beware that the file name
        you choose will determine the output directory's name. The
        remainder of these instructions assume that the file is called
        @filepath{manual.scrbl}.}

  @item{Start @filepath{manual.scrbl} like this:
          @verbatim[#:indent 2]|{
            #lang scribble/manual

            @title{My Library}

            Welcome to my documentation: @scheme[(list 'testing 1 2 3)].
          }|

        The first line starts the file in ``text'' mode and selects
        the PLT Scheme manual output format.
        It also introduces bindings like @scheme[title] and
        @scheme[scheme] for writing PLT Scheme documentation.}

  @item{Add the following entry to your collect or package's
        @filepath{info.ss}:

        @schemeblock[
          (define scribblings '(("manual.scrbl" ())))
        ]

        The @scheme[()] above is a list of options. When your document
        gets large enough that you want it split into multiple pages,
        add the @scheme['multi-page] option (omitting the quote, since
        the whole right-hand side of the definition is already
        quoted).

        If you do not already have an @filepath{info.ss} module,
        here's a suitable complete module:

        @schememod[
          setup/infotab
          (define scribblings '(("manual.scrbl" ())))
        ]}

  @item{Run @exec{setup-plt} to build your documentation. For a
        collection, optionally supply @Flag{l} followed by the
        collection name to limit the build process to that
        collection. For a @|PLaneT| package, optionally supply
        @Flag{P} followed by the package information to limit the
        build process to that package.}

  @item{The generated documentation is normally
        @filepath{doc/manual/index.html} within the collection or
        @|PLaneT| package directory. If the collection is in PLT
        Scheme's main @filepath{collects} directory, however, then the
        documentation is generated as @filepath{manual/index.html} in
        the installation's main @filepath{doc} directory.}

]

@; ----------------------------------------
@section[#:tag "scheme-hyperlinks"]{Scheme Typesetting and Hyperlinks}

In the document source at the start of this chapter
(@secref["setting-up"]), the Scheme expression
@scheme[(#,(schemeidfont "list") 'testing 1 2 3)] is typeset properly,
but the @schemeidfont{list} identifier is not hyperlinked to the usual
definition. To cause @schemeidfont{list} to be hyperlinked, add a
@scheme[require] form like this:

@verbatim[#:indent 2]|{
  @(require (for-label scheme))
}|

This @scheme[require] with @scheme[for-label] declaration introduces a
document-time binding for each export of the @schememodname[scheme]
module. When the document is built, the @scheme[scheme] form detects
the binding for @scheme[list], and so it generates a reference to the
specification of @scheme[list]. The setup process detects the
reference, and it finds the matching specification in the existing
documentation, and ultimately directs the hyperlink to that
specification.

Hyperlinks based on @scheme[for-label] and @scheme[scheme] are the
preferred mechanism for linking to information outside of a single
document. Such links require no information about where and how a
binding is documented elsewhere:

@verbatim[#:indent 2]|{
  #lang scribble/manual
  @(require (for-label scheme))

  @title{My Library}

  See also @scheme[list].
}|

The @scheme[scheme] form typesets a Scheme expression for inline text,
so it ignores the source formatting of the expression. The
@scheme[schemeblock] form, in contrast, typesets inset Scheme code,
and it preserves the expression's formatting from the document source.

@verbatim[#:indent 2]|{
  #lang scribble/manual
  @(require (for-label scheme))

  @title{My Library}

  Some example Scheme code:

  @schemeblock[
  (define (nobody-understands-me what)
    (list "When I think of all the"
          what
           "I've tried so hard to explain!"))
  (nobody-understands-me "glorble snop")
  ]
}|


@; ----------------------------------------
@section[#:tag "section-hyperlinks"]{Section Hyperlinks}

A @scheme[section] declaration in a document can include a
@scheme[#:tag] argument that declares a hyperlink-target tag. The
@scheme[secref] function generates a hyperlink, using the section name
as the text of the hyperlink. Use @scheme[seclink] to create a
hyperlink with text other than the section title.

The following example illustrates section hyperlinks:

@verbatim[#:indent 2]|{
  #lang scribble/manual
  @(require (for-label scheme))


  @title{My Library}

  Welcome to my documentation: @scheme[(list 'testing 1 2 3)].

  @table-of-contents[]


  @section[#:tag "chickens"]{Philadelphia Chickens}

  Dancing tonight!


  @section{Reprise}

  See @secref{chickens}.
}|

Since the page is so short, the hyperlinks in the above example are
 more effective if you change the @filepath{info.ss} file to add the
 @scheme['multi-file] flag:

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

@verbatim[#:indent 2]|{
  #lang scribble/manual
  @(require (for-label scheme))
  @(define ref-src
     '(lib "scribblings/reference/reference.scrbl"))

  @title{My Library}

  See also @italic{@secref[#:doc ref-src]{pairs}}.
}|

As mentioned in @secref{scheme-hyperlinks}, however, cross-document
references based on @scheme[(require (for-label ....))] and
@scheme[scheme] are usually better than cross-document references
using @scheme[secref].

@; ----------------------------------------
@section{Defining Scheme Bindings}

Use @scheme[defproc] to document a procedure, @scheme[defform] to
document a syntactic form, @scheme[defstruct] to document a structure
type, etc.  These forms provide consistent formatting of definitions,
and they declare hyperlink targets for @scheme[scheme]-based
hyperlinks.

To document a @scheme[my-helper] procedure that is exported by
@filepath{helper.ss} in the @filepath{my-lib} collection that contains
@filepath{manual.scrbl}:

@itemize[

 @item{Use @scheme[(require (for-label "helper.ss"))] to import the
       binding information about the bindings of @filepath{helper.ss}
       for use when typesetting identifiers. A relative reference
       @scheme["helper.ss"] works since it is relative to the
       documentation source.}

 @item{Add a @tt|{@defmodule[my-lib/helper]}| declaration, which
       specifies the library that is being documented within the
       section. The @scheme[defmodule] form needs an absolute module
       name @scheme[mylib/helper], instead of a relative reference
       @scheme["helper.ss"], since the module path given to
       @scheme[defmodule] appears verbatim in the generated
       documentation.}

 @item{Use @scheme[defproc] to document the procedure.}

]

Adding these pieces to @filepath{"manual.scrbl"} gives us the
following:

@; [Eli] This is also using `my-lib/helper' which doesn't work with
@; planet libraries
@verbatim[#:indent 2]|{
  #lang scribble/manual
  @(require (for-label scheme
                       "helper.ss"))

  @title{My Library}

  @defmodule[my-lib/helper]

  @defproc[(my-helper [lst list?])
           (listof 
            (not/c (one-of/c 'cow)))]{

   Replaces each @scheme['cow] in @scheme[lst] with
   @scheme['aardvark].}
}|

In @scheme[defproc], a contract is specified with each argument to the
procedure. In this example, the contract for the @scheme[_lst]
argument is @scheme[list?], which is the contract for a list. After
the closing parenthesis that ends the argument sequence, the contract
of the result must be given; in this case, @scheme[my-helper]
guarantees a result that is a list where none of the elements are
@scheme['cow].

Some things to notice in this example and the documentation that it
generates:

@itemize[

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

 @item{If you hover the mouse pointer over @scheme[my-helper], a popup
       reports that it is provided from @schemeidfont{my-lib/helper}.}

 @item{If you use @scheme[my-helper] in any documentation now, as long
       as that documentation source also has a @scheme[(require
       (for-label ....))] of @filepath{helper.ss}, then the
       reference is hyperlinked to the definition above.}

]

See @scheme[defproc*], @scheme[defform], @|etc| for more information
on forms to document Scheme bindings.

@; ----------------------------------------
@section{Showing Scheme Examples}

The @scheme[examples] form from @scheme[scribble/eval]
helps you generate examples in your documentation. @bold{Warning:} the
@scheme[examples] form is especially likely to change or be replaced.

To use @scheme[examples], the procedures to document must be suitable
for use at documentation time; in fact, @scheme[examples] uses
bindings introduced into the document source by
@scheme[require]. Thus, to generate examples using @scheme[my-helper]
from the previous section, @filepath{helper.ss} must be imported both
via @scheme[require-for-label] and @scheme[require]:

@verbatim[#:indent 2]|{
  #lang scribble/manual
  @(require scribble/eval    ; <--- added
            "helper.ss"      ; <--- added
            (for-label scheme
                       "helper.ss"))

  @title{My Library}

  @defmodule[my-lib/helper]{The @schememodname[my-lib/helper]
  module---now with extra cows!}

  @defproc[(my-helper [lst list?])
           (listof (not/c (one-of/c 'cow)))]{

   Replaces each @scheme['cow] in @scheme[lst] with
   @scheme['aardvark].

   @examples[
     (my-helper '())
     (my-helper '(cows such remarkable cows))
   ]}
}|

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

Revising @filepath{cows.scrbl} from the previous section:

@verbatim[#:indent 2]|{
  #lang scribble/manual

  @title[#:style '(toc)]{Cows}

  @local-table-of-contents[]

  @section[#:tag "singing"]{Singing}
  Wherever they go, it's a quite a show.

  @section{Dancing}
  See @secref["singing"].
}|

To run this example, remember to change @filepath{info.ss} to add the
@scheme['multi-page] style. You may also want to add a call to
@scheme[table-of-contents] in @filepath{manual.scrbl}.

The difference between @scheme[table-of-contents] and
@scheme[local-table-of-contents] is that the latter is ignored for
Latex output.

When using @scheme[local-table-of-contents], it often makes sense to
include introductory text before the call of
@scheme[local-table-of-contents]. When the introductory text is less
important and when when local table of contents is short, putting the
introductory text after the call of @scheme[local-table-of-contents]
may be appropriate.

@;----------------------------------------
@include-section["style.scrbl"]
