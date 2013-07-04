#lang scribble/doc
@(require scribble/manual scribble/bnf "utils.rkt")

@title[#:tag "how-to-doc"]{Getting Started with Documentation}

Although the @exec{scribble} command-line utility generates output
from a Scribble document, documentation of Racket libraries is
normally built by @exec{raco setup}. This chapter emphasizes the
@exec{raco setup} approach, which more automatically supports links
across documents.

@margin-note{See @secref["getting-started"] for information on using the
             @exec{scribble} command-line utility.}

@;----------------------------------------
@section[#:tag "setting-up"]{Setting Up Library Documentation}

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

            Welcome to my documentation: @racket[(list 'testing 1 2 3)].
          }|

        The first line starts the file in ``text'' mode and selects
        the Racket manual output format.
        It also introduces bindings like @racket[title] and
        @racket[racket] for writing Racket documentation.}

  @item{Add the following entry to your collect or package's
        @filepath{info.rkt}:

        @racketblock[
          (define scribblings '(("manual.scrbl" ())))
        ]

        The @racket[()] above is a list of options. When your document
        gets large enough that you want it split into multiple pages,
        add the @racket['multi-page] option (omitting the quote, since
        the whole right-hand side of the definition is already
        quoted).

        If you do not already have an @filepath{info.rkt} module,
        here's a suitable complete module:

        @racketmod[
          info
          (define scribblings '(("manual.scrbl" ())))
        ]}

  @item{Run @exec{raco setup} to build your documentation. For a
        collection, optionally supply @Flag{l} followed by the
        collection name to limit the build process to that
        collection. For a @|PLaneT| package, optionally supply
        @Flag{P} followed by the package information to limit the
        build process to that package.}

  @item{The generated documentation is normally
        @filepath{doc/manual/index.html} within the collection or
        @|PLaneT| package directory. If the collection is in
        Racket's main @filepath{collects} directory, however, then the
        documentation is generated as @filepath{manual/index.html} in
        the installation's main @filepath{doc} directory.}

]

@; ----------------------------------------
@section[#:tag "racket-hyperlinks"]{Racket Typesetting and Hyperlinks}

In the document source at the start of this chapter
(@secref["setting-up"]), the Racket expression
@racket[(#,(racketidfont "list") 'testing 1 2 3)] is typeset properly,
but the @racketidfont{list} identifier is not hyperlinked to the usual
definition. To cause @racketidfont{list} to be hyperlinked, add a
@racket[require] form like this:

@verbatim[#:indent 2]|{
  @(require (for-label racket))
}|

This @racket[require] with @racket[for-label] declaration introduces a
document-time binding for each export of the @racketmodname[racket]
module. When the document is built, the @racket[racket] form detects
the binding for @racket[list], and so it generates a reference to the
specification of @racket[list]. The setup process detects the
reference, and it finds the matching specification in the existing
documentation, and ultimately directs the hyperlink to that
specification.

Hyperlinks based on @racket[for-label] and @racket[racket] are the
preferred mechanism for linking to information outside of a single
document. Such links require no information about where and how a
binding is documented elsewhere:

@verbatim[#:indent 2]|{
  #lang scribble/manual
  @(require (for-label racket))

  @title{My Library}

  See also @racket[list].
}|

The @racket[racket] form typesets a Racket expression for inline text,
so it ignores the source formatting of the expression. The
@racket[racketblock] form, in contrast, typesets inset Racket code,
and it preserves the expression's formatting from the document source.

@verbatim[#:indent 2]|{
  #lang scribble/manual
  @(require (for-label racket))

  @title{My Library}

  Some example Racket code:

  @racketblock[
  (define (nobody-understands-me what)
    (list "When I think of all the"
          what
           "I've tried so hard to explain!"))
  (nobody-understands-me "glorble snop")
  ]
}|


@; ----------------------------------------
@section[#:tag "section-hyperlinks"]{Section Hyperlinks}

A @racket[section] declaration in a document can include a
@racket[#:tag] argument that declares a hyperlink-target tag. The
@racket[secref] function generates a hyperlink, using the section name
as the text of the hyperlink. Use @racket[seclink] to create a
hyperlink with text other than the section title.

The following example illustrates section hyperlinks:

@verbatim[#:indent 2]|{
  #lang scribble/manual
  @(require (for-label racket))


  @title{My Library}

  Welcome to my documentation: @racket[(list 'testing 1 2 3)].

  @table-of-contents[]


  @section[#:tag "chickens"]{Philadelphia Chickens}

  Dancing tonight!


  @section{Reprise}

  See @secref{chickens}.
}|

Since the page is so short, the hyperlinks in the above example are
 more effective if you change the @filepath{info.rkt} file to add the
 @racket['multi-file] flag:

@racketblock[
(define scribblings '(("manual.scrbl" (multi-page))))
]

A section can have a @techlink{tag prefix} that applies to all tags as
seen from outside the section. Such a prefix is automatically given to
each top-level document as processed by @exec{raco setup}. Thus,
referencing a section tag in a different document requires using a
prefix, which is based on the target document's main source file.  The
following example links to a section in the Racket reference
manual:

@verbatim[#:indent 2]|{
  #lang scribble/manual
  @(require (for-label racket))
  @(define ref-src
     '(lib "scribblings/reference/reference.scrbl"))

  @title{My Library}

  See also @italic{@secref[#:doc ref-src]{pairs}}.
}|

As mentioned in @secref{racket-hyperlinks}, however, cross-document
references based on @racket[(require (for-label ....))] and
@racket[racket] are usually better than cross-document references
using @racket[secref].

@; ----------------------------------------
@section{Defining Racket Bindings}

Use @racket[defproc] to document a procedure, @racket[defform] to
document a syntactic form, @racket[defstruct] to document a structure
type, etc.  These forms provide consistent formatting of definitions,
and they declare hyperlink targets for @racket[racket]-based
hyperlinks.

To document a @racket[my-helper] procedure that is exported by
@filepath{helper.rkt} in the @filepath{my-lib} collection that contains
@filepath{manual.scrbl}:

@itemize[

 @item{Use @racket[(require (for-label "helper.rkt"))] to import the
       binding information about the bindings of @filepath{helper.rkt}
       for use when typesetting identifiers. A relative reference
       @racket["helper.rkt"] works since it is relative to the
       documentation source.}

 @item{Add a @tt|{@defmodule[my-lib/helper]}| declaration, which
       specifies the library that is being documented within the
       section. The @racket[defmodule] form needs an absolute module
       name @racket[mylib/helper], instead of a relative reference
       @racket["helper.rkt"], since the module path given to
       @racket[defmodule] appears verbatim in the generated
       documentation.}

 @item{Use @racket[defproc] to document the procedure.}

]

Adding these pieces to @filepath{"manual.scrbl"} gives us the
following:

@; [Eli] This is also using `my-lib/helper' which doesn't work with
@; planet libraries
@verbatim[#:indent 2]|{
  #lang scribble/manual
  @(require (for-label racket
                       "helper.rkt"))

  @title{My Library}

  @defmodule[my-lib/helper]

  @defproc[(my-helper [lst list?])
           (listof 
            (not/c (one-of/c 'cow)))]{

   Replaces each @racket['cow] in @racket[lst] with
   @racket['aardvark].}
}|

In @racket[defproc], a contract is specified with each argument to the
procedure. In this example, the contract for the @racket[_lst]
argument is @racket[list?], which is the contract for a list. After
the closing parenthesis that ends the argument sequence, the contract
of the result must be given; in this case, @racket[my-helper]
guarantees a result that is a list where none of the elements are
@racket['cow].

Some things to notice in this example and the documentation that it
generates:

@itemize[

 @item{The @racket[list?], @racket[listof], @|etc| elements of
       contracts are hyperlinked to their documentation.}

 @item{The result contract is formatted in the generated documentation
       in the same way as in the source. That is, the source layout of
       contracts is preserved. (In this case, putting the contract all
       on one line would be better.)}

 @item{In the prose that documents @racket[my-helper], @racket[_lst]
       is automatically typeset in italic, matching the typesetting in
       the blue box. The @racket[racket] form essentially knows that
       it's used in the scope of a procedure with argument
       @racket[_lst].}

 @item{If you hover the mouse pointer over @racket[my-helper], a popup
       reports that it is provided from @racketidfont{my-lib/helper}.}

 @item{If you use @racket[my-helper] in any documentation now, as long
       as that documentation source also has a @racket[(require
       (for-label ....))] of @filepath{helper.rkt}, then the
       reference is hyperlinked to the definition above.}

]

See @racket[defproc*], @racket[defform], @|etc| for more information
on forms to document Racket bindings.

@; ----------------------------------------
@section{Showing Racket Examples}

The @racket[examples] form from @racket[scribble/eval]
helps you generate examples in your documentation. @bold{Warning:} the
@racket[examples] form is especially likely to change or be replaced.

To use @racket[examples], the procedures to document must be suitable
for use at documentation time; in fact, @racket[examples] uses
bindings introduced into the document source by
@racket[require]. Thus, to generate examples using @racket[my-helper]
from the previous section, @filepath{helper.rkt} must be imported both
via @racket[require-for-label] and @racket[require]:

@verbatim[#:indent 2]|{
  #lang scribble/manual
  @(require scribble/eval    ; <--- added
            "helper.rkt"     ; <--- added
            (for-label racket
                       "helper.rkt"))

  @title{My Library}

  @defmodule[my-lib/helper]{The @racketmodname[my-lib/helper]
  module---now with extra cows!}

  @defproc[(my-helper [lst list?])
           (listof (not/c (one-of/c 'cow)))]{

   Replaces each @racket['cow] in @racket[lst] with
   @racket['aardvark].

   @examples[
     (my-helper '())
     (my-helper '(cows such remarkable cows))
   ]}
}|

@;----------------------------------------
@section{Multi-Page Sections}

Setting the @racket['multi-page] option (see
@secref["section-hyperlinks"]) causes each top-level section of a
document to be rendered as a separate HTML page.

To push sub-sections onto separate pages, use the @racket['toc] style
for the enclosing section (as started by @racket[title],
@racket[section], @racket[subsection], etc.) and use
@racket[local-table-of-contents] to generate hyperlinks to the
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

To run this example, remember to change @filepath{info.rkt} to add the
@racket['multi-page] style. You may also want to add a call to
@racket[table-of-contents] in @filepath{manual.scrbl}.

The difference between @racket[table-of-contents] and
@racket[local-table-of-contents] is that the latter is ignored for
Latex output.

When using @racket[local-table-of-contents], it often makes sense to
include introductory text before the call of
@racket[local-table-of-contents]. When the introductory text is less
important and when local table of contents is short, putting the
introductory text after the call of @racket[local-table-of-contents]
may be appropriate.
