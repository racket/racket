#lang scribble/base
@(require scribble/core
          scribble/manual)

@title[#:tag "top" #:version "1.0" #:style 'toc-hidden]{All-Styles Document, Title in ``H2''}

@author["Jack" "Jill"]

All of this content is within ``maincolumn'', then ``main''.

@"\U2192" The version on the top left of this page is in
``versionbox'' and then either span ``versionNoNav'' (no navigation
bar, as for single-page rendering) or ``version'' (with navigation
bar, as for multi-page rendering).

@"\U2192" The author on the top left of this page is in
``SAuthorListBox'', then ``SAuthorList'', and then a span ``author'',
where @tt{<br/>} separates multiple authors.

@"\U2190" The table-of-contents panels are both in a table ``tocset'':

@margin-note{This note is in ``refpara'', then ``refcolumn'', then
             ``refcontent''.}

@itemlist[

 @item{The top panel is in ``tocview''.

       The top panel can have multiple layers of the hierarchy. For a
       single-page rendering, only one layer is present. For an
       example of multiple layers when rendering this document to
       multiple pages, go to @secref["deepest"].

       Each layer is in a ``tocviewlist'' that also has the class
       ``tocviewlisttopspace'' in the case of the first layer.  The
       always-visible name of a layer is in a span ``tocviewtoggle'',
       but that span is also in a ``tocviewtitle'' in the case of the
       first layer. Each item under the title is in a
       ``tocviewsublist'' or a variant: ``tocviewsublistonly'' if only
       a single item is present, ``tocviewsublistfirst'' for the first
       item of multi, ``tocviewsublistlast'' for the last item of
       multiple.  Then, each item is in a span ``tocviewlink''.

       Each section link in the panel is a span ``tocviewlink'' or a
       span ``tocviewselflink'' if the link corresponds to the current
       page or on the path to the current page.}

 @item{A bottom panel is visible here only for a single-page
       rendering. See its description in @secref["h3"].}

]


@margin-note[#:left? #t]{This note is in ``refparaleft'', then
 ``refcolumnleft'', then ``refcontent''.}

Table of contents uses ``toptoclink'' for the top layer, and
``toclink'' for nested levels:

@table-of-contents[]

@margin-note*{This note is in ``refelem'', then ``refcolumn'', then
              ``refcontent''.}
@margin-note*[#:left? #t]{This note is in ``refelemleft'', then 
              ``refcolumnleft'', then ``refcontent''.}

@; ======================================================================
@section[#:tag "h3"]{Section in ``H3''}

@"\U2190" For either single-page or multi-page rendering, the
table-of-contents column here has two panels. The top panel is
described in the @seclink["h3"]{starting prose}. For the bottom panel:

@itemlist[

 @item{The bottom panel is in a ``tocsub''. For a multi-page
      rendering, the on-this-page title is in ``tocsubtitle''. The
      rest is always in a table ``tocsublist''. For each entry, the
      number part is in a span ``tocsublinknumber'', and the title
      part in a span, one of the following: ``tocsubseclink'' if the
      link represents a (sub)section, ``tocnonseclink'' if the link is
      not a (sub)section but there are (sub)sections in the list (and
      there is an example target in this section), or ``tocsublink''
      if no links represent a (sub)section (see
      @secref["all-non-sec"]).}

]

When a part that corresponds to a page has a @racket['no-toc] style,
the top panel of the table-of-contents column is missing and the
bottom panel is in a ``tocview'' instead of ``tocsub''. See
@secref["no-toc"].

Here is the target for the
@toc-target-element[#f @elem{``tocnonseclink''} `(demo (prefixable "non-sec"))]
link.

@subsection[#:style 'toc]{Subsection in ``H4''}

@local-table-of-contents[]

@subsubsection[#:tag "deepest"]{Subsubsection in ``H5''}

@"\U2190" This page has no on-this-page panel in a multi-page
rendering, because there are no numbered subsections, but it has three
levels shown in the table-of-contents panel.

@subsubsub*section{``SSubSubSubSection''}


@subsection{Second Subsection in ``H4''}


@; ======================================================================
@section[#:tag "no-toc" #:style 'no-toc]{Suppressed ToC Panel}

In multi-page rendering, this page has no gobal table-of-contents
panel, because it is suppressed with @racket['no-toc].

@subsection{Subsection}

@subsection{Another Subsection}


@; ======================================================================
@section[#:tag "all-non-sec"]{Non-Section On-This-Page Links}

This section has only non-section targets in the on-this-page
panel of a multi-page rendering.
Here is the target for the
@toc-target-element[#f @elem{``tocsublink'' 1} `(demo (prefixable "non-sec 1"))]
link.
Here is the target for the
@toc-target-element[#f @elem{``tocsublink'' 2} `(demo (prefixable "non-sec 2"))]
link.

Here is the target for the @as-index{``indexlink''} link in the
@seclink["doc-index"]{index} (where ``indexlink'' is used for the
index entry and not here).

@; ======================================================================
@section{Element Styles}

Some spans:

@itemlist[

 @item{@tt{``stt''}}

 @item{@elem[#:style 'roman]{``sroman''}}

 @item{@elem[#:style "slant"]{``slant''}}

 @item{@elem[#:style 'sf]{``ssanserif''}}

 @item{@smaller{``Smaller''}}

 @item{@larger{``Larger''}}

 @item{``hspace'' is used for forced @hspace[3] space}

 @item{``url'' is used for URLs: @url{http://racket-lang.org}}

 @item{@elem[#:style 'no-break]{``nobreak'', which is used to prevent
 line breaks anywhere in the element so that the element may run too
 far right}}

 @item{@italic{italic} directly sets @tt{font-style} to @tt{italic}}

 @item{@bold{bold} directly sets @tt{font-weight} to @tt{bold}}

 @item{@elem[#:style 'superscript]{superscript} directly sets
        @tt{vertical-align} to @tt{super} and @tt{font-size} to @tt{80%}.}

 @item{@elem[#:style 'subscript]{subscript} directly sets
        @tt{vertical-align} to @tt{sub} and @tt{font-size} to @tt{80%}.}

]

Link spans:

@itemlist[

 @item{@elemref[#:underline? #f '(prefixable "plain-target")]{``plainlink''}
       hyperlink to @elemtag['(prefixable "plain-target")]{here}}

 @item{@deftech{technical term} definitions are simply italicized by default}

 @item{@tech{technical term} references are in ``techoutside'', then ``techinside''}

]

@; ======================================================================
@section{Block Styles}

@nested{This paragraph is in a ``SubFlow'' @tt{<blockquote>}.}

@nested[#:style 'inset]{This paragraph is in a plain @tt{<blockquote>}.}

@nested[#:style 'code-inset]{This paragraph is in a ``SCodeFlow''
@tt{<blockquote>}.}

@nested[#:style 'vertical-inset]{This paragraph is in a
``SVInsetFlow'' @tt{<blockquote>}. This style is useful when space is
not normally included between blocks.}

@centered{This paragraph is in a ``SCentered'' @tt{<blockquote>}.}

@tabular[#:style 'boxed (list (list @t{A ``boxed'' table.}))]

@; ======================================================================
@section{Enumerations}

This one is unordered, so it uses @tt{<ul>}:

@itemlist[

 @item{six}

 @item{half-dozen}

]

This one is ordered, so it uses @tt{<ol>}:

@itemlist[#:style 'ordered

 @item{First}

 @item{Second

       @itemlist[#:style 'ordered

          @item{Second, first half}

          @item{Second, second half

                @itemlist[#:style 'ordered

                           @item{First half of that}

                           @item{Second half of that

                                 @itemlist[#:style 'ordered

                                    @item{Thin-slice start}

                                    @item{Thin-sliced end}

                                 ]}
                          ]}

                ]}

 @item{Third}
]

This one is ``compact'':

@itemlist[ #:style 'compact

 @item{six}

 @item{half-dozen}

]

This paragraph follows the enumeration above.

@; ======================================================================
@section{Paragraph Spacing}

This sentence is a paragraph all by itself.

@t{This sentence is a paragraph.}
@t{This sentence is also a paragraph, but it is connected to the
   previous paragraph as a compound paragraph by virtue of having no
   paragraph-breaking space before it, and each paragraph is in a
   ``SIntraPara'' @tt{<div>} instead of a @tt{<p>}.}

This sentence is a paragraph, as is each of A1, B1, A2, B2, A3, B3a,
and B2a in the following table, but B3a and B2a form a compound paragraph.
@;
@tabular[(list (list "A1"
                     "B1") 
               (list "A2"
                     "B2") 
               (list "A3" 
                     @compound-paragraph[plain (list @t{B3a} @t{B3b})]))]
@;
This sentence is a paragraph, and with the preceding table and
  paragraph forms a compound paragraph.

@nested{
 @t{This is a first paragraph in a @tt{<blockquote>}.}
 @t{This is a second paragraph in a @tt{<blockquote>}.}
}


@; ======================================================================
@section{Navigation Bars}

For multi-page rendering, this page will have a navigation bar at the
top and bottom. The bars are within ``maincolumn'' and ``main''.

The tap bar is in ``navsettop'', and the bottom one is in
``navsetbottom''.  Within those divs, ``navsetleft'' wraps content to
be left-aligned and ``navsetright'' wraps content to be right-aligned.

Links that are disabled (such as a next-page link on the last page)
are each in a span ``nonavigation''.

When a search box is included, then it is in ``searchform'' and then
``searchbox''. If no search box is included, then a ``nosearchform''
@tt{div} is used.

Finally, and not part of the nagivation bar, the bottom nagivation bar
is followed by a @tt{div} with the name
``contextindicator''. JavaScript code attached to the page copies the
@tt{ctxtname} query argument, if any, to the @tt{div} and makes it
visible.

@; ======================================================================
@index-section[]
