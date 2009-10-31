#lang scribble/doc
@(require "ss.ss"
          (for-label (except-in scheme/gui drop)
                     slideshow/step
                     slideshow/slides-to-picts))

@(define d=> @elem['rarr])

@title[#:style 'toc]{Making Slides}

@declare-exporting[slideshow/base slideshow]

@defmodule*/no-declare[(slideshow/base)]{The
@schememodname[slideshow/base] module, which is re-provided by
@schememodname[slideshow], provides the functions for creating slides.}

@local-table-of-contents[]

@; ----------------------------------------------------------------------

@section{Primary Slide Functions}

@defproc[(slide [#:title title (or/c #f string? pict?) #f]
                [#:name  name (or/c #f string?) title]
                [#:layout layout (or/c 'auto 'center 'top 'tall) 'auto]
                [#:inset inset slide-inset? (make-slide-inset 0 0 0 0)]
                [#:timeout secs (or/c #f real?) #f]
                [#:condense? condense? any/c (and timeout #t)]
                [element (flat-rec-contract elem/c
                           (or/c pict? 
                                'next 'next! 'alts 'alts~ 'nothing
                                comment?
                                (listof (listof elem/c))))] ...)
          void?]{

Creates and registers a slide. See @secref["staging"] for information
about @scheme[element]s.

When this function is first called in non-printing mode, then the
viewer window is opened. Furthermore, each call to the function
@scheme[yield]s, so that the viewer window can be refreshed, and so
the user can step through slides.

If @scheme[title] is not @scheme[#f], then a title is shown for the
slide. The @scheme[name] is used in the slide-navigation dialog, and
it defaults to @scheme[title].

If @scheme[layout] is @scheme['top], then the content is top-aligned,
with @scheme[(* 2 gap-size)] space between the title and the
content. The @scheme['tall] layout is similar, but with only
@scheme[gap-size]. The @scheme['center] mode centers the content
(ignoring space consumed by the title). The @scheme['auto] mode is
like @scheme['center], except when @scheme[title] is non-@scheme[#f]
and when the space between the title and content would be less than
@scheme[(* 2 gap-size)], in which case it behaves like @scheme['top].

The @scheme[inset] argument supplies an inset that makes the
slide-viewing window smaller when showing the slide. See
@scheme[make-slide-inset] for more information.

If @scheme[secs] argument for @scheme[#:timeout] is not @scheme[#f],
then the viewer automatically advances from this slide to the next
after @scheme[secs] seconds, and manual advancing skips this slide.

If @scheme[condense?] is ture, then in condense mode (as specified by
the @Flag{c} command-line flag), the slide is not created and
registered.}


@defproc[(t [str string?]) pict?]{

The normal way to make plain text. Returns @scheme[(text str
(current-main-font) (current-font-size))].}

@defproc[(it [str string?]) pict?]{

The normal way to make italic text. Returns @scheme[(text str (cons
'italic (current-main-font)) (current-font-size))].}

@defproc[(bt [str string?]) pict?]{

The normal way to make bold text. Returns @scheme[(text str (cons
'bold (current-main-font)) (current-font-size))].}

@defproc[(bit [str string?]) pict?]{

Bold-italic text. Returns @scheme[(text str (list* 'bold 'italic
(current-main-font)) (current-font-size))].}

@defproc[(tt [str string?]) pict?]{

The normal way to make monospaced text. Returns @scheme[(text str
`(bold . modern) (current-font-size))].}

@defproc[(rt [str string?]) pict?]{

The normal way to make serif text. Returns @scheme[(text str 'roman
(current-font-size))].}

@defproc[(titlet [str string?]) pict?]{

Creates title text. Returns @scheme[((current-titlet) str)].}

@defproc[(para [#:width width real? (current-para-width)]
               [#:align align (or/c 'left 'center 'right) 'left]
               [#:fill? fill? any/c #t]
               [#:decode? decode? any/c #t]
               [element (flat-rec-contract elem/c
                          (or/c string? pict? (listof elem/c)))] ...)
         pict?]{

Generates a paragraph pict that is no wider than @scheme[width] units,
and that is exactly @scheme[width] units if @scheme[fill?] is true. If
@scheme[fill?] is @scheme[#f], then the result pict is as wide as the
widest line.

Each list within @scheme[element]s is spliced into the sequence of
string and pict elements. If @scheme[decode?] is true, then strings
among the @scheme[element]s are decoded by performing the following
substitutions: @litchar{---} @d=> @litchar["\u2014"], @litchar{--}
@d=> @litchar["\u2013"], @litchar{``} @d=> @litchar["\u201C"],
@litchar{''} @d=> @litchar["\u201D"], @litchar{'} @d=>
@litchar["\u2019"]. In addition, to better work with
@schememodname[at-exp] notation, if an @scheme[element] is @scheme["\n"],
then it is dropped along with any spaces at the start of the next
element.

Strings are split at spaces for word-wrapping to fit the page, and a
space is added between elements. If a string element starts with one
of the following punctuation marks (after decoding), however, no space
is added before the string:

@t{
 @hspace[2] @litchar{-} @litchar{'} @litchar{,} @litchar{.} @litchar{ } @litchar{:} 
 @litchar{;} @litchar{?} @litchar{!} @litchar{)} @litchar["\u201D"] @litchar["\u2019"]
}

The @scheme[align] argument specifies how to align lines within the
paragraph.

See the spacing between lines is determined by the
@scheme[current-line-sep] parameter.}


@defproc[(item [#:width width real? (current-para-width)]
               [#:bullet blt pict? bullet]
               [#:align align (or/c 'left 'center 'right) 'left]
               [#:fill? fill? any/c #t]
               [#:decode? decode? any/c #t]
               [element (flat-rec-contract elem/c
                          (or/c string? pict? (listof elem/c)))] ...)
         pict?]{

Like @scheme[para], but with @scheme[blt] followed by @scheme[(/
gap-size 2)] space appended horizontally to the resulting paragraph,
aligned with the top line. The paragraph width of @scheme[blt] plus
@scheme[(/ gap-size 2)] is subtracted from the maximum width of the
paragraph.}


@defproc[(subitem [#:width width real? (current-para-width)]
                  [#:bullet blt pict? o-bullet]
                  [#:align align (or/c 'left 'center 'right) 'left]
                  [#:fill? fill? any/c #t]
                  [#:decode? decode? any/c #t]
                  [element (flat-rec-contract elem/c
                             (or/c string? pict? (listof elem/c)))] ...)
         pict?]{

Like @scheme[item], but an additional @scheme[(* 2 gap-size)] is
subtracted from the paragraph width and added as space to the left of
the pict. Also, @scheme[o-bullet] is the default bullet, instead of
@scheme[bullet].}


@defproc[(clickback [pict pict?] [thunk (-> any)])
         pict?]{

Creates a pict that embeds the given one, and is the same size as the
given pict, but that when clicked during a presentation calls
@scheme[thunk].}


@defproc[(size-in-pixels [pict pict?]) pict?]{

Scales @scheme[pict] so that it is displayed on the screen as
@scheme[(pict-width pict)] pixels wide and @scheme[(pict-height pict)]
pixels tall. The result is @scheme[pict] when using a 1024 by 768
display.}


@defproc[(make-outline [name (or/c symbol? (listof symbol?))]
                       [title (or/c string? pict?)]
                       [subitems (or/c #f null?
                                       (symbol? . -> . pict?))]
                       ...)
          (symbol? . -> . void?)]{

Returns a function that takes a symbol and generates an outline
slide.

The @scheme[...] above applies to all three arguments together.  Each
trio of arguments defines a section for the outline:

@itemize[

 @item{The section @scheme[name] is either a symbol or a list of symbols. When
        the outline function is called later to make an outline, the
        given symbol is compared to the section's symbol(s), and the
        section is marked as current if the symbol matches.}

 @item{The @scheme[title] is used as the displayed name of the
       section.}

 @item{The @scheme[subitems] are displayed when the section is
       active. It can be @scheme[#f] or @scheme[null] (for historical
       reasons) if no subitems are to be displayed. Otherwise, it
       should be a function that takes a symbol (the same one passed
       to the outline maker) and produces a pict.}

]}

@defproc[(comment [text (or/c string? pict?)] ...)
         comment?]{

Combines strings and picts to be used as a slide element for (usually
hidden) commentary. Use the result as an argument to @scheme[slide].}

@defproc[(comment? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a comment produced by
@scheme[comment].}

@; ------------------------------------------------------------------------

@section{Slide Registration}

@defproc[(most-recent-slide) slide?]{

Returns a slide structure that be supplied @scheme[re-slide] to make a
copy of the slide.}

@defproc[(retract-most-recent-slide) slide?]{

Cancels the most recently created slide, and also returns a slide
structure that be supplied to @scheme[re-slide] to restore the slide
(usually in a later position).}

@defproc[(re-slide [slide slide?] [pict pict? (blank)])
         void?]{

Re-inserts a slide, @scheme[lt-superimpose]ing the given additional
@scheme[pict].}

@defproc[(slide? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a slide produced by
@scheme[most-recent-slide] or @scheme[retract-most-recent-slide].}

@; ------------------------------------------------------------------------

@section{Viewer Control}

@defproc[(start-at-recent-slide) void?]{

Sets the starting slide for the talk to the most recently created
slide. If this function is used multiple times, the last use overrides
the earlier uses.}


@defproc[(enable-click-advance! [on? any/c]) void?]{

Enables or disables slide advance as a result of a mouse click.}


@defproc[(set-use-background-frame! [on? any/c]) void?]{

Enables or disables the creation of a background frame, which is
typically useful only when @scheme[make-slide-inset] is used are
active. The last enable/disable before the first slide registration
takes effect once and for all.}

@defproc[(set-page-numbers-visible! [on? any/c]) void?]{

Determines whether slide numbers are initially visible in the viewer.}


@defparam[current-page-number-font font (is-a?/c font%)]{

Parameter that determines the font used to draw the page number (if
visible).}


@defparam[current-page-number-color color (or/c string? (is-a?/c color%))]{

Parameter that determines the color used to draw the page number (if
visible).}

@defparam[current-page-number-adjust proc (-> number? string? string?)]{ 
Parameter that controls the precise text
that appears to indicate the page numbers (if visible). The
input to the function is the default string and the slide
number, and the result is what is drawn in the bottom right
corner. The default parameter value just returns its first
argument.
} 

@; ------------------------------------------------------------------------

@section{Constants and Layout Variables}

@defthing[gap-size 24]{

A width commonly used for layout.}


@defthing[bullet pict?]{

A filled bullet used by default by @scheme[item].}


@defthing[o-bullet pict?]{

A hollow bullet used by default by @scheme[subitem].}


@defidform[client-w]{

Produces the width of the display area, minus @scheme[margin]s. The
result of the form changes if the margin is adjusted via
@scheme[set-margin!].}


@defidform[client-h]{

Produces the height of the display area, minus @scheme[margin]s, but
including the title area). The result of the form changes if the
margin is adjusted via @scheme[set-margin!].}


@defidform[full-page]{

Produces an empty pict that is the same size as the client area, which
is like @scheme[(blank client-w client-h)].}


@defidform[titleless-page]{

Produces an empty pict that is the same size as the client area minus
the title area in @scheme['top] layout mode, which is like
@scheme[(blank client-w (- client-h title-h (* 2 gap-size)))].}


@defidform[margin]{

Produces a number that corresponds to the current margin, which
surrounds every side of the slide. The client area for a slide
corresponds to the display area (which is always 1024 by 768) minus
this margin on each side. The default margin is @scheme[20].

The margin can be adjusted via @scheme[set-margin!].}


@defidform[title-h]{

Produces a number that corresponds to the height of a title created by
@scheme[titlet].

If @scheme[titlet] is changed via the @scheme[current-titlet]
parameter, the title height should be updated via
@scheme[set-title-h!].}


@defthing[printing? boolean?]{

The value is @scheme[#t] if slides are being generated for printed
output, @scheme[#f] for normal on-screen display. Printing mode is
normally triggered via the @DFlag{print} or @DFlag{ps} command-line
flag.}


@defthing[condense? boolean?]{

The value is @scheme[#t] if slides are being generated in condensed
mode, @scheme[#f] for normal mode. Condensed mode is normally
triggered via the @DFlag{condense} command-line flag.}

@; ------------------------------------------------------------------------

@section{Configuration}

@defparam[current-font-size n exact-nonnegative-integer?]{

Parameter that determines he font size used by @scheme[t],
@scheme[para], etc. The default size is @scheme[32].}


@defparam[current-main-font style text-style/c]{

Parameter that determines the font size used by @scheme[t],
@scheme[para], etc.  The default is platform-specific; possible
initial values include @scheme['swiss], @scheme["Verdana"], and
@scheme["Gill Sans"].}


@defparam[current-line-sep n exact-nonnegative-integer?]{

Parameter that controls the amount of space used between lines by
@scheme[para], @scheme[item], and @scheme[subitem].}


@defparam[current-para-width n exact-nonnegative-integer?]{

Parameter that controls the width of a pict created by
@scheme[para], @scheme[item], and @scheme[subitem].}


@defparam[current-title-color color (or/c string? (is-a?/c color%))]{

Parameter used by the default @scheme[current-titlet] to colorize the
title. The default is @scheme["black"].}


@defparam[current-slide-assembler proc ((or/c string? #f)
                                        exact-nonnegative-integer?
                                        pict?
                                        . -> .
                                        pict?)]{

Parameter whose value is a function for assembling slide content into
a single pict; the assembling function takes a string for the title
(or @scheme[#f]), a separation for the title (if any) and pict, and a
pict for the slide content (not counting the title).

The result is of the assembler is @scheme[ct-superimpose]d with the
client area, but the result pict might draw outside the client region
to paint the screen margins, too.

The default assembler uses @scheme[titlet] to turn a title string (if
any) to a pict. See also @scheme[current-titlet] and
@scheme[set-title-h!],.

The slide assembler is @emph{not} responsible for adding page
numbers to the slide; that job belongs to the viewer. See also
@scheme[current-page-number-font], @scheme[current-page-number-color],
and @scheme[set-page-numbers-visible!].}


@defparam[current-titlet proc (string? . -> . pict?)]{

Parameter to configure @scheme[titlet]. The default is

@schemeblock[
 (lambda (s)
   (colorize (text s (current-main-font) 40)
             (current-title-color)))
]

If this parameter is changed such that the result is a different
height, then @scheme[set-title-h!] should be called to update the
value produced by @scheme[title-h], @scheme[titleless-page], etc.}


@defproc[(set-margin! [amt real?]) void?]{

Changes the margin that surrounds the client area. See also
@scheme[margin].}


@defproc[(set-title-h! [amt real?]) void?]{

Changes the expected height of a title, which adjusts @scheme[title-h],
@scheme[client-h], @scheme[full-page], and @scheme[titleless-page].}


@defproc[(make-slide-inset [left-inset exact-nonnegative-integer?]
                           [top-inset exact-nonnegative-integer?]
                           [right-inset exact-nonnegative-integer?]
                           [bottom-inset exact-nonnegative-integer?])
          slide-inset?]{

Creates a slide inset, which describes a number of pixels to inset the
viewer for a slide on each side.}


@defproc[(slide-inset? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a slide inset created by
@scheme[make-slide-inset], @scheme[#f] otherwise.}

@; ----------------------------------------------------------------------

@section{Pict-Staging Helper}

@defmodule[slideshow/step]{The @schememodname[slideshow/step] library
provides syntax for breaking a complex slide into steps that are more
complex than can be handled with @scheme['next] and @scheme['alts] in
a @scheme[slide] sequence.}

@defform[(with-steps (id ...) body ...)]{

Evaluates the @scheme[body]s once for each @scheme[id], skipping an
@scheme[id] if its name ends with @litchar{~} and @scheme[condense?]
is true. The results of the last @scheme[body] for each iteration are
collected into a list, which is the result of the @scheme[with-steps]
form.

Within the @scheme[body]s, several keywords are bound non-hygienically
(using the first @scheme[body]'s lexical context):

@itemize[

 @item{@scheme[(only? id)] --- returns @scheme[#t] during the
    @scheme[id] step (i.e., during the evaluation of the
    @scheme[body]s for @scheme[id]), @scheme[#f] otherwise.}

 @item{@scheme[(vonly id)] --- returns the identity function during
    the @scheme[id] step, @scheme[ghost] otherwise.}

 @item{@scheme[(only id _then-expr)] returns the result of
    @scheme[_then-expr] during the @scheme[id] step, @scheme[values]
    otherwise.}

 @item{@scheme[(only id _then-expr _else-expr)] returns the result
    of @scheme[_then-expr] during the @scheme[id] step, the result of
    @scheme[_else-expr] otherwise.}

 @item{@scheme[(before? id)] --- returns @scheme[#t] before the
    @scheme[id] step, @scheme[#f] starting for the @scheme[id] and
    afterward.}

 @item{@scheme[(vbefore id)], @scheme[(before id _then-expr)], or
    @scheme[(before id _then-expr _else-expr)] --- analogous to
    @scheme[vonly] and @scheme[only].}
    
 @item{@scheme[(after? id)] --- returns @scheme[#t] after the
    @scheme[id] step, @scheme[#f] through the @scheme[id] step.}

 @item{@scheme[(vafter id)], @scheme[(after id _then-expr)], or
    @scheme[(after id _then-expr _else-expr)] --- analogous to
    @scheme[vonly] and @scheme[only].}
    
 @item{@scheme[(between? _a-id _b-id)] --- returns @scheme[#t]
    starting from the @scheme[_a-id] step through the @scheme[_b-id]
    step, @scheme[#f] otherwise.}

 @item{@scheme[(vbetween _a-id _b-id)], @scheme[(between _a-id
    _b-id _then-expr)], or @scheme[(between _a-id _b-id _then-expr
    _else-expr)] --- analogous to @scheme[vonly] and @scheme[only].}

 @item{@scheme[(between-excel? _a-id _b-id)] --- returns
    @scheme[#t] starting from the @scheme[_a-id] step through steps
    before the @scheme[_b-id] step, @scheme[#f] for the @scheme[_b-id]
    step and afterward.}

 @item{@scheme[(vbetween-excl _a-id _b-id)], @scheme[(between-excl
    _a-id _b-id _then-expr)], or @scheme[(between-excl _a-id _b-id
    _then-expr _else-expr)] --- analogous to @scheme[vonly] and
    @scheme[only].}
]}


@defform[(with-steps~ (id ...) body ...)]{

Like @scheme[with-steps], but when @scheme[condense?] is true, then
@scheme[expr] is evaluated only for the last @scheme[id] (independent
of whether the name fo the last @scheme[id] name ends in @litchar{~}).
}

@; ----------------------------------------------------------------------

@section{Slides to Picts}

@defmodule[slideshow/slides-to-picts]

@defproc[(get-slides-as-picts [path path-string?]
                              [width real?]
                              [height real?]
                              [condense? any/c]
                              [stop-after (or/c #f exact-nonnegative-integer?) #f])
         (listof pict?)]{

Executes the Slideshow program indicated by @scheme[path] in a fresh
namespace, and returns a list of picts for the slides. Each pict has
the given @scheme[width] and @scheme[height], and @scheme[condense?]
determines whether the Slideshow program is executed in condense
mode.

If @scheme[stop-after] is not @scheme[#f], then the list is truncated
after @scheme[stop-after] slides are converted to picts.}

