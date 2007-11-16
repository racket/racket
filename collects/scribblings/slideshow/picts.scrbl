#lang scribble/doc
@require["ss.ss"]
@require[(for-label mred)]

@title[#:style 'toc]{Making Pictures}

@declare-exporting[slideshow/pict slideshow]

The @schememodname[slideshow/pict] layer provides core functions for
constructing pictures, and it is independent of the slide viewer. This
layer can be used, for example, to generate a picture as encapsulated
PostScript for inclusion into a larger document. The
@schememodname[slideshow/pict] layer is re-provided by the
@schememodname[slideshow] module.

@local-table-of-contents[]

@; ------------------------------------------------------------------------

@section{Pict Datatype}

A picture is a @scheme[pict] structure. Some functions, such as
@scheme[hline], create new simple picts. Other functions, such as
@scheme[ht-append], build new picts out of existing picts. In the
latter case, the embedded picts retain their identity, so that
offset-finding functions, such as @scheme[lt-find], can find the
offset of an embedded pict in a larger pict.

In addition to its drawing part, a pict has the following
@deftech{bounding box} structure:

@verbatim[#<<EOS
                w
        ------------------
       |                  | a  \
       |------------------|    |
       |                  |    | h
       |------------------|    |
       |                  | d  /
        ------------------     
EOS
]

That is, the bounding box has a width @math{w} and a height
@math{h}. For a single text line, @math{d} is descent below the
baseline, and @math{a+d=h}. For multiple text lines (often created
with a function like @scheme[vc-append]), @math{a} is the ascent of
the top line, and @math{d} is the descent of the bottom line, so
@math{a+d<h}. Many picts have @math{d=0} and @math{a=h}.

The size information for a pict is computed when the pict is
created. This strategy supports programs that create new picts though
arbitrarily complex computations on the size and shape of existing
picts. The functions @scheme[pict-width], @scheme[pict-height],
@scheme[pict-descent], and @scheme[pict-ascent] extract bounding-box
information from a pict.


@defstruct[pict ([draw ((is-a?/c dc<%>) real? real? . -> . any)]
                 [width real?]
                 [height real?]
                 [ascent real?]
                 [descent real?]
                 [children (listof child?)]
                 [panbox (or/c false/c any/c)])]{

A @scheme[pict] structure is normally not created directly with
@scheme[make-pict]. Instead, functions like @scheme[text],
@scheme[hline], and @scheme[dc] are used to construct a pict.

The @scheme[draw] field contains the pict's drawing procedure, which
takes a @scheme[dc<%>] drawing context and an offset for the pict's
top-left corner (i.e., it's bounding box's top left corner relative to
the @scheme[dc<%>] origin). The state of the @scheme[dc<%>] is
intended to affect the pict's drawing; for example, the pen and brush
will be set for a suitable default drawing mode, and the
@scheme[dc<%>] scale will be set to scale the resulting image.

The @scheme[panbox] field is internal, and it should be ininitialized
to @scheme[#f].}


@defstruct[child ([pict pict?]
                  [dx real?]
                  [dy real?]
                  [sx real?]
                  [sy real?])]{
Records, for a pict constructed of other picts, the relative location
and scale of one nested pict.

A @scheme[child] structure is normally not created directly with
@scheme[make-child]. Instead, functions like @scheme[hc-append] create
@scheme[child] structures when combining picts to create a new one.}

@; ------------------------------------------------------------------------

@section{Basic Pict Constructors}

@defproc*[([(dc [draw ((is-a?/c dc<%>) real? real? . -> . any)]
                [w real?]
                [h real?])
            pict?]
           [(dc [draw ((is-a?/c dc<%>) real? real? . -> . any)]
                [w real?]
                [h real?]
                [a real?]
                [d real?])
            pict?])]{

Creates an arbitrary self-rendering pict.  The arguments to the
rendering procedure will be a device context and top-left location for
drawing.
  
When the rendering procedure is called, the current pen and brush will
be solid and in the pict's color (and linewidth), and the scale and
offset of the dc will be set. The text mode will be transparent, but
the font and text colors are not guaranteed to be anything in
particular.}

@defproc*[([(blank [size real? 0]) pict?]
           [(blank [w real?][h real?]) pict?]
           [(blank [w real?][a real?][d real?]) pict?]
           [(blank [w real?][h real?][a real?][d real?]) pict?])]{

Creates a pict that draws nothing. The one-argument case supplies a
value sued for both the width and height. In the one- and two-argument
case, the ascent and descent are @math{0} for the resulting pict's
bounding box; in the three-argument case, the height is computed by
adding the given ascent and descent.}

@defproc[(text [content string?]
               [style text-style/c null]
               [size (integer-in 1 255) 12] 
               [angle real? 0])
         pict?]{

Creates a pict that draws text. For creating text picts within a slide
presentation, see @scheme[t], instead. Otherwise, before calling this
function, a drawing context must be installed with
@scheme[dc-for-text-size].

The @scheme[style] argument must be one of the following:

@itemize{

 @item{@scheme[null] --- the default, same as @scheme['default]}

 @item{a @scheme[font%] object}

 @item{a font family symbol, such a @scheme['roman] (see @scheme[font%])}

 @item{a font face string, such as @scheme["Helvetica"] (see @scheme[font%])}

 @item{@scheme[(cons _str _sym)] combining a face string and a font
       family (in case the face is unavailable; see @scheme[font%])}

 @item{@scheme[(cons 'bold style)] for a valid @scheme[style]}

 @item{@scheme[(cons 'italic style)]}
 @item{@scheme[(cons 'subscript style)]}
 @item{@scheme[(cons 'superscript style)]}
 @item{@scheme[(cons 'caps style)]}

 @item{@scheme[(cons 'combine style)] --- allows kerning and ligatures
      (the default, unless the @scheme['modern] family is specified)}

 @item{@scheme[(cons 'no-combine style)] --- renders characters individually}
}

If both @scheme['combine] and @scheme['no-combine] are specified, the
first one takes precedence. If caps is specified, the angle must be
zero.

The given @scheme[size] is in pixels, but it is ignored if a
@scheme[font%] object is provided in the text-style.

The @scheme[rotation] is in radians, and positive values rotate
counter-clockwise. For a non-zero @scheme[rotation], the resulting
pict's bounding box covers the rotated text, and the descent is zero
and the ascent is the height.}


@defproc*[([(hline [w real?] [h real?] 
                   [#:segment seg-length (or/c false/c real?) #f]) pict?]
           [(vline [w real?] [h real?] 
                   [#:segment seg-length (or/c false/c real?) #f]) pict?])]{

Straight lines, centered within their bounding boxes.}

@defproc[(frame [pict pict?]
                [#:segment seg-length (or/c #f real?) #f]
                [#:color color (or/c false/c string? (is-a?/c color<%>)) #f]
                [#:line-width width (or/c #f real?) #f])
          pict?]{

Frames a given pict. If the color or line width are provided, the
override settings supplied by the context.}

@defproc*[([(ellipse [w real?] [h real?]) pict?]
           [(circle [diameter real?]) pict?]
           [(filled-ellipse [w real?] [h real?]) pict?]
           [(disk [diameter real?]) pict?])]{

Unfilled and filled ellipses.}

@defproc*[([(rectangle [w real?] [h real?]) pict?]
           [(filled-rectangle [w real?] [h real?]) pict?])]{

Unfilled and filled rectangles.}

@defproc*[([(rounded-rectangle [w real?] [h real?] 
                               [corner-radius real? 0.25])
            pict?]
           [(filled-rounded-rectangle [w real?] [h real?]
                                      [corner-radius real? 0.25])
            pict?])]{

Unfilled and filled rectangles with rounded corners. If the
@scheme[corner-radius] is less than @scheme[1], then it is a
percentage of the smaller of @scheme[width] and @scheme[height].}

@defproc[(bitmap [img (or/c path-string? (is-a?/c bitmap%))])
         pict]{

A pict that display a bitmap. When a path is provided, the image is
loaded with the @scheme['unknown/mask] flag, which means that a mask
bitmap is generated if the file contains a mask.

If the bitmap cannot be loaded, if the given @scheme[bitmap%] object
is not valid, or if the @scheme[bitmap-draft-mode] parameter is set to
@scheme[#t], the result pict draws the word ``bitmap failed''.}

@defproc*[([(arrow [size real?] [radians real?]) pict?]
           [(arrowhead [size real?] [radians real?]) pict?])]{

Creates an arrow or arrowhead in the specific direction within a
@scheme[size] by @scheme[size] pict. Points on the arrow may extend
slightly beyond the bounding box.}

@defproc*[([(pip-line [dx real?] [dy real?] [size real?]) pict?]
           [(pip-arrow-line [dx real?] [dy real?] [size real?]) pict?]
           [(pip-arrows-line [dx real?] [dy real?] [size real?]) pict?])]{

Creates a line (with some number of arrowheads) as a zero-sized pict
suitable for use with @scheme[pin-over]. The 0-sized picture contains
the starting point.

The @scheme[size] is used for the arrowhead size. Even though
@scheme[pip-line] creates no arrowheads, it accepts the @scheme[size]
argument for consistency with the other functions.}

@defproc*[([(pin-line [pict pict?]
                      [src pict-path?]
                      [find-src (pict? pict-path? . ->* . (real? real?))]
                      [dest pict-path?]
                      [find-dest (pict? pict-path? . ->* . (real? real?))]
                      [#:line-width line-width (or/c false/c real?) #f]
                      [#:color color (or/c false/c string? (is-a/c? color%)) #f]
                      [#:under? under? any/c #f])
            pict?]
           [(pin-arrow-line [arrow-size real?] [pict pict?]
                      [src pict-path?]
                      [find-src (pict? pict-path? . ->* . (real? real?))]
                      [dest pict-path?]
                      [find-dest (pict? pict-path? . ->* . (real? real?))]
                      [#:line-width line-width (or/c false/c real?) #f]
                      [#:color color (or/c false/c string? (is-a/c? color%)) #f]
                      [#:under? under? any/c #f]
                      [#:solid? solid? any/c #t])
            pict?]
           [(pin-arrows-line [arrow-size real?] [pict pict?]
                      [src pict-path?]
                      [find-src (pict? pict-path? . ->* . (real? real?))]
                      [dest pict-path?]
                      [find-dest (pict? pict-path? . ->* . (real? real?))]
                      [#:line-width line-width (or/c false/c real?) #f]
                      [#:color color (or/c false/c string? (is-a/c? color%)) #f]
                      [#:under? under? any/c #f]
                      [#:solid? solid? any/c #t]) 
            pict?])]{

Adds a line or line-with-arrows onto @scheme[pict], using one of the
pict-finding functions (e.g., @scheme[lt-find]) to extract the source
and destination of the line.

If @scheme[under?] is true, then the line and arrows are added under
the existing @scheme[pict] drawing, instead of on top. If
@scheme[solid?] is false, then the arrowheads are hollow instead of
filled.}

@defthing[text-style/c contract?]{

A contract that matches the second argument of @scheme[text].}

@defboolparam[bitmap-draft-mode on?]{

A parameter that determines whether @scheme[bitmap] loads/uses a
bitmap.}


@; ------------------------------------------------------------------------

@section{Pict Combiners}

@defproc*[([(vl-append [pict pict?] ...) pict?]
           [(vl-append [d real?] [pict pict?] ...) pict?]
           [(vc-append [pict pict?] ...) pict?]
           [(vc-append [d real?] [pict pict?] ...) pict?]
           [(vr-append [pict pict?] ...) pict?]
           [(vr-append [d real?] [pict pict?] ...) pict?]
           [(ht-append [pict pict?] ...) pict?]
           [(ht-append [d real?] [pict pict?] ...) pict?]
           [(htl-append [pict pict?] ...) pict?]
           [(htl-append [d real?] [pict pict?] ...) pict?]
           [(hc-append [pict pict?] ...) pict?]
           [(hc-append [d real?] [pict pict?] ...) pict?]
           [(hbl-append [pict pict?] ...) pict?]
           [(hbl-append [d real?] [pict pict?] ...) pict?]
           [(hb-append [pict pict?] ...) pict?]
           [(hb-append [d real?] [pict pict?] ...) pict?])]{

Creates a new pict as a column (for @scheme[v...-append]) or row (for
@scheme[h...-append]) of other picts. The optional @scheme[d] argument
specifies amount of space to insert between each pair of pictures in
making the column or row.

Different procedures align pictures in the orthogonal direction in
different ways. For example, @scheme[vl-append] left-aligns all of the
pictures.

The descent of the result corresponds to baseline that is lowest in
the result among all of the picts' descent-specified baselines;
similarly, the ascent of the result corresponds to the highest
ascent-specified baseline.}

@defproc*[([(lt-superimpose [pict pict?] ...) pict?]
           [(ltl-superimpose [pict pict?] ...) pict?]
           [(lc-superimpose [pict pict?] ...) pict?]
           [(lbl-superimpose [pict pict?] ...) pict?]
           [(lb-superimpose [pict pict?] ...) pict?]
           [(ct-superimpose [pict pict?] ...) pict?]
           [(ctl-superimpose [pict pict?] ...) pict?]
           [(cc-superimpose [pict pict?] ...) pict?]
           [(cbl-superimpose [pict pict?] ...) pict?]
           [(cb-superimpose [pict pict?] ...) pict?]
           [(rt-superimpose [pict pict?] ...) pict?]
           [(rtl-superimpose [pict pict?] ...) pict?]
           [(rc-superimpose [pict pict?] ...) pict?]
           [(rbl-superimpose [pict pict?] ...) pict?]
           [(rb-superimpose [pict pict?] ...) pict?])]{

Creates a new picture by superimposing a set of pictures. The name
prefixes are alignment indicators: horizontal alignment then vertical
alignment.

The descent of the result corresponds to baseline that is lowest in
the result among all of the picts' descent-specified baselines;
similarly, the ascent of the result corresponds to the highest
ascent-specified baseline.}

@defproc*[([(pin-over [base pict?] [dx real?] [dy real?] [pict pict?])
            pict?]
           [(pin-over [base pict?] 
                      [find-pict pict-path?]
                      [find (pict? pict-path? . ->* . (real? real?))]
                      [pict pict?])
            pict?])]{

Creates a pict with the same bounding box, ascent, and descent as
@scheme[base], but with @scheme[pict] placed on top.  The @scheme[dx]
and @scheme[dy] arguments specify how far right and down the second
pict's corner is from the first pict's corner.  Alternately, the
@scheme[find-pict] and @scheme[find] arguments find a point in
@scheme[base] for @scheme[find-pict]; the @scheme[find] procedure
should be something like @scheme[lt-find].}

@defproc*[([(pin-under [base pict?] [dx real?] [dy real?] [pict pict?])
            pict?]
           [(pin-under [base pict?] 
                       [find-pict pict?]
                       [find (pict? pict? . ->* . (real? real?))]
                       [pict pict?])
            pict?])]{

Like @scheme[pin-over], but @scheme[pict] is drawn before
@scheme[base] in the resulting combination.}

@defproc[(table [ncols exact-positive-integer?]
                [picts (listof pict?)]
                [col-aligns (table-list-of (pict? pict? -> pict?))]
                [row-aligns (table-list-of (pict? pict? -> pict?))]
                [col-seps (table-list-of real?)]
                [row-seps (table-list-of real?)])
         pict?]{

Creates a table given a list of picts. The @scheme[picts] list is a
concatenation of the table's rows (which means that a Scheme
@scheme[list] call can be formatted to reflect the shape of the output
table).
  
The @scheme[col-aligns], @scheme[row-aligns], @scheme[col-seps], and
@scheme[row-seps] arguments are ``lists'' specifying the row and
columns alignments separation between rows and columns.  For @math{c}
columns and @math{r} rows, the first two should have @math{c} and
@math{r} superimpose procedures, and the last two should have
@math{c-1} and @math{r-1} numbers, respectively. The lists can be
``improper'' (i.e., ending in a number instead of an empty list), in
which case the non-pair cdr is used as the value for all remaining
list items that were expected. The @scheme[col-aligns] and
@scheme[row-aligns] procedures are used to superimpose all of the
cells in a column or row; this superimposition determines the total
width or height of the column or row, and also determines the
horizontal or vertical placement of each cell in the column or row.}

@; ------------------------------------------------------------------------

@section{Pict Drawing Adjusters}

@defproc*[([(scale [pict pict?] [factor real?]) pict?]
           [(scale [pict pict?] [w-factor real?] [h-factor real?]) pict?])]{

Scales a pict drawing, as well as its @tech{bounding-box}. The drawing
is scaled by adjusting the destination @scheme[dc<%>]'s scale while
drawing the original @scheme[pict].}

@defproc[(ghost [pict pict?]) pict?]{

Creats a container picture that doesn't draw the child picture,
but uses the child's size.}

@defproc[(linewidth [w real?] [pict pict?]) pict?]{

Selects a specific pen width for drawing, which applies to pen drawing
for @scheme[pict] that does not already use a specific pen width.}

@defproc[(colorize [pict pict?] [color (or/c string? 
                                             (is-a?/c color%)
                                             (list (integer-in 0 255)
                                                   (integer-in 0 255)
                                                   (integer-in 0 255)))])
         pict?]{

Selects a specific color drawing, which applies to drawing in
@scheme[pict] that does not already use a specific color. The
@scheme[black-and-white] parameter causes all non-white colors to be
converted to black.}

@defproc[(cellophane [pict pict?] [opacity (real-in 0 1)])
         pict?]{

Makes the given @scheme[pict] semi-transparent, where an opacity of
@scheme[0] is fully transparent, and an opacity of @scheme[1] is fully
opaque.  See @method[dc<%> set-alpha] for information about the
contexts and cases when semi-transparent drawing works.}

@defproc[(clip [pict pict?]) pict]{

Clips a pict's drawing to its @tech{bounding box}.}

@defproc*[([(inset/clip [pict pict?] [amt real?]) pict?]
           [(inset/clip [pict pict?] [h-amt real?] [v-amt real?]) pict?]
           [(inset/clip [pict pict?] [l-amt real?] [t-amt real?] 
                        [r-amt real?] [b-amt real?]) pict?])]{

Insets and clips the pict's drawing to its @tech{bounding
box}. Usually, the inset amounts are negative.}

@defform*[[(scale/improve-new-text pict-expr scale-expr)
           (scale/improve-new-text pict-expr x-scale-expr y-scale-expr)]]{

Like the @scheme[scale] procedure, but also sets
@scheme[current-expected-text-scale] while evaluating @scheme[pict-expr].}

@defboolparam[black-and-white on?]{

A parameter that determines whether @scheme[colorize] uses color or
black-and-white colors.}

@; ------------------------------------------------------------------------

@section{Bounding-Box Adjusters}

@defproc*[([(inset [pict pict?] [amt real?]) pict?]
           [(inset [pict pict?] [h-amt real?] [v-amt real?]) pict?]
           [(inset [pict pict?] [l-amt real?] [t-amt real?] 
                   [r-amt real?] [b-amt real?]) pict?])]{

Extends @scheme[pict]'s @tech{bounding box} by adding the given amounts
to the corresponding sides; ascent and descent are extended, too.}


@defproc[(clip-descent [pict pict?]) pict?]{

Truncates @scheme[pict]'s @tech{bounding box} by removing the descent part.}


@defproc[(lift [pict pict?] [amt real?]) pict?]{

Lifts @scheme[pict] relative to its baseline, extending the
@tech{bounding-box} height if necessary.}

@defproc[(drop [pict pict?] [amt real?]) pict?]{

Drops @scheme[pict] relative to its ascent line, extending the
@tech{bounding-box} height if necessary.}

@defproc[(baseless [pict pict?]) pict?]{

Makes the descent @scheme[0] and the ascent the same as the height.}

@defproc[(refocus [pict pict?] [sub-pict pict?]) pict?]{

Assuming that @scheme[sub-pict] can be found within @scheme[pict],
shifts the overall bounding box to that of @scheme[sub-pict] (but
preserving all the drawing of @scheme[pict]).}

@defproc[(panorama [pict pict?]) pict?]{

Shifts the given pict's bounding box to enclose the bounding boxes of
all sub-picts (even @scheme[launder]ed picts).}


@; ------------------------------------------------------------------------

@section{Pict Finders}

@defproc*[([(lt-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(ltl-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(lc-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(lbl-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(lb-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(ct-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(ctl-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(cc-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(cbl-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(cb-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(rt-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(rtl-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(rc-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(rbl-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(rb-find [pict pict?] [find pict-path?]) (values real? real?)])]{

Locates a pict designated by @scheme[find] is within @scheme[pict]. If
@scheme[find] is a pict, then the @scheme[pict] must have been created
as some combination involving @scheme[find].

If @scheme[find] is a list, then the first element of @scheme[find]
must be within @scheme[pict], the second element of @scheme[find] must
be within the second element, and so on.}

@defproc[(pict-path? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @scheme[pict] or a non-empty
list of @scheme[pict]s.}

@defproc[(launder [pict pict?]) pict?]{

Creates a pict that has the same drawing and bounding box of
@scheme[pict], but which hides all of its sub-picts so that they
cannot be found with functions like @scheme[lt-find].}

@; ------------------------------------------------------------------------

@section{Dingbats}

@defproc[(cloud [w real?]
                [h real?] 
                [color (or/c string? (is-a?/c color%)) "gray"])
         pict?]{

Creates a fluffy cloud.}

@defproc[(file-icon [w real?]
                    [h real?] 
                    [color (or/c string? (is-a?/c color%) any/c)]
                    [shaded? any/c #f])]{

Creates a Mac-like file icon, optionally shaded. If @scheme[color] is
not a string or @scheme[color%] object, it is treated as a boolean, in
which case true means @scheme["gray"] and false means
@scheme["white"].}

@defproc[(standard-fish [w real?]
                        [h real?] 
                        [direction (one-of/c 'left 'right)] 
                        [color (or/c string? (is-a?/c color%)) "blue"] 
                        [eye-color  (or/c string? (is-a?/c color%)) "black"]
                        [open-mouth? any/c #t])
         pict?]{

Creates a fish, swimming either @scheme['left] or @scheme['right].}


@defproc[(jack-o-lantern [size real?]
                         [pumpkin-color (or/c string? (is-a?/c color%)) "orange"] 
                         [face-color (or/c string? (is-a?/c color%)) "black"])
         pict?]{

Creates a jack-o-lantern; use the same pumpkin and face color to get a
plain pumpkin. The @scheme[size] determines the width.}

@defproc[(angel-wing [w real?]
                     [h real?] 
                     [left? any/c])
         pict?]{

Creates an angel wing, left or right, or any size.  The color and pen
width for drawing the wing outline is the current one.}

@; ------------------------------------------------------------------------

@section{Miscellaneous}

@defproc[(hyperlinkize [pict pict?])
         pict?]{

Adds an underline and blue color. The @scheme[pict]'s height and
descent are extended.}


@defproc[(scale-color [factor real?]
                      [color (or/c string (is-a?/c color%))])
         (is-a?/c color%)]{

Scales a color, making it brighter or darker. If the factor is less
than 1, the color is darkened by multiplying the RGB components by the
factor. If the factor is greater tham 1, the color is lightened by
dividing the gap between the RGB components and 255 by the factor.}

@defproc[(color-series [dc (is-a?/c dc<%>)]
                       [max-step nonnegative-exact-integer?]
                       [step-delta (and/c exact? positive?)]
                       [start (or/c string? (is-a?/c color%))]
                       [end (or/c string? (is-a?/c color%))]
                       [proc (exact? . -> . any)]
                       [set-pen? any/c]
                       [set-brush? any/c])
          void?]{

Calls a @scheme[proc] multiple times, gradually changing the pen
and/or brush color for each call. For the first call, the current pen
and/or brush color matches @scheme[start]; for the last call, it
matches scheme[end]; and for intermediate calls, the color is an
intermediate color.

The @scheme[max-step] and @scheme[step-delta] arguments should be
exact numbers; the procedure is called with each number from 0 to
@scheme[max-step] inclusive using a @scheme[step-delta] increment.}


@; ------------------------------------------------------------------------

@section{Rendering}

@defparam[dc-for-text-size dc (or/c false/c (is-a?/c dc<%>))]{

A parameter that is used to determine the @tech{bounding box} of picts
created with @scheme[text].

The drawing context installed in this parameter need not be the same
as the ultimate drawing context, but it must measure text in the same
way. In particular, use a @scheme[post-script-dc%] for preparing
PostScript output, while a @scheme[bitmap-dc%] instance will work fine
for either @scheme[bitmap-dc%] or @scheme[canvas%] output.}


@defproc[(draw-pict [pict pict?]
                    [dc (is-a?/c dc<%>)]
                    [x real?]
                    [y real?])
         void?]{

Draws @scheme[pict] to @scheme[dc], with its top-left corner at offset
 (@scheme[x], @scheme[y]).}


@defproc[(make-pict-drawer [pict pict?])
         ((is-a?/c dc<%>) real? real? . -> . void?)]{

Generates a pict-drawer procedure for multiple renderings of
@scheme[pict]. Using the generated procedure can be faster than
repeated calls to @scheme[draw-pict].}


@defproc[(show-pict [pict pict?]
                    [w (or/c false/c nonnegative-exact-integer?) #f] 
                    [h (or/c false/c nonnegative-exact-integer?) #f])
         void?]{

Opens a frame that displays @scheme[pict].  The frame adds one method,
@scheme[set-pict], which takes a pict to display. The optional
@scheme[w] and @scheme[h] arguments specify a minimum size for the
frame's drawing area.}

@defparam[current-expected-text-scale scales (list real? real?)]{

A parameter used to refine text measurements to better match an
expected scaling of the image. The @scheme[scale/improve-new-text]
form sets this parameter while also scaling the resulting pict.}
