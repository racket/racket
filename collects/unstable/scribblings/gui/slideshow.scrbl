#lang scribble/manual
@(require "../utils.rkt"
          (for-label slideshow
                     unstable/contract
                     unstable/gui/slideshow))

@title{Slideshow Presentations}

@defmodule[unstable/gui/slideshow]

@section{Text Formatting}

@defform[(with-size size expr)]{

Sets @scheme[current-font-size] to @scheme[size] while running @scheme[expr].

}

@defform[(with-scale scale expr)]{

Multiplies @scheme[current-font-size] by @scheme[scale] while running
@scheme[expr].

}

@deftogether[(
@defform[(big text)]
@defform[(small text)]
)]{

Scale @scheme[current-font-size] by @scheme[3/2] or @scheme[2/3], respectively,
while running @scheme[text].

}

@defform[(with-font font expr)]{

Sets @scheme[current-main-font] to @scheme[font] while running @scheme[expr].

}

@defform[(with-style style expr)]{

Adds @scheme[style] to @scheme[current-main-font] (via @scheme[cons]) while
running @scheme[expr].

}

@deftogether[(
@defform[(bold text)]
@defform[(italic text)]
@defform[(subscript text)]
@defform[(superscript text)]
@defform[(caps text)]
)]{

Adds the attributes for bold, italic, superscript, subscript, or small caps
text, respectively, to @scheme[current-main-font] while running @scheme[text].

}

@section{Pict Colors}

@defproc[(color [c color/c] [p pict?]) pict?]{

Applies color @scheme[c] to picture @scheme[p].  Equivalent to @scheme[(colorize
p c)].

}

@deftogether[(
@defproc[(red [pict pict?]) pict?]
@defproc[(orange [pict pict?]) pict?]
@defproc[(yellow [pict pict?]) pict?]
@defproc[(green [pict pict?]) pict?]
@defproc[(blue [pict pict?]) pict?]
@defproc[(purple [pict pict?]) pict?]
@defproc[(black [pict pict?]) pict?]
@defproc[(brown [pict pict?]) pict?]
@defproc[(gray [pict pict?]) pict?]
@defproc[(white [pict pict?]) pict?]
@defproc[(cyan [pict pict?]) pict?]
@defproc[(magenta [pict pict?]) pict?]
)]{

These functions apply appropriate colors to picture @scheme[p].

}

@deftogether[(
@defproc[(light [color color/c]) color/c]
@defproc[(dark [color color/c]) color/c]
)]{

These functions produce ligher or darker versions of a color.

}

@defthing[color/c flat-contract?]{

This contract recognizes color strings, @scheme[color%] instances, and RGB color
lists.

}

@section{Pict Manipulation}

@defproc[(fill [pict pict?] [width (or/c real? #f)] [height (or/c real? #f)])
         pict?]{

Extends @scheme[pict]'s bounding box to a minimum @scheme[width] and/or
@scheme[height], placing the original picture in the middle of the space.

}

@subsection{Conditional Manipulations}

These pict transformers all take boolean arguments that determine whether to
transform the pict or leave it unchanged.  These transformations can be useful
for staged slides, as the resulting pict always has the same size and shape, and
its contents always appear at the same position, but changing the boolean
argument between slides can control when the transformation occurs.

@deftogether[(
@defproc[(show [pict pict?] [show? truth/c #t]) pict?]
@defproc[(hide [pict pict?] [hide? truth/c #t]) pict?]
)]{

These functions conditionally show or hide an image, essentially choosing
between @scheme[pict] and @scheme[(ghost pict)].  The only difference between
the two is the default behavior and the opposite meaning of the @scheme[show?]
and @scheme[hide?] booleans.  Both functions are provided for mnemonic purposes.

}

@defproc[(strike [pict pict?] [strike? truth/c #t]) pict?]{

Displays a strikethrough image by putting a line through the middle of
@scheme[pict] if @scheme[strike?] is true; produces @scheme[pict] unchanged
otherwise.

}

@defproc[(shade [pict pict?]
                [shade? truth/c #t]
                [#:ratio ratio (real-in 0 1) 1/2])
         pict?]{

Shades @scheme[pict] to show with @scheme[ratio] of its normal opacity; if
@scheme[ratio] is @scheme[1] or @scheme[shade?] is @scheme[#f], shows
@scheme[pict] unchanged.

}

@subsection{Conditional Combinations}

These pict control flow operators decide which pict of several to use.  All
branches are evaluated; the resulting pict is a combination of the pict chosen
by normal conditional flow with @scheme[ghost] applied to all the other picts.
The result is a picture large enough to accomodate each alternative, but showing
only the chosen one.  This is useful for staged slides, as the pict chosen may
change with each slide but its size and position will not.

@defform/subs[(pict-if maybe-combine test-expr then-expr else-expr)
              ([maybe-combine code:blank (code:line #:combine combine-expr)])]{

Chooses either @scheme[then-expr] or @scheme[else-expr] based on
@scheme[test-expr], similarly to @scheme[if].  Combines the chosen, visible
image with the other, invisible image using @scheme[combine-expr], defaulting to
@scheme[pict-combine].

}

@defform/subs[(pict-cond maybe-combine [test-expr pict-expr] ...)
              ([maybe-combine code:blank (code:line #:combine combine-expr)])]{

Chooses a @scheme[pict-expr] based on the first successful @scheme[test-expr],
similarly to @scheme[cond].  Combines the chosen, visible image with the other,
invisible images using @scheme[combine-expr], defaulting to
@scheme[pict-combine].

}

@defform/subs[(pict-case test-expr maybe-combine [literals pict-expr] ...)
              ([maybe-combine code:blank (code:line #:combine combine-expr)])]{

Chooses a @scheme[pict-expr] based on @scheme[test-expr] and each list of
@scheme[literals], similarly to @scheme[case].  Combines the chosen, visible
image with the other, invisible images using @scheme[combine-expr], defaulting
to @scheme[pict-combine].

}

@defform/subs[(pict-match test-expr maybe-combine [pattern pict-expr] ...)
              ([maybe-combine code:blank (code:line #:combine combine-expr)])]{

Chooses a @scheme[pict-expr] based on @scheme[test-expr] and each
@scheme[pattern], similarly to @scheme[match].  Combines the chosen, visible
image with the other, invisible images using @scheme[combine-expr], defaulting
to @scheme[pict-combine].

}

@defform[#:id pict-combine pict-combine]{

This syntax parameter determines the default pict combining form used by the
above macros.  It defaults to @scheme[lbl-superimpose].

}

@defform[(with-pict-combine combine-id body ...)]{

Sets @scheme[pict-combine] to refer to @scheme[combine-id] within each of the
@scheme[body] terms, which are spliced into the containing context.

}

@section{Staged Slides}

@defform[(staged [name ...] body ...)]{

Executes the @scheme[body] terms once for each stage @scheme[name].  The terms
may include expressions and mutually recursive definitions.  Within the body,
each @scheme[name] is bound to a number from @scheme[1] to the number of stages
in order.  Furthermore, during execution @scheme[stage] is bound to the number
of the current stage and @scheme[stage-name] is bound to a symbol representing
the @scheme[name] of the current stage.  By comparing @scheme[stage] to the
numeric value of each @scheme[name], or @scheme[stage-name] to quoted symbols of
the form @scheme['name], the user may compute based on the progression of the
stages.

}

@deftogether[(
@defform[#:id stage stage]
@defform[#:id stage-name stage-name]
)]{

These keywords are bound during the execution of @scheme[staged] and should not
be used otherwise.

}

@defform[(slide/staged [name ...] arg ...)]{

Creates a staged slide.  Equivalent to @scheme[(staged [name ...] (slide arg
...))].

Within a staged slide, the boolean arguments to @scheme[hide], @scheme[show],
@scheme[strike], and @scheme[shade] can be used to determine in which stages to
perform a transformation.  The macros @scheme[pict-if], @scheme[pict-cond],
@scheme[pict-case], and @scheme[pict-match] may also be used to create images
which change naturally between stages.

}

@section{Tables}

@defproc[(tabular [row (listof (or/c string? pict?))] ...
                  [#:gap gap natural-number/c gap-size]
                  [#:hgap hgap natural-number/c gap]
                  [#:vgap vgap natural-number/c gap]
                  [#:align align (->* [] [] #:rest (listof pict?) pict?) lbl-superimpose]
                  [#:halign halign (->* [] [] #:rest (listof pict?) pict?) align]
                  [#:valign valign (->* [] [] #:rest (listof pict?) pict?) align])
         pict?]{

Constructs a table containing the given @scheme[row]s, all of which must be of
the same length.  Applies @scheme[t] to each string in a @scheme[row] to
construct a pict.  The @scheme[hgap], @scheme[vgap], @scheme[halign], and
@scheme[valign] are used to determine the horizontal and vertical gaps and
alignments as in @scheme[table] (except that every row and column is uniform).

}

@section{Multiple Columns}

@defform[(two-columns one two)]{

Constructs a two-column pict using @scheme[one] and @scheme[two] as the two
columns.  Sets @scheme[current-para-width] appropriately in each column.

}

@defproc[(mini-slide [pict pict?] ...) pict?]{

Appends each @scheme[pict] vertically with space between them, similarly to the
@scheme[slide] function.

}

@defproc[(columns [pict pict?] ...) pict?]{

Combines each @scheme[pict] horizontally, aligned at the top, with space in
between.

}

@defform[(column width body ...)]{

Sets @scheme[current-para-width] to @scheme[width] during execution of the
@scheme[body] expressions.

}

@defproc[(column-size [n exact-positive-integer?]
                      [r real? (/ n )])
         real?]{

Computes the width of one column out of @scheme[n] that takes up a ratio of
@scheme[r] of the available space (according to @scheme[current-para-width]).

}

@addition{Vincent St-Amour}

@deftogether[(
@defproc[(ellipse/border [w real?] [h real?]
                         [#:color color color/c]
			 [#:border-color border-color color/c]
			 [#:border-width border-width real?])
         pict?]
@defproc[(circle/border [diameter real?]
                         [#:color color color/c]
			 [#:border-color border-color color/c]
			 [#:border-width border-width real?])
         pict?]
@defproc[(rectangle/border [w real?] [h real?]
                         [#:color color color/c]
			 [#:border-color border-color color/c]
			 [#:border-width border-width real?])
         pict?]
@defproc[(rounded-rectangle/border [w real?] [h real?]
                         [#:color color color/c]
			 [#:border-color border-color color/c]
			 [#:border-width border-width real?])
         pict?]
)]{
These functions create shapes with border of the given color and width.
}
