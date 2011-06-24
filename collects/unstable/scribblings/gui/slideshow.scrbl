#lang scribble/manual
@(require "../utils.rkt"
          (for-label slideshow
                     unstable/contract
                     unstable/gui/slideshow))

@title{Slideshow Presentations}

@defmodule[unstable/gui/slideshow]

@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@section{Text Formatting}

@defform[(with-size size expr)]{

Sets @racket[current-font-size] to @racket[size] while running @racket[expr].

}

@defform[(with-scale scale expr)]{

Multiplies @racket[current-font-size] by @racket[scale] while running
@racket[expr].

}

@deftogether[(
@defform[(big text)]
@defform[(small text)]
)]{

Scale @racket[current-font-size] by @racket[3/2] or @racket[2/3], respectively,
while running @racket[text].

}

@defform[(with-font font expr)]{

Sets @racket[current-main-font] to @racket[font] while running @racket[expr].

}

@defform[(with-style style expr)]{

Adds @racket[style] to @racket[current-main-font] (via @racket[cons]) while
running @racket[expr].

}

@deftogether[(
@defform[(bold text)]
@defform[(italic text)]
@defform[(subscript text)]
@defform[(superscript text)]
@defform[(caps text)]
)]{

Adds the attributes for bold, italic, superscript, subscript, or small caps
text, respectively, to @racket[current-main-font] while running @racket[text].

}

@section{Pict Colors}

@defproc[(color [c color/c] [p pict?]) pict?]{

Applies color @racket[c] to picture @racket[p].  Equivalent to @racket[(colorize
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

These functions apply appropriate colors to picture @racket[p].

}

@deftogether[(
@defproc[(light [color color/c]) color/c]
@defproc[(dark [color color/c]) color/c]
)]{

These functions produce ligher or darker versions of a color.

}

@defthing[color/c flat-contract?]{

This contract recognizes color strings, @racket[color%] instances, and RGB color
lists.

}

@section{Pict Manipulation}

@defproc[(fill [pict pict?] [width (or/c real? #f)] [height (or/c real? #f)])
         pict?]{

Extends @racket[pict]'s bounding box to a minimum @racket[width] and/or
@racket[height], placing the original picture in the middle of the space.

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
between @racket[pict] and @racket[(ghost pict)].  The only difference between
the two is the default behavior and the opposite meaning of the @racket[show?]
and @racket[hide?] booleans.  Both functions are provided for mnemonic purposes.

}

@defproc[(strike [pict pict?] [strike? truth/c #t]) pict?]{

Displays a strikethrough image by putting a line through the middle of
@racket[pict] if @racket[strike?] is true; produces @racket[pict] unchanged
otherwise.

}

@defproc[(shade [pict pict?]
                [shade? truth/c #t]
                [#:ratio ratio (real-in 0 1) 1/2])
         pict?]{

Shades @racket[pict] to show with @racket[ratio] of its normal opacity; if
@racket[ratio] is @racket[1] or @racket[shade?] is @racket[#f], shows
@racket[pict] unchanged.

}

@subsection{Conditional Combinations}

These pict control flow operators decide which pict of several to use.  All
branches are evaluated; the resulting pict is a combination of the pict chosen
by normal conditional flow with @racket[ghost] applied to all the other picts.
The result is a picture large enough to accommodate each alternative, but showing
only the chosen one.  This is useful for staged slides, as the pict chosen may
change with each slide but its size and position will not.

@defform/subs[(pict-if maybe-combine test-expr then-expr else-expr)
              ([maybe-combine code:blank (code:line #:combine combine-expr)])]{

Chooses either @racket[then-expr] or @racket[else-expr] based on
@racket[test-expr], similarly to @racket[if].  Combines the chosen, visible
image with the other, invisible image using @racket[combine-expr], defaulting to
@racket[pict-combine].

}

@defform/subs[(pict-cond maybe-combine [test-expr pict-expr] ...)
              ([maybe-combine code:blank (code:line #:combine combine-expr)])]{

Chooses a @racket[pict-expr] based on the first successful @racket[test-expr],
similarly to @racket[cond].  Combines the chosen, visible image with the other,
invisible images using @racket[combine-expr], defaulting to
@racket[pict-combine].

}

@defform/subs[(pict-case test-expr maybe-combine [literals pict-expr] ...)
              ([maybe-combine code:blank (code:line #:combine combine-expr)])]{

Chooses a @racket[pict-expr] based on @racket[test-expr] and each list of
@racket[literals], similarly to @racket[case].  Combines the chosen, visible
image with the other, invisible images using @racket[combine-expr], defaulting
to @racket[pict-combine].

}

@defform/subs[(pict-match test-expr maybe-combine [pattern pict-expr] ...)
              ([maybe-combine code:blank (code:line #:combine combine-expr)])]{

Chooses a @racket[pict-expr] based on @racket[test-expr] and each
@racket[pattern], similarly to @racket[match].  Combines the chosen, visible
image with the other, invisible images using @racket[combine-expr], defaulting
to @racket[pict-combine].

}

@defform[#:id pict-combine pict-combine]{

This syntax parameter determines the default pict combining form used by the
above macros.  It defaults to @racket[lbl-superimpose].

}

@defform[(with-pict-combine combine-id body ...)]{

Sets @racket[pict-combine] to refer to @racket[combine-id] within each of the
@racket[body] terms, which are spliced into the containing context.

}

@section{Staged Slides}

@defform[(staged [name ...] body ...)]{

Executes the @racket[body] terms once for each stage @racket[name].  The terms
may include expressions and mutually recursive definitions.  Within the body,
each @racket[name] is bound to a number from @racket[1] to the number of stages
in order.  Furthermore, during execution @racket[stage] is bound to the number
of the current stage and @racket[stage-name] is bound to a symbol representing
the @racket[name] of the current stage.  By comparing @racket[stage] to the
numeric value of each @racket[name], or @racket[stage-name] to quoted symbols of
the form @racket['name], the user may compute based on the progression of the
stages.

}

@deftogether[(
@defform[#:id stage stage]
@defform[#:id stage-name stage-name]
)]{

These keywords are bound during the execution of @racket[staged] and should not
be used otherwise.

}

@defform[(slide/staged [name ...] arg ...)]{

Creates a staged slide.  Equivalent to @racket[(staged [name ...] (slide arg
...))].

Within a staged slide, the boolean arguments to @racket[hide], @racket[show],
@racket[strike], and @racket[shade] can be used to determine in which stages to
perform a transformation.  The macros @racket[pict-if], @racket[pict-cond],
@racket[pict-case], and @racket[pict-match] may also be used to create images
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

Constructs a table containing the given @racket[row]s, all of which must be of
the same length.  Applies @racket[t] to each string in a @racket[row] to
construct a pict.  The @racket[hgap], @racket[vgap], @racket[halign], and
@racket[valign] are used to determine the horizontal and vertical gaps and
alignments as in @racket[table] (except that every row and column is uniform).

}

@section{Multiple Columns}

@defform[(two-columns one two)]{

Constructs a two-column pict using @racket[one] and @racket[two] as the two
columns.  Sets @racket[current-para-width] appropriately in each column.

}

@defproc[(mini-slide [pict pict?] ...) pict?]{

Appends each @racket[pict] vertically with space between them, similarly to the
@racket[slide] function.

}

@defproc[(columns [pict pict?] ...) pict?]{

Combines each @racket[pict] horizontally, aligned at the top, with space in
between.

}

@defform[(column width body ...)]{

Sets @racket[current-para-width] to @racket[width] during execution of the
@racket[body] expressions.

}

@defproc[(column-size [n exact-positive-integer?]
                      [r real? (/ n )])
         real?]{

Computes the width of one column out of @racket[n] that takes up a ratio of
@racket[r] of the available space (according to @racket[current-para-width]).

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

@addition{Scott Owens}

@defproc[(blank-line) pict?]{
Adds a blank line of the current font size's height.
}

@deftogether[(
@defproc[(pin-label-line [label pict?] [pict pict?]
                         [src-pict pict-path?]
			 [src-coord-fn (-> pict-path? (values real? real?))]
                         [dest-pict pict-path?]
			 [dest-coord-fn (-> pict-path? (values real? real?))]
			 [#:start-angle start-angle (or/c real? #f)]
			 [#:end-angle end-angle (or/c real? #f)]
			 [#:start-pull start-pull real?]
			 [#:end-pull end-pull real?]
			 [#:line-width line-width (or/c real? #f)]
			 [#:color color (or/c #f string? (is-a?/c color%))]
			 [#:under? under? any/c]
			 [#:x-adjust x-adjust real?]
			 [#:y-adjust y-adjust real?])
	 pict?]
@defproc[(pin-arrow-label-line [label pict?] [arrow-size real?] [pict pict?]
                         [src-pict pict-path?]
			 [src-coord-fn (-> pict-path? (values real? real?))]
                         [dest-pict pict-path?]
			 [dest-coord-fn (-> pict-path? (values real? real?))]
			 [#:start-angle start-angle (or/c real? #f)]
			 [#:end-angle end-angle (or/c real? #f)]
			 [#:start-pull start-pull real?]
			 [#:end-pull end-pull real?]
			 [#:line-width line-width (or/c real? #f)]
			 [#:color color (or/c #f string? (is-a?/c color%))]
			 [#:under? under? any/c]
			 [#:hide-arrowhead? hide-arrowhead? any/c]
			 [#:x-adjust x-adjust real?]
			 [#:y-adjust y-adjust real?])
	 pict?]
@defproc[(pin-arrows-label-line [label pict?] [arrow-size real?] [pict pict?]
                         [src-pict pict-path?]
			 [src-coord-fn (-> pict-path? (values real? real?))]
                         [dest-pict pict-path?]
			 [dest-coord-fn (-> pict-path? (values real? real?))]
			 [#:start-angle start-angle (or/c real? #f)]
			 [#:end-angle end-angle (or/c real? #f)]
			 [#:start-pull start-pull real?]
			 [#:end-pull end-pull real?]
			 [#:line-width line-width (or/c real? #f)]
			 [#:color color (or/c #f string? (is-a?/c color%))]
			 [#:under? under? any/c]
			 [#:hide-arrowhead? hide-arrowhead? any/c]
			 [#:x-adjust x-adjust real?]
			 [#:y-adjust y-adjust real?])
	 pict?]
)]{
These functions behave like @racket[pin-line], @racket[pin-arrow-line]
and @racket[pin-arrows-line] with the addition of a label attached to
the line.
}
