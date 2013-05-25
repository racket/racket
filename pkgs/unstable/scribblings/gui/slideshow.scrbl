#lang scribble/manual
@(require "../utils.rkt"
          (for-label slideshow
                     unstable/contract
                     unstable/gui/slideshow))

@title{Slideshow Presentations}
@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

@defmodule[unstable/gui/slideshow]

This module also exports everything provided by
@racketmodname[unstable/gui/pict].

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

@section{Revealing Slides}

@addition{Jon Rafkind}

@defform[(reveal number expr ...)]{

Expands to either @racket[(show expr ...)] or @racket[(hide expr ...)] if
@racket[number] is greater than or equal to the current revealed slide within a
@racket[revealing-slide].

}

@defform[(revealing-slide expr ...)]{

Creates N slides where N is the maximum number given to a @racket[reveal]
expression as the first argument. Each slide has the current reveal number
incremented by one so progressive slides can reveal picts in that appear in
arbitrary places.

@racketblock[
(revealing-slide
  (hc-append (reveal 0 @t{I show up first})
             (reveal 1 @t{I show up second}))
  (reveal 1 @t{I also show up second}))
]

}

@defform[(items-slide (name ...) expr ...)]{

Creates N slides where N is the maximum number given to a @racket[reveal]
similar to @racket[revealing-slide]. Each slide will show the names on the left
hand side using right justification and only one reveal will be displayed on the
right. The order of the names will be matched with the current reveal number so
the first item will be displayed while the first reveal is displayed. Reveals
that are not active will have their corresponding items displayed but in a light
font so as to indicate inactivity.

@racketblock[
(items-slide ("item1" "item2" "item3")
  (reveal 0
    @t{I will show up for item1})
  (reveal 1
    @t{I will show up for item2})
  (reveal 2
    @t{I will show up for item3}))
]

}

@section{Miscellaneous Slide Utilities}

@addition{Scott Owens}

@defproc[(blank-line) pict?]{
Adds a blank line of the current font size's height.
}
