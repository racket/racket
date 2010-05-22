#lang scribble/manual
@(require (for-label scribble/struct
                     scriblib/footnote
                     scheme/base
                     scheme/contract))

@title[#:tag "footnotes"]{Footnotes}

@defmodule[scriblib/footnote]

@defproc[(note [pre-content pre-content?] ...) element?]{

Creates a margin note for HTML and a footnote for Latex/PDF output.}

@defform[(define-footnote footnote-id footnote-part-id)]{

Binds @racket[footnote-id] to a form like @racket[note] that generates
a footnote in HTML output as well as Latex/PDF output. To trigger the
HTML output of the footnotes that are registered through
@racket[footnote-id], the function bound to @racket[footnote-part-id]
must be called at a position that corresponds the bottom of the HTML
page. (The generated section will not show a title or appear in a
table of contents; it will look like a footnote area.)

Beware that any content passed to @racket[footnote-id] will occur
twice in at least an intermediate form of the document, and perhaps
also in the rendered form of the document. Consequently, the content
passed to @racket[footnote-id] should not bind link targets or include
other one-time declarations.}




