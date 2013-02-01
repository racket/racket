#lang scribble/manual
@(require (except-in "utils.rkt" author) (for-label scribble/jfp))

@(define-syntax-rule (def base-author)
   (begin
     (require (for-label scribble/base))
     (define base-author @racket[author])))
@(def base-author)

@title{JFP Paper Format}

@defmodulelang[scribble/jfp]{The @racketmodname[scribble/jfp]
language is like @racketmodname[scribble/base], but configured with
Latex style defaults to use the @filepath{jfp1.cls} class
file. The class file is not included with Scribble due to license
issues, but if the file is not manually installed into the
@racket[scribble/jfp] collections, then it is downloaded on demand to
@racket[(find-system-path 'addon-dir)].}

Latex output with @racketmodname[scribble/jfp] uses a main-document
version supplied to @racket[title] as the short-form document name (to
be used in page headers).

@defproc[(abstract [pre-content pre-content?] ...) block?]{

Generates a @tech{nested flow} for a paper abstract.}

@defform[(include-abstract module-path)]{

Similar to @racket[include-section], but incorporates the document in the
specified module as an abstract. The document must have no title or
sub-parts.}

@defproc[(author [name pre-content?] ...)
         block?]{

A replacement for @base-author from @racketmodname[scribble/base].}

@defproc[((author/short [short-name pre-content?] ...) [long-name pre-content?] ...)
         block?]{

Like @racket[author], but allows the short-form names (to be used in
page headers) to be specified separately from the long-form name.}

@deftogether[(
@defproc[(affiliation [place pre-content?] ...) element?]
@defproc[(affiliation-mark [mark pre-content?] ...) element?]
@defproc[(affiliation-sep) element?]
)]{

Use @racket[affiliation] within @racket[author] or the long-name part
of @racket[author/short] to specify affiliations after all authors.
If different authors have different affiliations, use
@racket[affiliation-mark] with a number after each author, and then
use @racket[affiliation-mark] before each different affiliation within
a single @racket[affiliation], using @racket[(affiliation-sep)] to
separate affiliations.}
