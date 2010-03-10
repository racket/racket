#lang scribble/manual
@(require (except-in "utils.ss" author)
          (for-label scribble/jfp))

@(define-syntax-rule (def base-author)
   (begin
    (require (for-label scribble/base))
    (define base-author @scheme[author])))
@(def base-author)

@title{JFP Paper Format}

@defmodulelang[scribble/jfp]{The @schememodname[scribble/jfp]
language is like @schememodname[scribble/manual], but configured with
Latex style defaults to use the @filepath{jfp.cls} class
file that is included with Scribble.}

Latex output with @schememodname[scribble/jfp] uses a main-document
version supplied to @scheme[title] as the short-form document name (to
be used in page headers).

@defproc[(abstract [pre-content pre-content?] ...) block?]{

Generates a @tech{nested flow} for a paper abstract.}

@defform[(include-abstract module-path)]{

Similar to @scheme[include-section], but incorporates the document in the
specified module as an abstract. The document must have no title or
sub-parts.}

@defproc[(author [name pre-content?] ...)
         block?]{

A replacement for @base-author from @schememodname[scribble/base].}

@defproc[((author/short [short-name pre-content?] ...) [long-name pre-content?] ...)
         block?]{

Like @scheme[author], but allows the short-form names (to be used in
page headers) to be specified separately from the long-form name.}

@deftogether[(
@defproc[(affiliation [place pre-content?] ...) element?]
@defproc[(affiliation-mark [mark pre-content?] ...) element?]
@defproc[(affiliation-sep) element?]
)]{

Use @scheme[affiliation] within @scheme[author] or the long-name part
of @scheme[author/short] to specify affiliations after all authors.
If different authors have different affiliations, use
@scheme[affiliation-mark] with a number after each author, and then
use @scheme[affiliation-mark] before each different affiliation within
a single @scheme[affiliation], using @scheme[(affiliation-sep)] to
separate affiliations.}
