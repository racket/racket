#lang scribble/manual
@(require (for-label scribble/core
                     racket/base
                     scriblib/render-cond))

@(define scribble-doc '(lib "scribblings/scribble/scribble.scrbl"))

@title[#:tag "render-cond"]{Conditional Content}

@defmodule[scriblib/render-cond]

As much as possible, Scribble documents should be independent of the
target format for rendering the document. To customize generated
output, use styes plus ``back end'' configurations for each target
format (see @secref[#:doc scribble-doc "config"] in
@other-manual[scribble-doc]).

As a last resort, the @racket[cond-element] and @racket[cond-block]
forms support varying the document content depending on the target
format. More precisely, they generate parts of a document where
content is delayed until the @tech[#:doc scribble-doc]{traverse pass}
of document rendering. Format detection relies on the
@racket['scribble:current-render-mode] registration that is accessible
through a @racket[traverse-element] or @racket[traverse-block].

The syntax of @racket[cond-element] and @racket[cond-block] is based
on SRFI-0.

@defform*/subs[#:literals (and or not else)
               [(cond-element [feature-requirement body ...+])
                (cond-element [feature-requirement body ...+] [else body ...+])]
               ([feature-requirement identifier
                                     (not feature-requirement)
                                     (and feature-requirement ...)
                                     (or feature-requirement ...)])]{

Generates a @racket[traverse-element] whose replacement content is
produced by the @racket[body] of one of the first matching
@racket[cond-element] clause.

A @racket[feature-requirement] can be any identifier; a useful
identifier is one whose symbol form can appear in a
@racket['scribble:current-render-mode] list. The identifier matches
when its symbol form is in the @racket['scribble:current-render-mode]
list.  Typically, the identifier is @racket[html], @racket[latex], or
@racket[text] to indicate the corresponding rendering target.

A @racket[(not feature-requirement)] test matches when
@racket[feature-requirement] does not match, and so on. An
@racket[else] clause always matches. If no @racket[else] clause is
present and no clause matches, then the @racket[exn:fail:contract]
exception is raised. Similarly, if the result of the selected
@racket[body] is not content according to @racket[content?], then the
@racket[exn:fail:contract] exception is raised.}

@defform*[[(cond-block [feature-requirement body ...+])
           (cond-block [feature-requirement body ...+] [else body ...+])]]{

Like @racket[cond-element], but generates a @racket[traverse-block]
where the selected @racket[body] must produce a block according to
@racket[block?].}


