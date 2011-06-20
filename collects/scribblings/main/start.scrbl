#lang scribble/doc
@(require scribble/manual "private/utils.rkt" "private/manuals.rkt")

@main-page['start #t
                  ;; "racket.css" needs to be installed so it can be shared:
                  #:force-racket-css? #t]

@margin-note{
  @not-on-the-web{
    This is an installation-specific listing.  Running @exec{raco docs}
    (or @exec{Racket Documentation} on Windows or Mac OS X)
    may open a different page with local and user-specific
    documentation, including documentation for installed
    @link["http://planet.racket-lang.org/"]{@|PLaneT|} packages.}}

@(make-start-page #f)
