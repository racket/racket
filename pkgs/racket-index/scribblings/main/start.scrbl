#lang scribble/doc
@(require scribble/manual
          scribble/core
          scribble/html-properties
          "private/utils.rkt"
          "private/manuals.rkt")

@main-page['start #t #:show-root-info? #t]

@(define path-info-style (style "RootPathInfo" (list (attributes '((id . "rootPathInfo"))))))
@(define go-style (style "RootPathAction" (list (attributes '((onclick . "return GoToRootPath();"))))))
@(define disable-style (style "RootPathAction" (list (attributes '((onclick . "return DisableRootPath();"))))))

  @margin-note{
    @not-on-the-web{This is an installation-specific listing.}
    Running @exec{raco docs}
    (or @exec{Racket Documentation} on Windows or Mac OS X)
    may open a different page with local and user-specific
    documentation, including documentation for installed packages.

    @elem[#:style path-info-style]{Searching or following a
     ``top'' link will use go to a different starting point that
     includes user-specific information.
     @hyperlink["#"]{@elem[#:style go-style]{[Go to user-specific start]}}
     @hyperlink["#"]{@elem[#:style disable-style]{[Forget user-specific start]}}}}

@(make-start-page #f)

