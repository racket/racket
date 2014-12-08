#lang scribble/doc
@(require scribble/manual
          (for-label racket
                     help/search
                     help/help-utils
                     net/sendurl
                     setup/dirs))

@title{Help and Documentation Utilities}

This section describes utilities designed to connect to documentation
and to support bug reports. See also @racketmodname[scribble/xref].


@section{Searching in the Documentation}

@defmodule[help/search]

@defproc[(send-main-page [#:sub sub path-string? "index.html"]
                         [#:notify notify (-> (or/c path? string?) void) void]
                         [#:fragment fragment (or/c #f string?) #f]
                         [#:query query (or/c #f string?) #f])
         any]{
  Visits the documentation file @racket[sub] in the user's browser. 
  
  When @racket[get-doc-open-url] returns @racket[#f], @racket[send-main-page]
  builds a URL that points into the main collection documentation
  or into the user-specific documentation, depending on the @racket[sub] argument.
  Once it finds the path, @racket[send-main-page] passes the path to
  @racket[notify]. The @racket[fragment] and @racket[query] arguments are passed
  to @racket[send-url/file], along with the URL.

  When @racket[get-doc-open-url] returns a URL string,
  @racket[send-main-page] appends @racket[sub] to the URL and passes
  it to @racket[notify]. It then appends @racket[fragment] and
  @racket[query] to the URL and passes it on to @racket[send-url].

  @history[#:changed "6.0.1.6" @elem{Added @racket[get-doc-open-url] support.}]
}

@defproc[(perform-search [str string?]
                         [context (or/c #f
                                        string?
                                        (list/c string? string?))
                                  #f])
         void?]{
  Searches for @racket[str] in the documentation. The @racket[context] argument
  supplies a context for the search or, if it is two strings, a context for
  the search and a label for that context.
}

@; ------------------------------------------------------------

@section{Connecting to @exec{racket}}

@defmodule[help/help-utils]{The @racketmodname[help/help-utils]
library is dynamically loaded by the @racket[help] form that is
available by default in @exec{racket}.}

@defproc[(search-for [strs (listof string?)]) void?]{
  Calls @racket[perform-search] after concatenating the
  elements of @racket[strs] and adding spaces between them.
}
@defproc[(find-help/lib [id symbol?] [lib module-path?]) void?]{
  Visits the documentation page for @racket[id] as an export of @racket[lib].
}

@defproc[(find-help [id identifier?]) void?]{
  Visits the documentation for @racket[id].
}

@defproc[(go-to-main-page) void?]{
  Visits the main entry page for the documentation.
}
