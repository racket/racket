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
                         [#:query query (or/c #f string?) #f]
                         [#:query-table query-table (hash/c symbol? string? #:immutable #t) (hash)])
         any]{

  Visits the documentation file indicated by @racket[sub]. If @racket[sub]
  is not an absolute path, it is relative to a documentation directory
  in @racket[(get-doc-search-dirs)].
  
  When @racket[get-doc-open-url] returns @racket[#f], @racket[send-main-page]
  builds a URL that points into the main collection documentation
  or into the user-specific documentation, depending on the @racket[sub] argument.
  Once it finds the path, @racket[send-main-page] passes the path to
  @racket[notify]. The @racket[fragment] and @racket[query] arguments are passed
  to @racket[send-url/file], along with the URL. The keys and values in
  @racket[query-table] are used as additional query parameters.

  When @racket[get-doc-open-url] returns a URL string,
  @racket[send-main-page] appends @racket[sub] to the URL and passes
  it to @racket[notify]. It then appends @racket[fragment] and
  @racket[query] to the URL and passes it on to @racket[send-url].

  @history[#:changed "1.2" @elem{Added @racket[get-doc-open-url] support.}
           #:changed "1.60" @elem{Added the @racket[query-table] argument.}]
}

@defproc[(send-language-family-page [language-family (or/c #f string?) #f])
         void?]{

  Calls @racket[send-main-page]. If @racket[language-family] is the
  name of a language family that is registered by a definition of
  @racketidfont{language-family} in some collection's
  @filepath{info.rkt}, then a starting point and search and navigation
  configuration for the language family are incoprorated into the
  call. Otherwise, @racket[send-main-page] is called with no
  arguments.

  @history[#:added "1.60"]
}

@defproc[(perform-search [str string?]
                         [context (or/c #f
                                        string?
                                        (list/c string? string?))
                                  #f]
                         [#:language-family language-family (or/c #f string?) #f])
         void?]{
  Searches for @racket[str] in the documentation. The @racket[context] argument
  supplies a context for the search or, if it is two strings, a context for
  the search and a label for that context. The search involves visiting a URL
  that performs the search.

  If @racket[language-family] is the name of a language family that is
  registered by a definition of @racketidfont{language-family} in some
  collection's @filepath{info.rkt}, then search and navigation
  configuration for the language family are incoprorated into the
  search request.

  @history[#:changed "1.60" @elem{Added the @racket[language-family] argument.}]
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
