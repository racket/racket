#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     pkg/name))
                     

@title[#:tag "name"]{Package Source Parsing}

@defmodule[pkg/name]{The @racketmodname[pkg/name] library provides
functions for parsing and normalizing a package source, especially for
extracting a package name.}


@defproc[(package-source-format? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is @racket['name] , @racket['file],
@racket['dir], @racket['github], @racket['file-url],
@racket['dir-url], @racket['link], or @racket['static-link], and
returns @racket[#f] otherwise.

The @racket['link] and @racket['static-link] formats are the same as
@racket['dir] in terms of parsing, but they are treated differently
for tasks such as package installation.}


@defproc[(package-source->name [source string?]
                               [type (or/c package-source-format? #f)
                                #f])
          (or/c #f string?)]{

Extracts the @tech{package name} from a @tech{package source}, where
the package source type is inferred if @racket[type] is @racket[#f].
If a valid name cannot be inferred, the result is @racket[#f].}


@defproc[(package-source->name+type [source string?]
                                    [type (or/c package-source-format? #f)
                                     #f])
          (values (or/c #f string?)
                  (or/c package-source-format? #f))]{

Like @racket[package-source->name], but also returns the type of the
source (which is useful when the type is inferred). If the source is
not well-formed, the second result can be @racket[#f].}
