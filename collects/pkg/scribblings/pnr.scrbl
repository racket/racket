#lang scribble/manual
@(require (for-label racket/base
                     pkg/pnr
                     pkg/lib
                     net/url
                     syntax/modcollapse))

@title[#:tag "pnr"]{Package Information Download}

@defmodule[pkg/pnr]{The @racketmodname[pkg/pnr]
library provides tools for consulting a @tech{package name resolver}
and package servers to obtain information about a package.}

In particular, @racketmodname[pkg/pnr] uses some of the functions from
@racketmodname[pkg/lib] to obtain information is that useful to
populate a database that is managed by @racketmodname[pkg/db].

See also @racket[pkg-indexes] from @racketmodname[pkg/lib].


@defproc[(get-pkg-names-from-pnr [pnr url?]) (listof string?)]{

Obtains a list of unique package names from the specified
@tech{package name resolver}.}

@defproc[(get-pkg-details-from-pnr [pnr url?]
                                   [name string?])
         hash?]{

Returns a hash table representing information about @racket[name] as
reported by @racket[pnr].}


@defproc[(get-pkg-modules [pnr url?]
                          [name string?])
         (values (or/c #f string?) (listof module-path?))]{

Gets a list of module paths that are provided by @racket[name] as
resolved by @racket[pnr] and as provided by the package's server. The
result reports both the checksum of the package (as reported by
@racket[pnr]) and list of module paths.

The module paths are normalized in the sense of
@racket[collapse-module-path].}
