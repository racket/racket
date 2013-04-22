#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     pkg/pnr-db
                     syntax/modcollapse))

@title[#:tag "pnr-db"]{Package Name Database}

@defmodule[pkg/pnr-db]{The @racketmodname[pkg/pnr-db] library provides
tools for storing and retrieving @tech{package name resolver}
information in a local database.}

The functions provided by @racketmodname[pkg/pnr-db] do not actually
manage packages; they do not change or consult the local database of
installed modules in any @tech{package scope}. The functions provided
by @racketmodname[pkg/pnr-db] simply reflect a local copy of the
information that a @tech{package name resolver} and individual package
might provide (but with no guarantee of being in sync with an actual
@tech{package name resolver} or package).

The database is implemented as an SQLite database with its own
locking, so no additional locks are needed for database access, but
beware of concurrent database changes that could break your program
logic.

@defstruct[pkg ([name string?]
                [index string?]
                [author string?]
                [source string?]
                [checksum string?]
                [desc string?])
               #:transparent]{

Represents a package implementation in the database. The @racket[name]
(@tech{package name}) and @racket[index] (@tech{package name
resolver}, normally a URL) fields are always nonempty
strings. Otherwise, unknown fields are represented by empty strings.}


@defparam[current-pkg-index-file file path-string?]{

A parameter that determines the file path used to hold the SQLite
database. The default value is in the user's add-on directory and in
a version-specific subdirectory.}


@deftogether[(
@defproc[(get-indexes) (listof string?)]
@defproc[(set-indexes! [indexes (listof string?)]) void?]
)]{

Returns or sets the list of strings for all @tech{package name
resolvers} represented in the database. (Within the database, each
@tech{package name resolver} gets its own identifying number.)
The order of indices in the list represents a search order.

The @racket[set-indexes!] function removes information for any other
@tech{package name resolvers} from the database.}


@defproc[(get-pkgs [#:name name (or/c #f string?) #f]
                   [#:index index (or/c #f string?) #f])
         (listof pkg?)]{

Gets a list of package descriptions. If @racket[name] or
@racket[index] is not @racket[#f] (or if both are not @racket[#f]),
then the result includes only matching packages.

The result list is ordered by precedence of the @tech{package name
resolver}.}


@defproc[(set-pkgs! [index string?] [pkgs (listof (or/c string? pkg?))]
                    [#:clear-other-checksums? clear-other-checksums? #t])
         void?]{

Sets the list of all packages that are recognized by the
@tech{package name resolver} @racket[index].

Information about any other package for @racket[index] is removed from
the database.  If a string is provided for @racket[pkgs], it is
treated as a package name; if additional information is already
recorded in the database for the package name, then the additional
information is preserved.

If @racket[clear-other-checksums?] is true, then for each element of
@racket[pkgs] that has a given checksum other than @racket[""], any
information in the database specific to another checksum (such as a
list of module paths) is removed from the database.}


@defproc[(set-pkg! [name string?]
                   [index string?]
                   [author string?]
                   [source string?]
                   [checksum string?]
                   [desc string?]
                   [#:clear-other-checksums? clear-other-checksums? (not (equal? checksum ""))])
           void?]{

Sets the information for a specific package @racket[name] as
recognized by the @tech{package name resolver} @racket[index].

If @racket[clear-other-checksums?] is true, then information (such as
a list of module paths) is removed from the database when it is
specific to a checksum other than @racket[checksum].}


@deftogether[(
@defproc[(get-pkg-tags [name string?] [index string?])
         (listof string?)]
@defproc[(set-pkg-tags! [name string?] [index string?]
                        [module-paths (listof string?)])
         void?]
)]{

Gets or sets a list of tags for the package
@racket[name] as recognized by the @tech{package name resolver}
@racket[index].}


@deftogether[(
@defproc[(get-pkg-modules [name string?] [index string?] [checksum string?])
         (listof module-path?)]
@defproc[(set-pkg-modules! [name string?] [index string?] [checksum string?]
                           [module-paths (listof module-path?)])
         void?]
)]{

Gets or sets a list of module paths that are provided for the package
@racket[name] as recognized by the @tech{package name resolver}
@racket[index] and for a specific @tech{checksum}. The module paths
should be normalized in the sense of @racket[collapse-module-path].}


@defproc[(get-module-pkgs [module-path module-path?])
         (listof pkg?)]{

Reports a list of packages that implement the given
@racket[module-path], which should be normalized in the sense of
@racket[collapse-module-path].}


@defproc[(get-pkgs-without-modules [#:index index (or/c #f string?) #f])
         (listof pkg?)]{

Returns a list of packages (optionally constrained to @racket[index])
for which the database has no modules recorded.

Each resulting @racket[pkg] has its @racket[name], @racketidfont{index}, and
@racket[checksum] field set, but other fields may be @racket[""].}
