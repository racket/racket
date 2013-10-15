#lang scribble/manual
@(require (for-label (except-in racket/base
                                remove)
                     racket/contract/base
                     pkg
                     pkg/lib
                     (only-in pkg/db current-pkg-catalog-file)
                     net/url
                     syntax/modcollapse
                     setup/getinfo))

@title[#:tag "path"]{Package Paths and Database}

@defmodule[pkg/path]{The @racketmodname[pkg/path] library provides
utilities for working with package paths and installed-package
databases.}

@defstruct*[pkg-info ([orig-pkg (or/c (list/c 'catalog string?)
                                      (list/c 'url string?)
                                      (list/c 'link string?)
                                      (list/c 'static-link string?))]
                      [checksum (or/c #f string?)]
                      [auto? boolean?])
                      #:prefab]{

A structure type that is used to report installed-package information.}


@defstruct*[(sc-pkg-info pkg-info) ()]{

A structure subtype that represents a package that is installed as
single-collection.}

@deftogether[(
@defstruct*[(pkg-info/alt pkg-info) ([dir-name string?])]
@defstruct*[(sc-pkg-info/alt sc-pkg-info) ([dir-name string?])]
)]{

Structure subtypes that are used when the installation directory for a
package does not match the package name, but is instead
@racket[dir-name]. The directory name always includes a @litchar{+}
(which is disallowed in a package name).}


@defproc[(path->pkg [path path-string?]
                    [#:cache cache (or/c #f (and/c hash? (not/c immutable?)))])
         (or/c string? #f)]{

Returns the installed package containing @racket[path], if any.

If @racket[cache] is not @racket[#f], then it is consulted and
modified to cache installed-package information across calls to
@racket[path->pkg] (with the assumption that the set of installed
packages does not change across calls that receive the same
@racket[cache]).}


@defproc[(path->pkg+subpath [path path-string?]
                            [#:cache cache (or/c #f (and/c hash? (not/c immutable?)))])
         (values (or/c string? #f) (or/c path? 'same #f))]{

Like @racket[path->pkg], but returns a second value that represents
the remainder of @racket[path] within the package's directory.}


@defproc[(path->pkg+subpath+collect [path path-string?]
                                    [#:cache cache (or/c #f (and/c hash? (not/c immutable?)))])
         (values (or/c string? #f) (or/c path? 'same #f) (or/c string? #f))]{

Like @racket[path->pkg+subpath], but returns a third value for a
collection name if the package is a single-collection package,
@racket[#f] otherwise.}


@defproc[(get-pkgs-dir [scope (or/c 'installation 'user 'shared
                                     (and/c path? complete-path?))]
                       [user-version string? (version)])
         path?]{

Returns the path of the directory that holds installed packages in the
given scope. The @racket[user-version] argument is used to generate
the result for @racket['user] scope.}


@defproc[(read-pkgs-db [scope (or/c 'installation 'user 'shared
                                     (and/c path? complete-path?))])
         (hash/c string? pkg-info?)]{

Returns a hash table representing the currently installed packages
in the specified scope.}


@defproc[(read-pkg-file-hash [path path?]) hash?]{

Reads a hash table from @racket[path], logging any errors and
returning an empty hash table if @racket[path] does not exist or if an
error is encountered.}
