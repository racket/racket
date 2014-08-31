#lang scribble/manual
@(require "common.rkt"
          (for-label (except-in racket/base
                                remove)
                     racket/contract/base
                     pkg
                     pkg/lib
                     (only-in pkg/db current-pkg-catalog-file)
                     net/url
                     syntax/modcollapse
                     setup/getinfo
                     setup/matching-platform))

@title[#:tag "lib"]{Package Management Functions}

@defmodule[pkg/lib]{The @racketmodname[pkg/lib] library provides
building blocks on which the @racket[pkg] library and @exec{raco pkg}
commands are built. It re-exports the bindings of @racketmodname[pkg/path].}


@deftogether[(
@defform[(with-pkg-lock body ...+)]
@defform[(with-pkg-lock/read-only body ...+)]
)]{

Evaluates the @racket[body]s while holding a lock to prevent
concurrent modification to the package database for the current
@tech{package scope}. Use the @racket[with-pkg-lock/read-only] form
for read-only access.  The lock is reentrant but not upgradable from
read-only.

Use these form to wrap uses of functions from @racketmodname[pkg/lib]
that are documented to require the lock. Other functions from
@racketmodname[pkg/lib] take the lock as needed.}

@deftogether[(
@defparam[current-pkg-scope scope (or/c 'installation 'user
                                        (and/c path? complete-path?))]
@defparam[current-pkg-scope-version s string?]
)]{

Parameters that determine @tech{package scope} for management
operations and, in the case of @racket['user] scope, the relevant
installation name/version.}

@deftogether[(
@defparam[current-pkg-lookup-version s string?]
)]{

Parameter that determines the relevant Racket version for
extracting package information from a catalog.

@history[#:added "6.0.1.7"]}


@defparam[current-pkg-error err procedure?]{

A parameter whose value is used to report errors that are normally
intended for end uses. The arguments to the procedure are the same
as for @racket[error], except that an initial symbol argument is
omitted.

The default value uses @racket[error] with @racket['pkg] as the first
argument. The @exec{raco pkg} command sets this parameter to use
@racket[raise-user-error] with the sub-command name as its first
argument.}


@defparam[current-pkg-catalogs catalogs (or/c #f (listof url?))]{

A parameter that determines the @tech{package catalogs} that are
consulted to resolve a @tech{package name}. If the parameter's value
is @racket[#f], then the result of @racket[pkg-config-catalogs] is
used.}


@defproc[(pkg-config-catalogs) (listof string?)]{

Returns a list of URL strings for the user's configured @tech{package
catalogs}.}


@deftogether[(
@defparam[current-pkg-download-cache-dir dir (or/c #f (and/c path-string? complete-path?))]
@defparam[current-pkg-download-cache-max-files max-files (or/c #f real?)]
@defparam[current-pkg-download-cache-max-bytes max-bytes (or/c #f real?)]
)]{

Parameters that determine the download cache location and limits.  If
a parameter's value is @racket[#f], then the user's configuration is
used.}


@defproc[(pkg-directory [name string?]) (or/c path-string? #f)]{

Returns the directory that holds the installation of the installed
(in any scope) package @racket[name], or @racket[#f] if no such package
is installed.}


@defproc[(default-pkg-scope) (or/c 'installation 'user
                                    (and/c path? complete-path?))]{

Returns the user's configured default @tech{package scope}.}


@defproc[(installed-pkg-names [#:scope scope (or/c #f 'installation 'user
                                                   (and/c path? complete-path?))])
         (listof string?)]{

Returns a list of installed package names for the given @tech{package
scope}, where @racket[#f] indicates the user's default @tech{package
scope}.}


@defproc[(installed-pkg-table [#:scope scope (or/c #f 'installation 'user
                                                   (and/c path? complete-path?))])
         (hash/c string? pkg-info?)]{

Returns a hash table of installed packages for the given @tech{package
scope}, where @racket[#f] indicates the user's default @tech{package
scope}.}


@deftogether[(
@defproc[(pkg-desc? [v any/c]) boolean?]
@defproc[(pkg-desc [source string?]
                   [type (or/c #f 'file 'dir 'link 'static-link 
                               'file-url 'dir-url 'github 'name)]
                   [name (or/c string? #f)]
                   [checksum (or/c string? #f)]
                   [auto? boolean?])
         pkg-desc?]
)]{

A @racket[pkg-desc] value describes a package source plus details of its
intended interpretation, where the @racket[auto?] field indicates that
the package is should be treated as installed automatically for a
dependency.}


@defproc[(pkg-stage [desc pkg-desc?]
                    [#:checksum checksum (or/c #f string?) #f]
                    [#:in-place? in-place? boolean? #f]
                    [#:namespace namespace namespace? (make-base-namespace)]
                    [#:strip strip (or/c #f 'source 'binary 'binary-lib) #f]
                    [#:force-strip? force-string? boolean? #f]
                    [#:use-cache? use-cache? boolean? #f]
                    [#:quiet? quiet? boolean? #t])
         (values string? path? (or/c #f string?) boolean? (listof module-path?))]{

Locates the implementation of the package specified by @racket[desc]
and downloads and unpacks it to a temporary directory (as needed).

If @racket[desc] refers to an existing directory and
@racket[in-place?] is true, then the directory is used in place.

The @racket[namespace] argument is passed along to
@racket[get-info/full] when the package's @filepath{info.rkt} is
loaded.

If @racket[strip] is not @racket[#f], then files and directories are
removed from the prepared directory the same as when creating the
corresponding kind of package. A directory that is staged in-place
cannot be stripped. If @racket[force-strip?] is true, then a
consistency check (intended to avoid stripping a source package as
binary, for example) is skipped.

If @racket[use-cache?] is true, then a local cache is consulted before
downloading a particular package with a particular checksum. Note that
the default for @racket[use-cache?] is @racket[#f], while the default
is @racket[#t] for other functions that accept @racket[#:use-cache?].

The result is the package name, the directory containing the unpacked package content,
the checksum (if any) for the unpacked package, whether the
directory should be removed after the package content is no longer
needed, and a list of module paths provided by the package.}


@defproc[(pkg-config [set? boolean?] [keys/vals list?]
                     [#:from-command-line? from-command-line? boolean? #f])
         void?]{

Implements @racket[pkg-config-command].

If @racket[from-command-line?]  is true, error messages may suggest
specific command-line flags for @command-ref{config}.

The package lock must be held (allowing writes if @racket[set?] is true); see
@racket[with-pkg-lock].}


@defproc[(pkg-create [format (or/c 'zip 'tgz 'plt 'MANIFEST)]
                     [dir path-string?]
                     [#:source source (or/c 'dir 'name)]
                     [#:mode mode (or/c 'as-is 'source 'binary 'binary-lib 'built)]
                     [#:dest dest-dir (or/c (and/c path-string? complete-path?) #f)]
                     [#:quiet? quiet? boolean? #f]
                     [#:from-command-line? from-command-line? boolean? #f])
        void?]{

Implements @racket[pkg-create-command].

Unless @racket[quiet?] is true, information about the output is
reported to the current output port. If @racket[from-command-line?]
is true, error messages may suggest specific command-line flags for
@command-ref{create}.}


@defproc[(pkg-install      [descs (listof pkg-desc?)]
                           [#:dep-behavior dep-behavior
                                           (or/c #f 'fail 'force 'search-ask 'search-auto)
                                           #f]
                           [#:update-deps? update-deps? boolean? #f]
                           [#:force? force? boolean? #f]
                           [#:ignore-checksums? ignore-checksums? boolean? #f]
                           [#:strict-doc-conflicts? strict-doc-conflicts? boolean? #f]
                           [#:use-cache? use-cache? boolean? #t]
                           [#:quiet? boolean? quiet? #f]
                           [#:from-command-line? from-command-line? boolean? #f]
                           [#:strip strip (or/c #f 'source 'binary 'binary-lib) #f]
                           [#:force-strip? force-string? boolean? #f]
                           [#:link-dirs? link-dirs? boolean? #f])
         (or/c 'skip
               #f
               (listof (or/c path-string?
                             (non-empty-listof path-string?))))]{

Implements @racket[pkg-install-command]. The result indicates which
collections should be setup via @exec{raco setup}: @racket['skip]
means that no setup is needed, @racket[#f] means all, and a list means
only the indicated collections.

The @racket[link-dirs?] argument determines whether package sources
inferred to be directory paths should be treated as links or copied
(like other package sources). Note that the default is @racket[#f],
unlike the default built into @racket[pkg-install-command].

Status information and debugging details are mostly reported to a logger
named @racket['pkg], but information that is especially relevant to a
user (such as a download action) is reported to the current output
port, unless @racket[quiet?] is true.

If @racket[from-command-line?]  is true, error messages may suggest
specific command-line flags for @command-ref{install}.

The package lock must be held; see @racket[with-pkg-lock].}


@defproc[(pkg-update      [names (listof (or/c string? pkg-desc?))]
                          [#:all? all? boolean? #f]
                          [#:dep-behavior dep-behavior
                                          (or/c #f 'fail 'force 'search-ask 'search-auto)
                                          #f]
                          [#:update-deps? update-deps? boolean? #f]
                          [#:force? force? boolean? #f]
                          [#:ignore-checksums? ignore-checksums? boolean? #f]
                          [#:strict-doc-conflicts? strict-doc-conflicts? boolean? #f]
                          [#:use-cache? use-cache? quiet? #t]
                          [#:quiet? boolean? quiet? #f]
                          [#:from-command-line? from-command-line? boolean? #f]
                          [#:strip strip (or/c #f 'source 'binary 'binary-lib) #f]
                          [#:force-strip? force-string? boolean? #f]
                          [#:link-dirs? link-dirs? boolean? #f])
        (or/c 'skip
              #f
              (listof (or/c path-string?
                            (non-empty-listof path-string?))))]{

Implements @racket[pkg-update-command]. The result is the same as for
@racket[pkg-install].

A string in @racket[names] refers to an installed package that should
be checked for updates. A @racket[pkg-desc] in @racket[names] indicates
a package source that should replace the current installation.

If @racket[from-command-line?]  is true, error messages may suggest
specific command-line flags for @command-ref{update}.

The package lock must be held; see @racket[with-pkg-lock].}


@defproc[(pkg-remove      [names (listof string?)]
                          [#:demote? demote? boolean? #f]
                          [#:auto? auto? boolean? #f]
                          [#:force? force? boolean? #f]
                          [#:quiet? boolean? quiet? #f]
                          [#:from-command-line? from-command-line? boolean? #f])
         (or/c 'skip
               #f
               (listof (or/c path-string? 
                             (non-empty-listof path-string?))))]{

Implements @racket[pkg-remove-command]. The result is the same as for
@racket[pkg-install], indicating collects that should be setup via
@exec{raco setup}.

If @racket[from-command-line?]  is true, error messages may suggest
specific command-line flags for @command-ref{remove}.

The package lock must be held; see @racket[with-pkg-lock].}


@defproc[(pkg-show [indent string?]
                   [#:directory show-dir? boolean? #f])
         void?]{

Implements @racket[pkg-show-command] for a single package scope,
printing to the current output port. See also
@racket[installed-pkg-names] and @racket[installed-pkg-table].

The package lock must be held to allow reads; see
@racket[with-pkg-lock/read-only].}


@defproc[(pkg-migrate      [from-version string?]
                           [#:dep-behavior dep-behavior
                                           (or/c #f 'fail 'force 'search-ask 'search-auto)
                                           #f]
                           [#:force? force? boolean? #f]
                           [#:use-cache? use-cache? boolean? #t]
                           [#:ignore-checksums? ignore-checksums? boolean? #f]
                           [#:strict-doc-conflicts? strict-doc-conflicts? boolean? #f]
                           [#:quiet? boolean? quiet? #f]
                           [#:from-command-line? from-command-line? boolean? #f]
                           [#:strip strip (or/c #f 'source 'binary 'binary-lib) #f]
                           [#:force-strip? force-string? boolean? #f])
         (or/c 'skip
               #f
               (listof (or/c path-string?
                             (non-empty-listof path-string?))))]{

Implements @racket[pkg-migrate-command].  The result is the same as for
@racket[pkg-install].

If @racket[from-command-line?]  is true, error messages may suggest
specific command-line flags for @command-ref{migrate}.

The package lock must be held; see @racket[with-pkg-lock].}


@defproc[(pkg-catalog-show [names (listof string?)]
                           [#:all? all? boolean? #f]
                           [#:only-names? only-names? boolean? #f]
                           [#:modules? modules? boolean? #f])
         void?]{

Implements @racket[pkg-catalog-show-command]. If @racket[all?] is true,
then @racket[names] should be empty.

The @racket[current-pkg-lookup-version] parameter determines the version
included in the catalog query.

@history[#:changed "6.0.1.7" @elem{Use @racket[current-pkg-lookup-version]
                                   instead of @racket[current-pkg-scope-version].}]}


@defproc[(pkg-catalog-copy [sources (listof path-string?)]
                           [dest path-string?]
                           [#:from-config? from-config? boolean? #f]
                           [#:merge? merge? boolean? #f]
                           [#:force? force? boolean? #f]
                           [#:override? override? boolean? #f]
                           [#:relative-sources? relative-sources? boolean? #f])
         void?]{

Implements @racket[pkg-catalog-copy-command].

The @racket[current-pkg-lookup-version] parameter determines the version
for extracting existing catalog information.

@history[#:changed "6.0.1.7" @elem{Use @racket[current-pkg-lookup-version]
                                   instead of @racket[current-pkg-scope-version].}]}


@defproc[(pkg-catalog-archive [dest-dir path-string?]
                              [sources (listof path-string?)]
                              [#:from-config? from-config? boolean? #f]
                              [#:state-catalog state-catalog (or/c #f path-string?) #f]
                              [#:relative-sources? relative-sources? boolean? #f]
                              [#:quiet? quiet? boolean? #f]
                              [#:package-exn-handler package-exn-handler (string? exn:fail? . -> . any) (lambda (_pkg-name _exn) (raise _exn))])
         void?]{

Implements @racket[pkg-catalog-archive-command].

The @racket[package-exn-handler] argument handles any exception that
is raised while trying to archive an individual package; the first
argument is the package name, and the second is the exception. The
default re-@racket[raise]s the exception, which aborts the archiving
process, while a function that logs the exception message and returns
would allow archiving to continue for other packages.

The @racket[current-pkg-lookup-version] parameter determines the version
for extracting existing catalog information.

@history[#:added "6.0.1.7"
         #:changed "6.0.1.13" @elem{Added the @racket[#:package-exn-handler] argument.}]}

@defproc[(pkg-archive-pkgs [dest-dir path-string?]
                           [pkgs (listof path-string?)]
                           [#:include-deps? include-deps? boolean? #f]
                           [#:exclude exclude (listof string?) null]
                           [#:relative-sources? relative-sources? boolean? #f]
                           [#:quiet? quiet? boolean? #f]
                           [#:package-exn-handler package-exn-handler (string? exn:fail? . -> . any) (lambda (_pkg-name _exn) (raise _exn))])
         void?]{

Implements @racket[pkg-archive-command].

The @racket[package-exn-handler] argument handles any exception that
is raised while trying to archive an individual package; the first
argument is the package name, and the second is the exception. The
default re-@racket[raise]s the exception, which aborts the archiving
process, while a function that logs the exception message and returns
would allow archiving to continue for other packages.

@history[#:added "6.1.0.8"]}


@defproc[(pkg-catalog-update-local [#:catalogs catalogs (listof string?) (pkg-config-catalogs)]
                                   [#:catalog-file catalog-file path-string? (current-pkg-catalog-file)]
                                   [#:quiet? quiet? boolean? #f]
                                   [#:set-catalogs? set-catalogs? boolean? #t]
                                   [#:consult-packages? consult-packages? boolean? #f])
         void?]{

Consults the @tech{package catalogs} specified by @racket[catalogs] (in the
same way as @racket[pkg-catalog-copy]) and the user's configured package servers (if
@racket[consult-packages?] is true) to populate the database
@racket[catalog-file] with information about available packages and the
modules that they implement.

The local catalog @racket[catalog-file] records the set of source catalogs,
including @racket[catalogs], from which its information is drawn. If
@racket[set-catalogs?] is true (which is the default), then
@racket[catalogs] is recorded as the set of sources, and information from any
other catalog is discarded. If @racket[set-catalogs?] is @racket[#f],
then @racket[catalogs] must be a subset of the source catalogs already
recorded in @racket[catalog-file].

@history[#:changed "6.0.1.6" @elem{Added @racket[#:catalogs] and @racket[#:set-catalogs?] arguments.}]}


@defproc[(pkg-catalog-suggestions-for-module 
          [module-path module-path?]
          [#:catalog-file catalog-file path-string? ....])
         (listof string?)]{

Consults @racket[catalog-file] and returns a list of available packages
that provide the module specified by @racket[module-path].

The default @racket[catalog-file] is @racket[(current-pkg-catalog-file)]
if that file exists, otherwise a file in the racket installation is
tried.}

@defproc[(get-all-pkg-scopes) (listof (or/c 'installation 'user path?))]{
 Obtains a list of all the currently-available package scopes.

 @history[#:added "6.1.0.5"]}

@defproc[(get-all-pkg-names-from-catalogs) (listof string?)]{

Consults @tech{package catalogs} to obtain a list of available
@tech{package names}.}


@defproc[(get-all-pkg-details-from-catalogs)
         (hash/c string? (hash/c symbol? any/c))]{

Consults @tech{package catalogs} to obtain a hash table of available
@tech{package names} mapped to details about the package. Details for
a particular package are provided by a hash table that maps symbols
such as @racket['source], @racket['checksum], and @racket['author].}


@defproc[(get-pkg-details-from-catalogs [name string?])
         (or/c #f (hash/c symbol? any/c))]{

Consults @tech{package catalogs} to obtain information for a
single @tech{package name}, returning @racket[#f] if the @tech{package
name} has no resolution. Details for the package are provided in the
same form as from @racket[get-all-pkg-details-from-catalogs].}


@defproc[(pkg-single-collection [dir path-string?]
                                [#:name name string? @elem{... from @racket[dir] ...}]
                                [#:namespace namespace namespace? (make-base-namespapce)])
         (or/c #f string?)]{

Returns a string for a collection name if @racket[dir] represents a
@tech{single-collection package}, or returns @racket[#f] if @racket[dir]
represents a @tech{multi-collection package}.

For some single-collection packages, the package's single collection
is the package name; if the package name is different from the
directory name, supply @racket[name].

Determining a single-collection package's collection name may require
loading an @filepath{info.rkt} file, in which case @racket[namespace]
is passed on to @racket[get-info/full].}


@defproc[(get-pkg-content [desc pkg-desc?]
                          [#:extract-info 
                           extract-proc
                           ((or/c #f
                                  ((symbol?) ((-> any)) . ->* . any))
                            . -> . any)
                           (lambda (get-pkg-info) ...)])
         (values (or/c #f string?) 
                 (listof module-path?)
                 any/c)]{

Gets information about the content of the package specified by
@racket[desc]. The information is determined inspecting the
package---resolving a @tech{package name}, downloading, and unpacking
into a temporary directory as necessary.

The results are as follows:

@itemize[

 @item{The checksum, if any, for the downloaded package.}

 @item{A list of module paths that are provided by the package.
       Each module path is normalized in the sense of
       @racket[collapse-module-path].}

 @item{Information extracted from the package's metadata.  By default,
       this information is the package's dependencies, but in general
       it is the result of @racket[extract-proc], which receives an
       information-getting function (or @racket[#f]) as returned by
       @racket[get-info].}

]}

@defproc[(extract-pkg-dependencies [info (symbol? (-> any/c) . -> . any/c)]
                                   [#:build-deps? build-deps? boolean? #f]
                                   [#:filter? filter? boolean? #f]
                                   [#:versions? versions? boolean? #f])
         (listof (or/c string? (cons/c string? list?)))]{

Returns packages dependencies reported by the @racket[info] procedure
(normally produced by @racket[get-info]).

If @racket[build-deps?] is true, then the result includes both
run-time dependencies and build-time dependencies.

If @racket[filter?] is true, then platform-specific dependencies are
removed from the result list when they do not apply to the current
platform, and other information is stripped so that the result list is
always a list of either strings (when @racket[versions?] is true) or a
two-element list containing a string and a version (when
@racket[versions?] is @racket[#f]).

@history[#:changed "6.0.1.6" @elem{Added the @racket[#:versions?] argument.}]}


@defproc[(pkg-directory->module-paths [dir path-string?]
                                      [pkg-name string]
                                      [#:namespace namespace namespace? (make-base-namespace)])
         (listof module-path?)]{

Returns a list of module paths (normalized in the sense of
@racket[collapse-module-path]) that are provided by the package
represented by @racket[dir] and named @racket[pkg-name].}


@defproc[(pkg-directory->additional-installs [dir path-string?]
                                             [pkg-name string]
                                             [#:namespace namespace namespace? (make-base-namespace)]
                                             [#:system-type sys-type (or/c #f symbol?) (system-type)]
                                             [#:system-library-subpath sys-lib-subpath (or/c #f path?)
                                                                       (system-library-subpath #f)])
         (listof (cons/c symbol? string?))]{

Returns a list of pairs for items that are installed by the package
represented by @racket[dir] and named @racket[pkg-name]. Installed
items can include documentation, executables, foreign libraries, other
shared files, and man pages---all as specified by @filepath{info.rkt}
files. The symbol for each item gives it a category, such as
@racket['doc] or @racket['exe], and the string part is a normalized
name, such as the destination name for a document or a case-folded
executable name without a file suffix.

The @racket[sys-type] and @racket[sys-lib-subpath] arguments are used
in the same way as for @racket[matching-platform?] to determine
platform-specific installations as determined by
@racketidfont{install-platform} definitions in @filepath{info.rkt}
files.

@history[#:added "6.0.1.13"]}
