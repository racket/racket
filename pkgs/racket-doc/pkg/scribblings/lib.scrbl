#lang scribble/manual
@(require "common.rkt"
          (for-label (except-in racket/base
                                remove)
                     racket/contract/base
                     (only-in racket/set set/c)
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


@deftogether[(
@defparam[current-pkg-trash-max-packages max-packages (or/c #f real?)]
@defparam[current-pkg-trash-max-seconds max-seconds (or/c #f real?)]
)]{

Parameters that determine the trash-directory limits.  If
a parameter's value is @racket[#f], then the user's configuration is
used.

@history[#:added "6.1.1.6"]}


@deftogether[(
@defparam[current-pkg-network-retries max-retries (or/c #f real?)]
)]{

A parameter that determines the number of times to retry a network communication
that fails due to a connection error.  If
a parameter's value is @racket[#f], then the user's configuration is
used.

@history[#:added "6.3"]}


@deftogether[(
@defparam[current-pkg-network-timeout max-seconds (or/c #f real?)]
)]{

A parameter that determines the number of seconds to wait for a network communication,
such as a download or a checksum fetch. If
a parameter's value is @racket[#f], then the user's configuration is
used.

@history[#:added "9.0.0.2"]}


@defproc[(pkg-directory [name string?]
                        [#:cache cache (or/c #f (and/c hash? (not/c immutable?))) #f])
         (or/c path-string? #f)]{

Returns the directory that holds the installation of the installed
(in any scope) package @racket[name], or @racket[#f] if no such package
is installed.

For multiple calls to @racket[pkg-directory], supply the same
@racket[equal?]-based mutable hash table (initially empty) as the
@racket[cache] argument. Otherwise, package-installation information
must be re-parsed on every call to @racket[pkg-directory].

@history[#:changed "6.1.1.6" @elem{Added the @racket[#:cache] argument.}]}


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
                   [type (or/c #f 'name 'file 'dir 'link 'static-link
                               'file-url 'dir-url 'git 'git-url 'github 'clone)]
                   [name (or/c string? #f)]
                   [checksum (or/c string? #f)]
                   [auto? boolean?]
                   [#:path path (or/c #f path-string?) #f])
         pkg-desc?]
)]{

A @racket[pkg-desc] value describes a package source plus details of its
intended interpretation, where the @racket[auto?] field indicates that
the package is should be treated as installed automatically for a
dependency.

The optional @racket[path] argument is intended for use when
@racket[type] is @racket['clone], in which case it specifies a
directory containing the repository clone (where the repository itself
is a directory within @racket[path]).

@history[#:changed "6.1.1.1" @elem{Added @racket['git] as a @racket[type].}
         #:changed "6.1.1.5" @elem{Added @racket['clone] as a @racket[type].}
         #:changed "8.0.0.13" @elem{Added @racket['git-url] as a @racket[type].}]}


@defproc[(pkg-stage [desc pkg-desc?]
                    [#:checksum checksum (or/c #f string?) #f]
                    [#:in-place? in-place? boolean? #f]
                    [#:namespace namespace namespace? (make-base-namespace)]
                    [#:strip strip (or/c #f 'source 'binary 'binary-lib) #f]
                    [#:force-strip? force-strip? boolean? #f]
                    [#:use-cache? use-cache? boolean? #f]
                    [#:quiet? quiet? boolean? #t])
         (values string? path? (or/c #f string?) boolean? (set/c module-path?))]{

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
                     [#:from-command-line? from-command-line? boolean? #f]
                     [#:default-scope-scope default-scope-scope (or/c #f 'installation 'user (and/c path? complete-path?)) #f])
         void?]{

Implements @racket[pkg-config-command].

If @racket[from-command-line?]  is true, error messages may suggest
specific command-line flags for @command-ref{config}.

If @racket[default-scope-scope] is not @racket[#f], then it specifies
potentially narrower scope than @racket[(current-pkg-scope)] where
@racket['default-scope] is configured. That information may trigger
output to warn a user that a @racket['default-scope] setting in a
wider scope does not have any effect. See also
@racket[pkg-config-default-scope-scope].

The package lock must be held (allowing writes if @racket[set?] is true); see
@racket[with-pkg-lock].

@history[#:changed "7.7.0.9" @elem{Added the @racket[#:default-scope-scope] argument.}]}


@defproc[(pkg-create [format (or/c 'zip 'tgz 'plt 'MANIFEST)]
                     [dir path-string?]
                     [#:source source (or/c 'dir 'name)]
                     [#:mode mode (or/c 'as-is 'source 'binary 'binary-lib 'built)]
                     [#:dest dest-dir (or/c (and/c path-string? complete-path?) #f)]
                     [#:original original-source (or/c string? #f) #f]
                     [#:quiet? quiet? boolean? #f]
                     [#:from-command-line? from-command-line? boolean? #f])
        void?]{

Implements @racket[pkg-create-command].

Unless @racket[quiet?] is true, information about the output is
reported to the current output port. If @racket[from-command-line?]
is true, error messages may suggest specific command-line flags for
@command-ref{create}.

@history[#:changed "8.14.0.2" @elem{Added the @racket[#:original] argument.}]}


@defproc[(pkg-install      [descs (listof pkg-desc?)]
                           [#:dep-behavior dep-behavior
                                           (or/c #f 'fail 'force 'search-ask 'search-auto)
                                           #f]
                           [#:update-deps? update-deps? boolean? #f]
                           [#:force? force? boolean? #f]
                           [#:ignore-checksums? ignore-checksums? boolean? #f]
                           [#:strict-doc-conflicts? strict-doc-conflicts? boolean? #f]
                           [#:use-cache? use-cache? boolean? #t]
                           [#:quiet? quiet? boolean? #f]
                           [#:use-trash? use-trash? boolean? #f]
                           [#:from-command-line? from-command-line? boolean? #f]
                           [#:strip strip (or/c #f 'source 'binary 'binary-lib) #f]
                           [#:force-strip? force-strip? boolean? #f]
                           [#:multi-clone-mode multi-clone-mode (or/c 'fail 'force 'convert 'ask) 'fail]
                           [#:pull-mode pull-mode (or/c 'ff-only 'try 'rebase) 'ff-only]
                           [#:link-dirs? link-dirs? boolean? #f]
                           [#:dry-run? dry-run? boolean? #f])
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

A @racket[pkg-desc] can have the type @racket['clone] and a source
with the syntax of a package name, in which case it refers to a
@tech{package name} that must be mapped to a Git repository by the
@tech{package catalog}, and in will be installed as a clone.

Status information and debugging details are mostly reported to a logger
named @racket['pkg], but information that is especially relevant to a
user (such as a download action) is reported to the current output
port, unless @racket[quiet?] is true.

If @racket[from-command-line?]  is true, error messages may suggest
specific command-line flags for @command-ref{install}.

The package lock must be held; see @racket[with-pkg-lock].

@history[#:changed "6.1.1.5" @elem{Added the @racket[#:multi-clone-mode] 
                                   and @racket[#:infer-clone-from-dir?] arguments.}
         #:changed "6.1.1.6" @elem{Added the @racket[#:use-trash?] argument.}
         #:changed "6.1.1.8" @elem{Added the @racket[#:pull-mode] argument.}
         #:changed "6.4.0.14" @elem{Added the @racket[#:dry-run] argument.}]}


@defproc[(pkg-update      [sources (listof (or/c string? pkg-desc?))]
                          [#:all? all? boolean? #f]
                          [#:dep-behavior dep-behavior
                                          (or/c #f 'fail 'force 'search-ask 'search-auto)
                                          #f]
                          [#:update-deps? update-deps? boolean? #f]
                          [#:force? force? boolean? #f]
                          [#:ignore-checksums? ignore-checksums? boolean? #f]
                          [#:strict-doc-conflicts? strict-doc-conflicts? boolean? #f]
                          [#:use-cache? use-cache? boolean? #t]
                          [#:skip-uninstalled? skip-uninstalled? boolean? #t]
                          [#:quiet? quiet? boolean? #f]
                          [#:use-trash? use-trash? boolean? #f]
                          [#:from-command-line? from-command-line? boolean? #f]
                          [#:strip strip (or/c #f 'source 'binary 'binary-lib) #f]
                          [#:force-strip? force-strip? boolean? #f]
                          [#:lookup-for-clone? lookup-for-clone? boolean? #f]
                          [#:multi-clone-mode multi-clone-mode (or/c 'fail 'force 'convert 'ask) 'fail]
                          [#:pull-mode pull-mode (or/c 'ff-only 'try 'rebase) 'ff-only]
                          [#:link-dirs? link-dirs? boolean? #f]
                          [#:infer-clone-from-dir? infer-clone-from-dir? boolean? #f]
                          [#:dry-run? dry-run? boolean? #f])
        (or/c 'skip
              #f
              (listof (or/c path-string?
                            (non-empty-listof path-string?))))]{

Implements @racket[pkg-update-command]. The result is the same as for
@racket[pkg-install].

A string in @racket[sources] refers to an installed package that should
be checked for updates. A @racket[pkg-desc] in @racket[sources]
indicates a package source that should replace the current
installation; as an exception, if a @racket[pkg-desc] has the type
@racket['clone] and a source with the syntax of a package name, it
refers to an existing package installation that should be converted to
a Git repository clone---unless @racket[lookup-for-clone?] is true,
in which case the package name is resolved through a catalog to
locate a Git repository clone.

The @racket[link-dirs?] and @racket[infer-clone-from-dir?] arguments
affect how directory paths in @racket[sources] are treated. The
@racket[link-dirs?] argument is propagated to
@racket[package-source->name+type], while
@racket[infer-clone-from-dir?]  introduces a conversion from a
directory source to a repository-clone source when the directory
corresponds to an existing repository-clone installation.

If @racket[from-command-line?]  is true, error messages may suggest
specific command-line flags for @command-ref{update}.

The package lock must be held; see @racket[with-pkg-lock].

@history[#:changed "6.1.1.5" @elem{Added the @racket[#:multi-clone-mode] 
                                   and @racket[#:infer-clone-from-dir?] arguments.}
         #:changed "6.1.1.6" @elem{Added the @racket[#:use-trash?] argument.}
         #:changed "6.1.1.8" @elem{Added the @racket[#:skip-uninstalled?] and @racket[#:pull-mode] arguments.}
         #:changed "6.4.0.14" @elem{Added the @racket[#:dry-run] argument.}]}


@defproc[(pkg-remove      [names (listof string?)]
                          [#:demote? demote? boolean? #f]
                          [#:auto? auto? boolean? #f]
                          [#:force? force? boolean? #f]
                          [#:quiet? quiet? boolean? #f]
                          [#:use-trash? boolean? use-trash? #f]
                          [#:from-command-line? from-command-line? boolean? #f]
                          [#:dry-run? dry-run? boolean? #f])
         (or/c 'skip
               #f
               (listof (or/c path-string? 
                             (non-empty-listof path-string?))))]{

Implements @racket[pkg-remove-command]. The result is the same as for
@racket[pkg-install], indicating collects that should be setup via
@exec{raco setup}.

If @racket[from-command-line?] is true, the function @racket[pkg-remove]
may recommend additional instructions for removing automatically installed
packages in the standard output.
The error messages can also suggest
specific command-line flags for @command-ref{remove}.

When @racket[quiet?] is true, the messages in the standard output are suppressed.

The package lock must be held; see @racket[with-pkg-lock].

@history[#:changed "6.1.1.6" @elem{Added the @racket[#:use-trash?] argument.}
         #:changed "6.4.0.14" @elem{Added the @racket[#:dry-run] argument.}
         #:changed "8.6.0.7" @elem{Added the suggestion for removing automatically
                             installed packages.}]}


@defproc[(pkg-new [name path-string?])
         (void?)]{
Implements @racket[pkg-new-command].

The @racket[name] parameter is the name of the new package.
}


@defproc[(pkg-show [indent string?]
                   [pkgs-or-patterns (or/c #f (listof string?))]
                   [#:prefix-line prefix-line (or/c #f string?) #f]
                   [#:auto? auto? boolean? #f]
                   [#:rx? rx? boolean? #f]
                   [#:long? long? boolean? #f]
                   [#:full-checksum? full-checksum? boolean? #f]
                   [#:directory show-dir? boolean? #f])
         void?]{

Implements @racket[pkg-show-command] for a single package scope,
printing to the current output port. If @racket[prefix-line]s is not
@racket[#f], it is printed before the output. See also
@racket[installed-pkg-names] and @racket[installed-pkg-table].

If @racket[pkgs-or-patterns] is @racket[#f], then information is shown
for all installed packages, otherwise only matching packages are shown.
In @racket[rx?] is true, then elements of @racket[pkgs-or-patterns]
are treated as regular expression patterns, otherwise they are treated
as package names.

The package lock must be held to allow reads; see
@racket[with-pkg-lock/read-only].

@history[#:changed "6.1.1.5" @elem{Added the @racket[#:long?] argument.}
         #:changed "6.1.1.6" @elem{Added the @racket[#:full-checksum?] and @racket[#:rx?] arguments.}
         #:changed "6.5.0.1" @elem{Added the @racket[#:prefix-line] argument.}]}


@defproc[(pkg-migrate      [from-version string?]
                           [#:dep-behavior dep-behavior
                                           (or/c #f 'fail 'force 'search-ask 'search-auto)
                                           #f]
                           [#:force? force? boolean? #f]
                           [#:use-cache? use-cache? boolean? #t]
                           [#:ignore-checksums? ignore-checksums? boolean? #f]
                           [#:strict-doc-conflicts? strict-doc-conflicts? boolean? #f]
                           [#:quiet? quiet? boolean? #f]
                           [#:from-command-line? from-command-line? boolean? #f]
                           [#:strip strip (or/c #f 'source 'binary 'binary-lib) #f]
                           [#:force-strip? force-strip? boolean? #f]
                           [#:dry-run? dry-run? boolean? #f])
         (or/c 'skip
               #f
               (listof (or/c path-string?
                             (non-empty-listof path-string?))))]{

Implements @racket[pkg-migrate-command].  The result is the same as for
@racket[pkg-install].

If @racket[from-command-line?]  is true, error messages may suggest
specific command-line flags for @command-ref{migrate}.

The package lock must be held; see @racket[with-pkg-lock].

@history[#:changed "6.4.0.14" @elem{Added the @racket[#:dry-run] argument.}]}


@defproc[(pkg-migrate-available-versions) (listof string?)]{

Returns a list of versions that are suitable as arguments to
@racket[pkg-migrate].

@history[#:added "8.11.1.7"]}


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
                              [#:include includes (or/c #f (listof string?)) #f]
                              [#:include-deps? include-deps? boolean? #f]
                              [#:include-deps-sys+subtype include-deps-sys+subtype (or/c #f (cons/c symbol?
                                                                                                    path-for-some-system?))
                                                           #f]
                              [#:exclude excludes (listof string?) '()]
                              [#:fast-file-copy? fast-file-copy? boolean? #f]
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
         #:changed "6.0.1.13" @elem{Added the @racket[#:package-exn-handler] argument.}
         #:changed "7.7.0.1" @elem{Added the @racket[#:include], @racket[#:include-deps?],
                                   @racket[#:include-deps-platform],
                                   @racket[#:exclude], and @racket[#:fast-file-copy?] arguments.}]}

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


@defproc[(pkg-empty-trash [#:list? show-list? boolean? #f]
                          [#:quiet? quiet? boolean? #t])
         void?]{

Implements @racket[pkg-empty-trash].

@history[#:added "6.1.1.6"]}


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
                           (lambda (get-pkg-info) ...)]
                          [#:namespace namespace namespace? (make-base-namespace)]
                          [#:use-cache? use-cache? boolean? #f]
                          [#:quiet? quiet? boolean? #t])
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

]

The @racket[namespace] argument is effectively passed along to
@racket[get-info/full] and/or @racket[pkg-stage] for reading package
and collection @filepath{info.rkt} files.

The @racket[use-cache?] and @racket[quiet?] arguments are effectively
passed to @racket[pkg-stage] to control the use of a download cache
and status reporting.

@history[#:changed "6.1.1.2" @elem{Added the @racket[#:use-cache?] and
                                   @racket[#:quiet?] arguments.}]}


@defproc[(extract-pkg-dependencies [info (or/c #f (symbol? (-> any/c) . -> . any/c))]
                                   [#:build-deps? build-deps? boolean? #t]
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
always a list of either strings (when @racket[versions?] is @racket[#f]) or a
two-element list containing a string and a version (when
@racket[versions?] is true).

If @racket[info] is @racket[#f], the result is @racket[(list)].

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
                                             [#:system-library-subpath sys-lib-subpath (or/c #f path-for-some-system?)
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


@defproc[(pkg-config-default-scope-scope) (or/c #f 'user 'installation (and/c path? complete-path?))]{

Reports the narrowest scope that is at least as wide as
@racket[current-pkg-scope] and that has a configuration for
@racket['default-scope]. The result can be useful with
@racket[pkg-config].

The package lock must be held; see @racket[with-pkg-lock]. Note that
@racket[pkg-config] cannot necessarily call
@racket[pkg-config-default-scope-scope] itself, because it may be
called with a lock that is wider than the narrowest relevant scope.

@history[#:added "7.7.0.9"]}


@defproc[(call-in-pkg-timeout-sandbox [thunk (-> any)]
                                      [#:make-exn make-exn exn:fail (string? continuation-mark-set? . -> . any/c)])
         any]{

Calls @racket[thunk] in a thread and under a custodian that is
shutdown when the thread terminates. If the thread does not terminate
within the number of seconds indicated by @racket[current-pkg-network-timeout],
the thread is forcibly terminated by shutting down its custodian.

The result of @racket[thunk] is returned as the result of
@racket[call-in-pkg-timeout-sandbox]. If the thread raises an
exception, the exception is re-@racket[raise]d by
@racket[call-in-pkg-timeout-sandbox] in the current thread.

The result of @racket[make-exn] is @racket[raise]d if the thread
terminates without returning a result or throwing an exception and if
the timeout expires. If the thread terminates without returning a
result or throwing an exception before the timeout, a ``thread
terminated'' exception is raised.

@history[#:added "9.0.0.2"]}
