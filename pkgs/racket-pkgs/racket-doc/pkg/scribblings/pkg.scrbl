#lang scribble/manual
@(require scribble/bnf
          scribble/core
          "common.rkt"
          (for-label pkg
                     (except-in racket/base remove)
                     setup/dirs
                     setup/matching-platform))

@(define @|Planet1| @|PLaneT|)

@(define package-name-chars
   @list{@litchar{a} through @litchar{z}, 
         @litchar{A} through @litchar{Z}, 
         @litchar{0} through @litchar{9}, 
         @litchar{_}, and @litchar{-}})

@(define (gtech s)
   @tech[#:doc '(lib "scribblings/guide/guide.scrbl") s])

@(define subcommand list)

@; ----------------------------------------

@title{Package Management in Racket}
@author[@author+email["Jay McCarthy" "jay@racket-lang.org"]]

The Racket package manager lets you install new libraries and
collections, and the Racket package catalog  helps other Racket
programmers find libraries that you make available.

@table-of-contents[]

@; ----------------------------------------

@include-section["getting-started.scrbl"]

@; ----------------------------------------

@section{Package Concepts}

A @deftech{package} is a set of modules in some number of
@gtech{collections}. Modules installed using the Racket package
manager are @racket[require]d like any other modules. For example, if
the package @pkgname{tic-tac-toe} contains the module
@filepath{matrix.rkt} in a @filepath{data} collection, then after
@pkgname{tic-tac-toe} is installed,

@racketblock[(require data/matrix)]

imports the module. The package name is not mentioned with
@racket[require], because packages are a way of managing library
collections, not a way of referencing them. It is common, however, for
a package to implement a collection whose name is the same as the
package name---in which case a @racket[require] might appear to be
referencing a @tech{package}, but it is actually referencing a
@gtech{collection} provided by the @tech{package}.

Each @tech{package} has associated @deftech{package metadata}:

@itemlist[
 @item{a @deftech{package name} --- a string made of the characters @|package-name-chars|.}
 @item{a @deftech{checksum} --- a string that identifies different releases of a package. A
                                package can be updated when its @tech{checksum} changes,
                                whether or not its @tech{version} changes. The checksum normally
                                can be computed as the SHA1 (see @racketmodname[openssl/sha1])
                                of the package's content.}
 @item{a @deftech{version} --- a string of the form @nonterm{maj}@litchar{.}@nonterm{min},
                     @nonterm{maj}@litchar{.}@nonterm{min}@litchar{.}@nonterm{sub}, or
                     @nonterm{maj}@litchar{.}@nonterm{min}@litchar{.}@nonterm{sub}@litchar{.}@nonterm{rel},
                     where @nonterm{maj}, @nonterm{min}, @nonterm{sub}, and @nonterm{rel} are
                     all canonical decimal representations of natural numbers, @nonterm{min} has no more
                     than two digits, and @nonterm{sub} and @nonterm{rel} has no more than
                     three digits. A version is intended to reflect available features of
                     a package, and should not be confused with different releases of
                     a package as indicated by the @tech{checksum}.}
 @item{a list of dependencies --- a list of packages to be installed simultaneously, optionally
                                 with a lower bound on each package's @tech{version}.}
]

A @tech{package} is typically represented by a directory with the same
name as the package. The @tech{checksum} is typically left implicit.
The package directory can contain a file named @filepath{info.rkt}
to declare other metadata (see @secref["metadata"]).

@subsection[#:tag "concept:multi-collection"]{Single-collection and Multi-collection Packages}

A @tech{package} can be a @tech{single-collection package} or a
@tech{multi-collection package}:

@itemlist[

 @item{A @deftech{single-collection package}'s directory doubles as a
       @gtech{collection} directory. By default, the package name also
       doubles as the collection name, but if the package has an
       @filepath{info.rkt} file that defines @racketidfont{collection} to a
       string, then the string is used as the name of the package's
       collection.}

 @item{A @deftech{multi-collection package}'s directory contains
       subdirectories, each of which is a @gtech{collection} that is
       provided by the package (where the directory name is used as
       the collection name). A @tech{multi-collection package} must
       have an @filepath{info.rkt} file that defines
       @racketidfont{collection} as @racket['multi].}

]

@subsection[#:tag "concept:source"]{Package Sources}

A @deftech{package source} identifies a @tech{package}
representation. Each package source type has a different way of
storing the @tech{checksum} and providing the package content (usually
with @tech{single-collection package} and @tech{multi-collection
package} variants).

The @tech{package source} types are:

@itemlist[

@item{a local file path naming an archive (as a plain path or @litchar{file://} URL)
--- The name of the package
is the basename of the archive file. The @tech{checksum} for archive
@filepath{f.@nonterm{ext}} is given by the file @filepath{f.@nonterm{ext}.CHECKSUM}.
The valid archive formats
are (currently) @filepath{.zip}, @filepath{.tar}, @filepath{.tgz}, 
@filepath{.tar.gz}, and
@filepath{.plt}.

For example, @filepath{~/tic-tac-toe.zip} is an archive package
source, and its @tech{checksum} would be inside
@filepath{~/tic-tac-toe.zip.CHECKSUM}.

An archive represents package content analogous to a directory, but if
the archive's content is contained within a single top-level
directory, then the directory's content (as opposed to the overall
archive content) is used as the package content. The @filepath{.plt}
format does not accommodate either an extra directory layer or a
@tech{single-collection package} representation.

A package source is inferred to refer to an archive file
only when it has a suffix matching a valid archive format
and when it starts with @litchar{file://} or does not start
with alphabetic characters followed by @litchar{://}. The inferred
package name is the filename without its suffix.

@history[#:changed "6.0.1.12"
         @elem{Changed treatment of an archive that contains all
               content within a top-level directory.}]}

@item{a local directory (as a plain path or @litchar{file://} URL)
--- The name of the package is the name of the
directory. The @tech{checksum} is not present.

For example,
@filepath{~/tic-tac-toe/} is a directory package source.

A package source is inferred to refer
to a directory only when it does not have a file-archive suffix, does
not match the grammar of a package name, and either starts with starts 
with @litchar{file://} or does not start
with alphabetic characters followed by @litchar{://}. The inferred
package name is the directory name.}

@item{a remote URL naming an archive --- This type follows the same
rules as a local file path, but the archive and @tech{checksum} files are
accessed via HTTP(S).

For example,
@filepath{http://game.com/tic-tac-toe.zip} is a remote URL package
source whose @tech{checksum} is found at
@filepath{http://game.com/tic-tac-toe.zip.CHECKSUM}.

A package source is inferred to be a URL only when it
starts with @litchar{http://} or @litchar{https://}, and it
is inferred to be a file URL when the URL ends with a path element
that could be inferred as a file archive.
The inferred package name is from the URL's file name in the same
way as for a file package source.}

@item{a remote URL naming a directory --- The remote directory must
contain a file named @filepath{MANIFEST} that lists all the contingent
files. These are downloaded into a local directory and then the rules
for local directory paths are followed. However, if the remote
directory contains a file named @filepath{.CHECKSUM}, then it is used
to determine the @tech{checksum}.

For example,
@filepath{http://game.com/tic-tac-toe/} is a directory URL package
source whose @tech{checksum} is found at
@filepath{http://game.com/tic-tac-toe/.CHECKSUM}.

A package source is inferred to be a URL the same for a directory or
file, and it is treated as a directory URL when it does not end with a
path element that has an archive file suffix. The inferred package name
is the directory name.}

@item{a remote URL naming a GitHub repository --- The format for such
URLs is:

@inset{@exec{git://github.com/}@nonterm{user}@exec{/}@nonterm{repo}@;
@optional{@exec{.git}}@optional{@exec{/}}@optional{@exec{?path=}@nonterm{path}}@;
@optional{@exec{#}@nonterm{rev}}}

where @nonterm{path} can contain multiple @litchar{/}-separated
elements to form a path within the repository, and defaults to the
empty path. The @nonterm{rev} can be a branch, tag, or commit, and it
defaults to @exec{master}.

For example, @filepath{git://github.com/game/tic-tac-toe#master}
is a GitHub package source.

For backward compatibility, an older format is also supported:

@inset{@exec{github://github.com/}@nonterm{user}@exec{/}@nonterm{repo}@;
@exec{/}@nonterm{rev}@optional{@exec{/}@nonterm{path}}}

The @exec{zip}-formatted archive for the repository (generated by
GitHub for any commit) is used as a remote URL archive path.  The
@tech{checksum} is the hash identifying @nonterm{rev} if @nonterm{rev}
is a branch or tag, otherwise @nonterm{rev} itself serves as the
@tech{checksum}.

A package source is inferred to be a GitHub reference when it
starts with @litchar{git://} or @litchar{github://}; a package source that is otherwise
specified as a GitHub reference is automatically prefixed with
@filepath{git://github.com/}. The inferred package name
is the last element of @nonterm{path} if it is
non-empty, otherwise the inferred name is @nonterm{repo}.}

@item{a @tech{package name} --- A @tech{package catalog} is
consulted to determine the source and @tech{checksum} for the package.

For
example, @exec{tic-tac-toe} is a package name that can be used as a
package source.

A package source is inferred
to be a package name when it fits the grammar of package names, which
means that it has only the characters @|package-name-chars|.}

]

@subsection[#:tag "concept:catalog"]{Package Catalogs}

A @deftech{package catalog} is a server or database that converts package
names to other package sources. A @tech{package catalog} is identified by a string
representing a URL, where a @litchar{http://} or @litchar{https://}
URL indicates a remote server, and a @litchar{file://} URL indicates a
local catalog in the form of an SQLite database or a directory tree.

PLT supports two @tech{package catalog} servers that are enabled by
default: @url{http://pkgs.racket-lang.org} for new packages and
@url{http://planet-compats.racket-lang.org} for automatically
generated packages for old @|PLaneT| packages. Anyone may host a
@tech{package catalog}, and any file-serving HTTP host can act
as a basic @tech{package catalog} server. See @secref["catalog-protocol"]
for information on how package information is extracted from a catalog.

@subsection[#:tag "concept:auto"]{Explicit vs@|._| Auto-Installation}

When a package is installed, the original source of its installation
is recorded, as well as whether the instalation was an @tech{automatic installation}. An
@deftech{automatic installation} is one that was installed because it
was a dependency of a non-@tech{automatic installation} package.

@subsection[#:tag "concept:conflicts"]{Package Conflicts}

Two packages are in @deftech{conflict} if they contain the same
module. For example, if the package @pkgname{tic-tac-toe} contains the
module file @filepath{data/matrix.rkt} and the package
@pkgname{factory-optimize} contains the module file
@filepath{data/matrix.rkt}, then @pkgname{tic-tac-toe} and
@pkgname{factory-optimize} are in conflict.

A package may also be in
conflict with Racket itself, if it contains a module file that is part
of the base Racket implementation. For example, any package that
contains @filepath{racket/list.rkt} is in conflict with Racket.

For the purposes of conflicts, a module is a file that ends in
@filepath{.rkt}, @filepath{.ss}, or @filepath{.scrbl}.

@subsection[#:tag "concept:updates"]{Package Updates}

Package A is a @deftech{package update} of Package B if (1) B is
installed, (2) A and B have the same name, and (3) A's @tech{checksum} is
different than B's. A @tech{single-collection package}
can be a @tech{package update} of a @tech{multi-collection package}
and vice versa.

Note that a package @tech{version} is not taken
into account when determining a @tech{package update}, although a change
in a package's @tech{version} (in either direction)
implies a change in the @tech{checksum} because the checksum is
computed from the package source and the meta-data that specifies
the version is part of the source.

@subsection[#:tag "concept:scope"]{Package Scopes}

A @deftech{package scope} determines the effect of package
installations, updates, @|etc|, with respect to different users and
Racket installations. The default @tech{package scope} can be
configured, but it is normally @exec{user}, which makes actions
specific to both the current user and the installation's name/version
(in the sense of @racket[get-installation-name]). The
@exec{installation} scope means that package operations affect
all users of the Racket installation.

A directory path can be used as a @tech{package scope}, in which case
package operations affect the set of packages installations in the
directory. An installation can be configured to include the
directory in its search path for installed packages (see
@secref["config-file" #:doc '(lib "scribblings/raco/raco.scrbl")]).

Conflict checking disallows installation of the same or conflicting
package in different scopes, but if such a configuration is forced,
collections are found first in packages with @exec{user} @tech{package
scope}. Search then proceeds in a configured order, where
@exec{installation} @tech{package scope} typically precedes other
directory @tech{package scopes}.


@; ----------------------------------------

@section[#:tag "cmdline"]{Using @exec{raco pkg}}

The @exec{raco pkg} command provides package-management tools via
sub-commands.

@command/toc{install} @nonterm{option} ... @nonterm{pkg-source} ... 
 --- Installs the given @tech{package sources} (eliminating exact-duplicate @nonterm{pkg-source}s).
     If a given @nonterm{pkg-source} is ``auto-installed'' (to satisfy some other package's
     dependency), then it is promoted to explicitly installed.

     If no @nonterm{pkg-source}s are supplied, the current directory is
     installed as a link. See the @DFlag{link} flag below for more details.

 The @exec{install} sub-command accepts 
 the following @nonterm{option}s:

 @itemlist[

 @item{@DFlag{type} @nonterm{type} or @Flag{t} @nonterm{type} --- specifies an interpretation of the package source,
       where @nonterm{type} is either @exec{file}, @exec{dir}, @exec{file-url}, @exec{dir-url}, @exec{github}, 
       or @exec{name}.}

 @item{@DFlag{name} @nonterm{pkg} or @Flag{n} @nonterm{pkg} --- specifies the name of the package,
       which makes sense only when a single @nonterm{pkg-source} is provided. The name is normally
       inferred for each @nonterm{pkg-source}.}

 @item{@DFlag{checksum} @nonterm{checksum} --- specifies a checksum for the package,
       which normally makes sense only when a single @nonterm{pkg-source} is provided. The use of
       @nonterm{checksum} depends on @nonterm{pkg-source}: for a GitHub source, @nonterm{checksum} selects a checksum;
       for a @tech{package name}, file path, or remote URL as a source, @nonterm{checksum} specifies an expected checksum;
       for a directory path (including a remote directory URL without a @filepath{.CHECKSUM} file) as a source,
       @nonterm{checksum} assigns a checksum.}

 @item{@DFlag{deps} @nonterm{behavior} --- Selects the behavior for dependencies, where @nonterm{behavior} is one of
  @itemlist[
   @item{@exec{fail} --- Cancels the installation if dependencies are uninstalled or version requirements are unmet. 
        This behavior is the default for a @nonterm{pkg-source} that is not a @tech{package name}.}
   @item{@exec{force} --- Installs the package(s) despite missing dependencies or version requirements.
         Forcing an installation may leave package content in an inconsistent state.}
   @item{@exec{search-ask} --- Looks for dependencies (when uninstalled) or updates (when version requirements are unmet)
         via the configured @tech{package catalogs},
         but asks if you would like the packages installed or updated. This behavior is the default for a
         @nonterm{pkg-source} that is a @tech{package name}.}
   @item{@exec{search-auto} --- Like @exec{search-ask}, but does not ask for permission to install or update.}
  ]}

  @item{@DFlag{auto} --- Shorthand for @exec{@DFlag{deps} search-auto}.}

  @item{@DFlag{update-deps} --- With @exec{search-ask} or @exec{search-auto} dependency behavior, checks
        already-installed dependencies transitively for updates (even when
        not forced by version requirements), asking or automatically updating a
        package when an update is available. When a package is updated or installed,
        unless @DFlag{skip-implies} is specified, any package that
        it implies (see @secref["metadata"]) is automatically updated independent of the behavior
        requested via @DFlag{update-deps} and @DFlag{deps}.}

  @item{@DFlag{skip-implies} --- Disables special treatment of dependencies that are listed
        in @racketidfont{implies} (see @secref["metadata"]) for an installed or updated package.}

  @item{@DFlag{link} --- Implies @exec{--type dir} (and overrides any specified type),
        and links the existing directory as an installed package, instead of copying the
        directory's content to install. Directory @tech{package sources} are treated as links
        by default, unless @DFlag{copy} is specified.

        The package is identified
        as a @tech{single-collection package} or a @tech{multi-collection package} at the
        time that it is installed, and that categorization does not change even if the @schemeidfont{collection}
        definition in @filepath{info.rkt} is changed (i.e., he package must be removed and re-installed
        for the change to take effect).}

  @item{@DFlag{static-link} --- Implies @DFlag{link}, and also indicates that subdirectories
        of the given directory will not change for each given directory that implements a
        @tech{multi-collection package}.}

  @item{@DFlag{pkgs} --- Disables default installation of the current directory when no @nonterm{pkg-source}s
        are supplied.}

  @item{@DFlag{copy} --- Disables default handling of directory @tech{package sources} as links,
        and instead treats them like other sources: package content is copied to install.}

  @item{@DFlag{binary} --- Strips source elements of a package before installing, and implies @DFlag{copy}.}

  @item{@DFlag{source} --- Strips built elements of a package before installing, and implies @DFlag{copy}.}

 @item{@DFlag{scope} @nonterm{scope} --- Selects the @tech{package scope} for installation, where @nonterm{scope} is one of
  @itemlist[
   @item{@exec{installation} --- Install packages for all users of a Racket installation, rather than user-specific.}
   @item{@exec{user} --- Install packages for the current user and current installation's name/version.}
  ]
  The default package scope is normally @exec{user}, but it can be configured with
  @command-ref{config}@exec{ --set default-scope @nonterm{scope}}.
  The default installation name is normally the Racket version, but it can be configured with
  @command-ref{config}@exec{ --set name @nonterm{name}}.}
 @item{@Flag{i} or @DFlag{installation} --- Shorthand for @exec{--scope installation}.}
 @item{@Flag{u} or @DFlag{user} --- Shorthand for @exec{--scope user}.}
 @item{@DFlag{scope-dir} @nonterm{dir} --- Select @nonterm{dir} as the @tech{package scope}.}
 
 @item{@DFlag{catalog} @nonterm{catalog} --- Use @nonterm{catalog} instead of of the currently configured 
       @tech{package catalogs}.}

  @item{@DFlag{skip-installed} --- Ignore any @nonterm{pkg-source}
        whose name corresponds to an already-installed package, except for promoting auto-installed
        packages to explicitly installed.}

  @item{@DFlag{all-platforms} --- Considers package dependencies independent of the current platform
        (instead of filtering dependencies to platforms other than the current one).}

  @item{@DFlag{force} --- Ignores module conflicts, including conflicts due to installing a single
        package in multiple scopes. Forcing an installation may leave package content in an
        inconsistent state.}

  @item{@DFlag{ignore-checksums} --- Ignores errors verifying package @tech{checksums} (unsafe).}

  @item{@DFlag{strict-doc-conflicts} --- Refuses to install in user @tech{package scope} when
        documentation-directory names would conflict with other packages. ``Conflicting''
        documentation names are normally allowed for installation in user @tech{package scope},
        but strict checking ensures that installation would succeed in other @tech{package scopes}.}

  @item{@DFlag{no-cache} --- Disables use of the download cache.}

  @item{@DFlag{no-setup} --- Does not run @exec{raco setup} after installation. This behavior is also the case if the
        environment variable @envvar{PLT_PKG_NOSETUP} is set to any non-empty value.}

  @item{@DFlag{jobs} @nonterm{n} or @Flag{j} @nonterm{n} --- Install and setup with @nonterm{n} parallel jobs.}

  @item{@DFlag{fail-fast} --- Breaks @exec{raco setup} as soon as any error is encountered.}
 ]



@subcommand{@command/toc{update} @nonterm{option} ... @nonterm{pkg-source} ... 
--- Checks the specified package names for @tech{package updates} or
replaces existing package installations with the given sources. If an
update or replacement cannot be installed (e.g. it conflicts with
another installed package), then this command fails without installing
any of the @nonterm{pkg-source}s (or their dependencies).

If a @tech{package scope} is not specified, the scope is inferred from
the given @nonterm{pkg-source}s.

 The @exec{update} sub-command accepts 
 the following @nonterm{option}s:

 @itemlist[
 @item{@DFlag{all} or @Flag{a} --- Update all packages, if no packages are given in the argument list.}

 @item{@DFlag{lookup} --- Checks Causes a @tech{package name} as a @nonterm{pkg-source} to be used
       as a replacement, instead of the name of a installed package that may have updates.
       (If the named package was installed through a package name, then there's effectively
        no difference.)}

 @item{@DFlag{type} @nonterm{type} or @Flag{t} @nonterm{type} --- Same as for @command-ref{install}.}
 @item{@DFlag{name} @nonterm{pkg} or @Flag{n} @nonterm{pkg} --- Same as for @command-ref{install}.}
 @item{@DFlag{checksum} @nonterm{checksum} --- Same as for @command-ref{install}.}
 @item{@DFlag{deps} @nonterm{behavior} --- Same as for @command-ref{install}.}
 @item{@DFlag{auto} --- Shorthand for @exec{@DFlag{deps} search-auto} plus @DFlag{update-deps}.}
 @item{@DFlag{update-deps} --- Same as for @command-ref{install}, but
       implied by @DFlag{auto} only for @command-ref{update}.}
 @item{@DFlag{skip-implies} --- Same as for @command-ref{install}.}
 @item{@DFlag{link} --- Same as for @command-ref{install}.}
 @item{@DFlag{static-link} --- Same as for @command-ref{install}.}
 @item{@DFlag{binary} --- Same as for @command-ref{install}.}
 @item{@DFlag{copy} --- Same as for @command-ref{install}.}
 @item{@DFlag{source} --- Same as for @command-ref{install}.}
 @item{@DFlag{scope} @nonterm{scope} --- Selects a @tech{package scope}, the same as for @command-ref{install}.}
 @item{@Flag{i} or @DFlag{installation} --- Shorthand for @exec{--scope installation}.}
 @item{@Flag{u} or @DFlag{user} --- Shorthand for @exec{--scope user}.}
 @item{@DFlag{scope-dir} @nonterm{dir} --- Selects @nonterm{dir} as the @tech{package scope}, the same as for @command-ref{install}.}
 @item{@DFlag{catalog} @nonterm{catalog} --- Same as for @command-ref{install}.}
 @item{@DFlag{all-platforms} --- Same as for @command-ref{install}.}
 @item{@DFlag{force} --- Same as for @command-ref{install}.}
 @item{@DFlag{ignore-checksums} --- Same as for @command-ref{install}.}
 @item{@DFlag{strict-doc-conflicts} --- Same as for @command-ref{install}.}
 @item{@DFlag{no-cache} --- Same as for @command-ref{install}.}
 @item{@DFlag{no-setup} --- Same as for @command-ref{install}.}
 @item{@DFlag{jobs} @nonterm{n} or @Flag{j} @nonterm{n} --- Same as for @command-ref{install}.}
 ]
}

@subcommand{@command/toc{remove} @nonterm{option} ... @nonterm{pkg} ... 
--- Attempts to remove the given packages. By default, if a package is the dependency
of another package that is not listed, this command fails without 
removing any of the @nonterm{pkg}s.

If a @tech{package scope} is not specified, the scope is inferred from
the given @nonterm{pkg}s.

 The @exec{remove} sub-command accepts 
 the following @nonterm{option}s:

 @itemlist[
 @item{@DFlag{demote} --- ``Remove'' explicitly installed packages by demoting them to auto-installed
                            (leaving auto-installed packages as such). Combined with @DFlag{auto}, removes
                            packages for which there are no dependencies.}
 @item{@DFlag{force} --- Ignore dependencies when removing packages.}
 @item{@DFlag{auto} --- In addition to removing each @nonterm{pkg},
                        remove auto-installed packages (i.e., installed by the @exec{search-auto} or @exec{search-ask}
                        dependency behavior, or demoted via @DFlag{demote}) that are no longer required by any
                        explicitly installed package.}
 @item{@DFlag{scope} @nonterm{scope} --- Selects a @tech{package scope}, the same as for @command-ref{install}.}
 @item{@Flag{i} or @DFlag{installation} --- Shorthand for @exec{--scope installation}.}
 @item{@Flag{u} or @DFlag{user} --- Shorthand for @exec{--scope user}.}
 @item{@DFlag{scope-dir} @nonterm{dir} --- Selects @nonterm{dir} as the @tech{package scope}, the same as for @command-ref{install}.}
 @item{@DFlag{no-setup} --- Same as for @command-ref{install}.}
 @item{@DFlag{jobs} @nonterm{n} or @Flag{j} @nonterm{n} --- Same as for @command-ref{install}.}
 ]
}

@subcommand{@command/toc{show} @nonterm{option} ... --- Print information about currently installed packages. 
 By default, packages are shown for all @tech{package scopes}, but only for packages
 not marked as auto-installed to fulfill dependencies.

 The @exec{show} sub-command accepts 
 the following @nonterm{option}s:

 @itemlist[

 @item{@Flag{a} or @DFlag{all} --- Includes auto-installed packages in the listing.} 
 @item{@Flag{d} or @DFlag{dir} --- Adds a column in the output for the directory where the package is installed.} 

 @item{@DFlag{scope} @nonterm{scope} --- Shows only packages in @nonterm{scope}, which is one of
  @itemlist[
   @item{@exec{installation} --- Show only installation-wide packages.}
   @item{@exec{user} --- Show only user-specific packages for the current installation's name/version
            or the name/version specified with @DFlag{version} or @Flag{v}.}
  ]
  The default is to show packages for all @tech{package scopes}.}
 @item{@Flag{i} or @DFlag{installation} --- Shorthand for @exec{--scope installation}.}
 @item{@Flag{u} or @DFlag{user} --- Shorthand for @exec{--scope user}.}
 @item{@DFlag{scope-dir} @nonterm{dir} --- Shows only packages installed in @nonterm{dir}.}
 @item{@DFlag{version} @nonterm{vers} or @Flag{v} @nonterm{vers} --- Show only user-specific packages for 
       the installation name/version @nonterm{vers}.}
 ]
}

@subcommand{@command/toc{migrate} @nonterm{option} ... @nonterm{from-version}
 --- Installs packages that were previously installed in @exec{user}
     @tech{package scope} for @nonterm{from-version}, where
     @nonterm{from-version} is an installation name/version.

 The @exec{migrate} sub-command accepts 
 the following @nonterm{option}s:
 @itemlist[

 @item{@DFlag{deps} @nonterm{behavior} --- Same as for @command-ref{install}, except that @exec{search-auto} is
       the default.}
  @item{@DFlag{binary} --- Same as for @command-ref{install}.}
  @item{@DFlag{source} --- Same as for @command-ref{install}.}
  @item{@DFlag{scope} @nonterm{scope} --- Same as for @command-ref{install}.}
  @item{@Flag{i} or @DFlag{installation} --- Shorthand for @exec{--scope installation}.}
  @item{@Flag{u} or @DFlag{user} --- Shorthand for @exec{--scope user}.}
  @item{@DFlag{scope-dir} @nonterm{dir} --- Select @nonterm{dir} as the @tech{package scope}.}
  @item{@DFlag{catalog} @nonterm{catalog} --- Same as for @command-ref{install}.}
  @item{@DFlag{all-platforms} --- Same as for @command-ref{install}.}
  @item{@DFlag{force} --- Same as for @command-ref{install}.}
  @item{@DFlag{ignore-checksums} --- Same as for @command-ref{install}.}
  @item{@DFlag{strict-doc-conflicts} --- Same as for @command-ref{install}.}
  @item{@DFlag{no-cache} --- Same as for @command-ref{install}.}
  @item{@DFlag{no-setup} --- Same as for @command-ref{install}.}
  @item{@DFlag{jobs} @nonterm{n} or @Flag{j} @nonterm{n} --- Same as for @command-ref{install}.}
 ]
}

@subcommand{@command/toc{create} @nonterm{option} ... @nonterm{directory-or-package}
--- Bundles a package into an archive. Bundling
    is not needed for a package that is provided directly from a
    GitHub repository or other non-archive formats. The @exec{create}
    sub-command can create an archive from a directory (the default) or
    from an installed package. It can also adjust the archive's content
    to include only sources, only compiled bytecode and rendered documentation,
    or both---but packages are
    normally provided as source and converted to binary form by an
    automatic service, instead of by a package author.

 The @exec{create} sub-command accepts 
 the following @nonterm{option}s:

 @itemlist[
 @item{@DFlag{from-dir} --- Treat @nonterm{directory-or-package} as a directory path; this is the default mode.}
 @item{@DFlag{from-install} --- Treat @nonterm{directory-or-package} as the name of an installed package 
       (instead of a directory).}
 @item{@DFlag{format} @nonterm{format} --- Specifies the archive format. 
      The allowed @nonterm{format}s are: @exec{zip} (the default), @exec{tgz}, and @exec{plt}. 
      This option must be specified if @DFlag{manifest} is not present.}
 @item{@DFlag{manifest} --- Creates a manifest file for a directory, rather than an archive.}
 @item{@DFlag{as-is} --- Bundle all content of the package directory as is, with no filtering
       of sources, compiled files, or repository elements.}
 @item{@DFlag{source} --- Bundle only sources in the package directory; see @secref["strip"].}
 @item{@DFlag{binary} --- Bundle compiled bytecode and rendered
       documentation in the package directory; see @secref["strip"].}
 @item{@DFlag{built} --- Bundle compiled sources, bytecode, and rendered
       documentation in the package directory, filtering repository elements; see @secref["strip"].}
  @item{@DFlag{dest} @nonterm{dest-dir} --- Writes generated bundles to @nonterm{dest-dir}.}
  ]
}

@subcommand{@command/toc{config} @nonterm{option} ... @optional[@nonterm{key}] @nonterm{val} ... --- 
View and modify the configuration of the package manager. If @nonterm{key} is not provided,
the values for all recognized keys are shown. The @nonterm{val} arguments are allowed
only when @DFlag{set} is used, in which case the @nonterm{val}s are used as the new values
for @nonterm{key}.

 The @exec{config} sub-command accepts 
 with the following @nonterm{option}s:

 @itemlist[
 @item{@DFlag{set} --- Sets an option, rather than printing it.}
 @item{@DFlag{scope} @nonterm{scope} --- Selects a @tech{package scope}, the same as for @command-ref{install}.
                                         A configuration value set at @exec{installation} scope serves
                                         as the default value at @exec{user} scope.}
 @item{@Flag{i} or @DFlag{installation} --- Shorthand for @exec{--scope installation}.}
 @item{@Flag{u} or @DFlag{user} --- Shorthand for @exec{--scope user}.}
 @item{@DFlag{scope-dir} @nonterm{dir} --- Same as for @command-ref{install}.}
 ]

 The valid @nonterm{key}s are:
 @itemlist[
  @item{@exec{name} --- A string for the installation's name, which is used by @exec{user}
        @tech{package scope} and defaults to the Racket version.}
  @item{@exec{catalogs} --- A list of URLs for @tech{package catalogs}.}
  @item{@exec{default-scope} --- Either @exec{installation} or @exec{user}.
        The value of this key at @exec{user} scope (possibly defaulting from
        @exec{installation} scope) is
        the default @tech{package scope} for @exec{raco pkg} commands for which
        a scope is not inferred from a given set of package names
        (even for @command{config}, which is consistent but potentially confusing).}
  @item{@exec{download-cache-dir} --- A directory that holds copies of
        downloaded packages, used to avoid re-downloading if the
        same URL and checksum combination is requested again. The default cache directory is
        user-specific (but not specific to a Racket version or
        installation name).}
  @item{@exec{download-cache-max-files} --- A limit on the number of files to
        be kept in the download cache directory.}
  @item{@exec{download-cache-max-bytes} --- A limit on the total size of files
        that are kept in the download cache directory.}
  @item{@exec{doc-open-url} --- A URL to use in place of a local
        filesystem path for viewing (or at least searching)
        documentation; an empty string, which is the default, disables
        the URL so that the local filesystem is used. This key can be
        set only in @exec{installation} scope.}
 ]
}

@subcommand{@command/toc{catalog-show} @nonterm{option} ... @nonterm{package-name} ...
--- Consults @tech{package catalogs} for a package (that is not necessarily installed)
    and displays the catalog's information for the package, such as its source URL and
    a checksum.

 The @exec{catalog-show} sub-command accepts 
 the following @nonterm{option}s:

 @itemlist[
 @item{@DFlag{all} --- Show information for all available packages. When using this flag,
                      supply no @nonterm{packaee-name}s.}
 @item{@DFlag{only-names} --- Show only package names. This option is mainly useful with 
                              @DFlag{all}, but when a @nonterm{packaee-name} is provided,
                              catalogs are consulted to ensure that he package is available.}
 @item{@DFlag{modules} --- Show the modules that are implemented by a package.}
 @item{@DFlag{catalog} @nonterm{catalog} --- Query @nonterm{catalog} instead of the currently configured 
       @tech{package catalogs}.}
 @item{@DFlag{version} @nonterm{version} or @Flag{v} @nonterm{version} --- Query catalogs 
       for a result specific to @nonterm{version},
       instead of the installation's Racket version.}
 ]
}

@subcommand{@command/toc{catalog-copy} @nonterm{option} ... @nonterm{src-catalog} ... @nonterm{dest-catalog}
--- Copies information from the @tech{package catalog} named by @nonterm{src-catalog}s
    to a local database or directory @nonterm{dest-catalog},
    which can be used as a new @tech{package catalog}.

    The @nonterm{src-catalog}s can be remote or local, while @nonterm{dest-catalog} must be local
    (i.e., a directory path or a SQLite database path, as inferred from the path).
    If a @nonterm{src-catalog} or @nonterm{dest-catalog} does not start with a URL scheme, it is
    treated as a filesystem path. Information from multiple @nonterm{src-catalog}s is merged,
    with information from earlier @nonterm{src-catalog}s taking precedence over later 
    @nonterm{src-catalog}s.

    The @exec{catalog-copy} sub-command accepts
    the following @nonterm{option}s:

 @itemlist[
 @item{@DFlag{from-config} --- Adds the currently configured
       @tech{package catalogs} to the end of the @nonterm{src-catalog}s list.}
 @item{@DFlag{force} --- Replaces @nonterm{dest-catalog} if it exists already.}
 @item{@DFlag{merge} --- Adds to @nonterm{dest-catalog} if it exists already. By default,
                         information already in @nonterm{dest-catalog} takes precedence
                         over new information.}
 @item{@DFlag{override} --- Changes merging so that new information takes precedence
                         over information already in @nonterm{dest-catalog}.}
 @item{@DFlag{relative} --- Write package sources to @nonterm{dest-catalog} in relative-path form,
                            when possible.}
 @item{@DFlag{version} @nonterm{version} or @Flag{v} @nonterm{version} --- Copy catalog
       results specific to @nonterm{version}
       (for catalogs that make a distinction), instead of the installation's Racket version.}
 ]
}

@subcommand{@command/toc{catalog-archive} @nonterm{option} ... @nonterm{dest-dir} @nonterm{src-catalog} ...
--- Copies information from the @tech{package catalog} named by @nonterm{src-catalog}s
    to a @filepath{catalog} directory catalog in @nonterm{dest-dir}, and also copies
    all package sources to a @filepath{pkgs} directory in @nonterm{dest-dir}.

    Packages sources are downloaded and repacked as needed, so that
    all packages are written to the @filepath{pkgs} directory as
    @filepath{.zip} archives. This conversion may change the checksum
    on each archived package.

    The @exec{catalog-archive} sub-command accepts
    the following @nonterm{option}s:

 @itemlist[
 @item{@DFlag{from-config} --- Adds the currently configured 
       @tech{package catalogs} to the end of the @nonterm{src-catalog}s list.}
 @item{@DFlag{state} @nonterm{state-database} --- To enable incremental
       updating, Reads and writes the database @nonterm{state-database}, which must have the suffix
       @filepath{.sqlite}, as the current state of @nonterm{dest-dir}.}
 @item{@DFlag{relative} --- Write package sources to @nonterm{dest-catalog} in relative-path form.}
 @item{@DFlag{version} @nonterm{version} or @Flag{v} @nonterm{version} --- Copy catalog
       results specific to @nonterm{version}
       (for catalogs that make a distinction), instead of the installation's Racket version.}
 ]

 @history[#:added "6.0.17"]
}

@; ----------------------------------------

@section[#:tag "metadata"]{Package Metadata}

Package metadata, including dependencies on other packages, is reported
by an @filepath{info.rkt} module within the package. This module must be
implemented in the @racketmodname[info] language.

For example, a basic @filepath{info.rkt} file might be

@codeblock{
#lang info
(define version "1.0")
(define deps (list _package-source-string ...))
}

The following @filepath{info.rkt} fields are used by the package manager:

@itemlist[

 @item{@racketidfont{collection} --- either @racket['multi] to
       implement a @tech{multi-collection package} or a string or
       @racket['use-pkg-name] to implement a @tech{single-collection
       package}. If @racketidfont{collection} is defined as a string,
       then the string is used as the name of the collection
       implemented by the package. If @racketidfont{collection} is
       defined as @racket['use-pkg-name], then the package name is used
       as the package's collection name.

       Beware that omitting @racketidfont{collection} or defining it
       as @racket['use-pkg-name] means that a package's content
       effectively changes with the package's name. A package's
       content should normally be independent of the package's name,
       and so defining @racketidfont{collection} to a string is
       preferable for a @tech{single-collection package}.}

 @item{@racketidfont{version} --- a @tech{version} string. The default
       @tech{version} of a package is @racket["0.0"].}

 @item{@racketidfont{deps} --- a list of dependencies, where each
       dependency has one of the following forms:

       @itemlist[

         @item{A string for a @tech{package source}.}

         @item{A list of the form
               @racketblock[(list _package-source-string
                                  _keyword-and-spec ...)]
               where each @racket[_keyword-and-spec] has a 
               distinct keyword in the form
               @racketgrammar*[#:literals (quote)
                               [keyword-and-spec 
                                (code:line '#:version version-string)
                                (code:line '#:platform platform-spec)]
                               [platform-spec string symbol regexp]]

               A @racket[_version-string] specifies a lower bound
               on an acceptable @tech{version} of the needed package.

               A @racket[_platform-spec] indicates that the dependency
               applies only for platforms with a matching result from
               @racket[(system-type)] when @racket[_platforms-spec] is
               a symbol or @racket[(path->string
               (system-library-subpath #f))] when
               @racket[_platform-spec] is a string or regular expression.
               See also @racket[matching-platform?]. For
               example, platform-specific binaries can be placed into
               their own packages, with one separate package and one
               dependency for each supported platform.}

         @item{A list of the form
               @racketblock[(list _package-source-string _version-string)]
               which is deprecated and equivalent to
               @racketblock[(list _package-source-string '#:version _version-string)]}
        
        ]

       Each element of the @racketidfont{deps} list determines a
       dependency on the @tech{package} whose name is inferred from
       the @tech{package source} (i.e., dependencies are on package
       names, not package sources), while the @tech{package source}
       indicates where to get the package if needed to satisfy the
       dependency.

       Using the package name @racket["racket"] specifies a dependency
       on the Racket run-time system, which is potentially useful when
       a version is included in the dependency. For most purposes,
       it's better to specify a versioned dependency on
       @racket["base"], instead.}

 @item{@racketidfont{build-deps} --- like @racketidfont{deps}, but for
       dependencies that can be dropped in a @tech{binary package},
       which does not include sources; see @secref["strip"]. The
       @racketidfont{build-deps} and @racketidfont{deps} lists are
       appended, while @command-ref["create"] strips away
       @racketidfont{build-deps} when converting a package for
       @DFlag{binary} mode.}

 @item{@racketidfont{implies} --- a list of strings and
       @racket['core]. Each string refers to a package listed in the
       @racketidfont{deps} and indicates that a dependency on the
       current package counts as a dependency on the named package;
       for example, the @pkgname{gui} package is defined to ensure
       access to all of the libraries provided by @pkgname{gui-lib},
       so the @filepath{info.rkt} file of @pkgname{gui} lists
       @racket["gui-lib"] in @racketidfont{implies}. Packages listed
       in @racketidfont{implies} list are treated specially by
       updating: implied packages are automatically updated whenever
       the implying package is updated. The special value
       @racket['core] is intended for use by an appropriate
       @pkgname{base} package to declare it as the representative of
       core Racket libraries.}

 @item{@racketidfont{setup-collects} --- a list of path strings and/or
       lists of path strings, which are used as collection names to
       set up via @exec{raco setup} after the package is installed, or
       @racket['all] to indicate that all collections need to be
       setup. By default, only collections included in the package are
       set up (plus collections for global documentation indexes and
       links).}

]

@; ----------------------------------------

@include-section["strip.scrbl"]

@; ----------------------------------------

@include-section["apis.scrbl"]

@include-section["catalog-protocol.scrbl"]

@; ----------------------------------------

@section{@|Planet1| Compatibility}

PLT maintains a @tech{package catalog} to serve packages that
were developed using the original @seclink[#:doc '(lib
"planet/planet.scrbl") "top"]{@|Planet1|} package system.  This
compatibility catalog is at
@link["http://planet-compats.racket-lang.org/"]{http://planet-compats.racket-lang.org/},
which is included by default in the package-server search path.

Copies of @|Planet1| packages are automatically created by the
server according to the following system: for all packages that are in
the @litchar{4.x} @|Planet1| repository, the latest minor version of
@tt{@nonterm{user}/@nonterm{package}.plt/@nonterm{major-version}} will be available as
@pkgname{planet-@nonterm{user}-@nonterm{package}@nonterm{major-version}}. For example,
@tt{jaymccarthy/opencl.plt/1} minor version @tt{2}, will be available as
@pkgname{planet-jaymccarthy-opencl1}.

The contents of these copies is a single collection with the name
@filepath{@nonterm{user}/@nonterm{package}@nonterm{major-version}} with all the files from the
original @|Planet1| package in it.

Each file has been transliterated to use direct Racket-style requires
rather than @|Planet1|-style requires. For example, if any file contains
@racket[(planet jaymccarthy/opencl/module)], then it is transliterated
to @racket[jaymccarthy/opencl1/module]. @emph{This transliteration is
purely syntactic and is trivial to confuse, but works for most
packages, in practice.} Any transliterations that occurred are automatically added as
dependencies for the compatibility package.

We do not intend to improve this compatibility system much more over
time, because it is simply a stop-gap as developers port their @|Planet1|
packages to the new system. Additionally, the existence of the compatibility 
server is not meant
to imply that we will be removing @|Planet1| from existence in the near
future.

@; ----------------------------------------

@section[#:style 'quiet]{FAQ}

This section answers anticipated frequently asked questions about
the package manager.

@subsection{Are package installations versioned with respect to the
Racket version?}

Most Racket installations are configured to that installing a package
installs it for a specific user and a specific version of Racket. That
is, the @tech{package scope} is user- and version-specific. More
precisely, it is user-specific and installation-name-specific, where
an installation name is typically a Racket version.

You can change the default @tech{package scope} (for a particular
Racket installation) with @command-ref{config}@exec{ -i --set
default-scope installation}, in which case package operations apply
for all users of a Racket installation.  You can also use the @Flag{i}
or @DFlag{installation} flag with a specific @exec{raco pkg} command,
instead of changing the default scope for all uses of @exec{raco
pkg}. Note that an installation-wide package is not exactly
version-specific, because the version of an installation can change if
it corresponds to a source-code checkout that is periodically updated
and rebuilt.

If you change the default @tech{package scope}, you can use the
@Flag{u} or @DFlag{user} flag with a specific @exec{raco pkg} command
to perform the command with user-specific @tech{package scope}.

@subsection{Where and how are packages installed?}

User-specific and Racket-version-specific packages are in
@racket[(find-user-pkgs-dir)], and installation-wide packages
are in @racket[(find-pkgs-dir)]. They are linked as
collections (for single-collection packages) or collection roots (for
multi-collection packages) with @exec{raco link}.

@subsection{How are user-specific and installation-wide @tech{package scopes}
related for conflict checking?}

User-specific packages are checked against installation-wide packages
for package-name conflicts and provided-module
conflicts. Installation-wide packages are checked against
user-specific packages only for provided-module conflicts.

Beware that a conflict-free, installation-wide change by one user can
create conflicts for a different user.

@subsection{Do I need to change a package's version when I update a package with error fixes, @|etc|?}

If you have new code for a package, then it should have a new
@tech{checksum}. When package updates are searched for, the checksum
of the installed package is compared with the checksum of the source,
if they are different, then the source is re-installed. This allows
code changes to be distributed. You do not need to declare an update a
version number, except to allow other package implementors to indicate
a dependency on particular features (where a bug fix might be
considered a feature, but it is not usually necessary to consider it
that way).

@subsection{How can I specify which version of a package I depend on
if its interface has changed and I need an @emph{old} version?}

In such a situation, the author of the package has released a
backwards incompatible edition of a package. The package manager provides
no help to deal with this situation (other than, of course, not
installing the ``update''). Therefore, package authors should not make
backwards incompatible changes to packages. Instead, they should
release a new package with a new name. For example, package
@pkgname{libgtk} might become @pkgname{libgtk2}. These packages
should be designed to not conflict with each other, as well.

@subsection{How can I fix my installation to a specific set of package
implementations or @tech{checksums}?}

Packages are updated only when you run a tool such as
@command-ref{update}, so packages are never updated
implicitly. Furthermore, you can snapshot a set of package archives
and install from those archives, instead of relying on package name
resolution through a @tech{package catalog}.

If you want to control the resolution of package names (including
specific @tech{checksum}s) but not necessary keep a copy of all package
code (assuming that old @tech{checksum}s remain available, such as
through Github), you can create a snapshot of the @tech{package name}
to @tech{package source} mapping by using @command-ref{catalog-copy}.
For example,

@commandline{raco pkg catalog-copy --from-config /home/joe/snapshot.sqlite}

creates a snapshot @filepath{/home/joe/snapshot.sqlite} of the current
package name resolution, and then

@commandline{raco pkg config --set catalogs file:///home/joe/snapshot.sqlite}

directs all package-name resolution to the snapshot. You can configure
resolution for specific package names by editing the snapshot.

You can go even further with

@commandline{raco pkg catalog-archive --from-config /home/joe/snapshot/}

which not only takes a snapshot of the catalog, but also takes a
snapshot of all package sources (so that you do not depend on the
original sources).


@subsection{Why is the package manager so different than @|Planet1|?}

There are two fundamental differences between @|Planet1| and this package manager.

The first is that @|Planet1| uses ``internal linking'' whereas the current package manager
uses ``external linking.'' For example, an individual module requires a
@|Planet1| package directly in a require statement:

@racketblock[
 (require (planet game/tic-tac-toe/data/matrix))
]

whereas using the package manager, the module would simply require the module of
interest:

@racketblock[
 (require data/matrix)             
]

and would rely on the external system having the
@pkgname{tic-tac-toe} package installed.

This change is good because it makes the origin of modules more
flexible---so that code can migrate in and out of the core, packages
can easily be split up, combined, or taken over by other authors, etc.

This change is bad because it makes the meaning of your program
dependent on the state of the system.

The second major difference is that @|Planet1| is committed to
guaranteeing that packages that never conflict with one another, so
that any number of major and minor versions of the same package can be
installed and used simultaneously. The package manager does not share this
commitment, so package authors and users must be mindful of potential
conflicts and plan around them.

This change is good because it is simpler and lowers the burden of
maintenance (provided most packages don't conflict.)

The change is bad because users must plan around potential conflicts.

In general, the goal of the package manager is to be a lower-level
system, more like the package systems used by operating systems. The
goals of @|Planet1| are not bad, but we believe they are needed
infrequently and a system like @|Planet1| could be more easily built
atop the package manager than the reverse.

In particular, our plans to mitigate the downsides of these changes
are documented in @secref["short-term"].

@; ----------------------------------------

@section{Future Plans}

@subsection[#:tag "short-term"]{Short Term}

This section lists some short term plans for the package manager. These are
important, but didn't block its release. The package manager will be considered
out of beta when these are completed.

@itemlist[

@item{The official catalog server will divide packages into three
categories: @reponame{ring-0}, @reponame{ring-1}, and @reponame{ring-2}. The definitions
for these categories are:

 @itemlist[

  @item{@reponame{ring-2} --- No restrictions.}

  @item{@reponame{ring-1} --- Must not conflict any package
in @reponame{ring-1} or @reponame{ring-0}.}

  @item{@reponame{ring-0} --- Must not conflict any package in @reponame{ring-1}
or @reponame{ring-0}. Must have documentation and tests. The author must be
responsive about fixing regressions against changes in Racket, etc.}

 ]

These categories will be curated by PLT.

Our goal is for all packages to be in @reponame{ring-1}, with
@reponame{ring-2} as a temporary place while the curators work with the
authors of conflicting packages to determine how modules should be
renamed for unity.

However, before curation is complete, each package will be
automatically placed in @reponame{ring-2} or @reponame{ring-1}
depending on its conflicts, with preference being given to older
packages. (For example, if a new package B conflicts with an old
package A, then A will be in @reponame{ring-1}, but B will be in
@reponame{ring-2}.) During curation, however, it is not necessarily
the case that older packages have preference. (For example,
@pkgname{tic-tac-toe} should probably not provide
@filepath{data/matrix.rkt}, but that could be spun off into another
package used by both @pkgname{tic-tac-toe} and
@pkgname{factory-optimize}.)

In contrast, the @reponame{ring-0} category will be a special category that
authors may apply for. Admission requires a code audit and implies
a "stamp of approval" from PLT. In the future, packages in this
category will have more benefits, such as automatic regression testing
on DrDr, testing during releases, provided binaries, and advertisement
during installation.

The @|Planet1| compatibility packages will also be included in
the @reponame{ring-1} category, automatically. 

}

@item{In order to mitigate the costs of external linking vis a vis the
inability to understand code in isolation, we will create exception
printers that search for providers of modules on the configured
@tech{package catalogs}. For example, if a module requires
@filepath{data/matrix.rkt}, and it is not available, then the catalog will
be consulted to discover what packages provide it. @emph{Only packages
in @reponame{ring-1} or @reponame{ring-0} will be
returned.} (This category restriction ensures that the package to
install is unique.)

Users can configure their systems to then automatically install the
package provided is has the appropriate category (i.e., some users may
wish to automatically install @reponame{ring-0} packages but not
@reponame{ring-1} packages, while others may not want to install
any.)

This feature will be generalized across all @tech{package catalogs}, 
so users could maintain their own category definitions with
different policies.}

]

@subsection{Long Term}

This section lists some long term plans for the package manager. Many of these
require a lot of cross-Racket integration.

@itemlist[

@item{The official catalog server is bare bones. It could conceivably do a lot
more: keep track of more statistics, enable "social" interactions
about packages, link to documentation, problem reports, licenses,
etc. Some of this is easy and obvious, but the community's needs are
unclear.}

@item{It would be nice to encrypt information from the official
@tech{package catalog} with a public key shipped with Racket, and
allow other catalogs to implement a similar security scheme.}

@item{Packages in the @reponame{ring-0} category should be tested on
DrDr. This would require a way to communicate information about how
they should be run to DrDr. This is currently done via the
@filepath{meta/props} script for things in the core. We should
generalize this script to a @filepath{meta/props.d} directory so that
packages can install DrDr metadata to it.}

@item{We hope that this package system will encourage more incremental
improvements to pieces of Racket. In particular, it would be wonderful
to have a very thorough @filepath{data} collection of different
data-structures. However, our existing setup for Scribble would force
each new data structue to have a different top-level documentation
manual, rather than extending the documentation of the existing
@filepath{data} collection. Similar issues will exist for the
@filepath{net} and @filepath{file} collections. We should design a way
to have such "documentation plugins" in Scribble and support
similar "plugin" systems elsewhere in the code-base.}

@item{The user interface could be improved, including integration with
DrRacket and a GUI. For example, it would be good if DrRacket would
poll for package updates periodically and if when it was first started
it would display available, popular packages.}

]

@; ----------------------------------------

@include-section["implementation.scrbl"]
