#lang scribble/manual
@(require "common.rkt"
          (for-label pkg/strip
                     setup/dirs))

@title[#:tag "strip"]{Source, Binary, and Built Packages}

A package, especially in a repository format, normally provides module
implementations and documentation in source form. Such @deftech{source
packages} may work with multiple Racket versions, and modules are
compiled to bytecode and documentation is rendered when the package is
installed.

A @deftech{binary package} provides only compiled bytecode and
rendered documentation, instead of package and documentation
sources. Since compiled bytecode is specific to a version of Racket, a
@tech{binary package} is specific to a version of Racket. The benefit
of a binary package is that it can have fewer dependencies (e.g., on
Scribble to implement the documentation or on other
packages whose documentation is referenced) and it can be installed
faster. A drawback of a binary package is that it is version-specific
and the source code may be less immediately accessible to other
programmers.

A @deftech{binary library package} is like a @tech{binary package},
but it further omits documentation.

A @deftech{built package} combines source and compiled elements. A
@tech{built package} can be installed more quickly than source, as
long as it is installed for a suitable Racket version, but the source
remains available as a back-up for other programmers to consult or to
re-build for a different Racket version.

Programmers normally supply only @tech{source packages}, while a
package catalog service may convert each source package to a
@tech{binary package}, @tech{binary library package}, or @tech{built package}. Alternatively,
programmers can create @tech{binary packages}, @tech{binary library packages}, or @tech{built packages}
by using the @command-ref{create} subcommand with @DFlag{binary}, @DFlag{binary-lib}, or
@DFlag{built}. As a convenience, the @command-ref{create} subcommand
can also create any kind of package from an installed package or
repository checkout, dropping repository elements (such as a
@filepath{.git} directory) and compiled code. Note that
@command-ref{create} by default bundles a package directory as-is,
with no filtering or annotation.

Although a package can be specifically annotated as a @tech{source package},
@tech{binary package}, @tech{binary library package}, or @tech{built
package} (see @racketidfont{package-content-state} in @secref["metadata"]), the different kinds of
packages are primarily just conventions based on the content of the package. All
forms of packages can be mixed in an installation, and a package can
be updated from any form to any other form. Furthermore,
@exec{raco pkg install} and @exec{raco pkg update} support
@DFlag{source}, @DFlag{binary}, @DFlag{binary-lib} flags to convert
to a package on installation;
in that case, the package's existing annotation is checked to verify that it
is consistent with the requested conversion.

Creating a @tech{source package}, @tech{binary package}, @tech{binary library package}, or
@tech{built package} from a directory or package installation prunes
the following files and directories:

@itemlist[

 @item{directories/files named @filepath{.svn};}

 @item{directories/files whose names start with @filepath{.git};}

 @item{directories/files whose names end with @filepath{~}; and}

 @item{directories/files whose names start and end with @filepath{#}.}

]

Any of the above can be suppressed, however, by a
@racket[source-keep-files] (for @tech{source package} and @tech{built
package} bundling), @racket[binary-keep-files] (for @tech{binary
package}, @tech{binary library package} @tech{built package} bundling),
or @racket[binary-lib-keep-files] (for @tech{binary library package} bundling) definition in an
@filepath{info.rkt} in the package or any subdirectory. A
@racket[source-keep-files], @racket[binary-keep-files], or @racket[binary-lib-keep-files] definition
should bind the name to a list of paths relative to the
@filepath{info.rkt} file.

Creating a @tech{source package} prunes the following additional files
and directories:

@itemlist[

 @item{directories/files named @filepath{compiled};}

 @item{directories/files named @filepath{doc};}

 @item{directories/files named @filepath{synced.rktd}, which can appear
       as a marker in rendered documentation;}
               
 @item{directories/files named in an @filepath{info.rkt} file's
       @racket[source-omit-files] definition.}

]

Any of the above removals can be suppressed through
@racketidfont{source-keep-files}---even for files and directories
within an otherwise removed directory.

Creating a @tech{binary package} prunes the following additional files
and directories:

@itemlist[

 @item{directories/files with names ending in @filepath{.rkt} or
       @filepath{.ss} for which a corresponding compiled bytecode file
       is present (in a @filepath{compiled} subdirectory), not
       counting @filepath{info.rkt};}

 @item{directories/files with names ending in @filepath{.scrbl},
       @filepath{_scrbl.zo}, or @filepath{.dep};}

 @item{directories/files ending with @filepath{.css} or @filepath{.js}
       immediately within a directory named @filepath{doc};}

 @item{directories/files named @filepath{tests} or
       @filepath{scribblings} (but see the exception below for
       @filepath{doc} and @filepath{info.rkt});}

 @item{directories/files named in an @filepath{info.rkt} file's
       @racket[binary-omit-files] definition.}

]

Any of the above removals can be suppressed through
@racketidfont{binary-keep-files}---even files and directories within
an otherwise removed directory. Furthermore, a @filepath{doc} or
@filepath{info.rkt} directory/file is kept when it is within a
@filepath{scribblings} directory and not within a @filepath{tests}
directory. Creating a @tech{binary package} further adjusts the
following files (when they are not pruned):

@itemlist[

 @item{for any file whose name ends in @filepath{.zo}, submodules
       named @racketidfont{test}, @racketidfont{doc}, or
       @racketidfont{srcdoc} are removed;}

 @item{for each @filepath{.html} file that refers to a
       @filepath{local-redirect.js} script, the path to the script is
       removed;}

 @item{each @filepath{info.rkt} is adjusted as follows: an
       @racket[assume-virtual-sources] definition is added, any
       @racket[copy-foreign-libs] definition is changed to
       @racket[move-foreign-libs], any
       @racket[copy-shared-files] definition is changed to
       @racket[move-shared-files], any @racket[copy-man-pages]
       definition is changed to @racket[move-man-pages], any
       @racket[build-deps] definition is removed, any
       @racket[update-implies] definition is removed, and
       a @racket[package-content-state] is added to changed to
       @racket[(list 'binary (version))]; and}

 @item{each collection within the path gets an @filepath{info.rkt} if
       it did not have one already, so that
       @racket[assume-virtual-sources] can be defined.}

]

Creating a @tech{binary library package} prunes the following
additional files and directories and makes additional adjustments
compared to a @tech{binary package}:

@itemlist[

 @item{directories/files named @filepath{doc} are removed;}

 @item{directories/files named in an @filepath{info.rkt} file's
       @racket[binary-lib-omit-files] definition are removed; and}

 @item{each @filepath{info.rkt} is adjusted to remove any
       @racket[scribblings] definition, and
       @racket[package-content-state] is adjusted to @racket[(list
       'binary-lib (version))].}

]

Any of the removals can be suppressed through
@racketidfont{binary-lib-keep-files}, in addition to suppressions
through @racketidfont{binary-keep-files}.

Creating a @tech{built package} removes any file or directory that
would be removed for a @tech{source package} @emph{and} @tech{binary
package}, it performs the @filepath{.html} file updating of a
@tech{binary package}, and the package's @filepath{info.rkt} file
(added if it does not exist already) is adjusted to define
@racket[package-content-state] as @racket[(list 'built (version))].

Finally, creating a @tech{binary package}, @tech{binary library
package}, or @tech{built package} ``unmoves'' files that were
installed via @racket[move-foreign-libs], @racket[move-shared-files],
or @racket[move-man-pages] definitions in an @filepath{info.rkt} file,
retrieving them if they are not present at referenced location but are
present in a user-specific target directory (i.e., the directory
reported by @racket[find-user-lib-dir], @racket[find-user-share-dir],
or @racket[find-user-man-dir], respectively). On Mac OS X, when an
unmoved file for @racket[move-foreign-libs] is a Mach-O file that
includes a reference to another library in one of the directories reported by
@racket[(get-lib-search-dirs)], then the reference is changed to a
@litchar{@"@"loader_path/} reference.

@defmodule[pkg/strip]{The @racketmodname[pkg/strip] module provides
support for copying a package-style directory to a given destination
with the same file/directory omissions and updates as
@command-ref{create}.}

@defproc[(generate-stripped-directory [mode (or/c 'source 'binary 'binary-lib 'built)]
                                      [src-dir path-string?]
                                      [dest-dir path-string?])
          void?]{

Copies @racket[src-dir] to @racket[dest-dir] with file/directory
omissions and updates corresponding the creation of a @tech{source
package}, @tech{binary package}, @tech{binary library package}, or @tech{built package} as indicated
by @racket[mode].}


@defproc[(check-strip-compatible [mode (or/c 'source 'binary 'binary-lib 'built)]
                                 [pkg-name string?]
                                 [dir path-string?]
                                 [error (string? . -> . any)])
          any]{

Check whether the content of @racket[dir] is consistent with the given
@racket[mode] conversion according to the content of a
@filepath{info.rkt} file in @racket[dir]. If not, @racket[error] is
called with an error-message string to describe the mismatch.}


@defproc[(fixup-local-redirect-reference [file path-string?]
                                         [js-path string?])
         void?]{

Assuming that @racket[file] is an HTML file for documentation, adjusts
the URL reference to @filepath{local-redirect.js}, if any, to use the
prefix @racket[js-path].}


@defboolparam[strip-binary-compile-info compile?]{

A parameter that determines whether @filepath{info.rkt} files are
included in bytecode form when converting package content for a
@tech{binary packages}, @tech{binary library packages}, and
@tech{built packages}.}

