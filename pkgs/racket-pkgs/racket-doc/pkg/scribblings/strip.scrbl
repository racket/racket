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

A @deftech{built package} combines source and compiled elements. A
@tech{built package} can be installed more quickly than source, as
long as it is installed for a suitable Racket version, but the source
remains available as a back-up for other programmers to consult or to
re-build for a different Racket version.

A package is not specifically tagged as a @tech{source package},
@tech{binary package}, or @tech{built package}. The different kinds of
packages are just conventions based on the content of the package. All
forms of packages can be mixed in an installation, and a package can
be updated from any form to any other form.

Programmers normally supply only @tech{source packages}, while a
package catalog service may convert each source package to a
@tech{binary package} or @tech{built package}. Alternatively,
programmers can create @tech{binary packages} or @tech{built packages}
by using the @command-ref{create} subcommand with @DFlag{binary} or
@DFlag{built}. As a convenience, the @command-ref{create} subcommand
can also create a @tech{source package} from an installed package or
repository checkout, dropping repository elements (such as a
@filepath{.git} directory) and compiled code. Note that
@command-ref{create} by default bundles a package directory as-is,
with no filtering at all.

Creating a @tech{source package}, @tech{binary package}, or
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
package} bundling) or @racket[binary-keep-files] (for @tech{binary
package} and @tech{built package} bundling) definition in an
@filepath{info.rkt} in the package or any subdirectory. A
@racket[binary-keep-files] or @racket[binary-keep-files] definition
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
 @filepath{.ss} for which a corresponding compiled bytecode file is
 present (in a @filepath{compiled} subdirectory);}

 @item{directories/files with names ending in @filepath{.scrbl},
       @filepath{_scrbl.zo}, or @filepath{.dep};}

 @item{directories/files ending with @filepath{.css} or @filepath{.js}
       immediately within a directory named @filepath{doc};}

 @item{directories/files named @filepath{tests} or
       @filepath{scribblings} (but see the exception below for
       @filepath{doc} and @filepath{info.rkt};}

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
       removed; and}

 @item{each @filepath{info.rkt} is adjusted as follows: an
       @racket[assume-virtual-sources] definition is added, any
       @racket[copy-foreign-libs] definition is changed to
       @racket[move-foreign-libs], any
       @racket[copy-shared-files] definition is changed to
       @racket[move-shared-files], any @racket[copy-man-pages]
       definition is changed to @racket[move-man-pages], and any
       @racket[build-deps] definition is removed.}

]

Creating a @tech{built package} removes any file or directory that
would be removed for a @tech{source package} and @tech{binary
package}, and it performs the @filepath{.html} file updating of a
@tech{binary package}.

Finally, creating @tech{built package} or @tech{source package}
``unmoves'' files that were installed via @racket[move-foreign-libs],
@racket[move-shared-files], or @racket[move-man-pages] definitions in
an @filepath{info.rkt} file, retrieving them if they are not present
at referenced location but are present in a user-specific target
directory (i.e., the directory reported by @racket[find-user-lib-dir],
@racket[find-user-share-dir], or @racket[find-user-man-dir],
respectively).

@defmodule[pkg/strip]{The @racketmodname[pkg/strip] module provides
support for copying a package-style directory to a given destination
with the same file/directory omissions and updates as
@command-ref{create}.}

@defproc[(generate-stripped-directory [mode (or/c 'source 'binary 'built)]
                                      [src-dir path-string?]
                                      [dest-dir path-string?])
          void?]{

Copies @racket[src-dir] to @racket[dest-dir] with file/directory
omissions and updates corresponding the creation of a @tech{source
package}, @tech{binary package}, or @tech{built package} as indicated
by @racket[mode].}


@defproc[(fixup-local-redirect-reference [file path-string?]
                                         [js-path string?])
         void?]{

Assuming that @racket[file] is an HTML file for documentation, adjusts
the URL reference to @filepath{local-redirect.js}, if any, to use the
prefix @racket[js-path].}
