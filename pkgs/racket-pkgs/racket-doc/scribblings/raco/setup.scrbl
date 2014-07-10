#lang scribble/doc

@(require scribble/manual
          scribble/bnf
          "common.rkt"
         (for-label racket
                    racket/future
                    setup/setup-unit
                    setup/option-unit
                    setup/option-sig
                    setup/dirs
                    setup/getinfo
                    setup/main-collects
                    setup/collection-name
                    setup/matching-platform
                    setup/path-to-relative
                    setup/xref scribble/xref
                    ;; info -- no bindings from this are used
                    (only-in info)
                    setup/pack
                    setup/unpack
                    setup/link
                    compiler/compiler
                    launcher/launcher
                    compiler/sig
                    launcher/launcher-sig
                    dynext/file-sig
                    racket/gui/base
                    racket/future
                    mrlib/terminal
                    (only-in ffi/unsafe ffi-lib)
                    racket/path
                    setup/collects
                    syntax/modcollapse
                    pkg/path))

@(define-syntax-rule (local-module mod . body)
   (begin
     (define-syntax-rule (go)
       (begin
         (require (for-label mod))
         . body))
     (go)))

@(define ref-src
   '(lib "scribblings/reference/reference.scrbl"))

@(define setup-plt @exec{setup-plt})

@(define (defaults v) 
   @elem{The default is @|v|.})

@title[#:tag "setup" #:style 'toc]{@exec{raco setup}: Installation Management}

The @exec{raco setup} command builds bytecode, documentation,
executables, and metadata indexes for all installed collections.

The collections that are built by @exec{raco setup} can be part of the
original Racket distribution, installed via the package manager (see
@other-manual['(lib "pkg/scribblings/pkg.scrbl")]), installed via
@|PLaneT| (see @other-manual['(lib "planet/planet.scrbl")]), linked
via @exec{raco link}, in a directory that is listed in the
@envvar{PLTCOLLECTS} environment variable, or placed into one of the
default collection directories.

The @exec{raco setup} tool itself does not directly support the
installation of collections, except through the now-discouraged
@Flag{A} flag (see @secref["raco-setup-A"]). The @exec{raco setup} command is
used by installation tools such as the package manager or @|PLaneT|.
Programmers who modify installed collections may find it useful to run
@exec{raco setup} as an alternative to un-installing and re-installing
a set of collections.

@local-table-of-contents[]

@; ------------------------------------------------------------------------

@section[#:tag "running"]{Running @exec{raco setup}}

With no command-line arguments, @exec{raco setup} finds all of the
current collections---see @secref[#:doc ref-src]{collects}---and
compiles libraries in each collection.  (Directories that are named
@filepath{.git} or @filepath{.svn} are not treated as collections.)

To restrict @exec{raco setup} to a set of collections, provide the
collection names as arguments. For example, @exec{raco setup
scribblings/raco} would only compile and render the documentation for
@exec{raco}, which is implemented in a @filepath{scribblings/raco}
collection.

An optional @filepath{info.rkt} within the collection can indicate
specifically how the collection's files are to be compiled and other
actions to take in setting up a collection, such as creating
executables or building documentation. See @secref["setup-info"] for
more information.

The @exec{raco setup} command accepts the following command-line
flags:

@itemize[

@item{Constraining to specified collections or @|PLaneT| packages:
@itemize[

 @item{@DFlag{only} --- restrict setup to specified collections and
   @|PLaneT| packages, even if none are specified. This mode is the
   default if any collection is specified as a command-line argument
   or through the @Flag{l}, @DFlag{pkgs}, or @Flag{P} flag.}

 @item{@Flag{l} @nonterm{collection} @racket[...] --- constrain setup
  actions to the specified @nonterm{collection}s (i.e., the same as
  providing @nonterm{collections}s without a flag, but with no
  possibility that a @nonterm{collection} is interpreted as a flag).}

 @item{@DFlag{pkgs} @nonterm{pkg} @racket[...] --- constrain setup
  actions to collections that are within (or partially within) the
  named @nonterm{pkg}s.}

 @item{@Flag{P} @nonterm{owner} @nonterm{package-name} @nonterm{maj}
  @nonterm{min} --- constrain setup actions to the specified @|PLaneT|
  package, in addition to any other specified @|PLaneT| packages or
  collections.}

 @item{@DFlag{doc-index} --- build collections that implement
  documentation indexes (when documentation building is enabled), in
  addition to specified collections.}

 @item{@DFlag{tidy} --- remove metadata cache information and
  documentation for non-existent collections or documentation (to
  clean up after removal), even when setup actions are otherwise
  confined to specified collections.}

]}
@item{Constraining to specific tasks:
@itemize[

 @item{@DFlag{clean} or @Flag{c} --- delete existing @filepath{.zo}
   files, thus ensuring a clean build from the source files. The exact
   set of deleted files can be controlled by @filepath{info.rkt}; see
   @elemref["clean"]{@racket[clean]} for more information.}

 @item{@DFlag{no-zo} or @Flag{n} --- refrain from compiling source
   files to @filepath{.zo} files.}

 @item{@DFlag{trust-zos} --- fix timestamps on @filepath{.zo} files on
   the assumption that they are already up-to-date.}

 @item{@DFlag{no-launcher} or @Flag{x} --- refrain from creating
   executables or installing @tt{man} pages (as specified in
   @filepath{info.rkt}; see @secref["setup-info"]).}

 @item{@DFlag{no-foreign-libs} or @Flag{F} --- refrain from installing foreign
   libraries (as specified in @filepath{info.rkt}; see
   @secref["setup-info"]).}

 @item{@DFlag{no-install} or @Flag{i} --- refrain from running
   pre-install actions (as specified in @filepath{info.rkt} files; see
   @secref["setup-info"]).}

 @item{@DFlag{no-post-install} or @Flag{I} --- refrain from running
   post-install actions (as specified in @filepath{info.rkt} files; see
   @secref["setup-info"]).}

 @item{@DFlag{no-info-domain} or @Flag{d} --- refrain from building
   a cache of metadata information from @filepath{info.rkt}
   files. This cache is needed by other tools. For example,
   @exec{raco} itself uses the cache to locate plug-in tools.}

 @item{@DFlag{no-docs} or @Flag{D} --- refrain from building
   documentation.}

 @item{@DFlag{doc-pdf} @nonterm{dir} --- in addition to building HTML
   documentation, render documentation to PDF and place files in
   @nonterm{dir}.}

 @item{@DFlag{no-pkg-deps} or @Flag{K} --- refrain from checking
  whether dependencies among libraries are properly reflected by
  package-level dependency declarations, whether modules are declared
  by multiple packages, and whether package version dependencies are
  satisfied. Dependency checking uses @filepath{.zo} files, associated
  @filepath{.dep} files, and the documentation index. Unless
  @DFlag{check-pkg-deps} is specified, dependency checking is disabled
  if any collection is specified for @exec{raco setup}, and missing
  dependencies are not treated as an error for a package that has no
  dependency declarations.}

 @item{@DFlag{check-pkg-deps} --- checks package dependencies (unless
  explicitly disabled) even when specific collections are provided to
  @exec{raco setup}, and even for packages that have no
  dependency declarations. Currently, dependency checking related to
  documentation cross-referencing is constrained to documents among
  specified collections.}

 @item{@DFlag{fix-pkg-deps} --- attempt to correct dependency
  mismatches by adjusting package @filepath{info.rkt} files (which makes
  sense only for packages that are installed as links).}

 @item{@DFlag{unused-pkg-deps} --- attempt to report dependencies that
  are declared but are unused. Beware that some package dependencies
  may be intentionally unused (e.g., declared to force installation of
  other packages as a convenience), and beware that package
  dependencies may be reported as unused only because compilation of
  relevant modules has been suppressed.}

]}
@item{Constraining user versus installation setup:
@itemize[

 @item{@DFlag{no-user} or @Flag{U} --- refrain from any user-specific
  (as opposed to installation-specific) setup actions.}

 @item{@DFlag{no-planet} --- refrain from any setup actions for
  @|PLaneT| actions; this flags is implied by @DFlag{no-user}.}

 @item{@DFlag{avoid-main} --- refrain from any setup actions that
  affect the installation, as opposed to user-specific actions.}

]}
@item{Selecting parallelism and other build modes:
@itemize[

 @item{@DFlag{jobs} @nonterm{n}, @DFlag{workers} @nonterm{n},
   or @Flag{j} @nonterm{n} --- use up
   to @nonterm{n} parallel processes.  By default, @exec{raco setup}
   uses @racket[(processor-count)] jobs, which typically uses
   all of the machine's processing cores.}

 @item{@DFlag{verbose} or @Flag{v} --- more verbose output about
   @exec{raco setup} actions.}

 @item{@DFlag{make-verbose} or @Flag{m} --- more verbose output about
   dependency checks.}

 @item{@DFlag{compiler-verbose} or @Flag{r} --- even more verbose
   output about dependency checks and compilation.}

 @item{@DFlag{mode} @nonterm{mode} --- use a @filepath{.zo} compiler
   other than the default compiler, and put the resulting
   @filepath{.zo} files in a subdirectory (of the usual place) named
   by @nonterm{mode}. The compiler is obtained by using @nonterm{mode}
   as a collection name, finding a @filepath{zo-compile.rkt} module in
   that collection, and extracting its @racket[zo-compile] export. The
   @racket[zo-compile] export should be a function like
   @racket[compile]; see the @filepath{errortrace} collection for an
   example.}

 @item{@DFlag{fail-fast} --- attempt to break as soon as any error is
  discovered.}

 @item{@DFlag{pause} or @Flag{p} --- pause for user input if any
  errors are reported (so that a user has time to inspect output that
  might otherwise disappear when the @exec{raco setup} process ends).}

]}
@item{Unpacking @filepath{.plt} archives:
@itemize[

 @item{@Flag{A} @nonterm{archive} @racket[...] --- Install each
  @nonterm{archive}; see @secref["raco-setup-A"].}

 @item{@DFlag{force} --- for use with @Flag{A}, treat version
  mismatches for archives as mere warnings.}

 @item{@DFlag{all-users} or @Flag{a} --- for use with @Flag{A},
  install archive into the installation instead of a user-specific
  location.}

]}

]

When building @exec{racket}, flags can be provided to @exec{raco
setup} as run by @exec{make install} by setting the
@as-index{@envvar{PLT_SETUP_OPTIONS}} environment variable. For
example, the following command line uses a single process to build
collections during an install:

   @commandline{env PLT_SETUP_OPTIONS="-j 1" make install}

@history[#:changed "1.2" @elem{Added the @DFlag{pkgs},
                               @DFlag{check-pkg-deps}, and
                               @DFlag{fail-fast} flags.}]

@; ------------------------------------------------------------------------

@section[#:tag "raco-setup-A"]{Installing @filepath{.plt} Archives}

A @filepath{.plt} file is a platform-independent distribution archive
for software based on Racket. A typical @filepath{.plt} file can be
installed as a package using @exec{raco pkg} (see @other-manual['(lib
"pkg/scribblings/pkg.scrbl")]), in which case @exec{raco pkg} supplies
facilities for uninstalling the package and managing dependencies.

An older approach is to supply a @filepath{.plt} file to @exec{raco
setup} with the @Flag{A} flag, the files contained in the
@filepath{.plt} archive are unpacked (according to specifications
embedded in the @filepath{.plt} file) and only collections specified
by the @filepath{.plt} file are compiled and setup. Archives processed
in this way can include arbitrary code that is executed at install
time, in addition to any actions triggered by the normal
collection-setup part of @exec{raco setup}.

Finally, the @exec{raco unpack} (see @secref["unpack"]) command can
list the content of a @filepath{.plt} archive or unpack the archive
without installing it as a package or collection.

@; ------------------------------------------------------------------------

@section[#:tag "setup-info"]{Controlling @exec{raco setup} with @filepath{info.rkt} Files}

To compile a collection's files to bytecode, @exec{raco setup} uses the
@racket[compile-collection-zos] procedure. That procedure, in turn,
consults the collection's @filepath{info.rkt} file, if it exists, for
specific instructions on compiling the collection. See
@racket[compile-collection-zos] for more information on the fields of
@filepath{info.rkt} that it uses, and see @secref["info.rkt"] for
information on the format of an @filepath{info.rkt} file.

Optional @filepath{info.rkt} fields trigger additional actions by
@exec{raco setup}:

@itemize[

 @item{@as-index{@racketidfont{scribblings}} : @racket[(listof (cons/c string? list?))] ---
   A list of documents to build. Each document in the list is itself
   represented as a list, where each document's list starts with a
   string that is a collection-relative path to the document's source
   file. A document name (which is derived from the source module's
   name by default) is intended to be globally unique in the same way
   as a package or module name.

   More precisely a @racketidfont{scribblings} entry must be a value
   that can be generated from an expression matching the following
   @racket[entry] grammar:

   @racketgrammar*[
     #:literals (list)
     [entry (list doc ...)]
     [doc (list src-string)
          (list src-string flags)
          (list src-string flags category)
          (list src-string flags category name)
          (list src-string flags category name out-k)
          (list src-string flags category name out-k order-n)]
     [flags (list mode-symbol ...)]
     [category (list category-symbol)
               (list category-symbol sort-number)]
     [name string
           #f]
   ]

   A document's list optionally continues with information on how to
   build the document. If a document's list contains a second item,
   @racket[_flags], it must be a list of mode symbols (described
   below). If a document's list contains a third item,
   @racket[_category], it must be a list that categorizes the document
   (described further below). If a document's list contains a fourth
   item, @racket[_name], it is a name to use for the generated
   documentation, instead of defaulting to the source file's name
   (sans extension), where @racket[#f] means to use the default; a
   non-@racket[#f] value for @racket[_name] must fit the grammar
   of a colelction-name element as checked by 
   @racket[collection-name-element?]. If a
   document's list contains a fifth item, @racket[_out-k], it is used
   a hint for the number of files to use for the document's
   cross-reference information; see below. If a document's list
   contains a fourth item, @racket[_order-n], it is used a hint for
   the order of rendering; see below.

   Each mode symbol in @racket[_flags] can be one of the following,
   where only @racket['multi-page] is commonly used:

   @itemize[

     @item{@racket['multi-page] : Generates multi-page HTML output,
           instead of the default single-page format.}

     @item{@racket['main-doc] : Indicates that the generated
           documentation should be written into the main installation
           directory, instead of to a user-specific directory. This
           mode is the default for a collection that is itself located
           in the main installation.}

     @item{@racket['user-doc] : Indicates that the generated
           documentation should be written a user-specific
           directory. This mode is the default for a collection that
           is not itself located in the main installation.}

     @item{@racket['depends-all] : Indicates that the document should
           be re-built if any other document is rebuilt---except for
           documents that have the @racket['no-depend-on] flag.}

     @item{@racket['depends-all-main] : Indicates that the document
           should be re-built if any other document is rebuilt that is
           installed into the main installation---except for documents
           that have the @racket['no-depend-on] flag.}

     @item{@racket['depends-all-user] : Indicates that the document
           should be re-built if any other document is rebuilt that is
           installed into the user's space---except for documents
           that have the @racket['no-depend-on] flag.}

     @item{@racket['always-run] : Build the document every time that
           @exec{raco setup} is run, even if none of its dependencies
           change.}

     @item{@racket['no-depend-on] : Removes the document for
           consideration for other dependencies. Furthermore,
           references from the document to other documents are always
           direct, instead of potentially indirect (i.e., resolved at
           document-viewing time and potentially redirected to a
           remote site).}

     @item{@racket['main-doc-root] : Designates the root document for
           the main installation. The document that currently has this
           mode should be the only one with the mode.}

     @item{@racket['user-doc-root] : Designates the root document for
           the user-specific documentation directory. The document
           that currently has this mode should be the only one with
           the mode.}

     @item{@racket['keep-style] : Leave the document's style as-is,
           instead of imposing the document style for manuals.}

     @item{@racket['no-search] : Build the document without a search
           box.}

    ]

    The @racket[_category] list specifies how to show the document in
    the root table of contents. The list must start with a symbol,
    usually one of the following categories, which are ordered as
    below in the root documentation page:

   @itemize[

     @item{@racket['getting-started] : High-level, introductory
           documentation, typeset at the same level as other
           category titles.}

     @item{@racket['language] : Documentation for a prominent
           programming language.}

     @item{@racket['tool] : Documentation for an executable.}

     @item{@racket['gui-library] : Documentation for GUI and graphics
           libraries.}

     @item{@racket['net-library] : Documentation for networking
           libraries.}

     @item{@racket['parsing-library] : Documentation for parsing
           libraries.}

     @item{@racket['tool-library] : Documentation for programming-tool
           libraries (i.e., not important enough for the more
           prominent @racket['tool] category).}

     @item{@racket['interop] : Documentation for interoperability
           tools and libraries.}

     @item{@racket['library] : Documentation for libraries; this
           category is the default and used for unrecognized category
           symbols.}

     @item{@racket['legacy] : Documentation for deprecated libraries,
           languages, and tools.}

     @item{@racket['experimental] : Documentation for an experimental
           language or library.}

     @item{@racket['other] : Other documentation.}

     @item{@racket['omit] : Documentation that should not be listed on
           the root page or indexed for searching.}

     @item{@racket['omit-start] : Documentation that should not be
           listed on the root page but should be indexed for
           searching.}

   ]

   If the category list has a second element, it must be a real number
   that designates the manual's sorting position with the category;
   manuals with the same sorting position are ordered
   alphabetically. For a pair of manuals with sorting numbers
   @racket[_n] and @racket[_m], the groups for the manuals are
   separated by space if @racket[(truncate (/ _n 10))]and
   @racket[(truncate (/ _m 10))] are different.

   The @racket[_out-k] specification is a hint on whether to break the
   document's cross-reference information into multiple parts, which
   can reduce the time and memory use for resolving a cross-reference
   into the document. It must be a positive, exact integer, and the
   default is @racket[1].

   The @racket[_order-n] specification is a hint for ordering document
   builds, since documentation references can be mutually recursive.
   The order hint can be any real number. A value of @racket[-10] or
   less disables running the document in parallel to other documents.
   The main Racket reference is given a value of @racket[-11], the
   search page is given a value of @racket[10], and the default is
   @racket[0].

   A directory for pre-rendered documentation is computed from the
   source file name by starting with the directory of the
   @filepath{info.rkt} file, adding @filepath{doc}, and then using the
   document name (which is usually the source file's name without a
   suffix); if such a directory exists and does not have a
   @filepath{synced.rktd} file, then it is treated as pre-rendered
   documentation and moved into place, in which case the documentation
   source file need not be present. Moving documentation into place
   may require no movement at all, depending on the way that the
   enclosing collection is installed, but movement includes adding a
   @filepath{synced.rktd} file to represent the installation.}

 @item{@as-index{@racketidfont{release-note-files}} : @racket[(listof (cons/c string? (cons/c string? list?)))] ---
   A list of release-notes text files to link from the main documentation pages.
   Each note is itself represented as a list, and the list can specify auxiliary
   notes that are grouped with the main note.

   A @racketidfont{release-note-files} entry must be a value
   that can be generated from an expression matching the following
   @racket[entry] grammar:

   @racketgrammar*[
     #:literals (list)
     [entry (list note ...)]
     [doc (list label-string note-path)
          (list label-string note-path order-integer)
          (list label-string note-path order-integer
                (list sub-note ...))]
     [sub-note (list label-string note-path)]
   ]

   The @racket[_order-integer] is used to order notes and defaults to @racket[0].}

 @item{@indexed-racket[racket-launcher-names] : @racket[(listof string?)]
   --- @elemtag["racket-launcher-names"] A list of executable names
   to be generated in the installation's executable directory to run
   Racket-based programs implemented by the collection. A parallel
   list of library names must be provided by
   @racket[racket-launcher-libraries] or
   @racket[racket-launcher-flags].

   For each name, a launching executable is set up using
   @racket[make-racket-launcher].  The arguments are @Flag{l-} and
   @tt{@nonterm{colls}/.../@nonterm{file}}, where @nonterm{file} is
   the file named by @racket[racket-launcher-libraries] and
   @tt{@nonterm{colls}/...}  are the collections (and subcollections)
   of the @filepath{info.rkt} file.

   In addition,

   @racketblock[
    (build-aux-from-path
     (build-path (collection-path #,(nonterm "colls") _...) #,(nonterm "suffixless-file")))
   ]

   is provided for the optional @racket[_aux] argument (for icons,
   etc.) to @racket[make-racket-launcher], where
   @nonterm{suffixless-file} is @nonterm{file} without its suffix.

   If @racket[racket-launcher-flags] is provided, it is used as a
   list of command-line arguments passed to @exec{racket} instead of
   the above default, allowing arbitrary command-line arguments. If
   @racket[racket-launcher-flags] is specified together with
   @racket[racket-launcher-libraries], then the flags will override
   the libraries, but the libraries can still be used to specify a
   name for @racket[build-aux-from-path] (to find related information
   like icon files etc).}

 @item{@indexed-racket[racket-launcher-libraries] : @racket[(listof
   path-string?)] --- A list of library names in parallel to
   @elemref["racket-launcher-names"]{@racket[racket-launcher-names]}.}

 @item{@indexed-racket[racket-launcher-flags] : @racket[(listof string?)]
   --- A list of command-line flag lists, in parallel to
   @elemref["racket-launcher-names"]{@racket[racket-launcher-names]}.}

 @item{@indexed-racket[mzscheme-launcher-names],
   @racket[mzscheme-launcher-libraries], and
   @racket[mzscheme-launcher-flags] --- Backward-compatible variant of
   @racket[racket-launcher-names], etc.}

 @item{@indexed-racket[gracket-launcher-names] : @racket[(listof string?)]  ---
   @elemtag["gracket-launcher-names"] Like
   @elemref["racket-launcher-names"]{@racket[racket-launcher-names]},
   but for GRacket-based executables. The launcher-name list is treated
   in parallel to @racket[gracket-launcher-libraries] and
   @racket[gracket-launcher-flags].}

 @item{@indexed-racket[gracket-launcher-libraries] : @racket[(listof path-string?)]
   --- A list of library names in parallel to
   @elemref["gracket-launcher-names"]{@racket[gracket-launcher-names]}.}

 @item{@indexed-racket[gracket-launcher-flags] : @racket[(listof string?)] --- A
   list of command-line flag lists, in parallel to
   @elemref["gracket-launcher-names"]{@racket[gracket-launcher-names]}.}

 @item{@indexed-racket[mred-launcher-names],
   @racket[mred-launcher-libraries], and
   @racket[mred-launcher-flags] --- Backward-compatible variant of
   @racket[gracket-launcher-names], etc.}

 @item{@indexed-racket[copy-foreign-libs] : @racket[(listof (and/c
   path-string? relative-path?))] --- Files to copy into a
   directory where foreign libraries are found by @racket[ffi-lib].
   If @racket[install-platform] is defined, then the files are copied
   only if the current platform matches the definition.}

 @item{@indexed-racket[move-foreign-libs] : @racket[(listof (and/c
   path-string? relative-path?))] --- Like @racket[copy-foreign-libs],
   but the original file is removed after it is copied (which makes sense
   for precompiled packages).}

 @item{@indexed-racket[copy-shared-files] : @racket[(listof (and/c
   path-string? relative-path?))] --- Files to copy into a
   directory where shared files are found.
   If @racket[install-platform] is defined, then the files are copied
   only if the current platform matches the definition.}

 @item{@indexed-racket[move-shared-files] : @racket[(listof (and/c
   path-string? relative-path?))] --- Like @racket[copy-shared-files],
   but the original file is removed after it is copied (which makes sense
   for precompiled packages).}

 @item{@indexed-racket[copy-man-pages] : @racket[(listof (and/c
   path-string? relative-path? filename-extension))] --- Files to copy
   into a @tt{man} directory. The file suffix determines its category;
   for example, @litchar{.1} should be used for a @tt{man} page
   describing an executable.}

 @item{@indexed-racket[move-man-pages] : @racket[(listof (and/c
   path-string? relative-path? filename-extension))] --- Like
   @racket[copy-man-pages], but the original file is removed after it
   is copied (which makes sense for precompiled packages).}

 @item{@indexed-racket[install-platform] : @racket[platform-spec?]
   --- Determines whether files are copied or moved
   for @racket[copy-foreign-libs], @racket[move-foreign-libs],
   @racket[copy-shared-files], or @racket[move-shared-files]. 
   See @racket[matching-platform?] for information on the way that the
   specification is compared to @racket[(system-type)]
   and @racket[(system-library-subpath #f)].}

 @item{@indexed-racket[install-collection] : @racket[path-string?]  --- A
   library module relative to the collection that provides
   @racket[installer]. The @racket[installer] procedure accepts one
   to three arguments. The first argument is a directory path to the
   parent of the Racket installation's @filepath{collects} directory; the
   second argument, if accepted, is a path to the collection's own
   directory; the third argument, if accepted, is a boolean indicating
   whether the collection is installed as user-specific (@racket[#f])
   or installation-wide (@racket[#f]). The procedure should perform collection-specific
   installation work, and it should avoid unnecessary work in the case
   that it is called multiple times for the same installation.}

 @item{@indexed-racket[pre-install-collection] : @racket[path-string?] ---
   Like @racket[install-collection], except that the corresponding
   installer is called @emph{before} the normal @filepath{.zo} build,
   instead of after. The provided procedure should be named
   @racket[pre-installer] in this case, so it can be provided by the
   same file that provides an @racket[installer].}

 @item{@indexed-racket[post-install-collection] : @racket[path-string?]  ---
   Like @racket[install-collection]. It is called right after the
   @racket[install-collection] procedure is executed. The only
   difference between these is that the @DFlag{no-install} flag can be
   used to disable the previous two installers, but not this one.  It
   is therefore expected to perform operations that are always needed,
   even after an installation that contains pre-compiled files. The
   provided procedure should be named @racket[post-installer] in this
   case, so it can be provided by the same file that provides the
   previous two.}

 @item{@indexed-racket[assume-virtual-sources] : @racket[any/c] ---
   A true value indicates that bytecode files without a corresponding
   source file should not be removed from @filepath{compiled} directories,
   and no files should not be removed when the
   @DFlag{clean} or @Flag{c} flag is passed to @exec{raco setup}.}

 @item{@indexed-racket[clean] : @racket[(listof path-string?)] ---
   @elemtag["clean"] A list of pathnames to be deleted when the
   @DFlag{clean} or @Flag{c} flag is passed to @exec{raco setup}. The
   pathnames must be relative to the collection. If any path names a
   directory, each of the files in the directory are deleted, but none
   of the subdirectories of the directory are checked. If the path
   names a file, the file is deleted. The default, if this flag is not
   specified, is to delete all files in the @filepath{compiled}
   subdirectory, and all of the files in the platform-specific
   subdirectory of the compiled directory for the current platform.

   Just as compiling @filepath{.zo} files will compile each module
   used by a compiled module, deleting a module's compiled image will
   delete the @filepath{.zo} of each module that is used by the
   module. More specifically, used modules are determined when
   deleting a @filepath{.dep} file, which would have been created to
   accompany a @filepath{.zo} file when the @filepath{.zo} was built
   by @exec{raco setup}. If the @filepath{.dep} file indicates another
   module, that module's @filepath{.zo} is deleted only if it also has
   an accompanying @filepath{.dep} file. In that case, the
   @filepath{.dep} file is deleted, and additional used modules are
   deleted based on the used module's @filepath{.dep} file, etc.
   Supplying a specific list of collections to @exec{raco setup} disables
   this dependency-based deletion of compiled files.}

]

@; ------------------------------------------------------------------------

@include-section["info.scrbl"]

@; ------------------------------------------------------------------------

@section[#:tag "setup-plt-plt"]{API for Setup}

@defmodule[setup/setup]

@defproc[(setup [#:file file (or/c #f path-string?) #f]
                [#:collections collections (or/c #f (listof (listof path-string?))) #f]
                [#:planet-specs planet-specs (or/c #f 
                                                   (listof (list/c string?
                                                                   string?
                                                                   exact-nonnegative-integer?
                                                                   exact-nonnegative-integer?)))
                                             #f]
                [#:make-user? make-user? any/c #t]
                [#:avoid-main? avoid-main? any/c #f]
                [#:make-docs? make-docs? any/c #t]
                [#:make-doc-index? make-doc-index? any/c #f]
                [#:clean? clean? any/c #f]
                [#:tidy? tidy? any/c #f]
                [#:jobs jobs exact-nonnegative-integer? #f]
                [#:fail-fast? fail-fast? any/c #f]
                [#:get-target-dir get-target-dir (or/c #f (-> path-string?)) #f])
          boolean?]{
Runs @exec{raco setup} with various options:

@itemlist[

 @item{@racket[file] --- if not @racket[#f], installs @racket[file] as
       a @filepath{.plt} archive.}

 @item{@racket[collections] --- if not @racket[#f], constrains setup to
       the named collections, along with @racket[planet-specs], if any}

 @item{@racket[planet-spec] --- if not @racket[#f], constrains setup to
       the named @|PLaneT| packages, along with @racket[collections], if any}

 @item{@racket[make-user?] --- if @racket[#f], disables any
       user-specific setup actions}

 @item{@racket[avoid-main?] --- if true, avoids setup actions that affect
       the main installation, as opposed to user directories}

 @item{@racket[make-docs?] --- if @racket[#f], disables any
       documentation-specific setup actions}

 @item{@racket[make-doc-index?] --- if @racket[#t], builds
       documentation index collections in addition to @racket[collections],
       assuming that documentation is built}

 @item{@racket[clean?] --- if true, enables cleaning mode instead of setup mode}

 @item{@racket[tidy?] --- if true, enables global tidying of
       documentation and metadata indexes even when @racket[collections]
       or @racket[planet-specs] is non-@racket[#f]}

 @item{@racket[jobs] --- if not @racket[#f], determines the maximum number of parallel
       tasks used for setup}

 @item{@racket[fail-fast?] --- if true, breaks the current thread as soon as an
       error is discovered}

 @item{@racket[get-target-dir] --- if not @racket[#f], treated as a
       value for @sigelem[setup-option^ current-target-directory-getter]}

]

The result is @racket[#t] if @exec{raco setup} completes without error,
@racket[#f] otherwise.}


@subsection{@exec{raco setup} Unit}

@defmodule[setup/setup-unit]

The @racketmodname[setup/setup-unit] library provides @exec{raco setup} in unit
form. The associated @racket[setup/option-sig] and
@racket[setup/option-unit] libraries provides the interface for
setting options for the run of @exec{raco setup}.

For example, to unpack a single @filepath{.plt} archive
@filepath{x.plt}, set the @sigelem[setup-option^ archives] parameter
to @racket[(list "x.plt")] and leave @sigelem[setup-option^
specific-collections] as @racket[null].

Link the options and setup units so that your option-setting code is
initialized between them, e.g.:

@racketblock[
(compound-unit
  _...
  (link _...
    [((OPTIONS : setup-option^)) setup:option@]
    [() my-init-options@ OPTIONS]
    [() setup@ OPTIONS _...])
  _...)
]

@defthing[setup@ unit?]{

Imports

  @itemize[#:style "compact"]{
    @item{@racket[setup-option^]}
    @item{@racket[compiler^]}
    @item{@racket[compiler:option^]}
    @item{@racket[launcher^]}
    @item{@racket[dynext:file^]}
  }

and exports nothing. Invoking @racket[setup@] starts the setup process.}

@; ----------------------------------------

@subsection[#:tag "setup-plt-options"]{Options Unit}

@defmodule[setup/option-unit]

@defthing[setup:option@ unit?]{

Imports nothing and exports @racket[setup-option^].}

@; ----------------------------------------

@subsection{Options Signature}

@defmodule[setup/option-sig]

@defsignature[setup-option^ ()]{

@signature-desc{Provides parameters used to control @exec{raco setup} in unit
form.}

  @defparam[setup-program-name name string?]{
    The prefix used when printing status messages.
    @defaults[@racket["raco setup"]]
  }
  
@defboolparam[verbose on?]{
  If on, prints message from @exec{make} to @envvar{stderr}.
  @defaults[@racket[#f]]}

@defboolparam[make-verbose on?]{
  If on, verbose @exec{make}. @defaults[@racket[#f]]}

@defboolparam[compiler-verbose on?]{
  If on, verbose @exec{compiler}. @defaults[@racket[#f]]}

@defboolparam[clean on?]{
 If on, delete @filepath{.zo} and
 @filepath{.so}/@filepath{.dll}/@filepath{.dylib} files in the
 specified collections. @defaults[@racket[#f]]}

@defparam[compile-mode path (or/c path? #f)]{
  If a @racket[path] is given, use a @filepath{.zo} compiler other than plain
  @exec{compile}, and build to @racket[(build-path "compiled" (compile-mode))].
  @defaults[@racket[#f]]}

@defboolparam[make-zo on?]{
  If on, compile @filepath{.zo}. @defaults[@racket[#t]]}

@defboolparam[make-info-domain on?]{
  If on, update @filepath{info-domain/compiled/cache.rkt} for each
  collection path. @defaults[@racket[#t]]}

@defboolparam[make-launchers on?]{
  If on, make collection @filepath{info.rkt}-specified launchers and @tt{man} pages. @defaults[@racket[#t]]}

@defboolparam[make-foreign-lib on?]{
  If on, install collection @filepath{info.rkt}-specified libraries. @defaults[@racket[#t]]}

  @defboolparam[make-docs on?]{
    If on, build documentation.
    @defaults[@racket[#t]]
  }
  
  @defboolparam[make-user on?]{
    If on, build the user-specific collection tree.
    @defaults[@racket[#t]]
  }
  
  @defboolparam[make-planet on?]{
    If on, build the planet cache.
    @defaults[@racket[#t]]
  }
  
@defboolparam[avoid-main-installation on?]{
 If on, avoid building bytecode in the main installation tree when building
 other bytecode (e.g., in a user-specific collection). @defaults[@racket[#f]]}

@defboolparam[make-tidy on?]{
 If on, remove metadata cache information and
  documentation for non-existent collections (to clean up after removal)
  even when @racket[specific-collections] or @racket[specific-planet-dirs]
  is non-@racket['()] or @racket[make-only] is true. @defaults[@racket[#f]]}

@defboolparam[call-install on?]{
  If on, call collection @filepath{info.rkt}-specified setup code.
  @defaults[@racket[#t]]}

@defboolparam[call-post-install on?]{
  If on, call collection @filepath{info.rkt}-specified post-install code.
  @defaults[@racket[#t]]}

@defboolparam[pause-on-errors on?]{
  If on, in the event of an error, prints a summary error and waits for
  @envvar{stdin} input before terminating. @defaults[@racket[#f]]}

  @defparam[parallel-workers num exact-nonnegative-integer?]{
    Determines the number of places to use for compiling bytecode
    and for building the documentation.
    @defaults[@racket[(min (processor-count) 8)]]
  }

@defboolparam[fail-fast on?]{
  If on, breaks the original thread as soon as an error is discovered.
  @defaults[@racket[#f]]

  @history[#:added "1.2"]}
  
@defboolparam[force-unpacks on?]{
  If on, ignore version and already-installed errors when unpacking a
  @filepath{.plt} archive. @defaults[@racket[#f]]}
  
@defparam[specific-collections colls (listof (listof path-string?))]{
  A list of collections to set up; the empty list means set-up all
  collections if the archives list and @racket[specific-planet-dirs]
  is also @racket['()]. @defaults[@racket['()]]}

  @defparam[specific-planet-dirs dir (listof (list/c string?
                                                     string?
                                                     exact-nonnegative-integer?
                                                     exact-nonnegative-integer?))]{
    A list of planet package version specs to set up; the empty list means to
    set-up all planet collections if the archives list and @racket[specific-collections]
    is also @racket['()]. @defaults[@racket['()]]
  }

@defboolparam[make-only on?]{
 If true, set up no collections if @racket[specific-collections]
 and @racket[specific-planet-dirs] are both @racket['()].}
  
@defparam[archives arch (listof path-string?)]{
  A list of @filepath{.plt} archives to unpack; any collections specified
  by the archives are set-up in addition to the collections listed in
  specific-collections. @defaults[@racket[null]]}

@defboolparam[archive-implies-reindex on?]{
  If on, when @racket[archives] has a non-empty list of packages, if any
  documentation is built, then suitable documentation start pages, search pages,
  and master index pages are re-built. @defaults[@racket[#t]]}

@defparam[current-target-directory-getter thunk (-> path-string?)]{
  A thunk that returns the target directory for unpacking a relative
  @filepath{.plt} archive; when unpacking an archive, either this or
  the procedure in @racket[current-target-plt-directory-getter] will
  be called. @defaults[@racket[current-directory]]}

@defparam[current-target-plt-directory-getter
          proc (path-string?
                path-string?
                (listof path-string?) . -> . path-string?)]{
  A procedure that takes a preferred path, a path to the parent of the main
  @filepath{collects} directory, and a list of path choices; it returns
  a path for a "plt-relative" install; when unpacking an archive, either
  this or the procedure in @racket[current-target-directory-getter] will
  be called, and in the former case, this procedure may be called
  multiple times. @defaults[@racket[(lambda (preferred main-parent-dir choices) preferred)]]}

}

@; ------------------------------------------------------------------------

@section[#:tag ".plt-archives"]{API for Installing @filepath{.plt} Archives}

The @racketmodname[setup/plt-single-installer] module provides a
function for installing a single @filepath{.plt} file.

@subsection{Non-GUI Installer}

@local-module[setup/plt-single-installer]{

@defmodule[setup/plt-single-installer]

@defproc[(run-single-installer
          (file path-string?)
          (get-dir-proc (-> (or/c path-string? #f)))
          [#:show-beginning-of-file? show-beginning-of-file? any/c #f])
         void?]{
   Creates a separate thread and namespace, runs the installer in that
   thread with the new namespace, and returns when the thread
   completes or dies. It also creates a custodian
   (see @secref[#:doc ref-src]{custodians}) to manage the
   created thread, sets the exit handler for the thread to shut down
   the custodian, and explicitly shuts down the custodian
   when the created thread terminates or dies.

   The @racket[get-dir-proc] procedure is called if the installer needs a
   target directory for installation, and a @racket[#f] result means that
   the user canceled the installation. Typically, @racket[get-dir-proc] is
   @racket[current-directory].
   
   If @racket[show-beginning-of-file?] is a true value and the installation
   fails, then @racket[run-single-installer] prints the first 1,000 characters
   of the file (in an attempt to help debug the cause of failures).
}

@defproc[(install-planet-package [file path-string?] 
                                 [directory path-string?] 
                                 [spec (list/c string? string? 
                                               (listof string?) 
                                               exact-nonnegative-integer?
                                               exact-nonnegative-integer?)])
         void?]{

 Similar to @racket[run-single-installer], but runs the setup process
 to install the archive @racket[file] into @racket[directory] as the
 @|PLaneT| package described by @racket[spec]. The user-specific
 documentation index is not rebuilt, so @racket[reindex-user-documentation]
 should be run after a set of @|PLaneT| packages are installed.}

@defproc[(reindex-user-documentation) void?]{
  Similar to @racket[run-single-installer], but runs only the part of
  the setup process that rebuilds the user-specific documentation
  start page, search page, and master index.}

@defproc[(clean-planet-package [directory path-string?] 
                               [spec (list/c string? string? 
                                             (listof string?) 
                                             exact-nonnegative-integer?
                                             exact-nonnegative-integer?)])
         void?]{
  Undoes the work of @racket[install-planet-package]. The user-specific
 documentation index is not rebuilt, so @racket[reindex-user-documentation]
 should be run after a set of @|PLaneT| packages are removed.}}


@; ----------------------------------------------------------

@section[#:tag "dirs"]{API for Finding Installation Directories}

@defmodule[setup/dirs]{
  The @racketmodname[setup/dirs] library provides several procedures for locating
  installation directories:}

@defproc[(find-collects-dir) (or/c path? #f)]{
  Returns a path to the installation's main @filepath{collects} directory, or
  @racket[#f] if none can be found. A @racket[#f] result is likely only
  in a stand-alone executable that is distributed without libraries.}

@defproc[(find-user-collects-dir) path?]{
  Returns a path to the user-specific @filepath{collects} directory; the
  directory indicated by the returned path may or may not exist.}

@defproc[(get-collects-search-dirs) (listof path?)]{
  Returns the same result as @racket[(current-library-collection-paths)],
  which means that this result is not sensitive to the value of the 
  @racket[use-user-specific-search-paths] parameter.}

@defproc[(get-main-collects-search-dirs) (listof path?)]{
  Returns a list of paths to installation @filepath{collects}
  directories, including the result of @racket[find-collects-dir].
  These directories are normally included in the result of
  @racket[(current-library-collection-paths)], but a
  @envvar{PLTCOLLECTS} setting or change to the parameter may cause
  them to be omitted. Any other path in
  @racket[(current-library-collection-paths)] is treated as
  user-specific. The dierctories indicated by the returned paths may
  or may not exist.}

@defproc[(find-config-dir) (or/c path? #f)]{
  Returns a path to the installation's @filepath{etc} directory, which
  contains configuration and package information---including
  configuration of some of the other directories (see @secref["config-file"]).
  A @racket[#f] result indicates that no configuration directory
  is available.}

@defproc[(find-links-file) path?]{
  Returns a path to the installation's @tech[#:doc
  reference-doc]{collection links file}.  The file indicated by the
  returned path may or may not exist.}

@defproc[(find-user-links-file [vers string? (get-installation-name)]) path?]{
  Returns a path to the user's @tech[#:doc reference-doc]{collection
  links file}.  The file indicated by the returned path may or may not
  exist.}

@defproc[(get-links-search-files) path?]{
  Returns a list of paths to installation @tech[#:doc
  reference-doc]{collection links files} that are search in
  order. (Normally, the result includes the result of
  @racket[(find-links-file)], which is where new installation-wide
  links are installed by @exec{raco link} or @racket[links].) The
  files indicated by the returned paths may or may not exist.}

@defproc[(find-pkgs-dir) path?]{
  Returns a path to the directory containing packages with
  installation scope; the directory indicated by the returned path may
  or may not exist.}

@defproc[(find-user-pkgs-dir [vers string? (get-installation-name)]) path?]{
  Returns a path to the directory containing packages with
  user-specific scope for installation name @racket[vers]; the directory indicated by
  the returned path may or may not exist.}

@defproc[(get-pkgs-search-dirs) (listof path?)]{
  Returns a list of paths to the directories containing packages in
  installation scope.  (Normally, the result includes the result of
  @racket[(find-pkgs-dir)], which is where new packages are installed
  by @exec{raco pkg install}.) The directories indicated by the returned
  paths may or may not exist.}

@defproc[(find-doc-dir) (or/c path? #f)]{
  Returns a path to the installation's @filepath{doc} directory.
  The result is @racket[#f] if no such directory is available.}

@defproc[(find-user-doc-dir) path?]{
  Returns a path to a user-specific @filepath{doc} directory. The directory
  indicated by the returned path may or may not exist.}

@defproc[(get-doc-search-dirs) (listof path?)]{
  Returns a list of paths to search for documentation, not including
  documentation stored in individual collections. Unless it is
  configured otherwise, the result includes any non-@racket[#f] result of
  @racket[(find-doc-dir)] and @racket[(find-user-doc-dir)]---but the latter is
  included only if the value of the @racket[use-user-specific-search-paths]
  parameter is @racket[#t].}

@defproc[(find-lib-dir) (or/c path? #f)]{
  Returns a path to the installation's @filepath{lib} directory, which contains
  libraries and other build information. The result is @racket[#f] if no such
  directory is available.}

@defproc[(find-user-lib-dir) path?]{
  Returns a path to a user-specific @filepath{lib} directory; the directory
  indicated by the returned path may or may not exist.}

@defproc[(find-share-dir) (or/c path? #f)]{ Returns a path to the
  installation's @filepath{share} directory, which contains installed
  packages and other platform-independent files. The result is
  @racket[#f] if no such directory is available.}

@defproc[(find-user-share-dir) path?]{
  Returns a path to a user-specific @filepath{share} directory; the directory
  indicated by the returned path may or may not exist.}

@defproc[(find-dll-dir) (or/c path? #f)]{
  Returns a path to the directory that contains DLLs for use with the
  current executable (e.g., @filepath{libmzsch.dll} on Windows).
  The result is @racket[#f] if no such directory is available, or if no
  specific directory is available (i.e., other than the platform's normal
  search path).}

@defproc[(get-lib-search-dirs) (listof path?)]{
  Returns a list of paths to search for foreign libraries. Unless it is
  configured otherwise, the result includes any non-@racket[#f] result of
  @racket[(find-lib-dir)], @racket[(find-dll-dir)],
  and @racket[(find-user-lib-dir)]---but the last is included only if the
  value of the @racket[use-user-specific-search-paths] parameter
  is @racket[#t].}

@defproc[(find-include-dir) (or/c path? #f)]{
  Returns a path to the installation's @filepath{include} directory, which
  contains @filepath{.h} files for building Racket extensions and embedding
  programs. The result is @racket[#f] if no such directory is available.}

@defproc[(find-user-include-dir) path?]{
  Returns a path to a user-specific @filepath{include} directory; the
  directory indicated by the returned path may or may not exist.}

@defproc[(get-include-search-dirs) (listof path?)]{
  Returns a list of paths to search for @filepath{.h} files. Unless it is
  configured otherwise, the result includes any non-@racket[#f] result of
  @racket[(find-include-dir)] and @racket[(find-user-include-dir)]---but the
  latter is included only if the value of the
  @racket[use-user-specific-search-paths] parameter is @racket[#t].}

@defproc[(find-console-bin-dir) (or/c path? #f)]{
  Returns a path to the installation's executable directory, where the
  stand-alone Racket executable resides. The result is @racket[#f] if no
  such directory is available.}

@defproc[(find-gui-bin-dir) (or/c path? #f)]{
  Returns a path to the installation's executable directory, where the
  stand-alone GRacket executable resides. The result is @racket[#f] if no such
  directory is available.}

@defproc[(find-user-console-bin-dir) path?]{
  Returns a path to the user's executable directory; the directory
  indicated by the returned path may or may not exist.}

@defproc[(find-user-gui-bin-dir) path?]{
  Returns a path to the user's executable directory for graphical
  programs; the directory indicated by the returned path may or may
  not exist.}

@defproc[(find-apps-dir) (or/c path? #f)]{
  Returns a path to the installation's directory @filepath{.desktop}
  files (for Unix). The result is @racket[#f] if no such directory
  exists.}

@defproc[(find-user-apps-dir) path?]{
  Returns a path to the user's directory for @filepath{.desktop} files
  (for Unix); the directory indicated by the returned path may or may
  not exist.}

@defproc[(find-man-dir) (or/c path? #f)]{
  Returns a path to the installation's man-page directory. The result is
  @racket[#f] if no such directory exists.}

@defproc[(find-user-man-dir) path?]{
  Returns a path to the user's man-page directory; the directory
  indicated by the returned path may or may not exist.}

@defproc[(get-doc-search-url) string?]{
  Returns a string that is used by the documentation system, augmented
  with a version and search-key query, for remote documentation links.}

@defproc[(get-doc-open-url) (or/c string? #f)]{
  Returns @racket[#f] or a string for a root URL to be used as an
  alternative to opening a local file for documentation. A
  non-@racket[#f] configuration means that DrRacket, for example,
  performs keyword searches for documentation via the specified URL
  instead of from locally installed documentation.

  @history[#:added "6.0.1.6"]}

@defproc[(get-installation-name) string?]{ Returns the current
  installation's name, which is often @racket[(version)] but can be
  configured via @racket['installation-name] in @filepath{config.rktd}
  (see @secref["config-file"]).}

@defproc[(get-build-stamp) (or/c #f string?)]{ Returns a string
   that identifies an installation build, which can be used to augment
   the Racket version number to more specifically identify the
   build. An empty string is normally produced for a release build.
   The result is @racket[#f] if no build stamp is available.}

@defproc[(get-absolute-installation?) boolean?]{
  Returns @racket[#t] if this installation uses
  absolute path names for executable and library references, 
  @racket[#f] otherwise.}

@; ------------------------------------------------------------------------

@section[#:tag "getinfo"]{API for Reading @filepath{info.rkt} Files}

@defmodule[setup/getinfo]{ The @racketmodname[setup/getinfo] library
   provides functions for accessing fields in @filepath{info.rkt}
   files.}

@defproc[(get-info [collection-names (listof string?)]
                   [#:namespace namespace (or/c namespace? #f) #f]
                   [#:bootstrap? bootstrap? any/c #f])
         (or/c
          (symbol? [(-> any)] . -> . any)
          #f)]{
   Accepts a list of strings naming a collection or sub-collection,
   and calls @racket[get-info/full] with the full path corresponding to the
   named collection and the @racket[namespace] argument.}

@defproc[(get-info/full [path path-string?]
                        [#:namespace namespace (or/c namespace? #f) #f]
                        [#:bootstrap? bootstrap? any/c #f])
         (or/c (->* (symbol?) ((-> any)) any)
               #f)]{

   Accepts a path to a directory. If it finds either a well-formed
   an @filepath{info.rkt} file or an @filepath{info.ss} file (with
   preference for the @filepath{info.rkt} file), 
   it returns an info procedure that accepts either one
   or two arguments. The first argument to the info procedure is
   always a symbolic name, and the result is the value of the name in
   the @filepath{info.rkt} file, if the name is defined. The optional
   second argument, @racket[_thunk], is a procedure that takes no
   arguments to be called when the name is not defined; the result of
   the info procedure is the result of the @racket[_thunk] in that
   case. If the name is not defined and no @racket[_thunk] is
   provided, then an exception is raised.

   The @racket[get-info/full] function returns @racket[#f] if there is
   no @filepath{info.rkt} (or @filepath{info.ss}) file in the directory. If there is a
   @filepath{info.rkt} (or @filepath{info.ss}) file that has the wrong shape (i.e., not a module
   using @racketmodname[info] or @racketmodname[setup/infotab]),
   or if the @filepath{info.rkt} file fails to load, then an exception
   is raised. If the @filepath{info.rkt} file loaded, @racket[get-info/full]
   returns the @racket[get-info] file. If the @filepath{info.rkt} file does not exist, 
   then @racket[get-info/full] does
   the same checks for the @filepath{info.rkt} file, either raising an exception
   or returning the @racket[get-info] function from the @filepath{info.rkt} file.

   The @filepath{info.rkt} (or @filepath{info.ss}) module is loaded
   into @racket[namespace] if it is not @racket[#f], or a private,
   weakly-held namespace otherwise.

   If @racket[bootstrap?] is true, then
   @racket[use-compiled-file-paths] is set to @racket['()] while
   reading @filepath{info.rkt} (or @filepath{info.ss}), in case an
   existing compiled file is broken. Furthermore, the
   @racketmodname[info] and @racketmodname[setup/infotab] modules are
   attached to @racket[namespace] from the namespace of
   @racket[get-info/full] before attempting to load
   @filepath{info.rkt} (or @filepath{info.ss}).}

@defproc[(find-relevant-directories
          (syms (listof symbol?))
          (mode (or/c 'preferred 'all-available 'no-planet 'no-user) 'preferred)) 
         (listof path?)]{

   Returns a list of paths identifying
   collections and installed @|PLaneT| packages whose
   @filepath{info.rkt} file defines one or more of the given
   symbols. The result is based on a cache that is computed by
   @exec{raco setup}.

   Note that the cache may be out of date by the time you call
   @racket[get-info/full], so do not assume that every returned
   directory's @filepath{info.rkt} file will supply one of the
   requested symbols.

   The result is in a canonical order (sorted lexicographically by
   directory name), and the paths it returns are suitable for
   providing to @racket[get-info/full].

   If @racket[mode] is specified, it must be either
   @racket['preferred] (the default), @racket['all-available],
   @racket['no-planet], or @racket['no-user]. If @racket[mode] is
   @racket['all-available], @racket[find-relevant-collections] returns
   all installed directories whose info files contain the specified
   symbols---for instance, all versions of all installed PLaneT
   packages will be searched if @racket['all-available] is
   specified. If @racket[mode] is @racket['preferred], then only a
   subset of ``preferred'' packages will be searched: only the
   directory containing the most recent version of any PLaneT package
   will be returned. If @racket[mode] is @racket['no-planet], then
   PLaneT packages are not included in the search. If @racket[mode] is
   @racket['no-user], then only installation-wide directories are
   search, which means omitting @|PLaneT| package directories.

   Collection links from the installation-wide @tech[#:doc
   reference-doc]{collection links file} or packages with installation
   scope are cached with the installation's main @filepath{lib}
   directory, and links from the user-specific @tech[#:doc
   reference-doc]{collection links file} and packages are cached with
   the user-specific directory @racket[(build-path (find-system-path
   'addon-dir) "collects")] for all-version cases, and in @racket[(build-path
   (find-system-path 'addon-dir) (version) "collects")] for
   version-specific cases.}

@defproc[(find-relevant-directory-records
          [syms (listof symbol?)]
          [key (or/c 'preferred 'all-available)])
         (listof directory-record?)]{
  Like @racket[find-relevant-directories], but returns @racket[directory-record] structs
  instead of @racket[path?]s.
}

@defstruct[directory-record ([maj integer?]
                             [min integer?]
                             [spec any/c]
                             [path path?]
                             [syms (listof symbol?)])]{
  A struct that records information about a collection or a @PLaneT package that has been installed.
  Collections will have the major version being @racket[1] and the minor version being @racket[0].
  The @racket[spec] field is a quoted module spec; the @racket[path] field is where the @tt{info.rkt}
  file for this collection or @PLaneT package exists on the filesystem the @racket[syms] field holds the 
  identifiers defined in that file.
}

@defproc[(reset-relevant-directories-state!) void?]{
   Resets the cache used by @racket[find-relevant-directories].}

@; ------------------------------------------------------------------------

@section[#:tag "relative-paths"]{API for Relative Paths}

The Racket installation tree can usually be moved around the filesystem.
To support this, care must be taken to avoid absolute paths.  The
following two APIs cover two aspects of this: a way to convert a path to
a value that is relative to the @filepath{collets} tree, and a way to
display such paths (e.g., in error messages).

@subsection{Representing Collection-Based Paths}

@defmodule[setup/collects]

@defproc[(path->collects-relative [path path-string?]
                                  [#:cache cache (or/c #f (and/c hash? (not/c immutable?)))])
         (or/c path-string?
               (cons/c 'collects
                       (cons/c bytes? (non-empty-listof bytes?))))]{

Checks whether @racket[path] (normalized by
@racket[path->complete-path] and @racket[simplify-path] with
@racket[#f] as its second argument) matches the result of
@racket[collection-file-path]. If so, the result is a list starting
with @racket['collects] and containing the relevant path elements as
byte strings. If not, the path is returned as-is.

The @racket[cache] argument is used with @racket[path->pkg], if needed.}

@defproc[(collects-relative->path
          [rel (or/c path-string?
                     (cons/c 'collects
                             (cons/c bytes? (non-empty-listof bytes?))))])
         path-string?]{

The inverse of @racket[path->collects-relative]: if @racket[rel]
is a pair that starts with @racket['collects], then it is converted
back to a path using @racket[collection-file-path].}

@defproc[(path->module-path [path path-string?]
                            [#:cache cache (or/c #f (and/c hash? (not/c imutable?)))])
         (or/c path-string? module-path?)]{

Like @racket[path->collects-relative], but the result is either
@racket[path] or a normalized (in the sense of
@racket[collapse-module-path]) module path.}

@subsection{Representing Paths Relative to @filepath{collects}}

@defmodule[setup/main-collects]

@defproc[(path->main-collects-relative [path (or/c bytes? path-string?)])
         (or/c path? (cons/c 'collects (non-empty-listof bytes?)))]{

Checks whether @racket[path] has a prefix that matches the prefix to
the main @filepath{collects} directory as determined by
@racket[(find-collects-dir)]. If so, the result is a list starting
with @racket['collects] and containing the remaining path elements as
byte strings. If not, the path is returned as-is.

The @racket[path] argument should be a complete path. Applying
@racket[simplify-path] before @racket[path->main-collects-relative] is
usually a good idea.

For historical reasons, @racket[path] can be a byte string, which is
converted to a path using @racket[bytes->path].

See also @racket[collects-relative->path].}

@defproc[(main-collects-relative->path
          [rel (or/c bytes?
                     path-string?
                     (cons/c 'collects (non-empty-listof bytes?)))])
         path>]{

The inverse of @racket[path->main-collects-relative]: if @racket[rel]
is a pair that starts with @racket['collects], then it is converted
back to a path relative to @racket[(find-collects-dir)].}

@subsection{Displaying Paths Relative to a Common Root}

@defmodule[setup/path-to-relative]

@defproc[(path->relative-string/library
          [path path-string?]
          [default (or/c (-> path-string? any/c) any/c)
                   (lambda (x) (if (path? x) (path->string x) x))]
          [#:cache cache (or/c #f (and/c hash? (not/c immutable?))) #f])
         any/c]{
  Produces a string suitable for display in error messages.  If the path
  is an absolute one that is inside a package, the
  result is a string that begins with @racket["<pkgs>/"]. If the path
  is an absolute one that is inside the @filepath{collects} tree, the
  result is a string that begins with @racket["<collects>/"].
  Similarly, a path in the user-specific collects results in a prefix of
  @racket["<user-collects>/"], and a @PLaneT path results in
  @racket["<planet>/"].

  If @racket[cache] is not @racket[#f], it is used as a cache argument
  for @racket[pkg->path] to speed up detection and conversion of
  package paths.

  If the path is not absolute, or if it is not in any of these, it is
  returned as-is (converted to a string if needed).  If @racket[default]
  is given, it specifies the return value instead: it can be a procedure
  that is applied onto the path to get the result, or the result
  itself.

  Note that this function can return a non-string only if
  @racket[default] is given and it does not return a string.
}

@defproc[(path->relative-string/setup
          [path path-string?]
          [default (or/c (-> path-string? any/c) any/c)
                   (lambda (x) (if (path? x) (path->string x) x))]
          [#:cache cache (or/c #f (and/c hash? (not/c immutable?))) #f])
         any/c]{

The same as @racket[path->relative-string/library], for backward
compatibility.}


@defproc[(make-path->relative-string
          [dirs (listof (cons (-> path?) string?))]
          [default (or/c (-> path-string? any/c) any/c)
                   (lambda (x) (if (path? x) (path->string x) x))])
         (path-string? any/c . -> . any)]{
  This function produces functions like
  @racket[path->relative-string/library] and
  @racket[path->relative-string/setup].

  The @racket[dirs] argument determines the prefix substitutions.  It must be an
  association list mapping a path-producing thunk to a prefix string for
  paths in the specified path.

  @racket[default] determines the default for the resulting function
  (which can always be overridden by an additional argument to this
  function).
}

@; ------------------------------------------------------------------------

@section[#:tag "collection-names"]{API for Collection Names}

@defmodule[setup/collection-name]

@defproc[(collection-name? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a string that is syntactically
valid as a collection name, which means that it is one or more
@litchar{/}-separated strings for which
@racket[collection-name-element?] returns true.}


@defproc[(collection-name-element? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a string that is syntactically
valid as a top-level collection name or as a part of a collection
name, which means that it is non-empty and contains only ASCII
letters, ASCII digits, @litchar{-}, @litchar{+}, @litchar{_}, and
@litchar{%}, where a @litchar{%} is allowed only when followed by two
lowercase hexadecimal digits, and the digits must form a number that
is not the ASCII value of a letter, digit, @litchar{-}, @litchar{+},
or @litchar{_}.}



@; ------------------------------------------------------------------------

@section[#:tag "matching-platform"]{API for Platform Specifications}

@defmodule[setup/matching-platform]

@history[#:added "6.0.1.13"]

@defproc[(platform-spec? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a symbol, string, or regexp value
(in the sense of @racket[regexp?]), @racket[#f] otherwise.}

@defproc[(matching-platform? [spec platform-spec?]
                             [#:system-type sys-type (or/c #f symbol?) (system-type)]
                             [#:system-library-subpath sys-lib-subpath (or/c #f path?)
                                                       (system-library-subpath #f)])
         boolean?]{

Reports whether @racket[spec] matches @racket[sys-type] or
@racket[sys-lib-subpath], where @racket[#f] values for the latter are
replaced with the default values.

If @racket[spec] is a symbol, then the result is @racket[#t] if
@racket[sys-type] is the same symbol, @racket[#f] otherwise.

If @racket[spec] is a string, then the result is @racket[#t] if
@racket[(path->string sys-lib-subpath)] is the same string,
@racket[#f] otherwise.

If @racket[spec] is a regexp value, then the result is @racket[#t] if
the regexp matches @racket[(path->string sys-lib-subpath)],
@racket[#f] otherwise.}

@; ------------------------------------------------------------------------

@section[#:tag "xref"]{API for Cross-References for Installed Manuals}

@defmodule[setup/xref]

@defproc[(load-collections-xref [on-load (-> any/c) (lambda () (void))])
         xref?]{

Like @racket[load-xref], but automatically find all cross-reference files for
manuals that have been installed with @exec{raco setup}.}
