#lang scribble/doc

@(require scribble/manual
          scribble/bnf
         (for-label racket
                    setup/setup-unit
                    setup/option-unit
                    setup/option-sig
                    setup/dirs
                    setup/main-collects
                    setup/xref scribble/xref
                    ;; setup/infotab -- no bindings from this are used
                    setup/getinfo
                    setup/plt-installer
                    setup/plt-installer-sig
                    setup/plt-installer-unit
                    setup/pack
                    setup/unpack
                    compiler/compiler
                    launcher/launcher
                    compiler/sig
                    launcher/launcher-sig
                    racket/gui/base))

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

The @exec{raco setup} command finds, compiles, configures,
and installs documentation for all collections in a Racket
installation. It can also install single @filepath{.plt} files.

@local-table-of-contents[]

@; ------------------------------------------------------------------------
@; ------------------------------------------------------------------------

@section[#:tag "running"]{Running @exec{raco setup}}

The @exec{raco setup} command performs two main services:

@itemize[

 @item{@bold{Compiling and setting up all (or some of the)
   collections:} When @exec{raco setup} is run without any arguments, it
   finds all of the current collections (see @secref[#:doc
   ref-src]{collects}) and compiles libraries in each collection.

   An optional @filepath{info.rkt} within the collection can indicate
   specifically how the collection's files are to be compiled and
   other actions to take in setting up a collection, such as creating
   executables or building documentation. See @secref["setup-info"]
   for more information.

   The @DFlag{clean} (or @Flag{c}) flag to @exec{raco setup} causes it to
   delete existing @filepath{.zo} files, thus ensuring a clean build
   from the source files. The exact set of deleted files can be
   controlled by @filepath{info.rkt}; see
   @elemref["clean"]{@racket[clean]} for more information.

   The @Flag{l} flag takes one or more collection names and restricts
   @exec{raco setup}'s action to those collections.

   The @DFlag{mode} @nonterm{mode} flag causes @exec{raco setup} to use a
   @filepath{.zo} compiler other than the default compiler, and to put
   the resulting @filepath{.zo} files in a subdirectory (of the usual
   place) named by @nonterm{mode}. The compiler is obtained by using
   @nonterm{mode} as a collection name, finding a
   @filepath{zo-compile.rkt} module in that collection, and extracting
   its @racket[zo-compile] export. The @racket[zo-compile] export
   should be a function like @racket[compile]; see the
   @filepath{errortrace} collection for an example.}

 @item{@bold{Unpacking @filepath{.plt} files:} A
   @filepath{.plt} file is a platform-independent distribution archive
   for software based on Racket. When one or more file names are
   provided as the command line arguments to @exec{raco setup}, the files
   contained in the @filepath{.plt} archive are unpacked (according to
   specifications embedded in the @filepath{.plt} file) and only
   collections specified by the @filepath{.plt} file are compiled and
   setup.}]

Run @exec{raco help setup} to see a list of all options accepted by
the @exec{raco setup} command.

@; ------------------------------------------------------------------------

@subsection[#:tag "setup-info"]{Controlling @exec{raco setup} with @filepath{info.rkt} Files}

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
   file.

   More precisely a @racketidfont{scribblings} entry must be a value
   that can be generated from an expression matching the following
   @racket[entry] grammar:

   @racketgrammar*[
     #:literals (list)
     [entry (list doc ...)]
     [doc (list src-string)
          (list src-string flags)
          (list src-string flags category)
          (list src-string flags category name-string)]
     [flags (list mode-symbol ...)]
     [category (list category-symbol)
               (list category-symbol sort-number)]
   ]

   A document's list optionally continues with information on how to
   build the document. If a document's list contains a second item, it
   must be a list of mode symbols (described below). If a document's
   list contains a third item, it must be a list that categorizes the
   document (described further below). If a document's list contains a
   fourth item, it is a name to use for the generated documentation,
   instead of defaulting to the source file's name (sans extension).

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
           documents that have the @racket['no-depends-on] mode.}

     @item{@racket['depends-all-main] : Indicates that the document
           should be re-built if any other document is rebuilt that is
           installed into the main installation---except for documents
           that have the @racket['no-depends-on] mode.}

     @item{@racket['always-run] : Build the document every time that
           @exec{raco setup} is run, even if none of its dependencies
           change.}

     @item{@racket['no-depend-on] : Removes the document for
           consideration for other dependencies. This mode is
           typically used with @racket['always-run] to avoid
           unnecessary dependencies that prevent reaching a stable
           point in building documentation.}

     @item{@racket['main-doc-root] : Designates the root document for
           the main installation. The document that currently has this
           mode should be the only one with the mode.}

     @item{@racket['user-doc-root] : Designates the root document for
           the user-specific documentation directory. The document
           that currently has this mode should be the only one with
           the mode.}

    ]

    The @racket[_category] list specifies how to show the document in
    the root table of contents. The list must start with a symbol,
    usually one of the following categories, which are ordered as
    below in the root documentation page:

   @itemize[

     @item{@racket['getting-started] : High-level, introductory
           documentation.}

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
           the root page.}

   ]

   If the category list has a second element, it must be a real number
   that designates the manual's sorting position with the category;
   manuals with the same sorting position are ordered
   alphabetically. For a pair of manuals with sorting numbers
   @racket[_n] and @racket[_m], the groups for the manuals are
   separated by space if @racket[(truncate (/ _n 10))]and
   @racket[(truncate (/ _m 10))] are different.}

 @item{@scheme[racket-launcher-names] : @scheme[(listof string?)]
   --- @elemtag["racket-launcher-names"] A list of executable names
   to be generated in the installation's executable directory to run
   Racket-based programs implemented by the collection. A parallel
   list of library names must be provided by
   @scheme[racket-launcher-libraries] or
   @scheme[racket-launcher-flags].

   For each name, a launching executable is set up using
   @scheme[make-racket-launcher].  The arguments are @Flag{l-} and
   @tt{@nonterm{colls}/.../@nonterm{file}}, where @nonterm{file} is
   the file named by @scheme[racket-launcher-libraries] and
   @tt{@nonterm{colls}/...}  are the collections (and subcollections)
   of the @filepath{info.rkt} file.

   In addition,

   @schemeblock[
    (build-aux-from-path
     (build-path (collection-path #,(nonterm "colls") _...) #,(nonterm "suffixless-file")))
   ]

   is provided for the optional @scheme[_aux] argument (for icons,
   etc.) to @scheme[make-racket-launcher], where where
   @nonterm{suffixless-file} is @nonterm{file} without its suffix.

   If @scheme[racket-launcher-flags] is provided, it is used as a
   list of command-line arguments passed to @exec{racket} instead of
   the above default, allowing arbitrary command-line arguments. If
   @scheme[racket-launcher-flags] is specified together with
   @scheme[racket-launcher-libraries], then the flags will override
   the libraries, but the libraries can still be used to specify a
   name for @scheme[build-aux-from-path] (to find related information
   like icon files etc).}

 @item{@scheme[racket-launcher-libraries] : @scheme[(listof
   path-string?)] --- A list of library names in parallel to
   @elemref["racket-launcher-names"]{@scheme[racket-launcher-names]}.}

 @item{@scheme[racket-launcher-flags] : @scheme[(listof string?)]
   --- A list of command-line flag lists, in parallel to
   @elemref["racket-launcher-names"]{@scheme[racket-launcher-names]}.}

 @item{@scheme[mzscheme-launcher-names],
   @scheme[mzscheme-launcher-libraries], and
   @scheme[mzscheme-launcher-flags] --- Backward-compatible variant of
   @racket[racket-launcher-names], etc.}

 @item{@scheme[gracket-launcher-names] : @scheme[(listof string?)]  ---
   @elemtag["gracket-launcher-names"] Like
   @elemref["racket-launcher-names"]{@scheme[racket-launcher-names]},
   but for GRacket-based executables. The launcher-name list is treated
   in parallel to @scheme[gracket-launcher-libraries] and
   @scheme[gracket-launcher-flags].}

 @item{@scheme[gracket-launcher-libraries] : @scheme[(listof path-string?)]
   --- A list of library names in parallel to
   @elemref["gracket-launcher-names"]{@scheme[gracket-launcher-names]}.}

 @item{@scheme[gracket-launcher-flags] : @scheme[(listof string?)] --- A
   list of command-line flag lists, in parallel to
   @elemref["gracket-launcher-names"]{@scheme[gracket-launcher-names]}.}

 @item{@scheme[mred-launcher-names],
   @scheme[mred-launcher-libraries], and
   @scheme[mred-launcher-flags] --- Backward-compatible variant of
   @racket[gracket-launcher-names], etc.}

 @item{@racket[install-collection] : @racket[path-string?]  --- A
   library module relative to the collection that provides
   @racket[installer]. The @racket[installer] procedure accepts either
   one or two arguments. The first argument is a directory path to the
   parent of the Racket installation's @filepath{collects} directory; the
   second argument, if accepted, is a path to the collection's own
   directory. The procedure should perform collection-specific
   installation work, and it should avoid unnecessary work in the case
   that it is called multiple times for the same installation.}

 @item{@racket[pre-install-collection] : @racket[path-string?] ---
   Like @racket[install-collection], except that the corresponding
   installer is called @emph{before} the normal @filepath{.zo} build,
   instead of after. The provided procedure should be named
   @racket[pre-installer] in this case, so it can be provided by the
   same file that provides an @racket[installer].}

 @item{@racket[post-install-collection] : @racket[path-string?]  ---
   Like @racket[install-collection]. It is called right after the
   @racket[install-collection] procedure is executed. The only
   difference between these is that the @DFlag{no-install} flag can be
   used to disable the previous two installers, but not this one.  It
   is therefore expected to perform operations that are always needed,
   even after an installation that contains pre-compiled files. The
   provided procedure should be named @racket[post-installer] in this
   case, so it can be provided by the same file that provides the
   previous two.}

 @item{@racket[clean] : @racket[(listof path-string?)] ---
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

@section[#:tag "setup-plt-plt"]{API for Installation}

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
    [(OPTIONS : setup-option^) setup:option@]
    [() my-init-options@ OPTIONS]
    [() setup@ OPTIONS _...])
  _...)
]

@subsection{@exec{raco setup} Unit}

@defmodule[setup/setup-unit]

@defthing[setup@ unit?]{

Imports

@itemize[#:style "compact"]{
    @item{@racket[setup-option^]}
    @item{@racket[compiler^]}
    @item{@racket[compiler:option^]}
    @item{@racket[launcher^]}}

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

@defparam[compile-mode path (or/c path? false/c)]{
  If a @racket[path] is given, use a @filepath{.zo} compiler other than plain
  @exec{compile}, and build to @racket[(build-path "compiled" (compile-mode))].
  @defaults[@racket[#f]]}

@defboolparam[make-zo on?]{
  If on, compile @filepath{.zo}. @defaults[@racket[#t]]}

@defboolparam[make-so on?]{
  If on, compile @filepath{.so}/@filepath{.dll} files. @defaults[@racket[#f]]}

@defboolparam[make-launchers on?]{
  If on, make collection @filepath{info.rkt}-specified launchers. @defaults[@racket[#t]]}

@defboolparam[make-info-domain on?]{
  If on, update @filepath{info-domain/compiled/cache.rkt} for each
  collection path. @defaults[@racket[#t]]}

@defboolparam[avoid-main-installation on?]{
 If on, avoid building bytecode in the main installation tree when building
 other bytecode (e.g., in a user-specific collection). @defaults[@racket[#f]]}

@defboolparam[call-install on?]{
  If on, call collection @filepath{info.rkt}-specified setup code.
  @defaults[@racket[#t]]}

@defboolparam[force-unpack on?]{
  If on, ignore version and already-installed errors when unpacking a
  @filepath{.plt} archive. @defaults[@racket[#f]]}

@defboolparam[pause-on-errors on?]{
  If on, in the event of an error, prints a summary error and waits for
  @envvar{stdin} input before terminating. @defaults[@racket[#f]]}

@defparam[specific-collections coll (listof path-string?)]{
  A list of collections to set up; the empty list means set-up all
  collections if the archives list is also empty @defaults[@racket[null]]}

@defparam[archives arch (listof path-string?)]{
  A list of @filepath{.plt} archives to unpack; any collections specified
  by the archives are set-up in addition to the collections listed in
  specific-collections. @defaults[@racket[null]]}

@defboolparam[archive-implies-reindex on?]{
  If on, when @racket[archives] has a non-empty list of packages, if any
  documentation is built, then suitable documentation start pages, search pages,
  and master index pages are re-built. @defaults[@racket[#t]]}

@defparam[current-target-directory-getter thunk (-> . path-string?)]{
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

@subsection{Installing a Single @filepath{.plt} File}

The @racketmodname[setup/plt-single-installer] module provides a
function for installing a single @filepath{.plt} file, and
@racketmodname[setup/plt-installer] wraps it with a GUI
interface.

@subsubsection{Non-GUI Installer}

@local-module[setup/plt-single-installer]{

@defmodule[setup/plt-single-installer]

@defproc[(run-single-installer
          (file path-string?)
          (get-dir-proc (-> (or/c path-string? false/c)))) void?]{
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
   @racket[current-directory].}
v
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


@subsubsection[#:tag "gui-unpacking"]{GUI Installer}

@defmodule[setup/plt-installer]{ The
  @racketmodname[setup/plt-installer] library in the setup collection
  defines procedures for installing a @filepath{.plt} archive with a
  GUI (using the facilities of @racketmodname[racket/gui/base]).}

@defproc[(run-installer (filename path-string?)) void?]{
  Run the installer on the @filepath{.plt} file
  in @racket[filename] and show the output in a window. This is a
  composition of @racket[with-installer-window] and
  @racket[run-single-installer] with a @racket[get-dir-proc] that prompts
  the user for a directory (turning off the busy cursor while the dialog
  is active).}

@defparam[on-installer-run thunk (-> any)]{
  A thunk that is run after a @filepath{.plt} file is installed.}

@defproc[(with-installer-window
          (do-install ((or/c (is-a?/c dialog%) (is-a?/c frame%)) 
                       . -> . void?))
          (cleanup-thunk (-> any)))
         void?]{
  Creates a frame, sets up the current error and output ports, and
  turns on the busy cursor before calling @racket[do-install] in a separate
  thread. 

  Returns before the installation process is complete;
  @racket[cleanup-thunk] is called on a queued callback to the
  eventspace active when @racket[with-installer-window] is
  invoked.}

@defproc[(run-single-installer (file path-string?)
                               (get-dir-proc (-> (or/c path-string? false/c))))
         void?]{
  The same as the export from @racketmodname[setup/plt-single-installer], 
  but with a GUI.}

@; ----------------------------------------

@subsubsection{GUI Unpacking Signature}
      
@defmodule[setup/plt-installer-sig]{
  @defsignature[setup:plt-installer^ ()]{
  Provides two names: @racket[run-installer] and @racket[on-installer-run].}
}

@; ----------------------------------------

@subsubsection{GUI Unpacking Unit}

@defmodule[setup/plt-installer-unit]{

Imports @racket[mred^] and exports @racket[setup:plt-installer^]. }

@; ------------------------------------------------------------------------

@subsection[#:tag "unpacking-.plt-archives"]{Unpacking @filepath{.plt} Archives}

@defmodule[setup/unpack]{The @racketmodname[setup/unpack]
library provides raw support for unpacking a @filepath{.plt} file.}

@defproc[(unpack [archive path-string?]
                 [main-collects-parent-dir path-string? (current-directory)]
                 [print-status (string? . -> . any) (lambda (x) (printf "~a\n" x))]
                 [get-target-directory (-> path-string?) (lambda () (current-directory))]
                 [force? any/c #f]
                 [get-target-plt-directory
                  (path-string? 
                   path-string? 
                   (listof path-string?) 
                   . -> . path-string?)
                  (lambda (_preferred-dir _main-dir _options)
                    _preferred-dir)])
          void?]{

Unpacks @racket[archive]. 

The @racket[main-collects-parent-dir] argument is passed along to
@racket[get-target-plt-directory].

The @racket[print-status] argument is used to report unpacking
progress.

The @racket[get-target-directory] argument is used to get the
destination directory for unpacking an archive whose content is
relative to an arbitrary directory.

If @racket[force?] is true, then version and required-collection
mismatches (comparing information in the archive to the current
installation) are ignored.

The @racket[get-target-plt-directory] function is called to select a
target for installation for an archive whose is relative to the
installation. The function should normally return one if its first two
arguments; the third argument merely contains the first two, but has
only one element if the first two are the same. If the archive does
not request installation for all uses, then the first two arguments
will be different, and the former will be a user-specific location,
while the second will refer to the main installation.}

@defproc[(fold-plt-archive [archive path-string?]
                           [on-config-fn (any/c any/c . -> . any/c)]
                           [on-setup-unit (any/c input-port? any/c . -> . any/c)]
                           [on-directory (path-string? any/c . -> . any/c)]
                           [on-file (path-string? input-port? any/c . -> . any/c)]
                           [initial-value any/c])
          any/c]{

Traverses the content of @racket[archive], which must be a
@filepath{.plt} archive that is created with the default unpacking
unit and configuration expression. The configuration expression is not
evaluated, the unpacking unit is not invoked, and not files are
unpacked to the filesystem. Instead, the information in the archive is
reported back through @racket[on-config], @racket[on-setup-unit],
@racket[on-directory], and @racket[on-file], each of which can build on
an accumulated value that starts with @racket[initial-value] and whose
final value is returned.

The @racket[on-config-fn] function is called once with an S-expression
that represents a function to implement configuration information.
The second argument to @racket[on-config] is @racket[initial-value],
and the function's result is passes on as the last argument to @racket[on-setup-unit].

The @racket[on-setup-unit] function is called with the S-expression
representation of the installation unit, an input port that points to
the rest of the file, and the accumulated value. This input port is
the same port that will be used in the rest of processing, so if
@racket[on-setup-unit] consumes any data from the port, then that data
will not be consumed by the remaining functions. (This means that
on-setup-unit can leave processing in an inconsistent state, which is
not checked by anything, and therefore could cause an error.)
The result of @racket[on-setup-unit] becomes the new accumulated value.

For each directory that would be created by the archive when unpacking
normally, @racket[on-directory] is called with the directory path and the
accumulated value up to that point, and its result is the new
accumulated value.

For each file that would be created by the archive when unpacking
normally, @racket[on-file] is called with the file path, an input port
containing the contents of the file, and the accumulated value up to
that point; its result is the new accumulated value. The input port
can be used or ignored, and parsing of the rest of the file continues
the same either way. After @racket[on-file] returns control, however,
the input port is drained of its content.}

@; ------------------------------------------------------------------------

@subsection[#:tag "format-of-.plt-archives"]{
  Format of @filepath{.plt} Archives}

The extension @filepath{.plt} is not required for a distribution
archive, but the @filepath{.plt}-extension convention helps users
identify the purpose of a distribution file.

The raw format of a distribution file is described below. This format
is uncompressed and sensitive to communication modes (text
vs. binary), so the distribution format is derived from the raw format
by first compressing the file using @exec{gzip}, then encoding the gzipped
file with the MIME base64 standard (which relies only the characters
@litchar{A}-@litchar{Z}, @litchar{a}-@litchar{z}, @litchar{0}-@litchar{9}, 
@litchar{+}, @litchar{/}, and @litchar{=}; all other characters are ignored
when a base64-encoded file is decoded).

The raw format is

@itemize[
  @item{
    @litchar{PLT} are the first three characters.}

  @item{
    A procedure that takes a symbol and a failure thunk and returns
    information about archive for recognized symbols and calls the
    failure thunk for unrecognized symbols. The information symbols
    are:
    
    @itemize[
      @item{
        @racket['name] --- a human-readable string describing the archive's
        contents. This name is used only for printing messages to the
        user during unpacking.}

      @item{
        @racket['unpacker] --- a symbol indicating the expected unpacking
        environment. Currently, the only allowed value is @racket['mzscheme].}

      @item{
        @racket['requires] --- collections required to be installed before
        unpacking the archive, which associated versions; see the
        documentation of @racket[pack] for details.}

     @item{
        @racket['conflicts] --- collections required @emph{not} to be installed
        before unpacking the archive.}

     @item{
        @racket['plt-relative?] --- a boolean; if true, then the archive's
        content should be unpacked relative to the plt add-ons directory.}

     @item{
        @racket['plt-home-relative?] --- a boolean; if true and if
        @racket['plt-relative?] is true, then the archive's content should be
        unpacked relative to the Racket installation.}

     @item{
        @racket['test-plt-dirs] --- @racket[#f] or a list of path strings;
        in the latter case, a true value of @racket['plt-home-relative?] is
        cancelled if any of the directories in the list (relative to the
        Racket installation) is unwritable by the user.}
   ]

   The procedure is extracted from the archive using the @racket[read]
   and @racket[eval] procedures in a fresh namespace.  }

 @item{
   An old-style, unsigned unit using @racket[(lib mzlib/unit200)] that
   drives the unpacking process. The unit accepts two imports: a path
   string for the parent of the main @filepath{collects} directory and
   an @racket[unmztar] procedure. The remainder of the unpacking
   process consists of invoking this unit. It is expected that the
   unit will call @racket[unmztar] procedure to unpack directories and
   files that are defined in the input archive after this unit. The
   result of invoking the unit must be a list of collection paths
   (where each collection path is a list of strings); once the archive
   is unpacked, @exec{raco setup} will compile and setup the specified
   collections.

   The @racket[unmztar] procedure takes one argument: a filter
   procedure. The filter procedure is called for each directory and
   file to be unpacked. It is called with three arguments:

   @itemize[
      @item{
        @racket['dir], @racket['file], @racket['file-replace] 
        --- indicates whether the item to be
        unpacked is a directory, a file, or a file to be replaced, }

      @item{
        a relative path string --- the pathname of the directory or file
        to be unpacked, relative to the unpack directory, and}

      @item{
        a path string for the unpack directory (which can vary for a
        Racket-relative install when elements of the archive start with
        @racket["collects"], @racket["lib"], etc.).}
   ]
   
   If the filter procedure returns @racket[#f] for a directory or file, the
   directory or file is not unpacked. If the filter procedure returns
   @racket[#t] and the directory or file for @racket['dir] or @racket['file]
   already exists, it is not created. (The file for @racket[file-replace]
   need not exist already.)

   When a directory is unpacked, intermediate directories are created
   as necessary to create the specified directory. When a file is
   unpacked, the directory must already exist.

   The unit is extracted from the archive using @racket[read] and
   @racket[eval].}  ]

Assuming that the unpacking unit calls the @racket[unmztar] procedure, the
archive should continue with @tech{unpackables}. @tech{Unpackables} are
extracted until the end-of-file is found (as indicated by an @litchar{=}
in the base64-encoded input archive).

An @deftech{unpackable} is one of the following:

@itemize[
   @item{
     The symbol @racket['dir] followed by a list. The @racket[build-path]
     procedure will be applied to the list to obtain a relative path for
     the directory (and the relative path is combined with the target
     directory path to get a complete path).

     The @racket['dir] symbol and list are extracted from the archive
     using @racket[read] (and the result is @emph{not}
     @racket[eval]uated).}

   @item{
     The symbol @racket['file], a list, a number, an asterisk, and the file
     data. The list specifies the file's relative path, just as for
     directories. The number indicates the size of the file to be
     unpacked in bytes. The asterisk indicates the start of the file
     data; the next n bytes are written to the file, where n is the
     specified size of the file.

     The symbol, list, and number are all extracted from the archive
     using @racket[read] (and the result is @emph{not}
     @racket[eval]uated). After the number is read, input characters
     are discarded until an asterisk is found. The file data must
     follow this asterisk immediately.}
   
   @item{
     The symbol @racket['file-replace] is treated like @racket['file], 
     but if the file exists on disk already, the file in the archive replaces
     the file on disk.}
]

@; ----------------------------------------------------------

@section[#:tag "dirs"]{API for Finding Installation Directories}

@defmodule[setup/dirs]{
  The @racketmodname[setup/dirs] library provides several procedures for locating
  installation directories:}

@defproc[(find-collects-dir) (or/c path? false/c)]{
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

@defproc[(find-doc-dir) (or/c path? false/c)]{
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

@defproc[(find-lib-dir) (or/c path? false/c)]{
  Returns a path to the installation's @filepath{lib} directory, which contains
  libraries and other build information. The result is @racket[#f] if no such
  directory is available.}

@defproc[(find-dll-dir) (or/c path? false/c)]{
  Returns a path to the directory that contains DLLs for use with the
  current executable (e.g., @filepath{libmzsch.dll} under Windows).
  The result is @racket[#f] if no such directory is available, or if no
  specific directory is available (i.e., other than the platform's normal
  search path).}

@defproc[(find-user-lib-dir) path?]{
  Returns a path to a user-specific @filepath{lib} directory; the directory
  indicated by the returned path may or may not exist.}

@defproc[(get-lib-search-dirs) (listof path?)]{
  Returns a list of paths to search for libraries. Unless it is
  configured otherwise, the result includes any non-@racket[#f] result of
  @racket[(find-lib-dir)], @racket[(find-dll-dir)],
  and @racket[(find-user-lib-dir)]---but the last is included only if the
  value of the @racket[use-user-specific-search-paths] parameter
  is @racket[#t].}

@defproc[(find-include-dir) (or/c path? false/c)]{
  Returns a path to the installation's @filepath{include} directory, which
  contains @filepath{.h} files for building MzRacket extensions and embedding
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

@defproc[(find-console-bin-dir) (or/c path? false/c)]{
  Returns a path to the installation's executable directory, where the
  stand-alone MzRacket executable resides. The result is @racket[#f] if no
  such directory is available.}

@defproc[(find-gui-bin-dir) (or/c path? false/c)]{
  Returns a path to the installation's executable directory, where the
  stand-alone GRacket executable resides. The result is @racket[#f] if no such
  directory is available.}

@defthing[absolute-installation? boolean?]{
  A binary boolean flag that is true if this installation is using
  absolute path names.}

@; ------------------------------------------------------------------------

@section[#:tag "getinfo"]{API for Reading @filepath{info.rkt} Files}

@defmodule[setup/getinfo]{ The @racketmodname[setup/getinfo] library
   provides functions for accessing fields in @filepath{info.rkt}
   files.}

@defproc[(get-info (collection-names (listof string?)))
         (or/c
          (symbol? [(-> any)] . -> . any)
          false/c)]{
   Accepts a list of strings naming a collection or sub-collection,
   and calls @racket[get-info/full] with the full path corresponding to the
   named collection.}

@defproc[(get-info/full (path path?))
         (or/c
          (symbol? [(-> any)] . -> . any)
          false/c)]{

   Accepts a path to a directory. If it finds either a well-formed
   an @filepath{info.rkt} file or an @filepath{info.rkt} file (with
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
   
   @racket[get-info/full] returns @racket[#f] if there is
   no @filepath{info.rkt} or @filepath{info.rkt} file in the directory. If there is a
   @filepath{info.rkt} file that has the wrong shape (i.e., not a module
   using @racketmodname[setup/infotab] or @racket[(lib "infotab.rkt" "setup")]),
   or if the @filepath{info.rkt} file fails to load, then an exception
   is raised. If the @filepath{info.rkt} file loaded, @racket[get-info/full]
   returns the @racket[get-info] file. If the @filepath{info.rkt} file does not exist, 
   then @racket[get-info/full] does
   the same checks for the @filepath{info.rkt} file, either raising an exception
   or returning the @racket[get-info] function from the @filepath{info.rkt} file.}

@defproc[(find-relevant-directories
          (syms (listof symbol?))
          (mode (symbols 'preferred 'all-available) 'preferred)) (listof path?)]{

   Returns a list of paths identifying installed directories (i.e.,
   collections and installed @|PLaneT| packages) whose
   @filepath{info.rkt} file defines one or more of the given
   symbols. The result is based on a cache that is computed by
   @exec{raco setup} and stored in the @indexed-file{info-domain}
   sub-directory of each collection directory (as determined by the
   @envvar{PLT_COLLECTION_PATHS} environment variable, etc.) and the
   file @filepath{cache.rkt} in the user add-on directory.

   The result is in a canonical order (sorted lexicographically by
   directory name), and the paths it returns are suitable for
   providing to @racket[get-info/full].

   If @racket[mode] is specified, it must be either
   @racket['preferred] (the default) or @racket['all-available]. If
   mode is @racket['all-available], @racket[find-relevant-collections]
   returns all installed directories whose info files contain the
   specified symbols---for instance, all installed PLaneT packages
   will be searched if @racket['all-available] is specified. If mode
   is @racket['preferred], then only a subset of ``preferred''
   packages will be searched, and in particular only the directory
   containing the most recent version of any PLaneT package will be
   returned.

   No matter what @racket[mode] is specified, if more than one
   collection has the same name, @racket[find-relevant-directories]
   will only search the one that occurs first in the
   @envvar{PLT_COLLECTION_PATHS} environment variable.}

@defproc[(reset-relevant-directories-state!) void?]{
   Resets the cache used by @racket[find-relevant-directories].}

@; ------------------------------------------------------------------------

@section[#:tag "main-collects"]{API for Paths Relative to @filepath{collects}}

@defmodule[setup/main-collects]

@defproc[(path->main-collects-relative [path (or/c bytes? path-string?)])
         (or/c path? (cons/c 'collects (listof bytes?)))]{

Checks whether @racket[path] has a prefix that matches the prefix to
the main @filepath{collects} directory as determined by
@racket[(find-collects-dir)]. If so, the result is a list starting
with @racket['collects] and containing the remaining path elements as
byte strings. If not, the path is returned as-is.

The @racket[path] argument should be a complete path. Applying
@racket[simplify-path] before @racket[path->main-collects-relative] is
usually a good idea.

For historical reasons, @racket[path] can be a byte string, which is
converted to a path using @racket[bytes->path].}

@defproc[(main-collects-relative->path
          [rel (or/c bytes? path-string?
                     (cons/c 'collects
                             (or/c (listof bytes?) bytes?)))])
         path?]{


The inverse of @racket[path->main-collects-relative]: if @racket[rel]
is a pair that starts with @racket['collects], then it is converted
back to a path relative to @racket[(find-collects-dir)].

For historical reasons, a single byte string is allowed in place of a
list of byte strings after @racket['collects], in which case it is
assumed to be a relative path after conversion with
@racket[bytes->path].

Also for historical reasons, if @racket[rel] is any kind of value other
than specified in the contract above, it is returned as-is.}

@; ------------------------------------------------------------------------

@section[#:tag "xref"]{API for Cross-References for Installed Manuals}

@defmodule[setup/xref]

@defproc[(load-collections-xref [on-load (-> any/c) (lambda () (void))])
         xref?]{

Like @racket[load-xref], but automatically find all cross-reference files for
manuals that have been installed with @exec{setup-plt}.}
