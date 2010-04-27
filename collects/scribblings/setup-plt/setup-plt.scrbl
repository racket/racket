#lang scribble/doc

@(require scribble/manual
          scribble/bnf
         (for-label scheme
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
                    scheme/gui/base))

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

@title{@|setup-plt|: PLT Configuration and Installation}

The @|setup-plt| executable finds, compiles, configures, and installs
documentation for all collections in a PLT Scheme installation. It can
also install single @filepath{.plt} files.

@table-of-contents[]

@; ------------------------------------------------------------------------
@; ------------------------------------------------------------------------

@section[#:tag "running"]{Running @|setup-plt| Executable}

The @|setup-plt| executable performs two main services:

@itemize[

 @item{@bold{Compiling and setting up all (or some of the)
   collections:} When @|setup-plt| is run without any arguments, it
   finds all of the current collections (see @secref[#:doc
   ref-src]{collects}) and compiles libraries in each collection.

   An optional @filepath{info.ss} within the collection can indicate
   specifically how the collection's files are to be compiled and
   other actions to take in setting up a collection, such as creating
   executables or building documentation. See @secref["setup-info"]
   for more information.

   The @DFlag{clean} (or @Flag{c}) flag to @|setup-plt| causes it to
   delete existing @filepath{.zo} files, thus ensuring a clean build
   from the source files. The exact set of deleted files can be
   controlled by @filepath{info.ss}; see
   @elemref["clean"]{@scheme[clean]} for more information.

   The @Flag{l} flag takes one or more collection names and restricts
   @|setup-plt|'s action to those collections.

   The @DFlag{mode} @nonterm{mode} flag causes @|setup-plt| to use a
   @filepath{.zo} compiler other than the default compiler, and to put
   the resulting @filepath{.zo} files in a subdirectory (of the usual
   place) named by @nonterm{mode}. The compiler is obtained by using
   @nonterm{mode} as a collection name, finding a
   @filepath{zo-compile.ss} module in that collection, and extracting
   its @scheme[zo-compile] export. The @scheme[zo-compile] export
   should be a function like @scheme[compile]; see the
   @filepath{errortrace} collection for an example.}

 @item{@bold{Unpacking @filepath{.plt} files:} A
   @filepath{.plt} file is a platform-independent distribution archive
   for software based on PLT Scheme. When one or more file names are
   provided as the command line arguments to @|setup-plt|, the files
   contained in the @filepath{.plt} archive are unpacked (according to
   specifications embedded in the @filepath{.plt} file) and only
   collections specified by the @filepath{.plt} file are compiled and
   setup.}]

Run @|setup-plt| with the @Flag{h} flag to see a list of all options
accepted by the @|setup-plt| executable.

@; ------------------------------------------------------------------------

@subsection[#:tag "setup-info"]{Controlling @|setup-plt| with @filepath{info.ss} Files}

To compile a collection's files to bytecode, @|setup-plt| uses the
@scheme[compile-collection-zos] procedure. That procedure, in turn,
consults the collection's @filepath{info.ss} file, if it exists, for
specific instructions on compiling the collection. See
@scheme[compile-collection-zos] for more information on the fields of
@filepath{info.ss} that it uses, and see @secref["info.ss"] for
information on the format of an @filepath{info.ss} file.

Optional @filepath{info.ss} fields trigger additional actions by
@|setup-plt|:

@itemize[

 @item{@as-index{@schemeidfont{scribblings}} : @scheme[(listof (cons/c string? list?))] ---
   A list of documents to build. Each document in the list is itself
   represented as a list, where each document's list starts with a
   string that is a collection-relative path to the document's source
   file.

   More precisely a @schemeidfont{scribblings} entry must be a value
   that can be generated from an expression matching the following
   @scheme[entry] grammar:

   @schemegrammar*[
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

   Each mode symbol in @scheme[_flags] can be one of the following,
   where only @scheme['multi-page] is commonly used:

   @itemize[

     @item{@scheme['multi-page] : Generates multi-page HTML output,
           instead of the default single-page format.}

     @item{@scheme['main-doc] : Indicates that the generated
           documentation should be written into the main installation
           directory, instead of to a user-specific directory. This
           mode is the default for a collection that is itself located
           in the main installation.}

     @item{@scheme['user-doc] : Indicates that the generated
           documentation should be written a user-specific
           directory. This mode is the default for a collection that
           is not itself located in the main installation.}

     @item{@scheme['depends-all] : Indicates that the document should
           be re-built if any other document is rebuilt---except for
           documents that have the @scheme['no-depends-on] mode.}

     @item{@scheme['depends-all-main] : Indicates that the document
           should be re-built if any other document is rebuilt that is
           installed into the main installation---except for documents
           that have the @scheme['no-depends-on] mode.}

     @item{@scheme['always-run] : Build the document every time that
           @|setup-plt| is run, even if none of its dependencies
           change.}

     @item{@scheme['no-depend-on] : Removes the document for
           consideration for other dependencies. This mode is
           typically used with @scheme['always-run] to avoid
           unnecessary dependencies that prevent reaching a stable
           point in building documentation.}

     @item{@scheme['main-doc-root] : Designates the root document for
           the main installation. The document that currently has this
           mode should be the only one with the mode.}

     @item{@scheme['user-doc-root] : Designates the root document for
           the user-specific documentation directory. The document
           that currently has this mode should be the only one with
           the mode.}

    ]

    The @scheme[_category] list specifies how to show the document in
    the root table of contents. The list must start with a symbol,
    usually one of the following categories, which are ordered as
    below in the root documentation page:

   @itemize[

     @item{@scheme['getting-started] : High-level, introductory
           documentation.}

     @item{@scheme['language] : Documentation for a prominent
           programming language.}

     @item{@scheme['tool] : Documentation for an executable.}

     @item{@scheme['gui-library] : Documentation for GUI and graphics
           libraries.}

     @item{@scheme['net-library] : Documentation for networking
           libraries.}

     @item{@scheme['parsing-library] : Documentation for parsing
           libraries.}

     @item{@scheme['tool-library] : Documentation for programming-tool
           libraries (i.e., not important enough for the more
           prominent @scheme['tool] category).}

     @item{@scheme['interop] : Documentation for interoperability
           tools and libraries.}

     @item{@scheme['library] : Documentation for libraries; this
           category is the default and used for unrecognized category
           symbols.}

     @item{@scheme['legacy] : Documentation for deprecated libraries,
           languages, and tools.}

     @item{@scheme['experimental] : Documentation for an experimental
           language or library.}

     @item{@scheme['other] : Other documentation.}

     @item{@scheme['omit] : Documentation that should not be listed on
           the root page.}

   ]

   If the category list has a second element, it must be a real number
   that designates the manual's sorting position with the category;
   manuals with the same sorting position are ordered
   alphabetically. For a pair of manuals with sorting numbers
   @scheme[_n] and @scheme[_m], the groups for the manuals are
   separated by space if @scheme[(truncate (/ _n 10))]and
   @scheme[(truncate (/ _m 10))] are different.}

 @item{@scheme[mzscheme-launcher-names] : @scheme[(listof string?)]
   --- @elemtag["mzscheme-launcher-names"] A list of executable names
   to be generated in the installation's executable directory to run
   MzScheme-based programs implemented by the collection. A parallel
   list of library names must be provided by
   @scheme[mzscheme-launcher-libraries] or
   @scheme[mzscheme-launcher-flags].

   For each name, a launching executable is set up using
   @scheme[make-mzscheme-launcher].  The arguments are @Flag{l-} and
   @tt{@nonterm{colls}/.../@nonterm{file}}, where @nonterm{file} is
   the file named by @scheme[mzscheme-launcher-libraries] and
   @tt{@nonterm{colls}/...}  are the collections (and subcollections)
   of the @filepath{info.ss} file.

   In addition,

   @schemeblock[
    (build-aux-from-path
     (build-path (collection-path #,(nonterm "colls") _...) #,(nonterm "suffixless-file")))
   ]

   is provided for the optional @scheme[_aux] argument (for icons,
   etc.) to @scheme[make-mzscheme-launcher], where where
   @nonterm{suffixless-file} is @nonterm{file} without its suffix.

   If @scheme[mzscheme-launcher-flags] is provided, it is used as a
   list of command-line arguments passed to @exec{mzscheme} instead of
   the above default, allowing arbitrary command-line arguments. If
   @scheme[mzscheme-launcher-flags] is specified together with
   @scheme[mzscheme-launcher-libraries], then the flags will override
   the libraries, but the libraries can still be used to specify a
   name for @scheme[build-aux-from-path] (to find related information
   like icon files etc).}

 @item{@scheme[mzscheme-launcher-libraries] : @scheme[(listof
   path-string?)] --- A list of library names in parallel to
   @elemref["mzscheme-launcher-names"]{@scheme[mzscheme-launcher-names]}.}

 @item{@scheme[mzscheme-launcher-flags] : @scheme[(listof string?)]
   --- A list of command-line flag lists, in parallel to
   @elemref["mzscheme-launcher-names"]{@scheme[mzscheme-launcher-names]}.}

 @item{@scheme[mred-launcher-names] : @scheme[(listof string?)]  ---
   @elemtag["mred-launcher-names"] Like
   @elemref["mzscheme-launcher-names"]{@scheme[mzscheme-launcher-names]},
   but for MrEd-based executables. The launcher-name list is treated
   in parallel to @scheme[mred-launcher-libraries] and
   @scheme[mred-launcher-flags].}

 @item{@scheme[mred-launcher-libraries] : @scheme[(listof path-string?)]
   --- A list of library names in parallel to
   @elemref["mred-launcher-names"]{@scheme[mred-launcher-names]}.}

 @item{@scheme[mred-launcher-flags] : @scheme[(listof string?)] --- A
   list of command-line flag lists, in parallel to
   @elemref["mred-launcher-names"]{@scheme[mred-launcher-names]}.}

 @item{@scheme[install-collection] : @scheme[path-string?]  --- A
   library module relative to the collection that provides
   @scheme[installer]. The @scheme[installer] procedure accepts either
   one or two arguments. The first argument is a directory path to the
   parent of the PLT installation's @filepath{collects} directory; the
   second argument, if accepted, is a path to the collection's own
   directory. The procedure should perform collection-specific
   installation work, and it should avoid unnecessary work in the case
   that it is called multiple times for the same installation.}

 @item{@scheme[pre-install-collection] : @scheme[path-string?] ---
   Like @scheme[install-collection], except that the corresponding
   installer is called @emph{before} the normal @filepath{.zo} build,
   instead of after. The provided procedure should be named
   @scheme[pre-installer] in this case, so it can be provided by the
   same file that provides an @scheme[installer].}

 @item{@scheme[post-install-collection] : @scheme[path-string?]  ---
   Like @scheme[install-collection]. It is called right after the
   @scheme[install-collection] procedure is executed. The only
   difference between these is that the @DFlag{no-install} flag can be
   used to disable the previous two installers, but not this one.  It
   is therefore expected to perform operations that are always needed,
   even after an installation that contains pre-compiled files. The
   provided procedure should be named @scheme[post-installer] in this
   case, so it can be provided by the same file that provides the
   previous two.}

 @item{@scheme[clean] : @scheme[(listof path-string?)] ---
   @elemtag["clean"] A list of pathnames to be deleted when the
   @DFlag{clean} or @Flag{c} flag is passed to @|setup-plt|. The
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
   by @|setup-plt|. If the @filepath{.dep} file indicates another
   module, that module's @filepath{.zo} is deleted only if it also has
   an accompanying @filepath{.dep} file. In that case, the
   @filepath{.dep} file is deleted, and additional used modules are
   deleted based on the used module's @filepath{.dep} file, etc.
   Supplying a specific list of collections to @|setup-plt| disables
   this dependency-based deletion of compiled files.}

]

@; ------------------------------------------------------------------------

@section[#:tag "setup-plt-plt"]{Running @|setup-plt| from Scheme}

The @schememodname[setup/setup-unit] library provides @|setup-plt| in unit
form. The associated @scheme[setup/option-sig] and
@scheme[setup/option-unit] libraries provides the interface for
setting options for the run of @|setup-plt|.

For example, to unpack a single @filepath{.plt} archive
@filepath{x.plt}, set the @sigelem[setup-option^ archives] parameter
to @scheme[(list "x.plt")] and leave @sigelem[setup-option^
specific-collections] as @scheme[null].

Link the options and setup units so that your option-setting code is
initialized between them, e.g.:

@schemeblock[
(compound-unit
  _...
  (link _...
    [(OPTIONS : setup-option^) setup:option@]
    [() my-init-options@ OPTIONS]
    [() setup@ OPTIONS _...])
  _...)
]

@subsection{@|setup-plt| Unit}

@defmodule[setup/setup-unit]

@defthing[setup@ unit?]{

Imports

@itemize[#:style "compact"]{
    @item{@scheme[setup-option^]}
    @item{@scheme[compiler^]}
    @item{@scheme[compiler:option^]}
    @item{@scheme[launcher^]}}

and exports nothing. Invoking @scheme[setup@] starts the setup process.}

@; ----------------------------------------

@subsection{Options Unit}

@defmodule[setup/option-unit]

@defthing[setup:option@ unit?]{

Imports nothing and exports @scheme[setup-option^].}

@; ----------------------------------------

@subsection{Options Signature}

@defmodule[setup/option-sig]

@defsignature[setup-option^ ()]{

@signature-desc{Provides parameters used to control @|setup-plt| in unit
form.}

@defboolparam[verbose on?]{
  If on, prints message from @exec{make} to @envvar{stderr}.
  @defaults[@scheme[#f]]}

@defboolparam[make-verbose on?]{
  If on, verbose @exec{make}. @defaults[@scheme[#f]]}

@defboolparam[compiler-verbose on?]{
  If on, verbose @exec{compiler}. @defaults[@scheme[#f]]}

@defboolparam[clean on?]{
 If on, delete @filepath{.zo} and
 @filepath{.so}/@filepath{.dll}/@filepath{.dylib} files in the
 specified collections. @defaults[@scheme[#f]]}

@defparam[compile-mode path (or/c path? false/c)]{
  If a @scheme[path] is given, use a @filepath{.zo} compiler other than plain
  @exec{compile}, and build to @scheme[(build-path "compiled" (compile-mode))].
  @defaults[@scheme[#f]]}

@defboolparam[make-zo on?]{
  If on, compile @filepath{.zo}. @defaults[@scheme[#t]]}

@defboolparam[make-so on?]{
  If on, compile @filepath{.so}/@filepath{.dll} files. @defaults[@scheme[#f]]}

@defboolparam[make-launchers on?]{
  If on, make collection @filepath{info.ss}-specified launchers. @defaults[@scheme[#t]]}

@defboolparam[make-info-domain on?]{
  If on, update @filepath{info-domain/compiled/cache.ss} for each
  collection path. @defaults[@scheme[#t]]}

@defboolparam[avoid-main-installation on?]{
 If on, avoid building bytecode in the main installation tree when building
 other bytecode (e.g., in a user-specific collection). @defaults[@scheme[#f]]}

@defboolparam[call-install on?]{
  If on, call collection @filepath{info.ss}-specified setup code.
  @defaults[@scheme[#t]]}

@defboolparam[force-unpack on?]{
  If on, ignore version and already-installed errors when unpacking a
  @filepath{.plt} archive. @defaults[@scheme[#f]]}

@defboolparam[pause-on-errors on?]{
  If on, in the event of an error, prints a summary error and waits for
  @envvar{stdin} input before terminating. @defaults[@scheme[#f]]}

@defparam[specific-collections coll (listof path-string?)]{
  A list of collections to set up; the empty list means set-up all
  collections if the archives list is also empty @defaults[@scheme[null]]}

@defparam[archives arch (listof path-string?)]{
  A list of @filepath{.plt} archives to unpack; any collections specified
  by the archives are set-up in addition to the collections listed in
  specific-collections. @defaults[@scheme[null]]}

@defboolparam[archive-implies-reindex on?]{
  If on, when @scheme[archives] has a non-empty list of packages, if any
  documentation is built, then suitable documentation start pages, search pages,
  and master index pages are re-built. @defaults[@scheme[#t]]}

@defparam[current-target-directory-getter thunk (-> . path-string?)]{
  A thunk that returns the target directory for unpacking a relative
  @filepath{.plt} archive; when unpacking an archive, either this or
  the procedure in @scheme[current-target-plt-directory-getter] will
  be called. @defaults[@scheme[current-directory]]}

@defparam[current-target-plt-directory-getter
          proc (path-string?
                path-string?
                (listof path-string?) . -> . path-string?)]{
  A procedure that takes a preferred path, a path to the parent of the main
  @filepath{collects} directory, and a list of path choices; it returns
  a path for a "plt-relative" install; when unpacking an archive, either
  this or the procedure in @scheme[current-target-directory-getter] will
  be called, and in the former case, this procedure may be called
  multiple times. @defaults[@scheme[(lambda (preferred main-parent-dir choices) preferred)]]}

}

@; ------------------------------------------------------------------------

@section[#:tag ".plt-archives"]{@filepath{.plt} Archives}

@; ------------------------------------------------------------------------
   
@subsection[#:tag "making-.plt-archives"]{Making @filepath{.plt} Archives}


@defmodule[setup/pack]{Although the @exec{mzc} executable can be used
to create @filepath{.plt} files (see @other-manual['(lib
"scribblings/mzc/mzc.scrbl")]), the
@schememodname[setup/pack] library provides a more general Scheme API
for making @filepath{.plt} archives:}

@defproc[(pack-collections-plt
          (dest path-string?)
          (name string?)
          (collections (listof (listof path-string?)))
          [#:replace? replace? boolean? #f]
          [#:at-plt-home? at-home? boolean? #f]
          [#:test-plt-collects? test? boolean? #t]
          [#:extra-setup-collections collection-list (listof path-string?) null] 
          [#:file-filter filter-proc (path-string? . -> . boolean?) std-filter]) void?]{

  Creates the @filepath{.plt} file specified by the pathname @scheme[dest],
  using the @scheme[name] as the name reported to @|setup-plt|
  as the archive's description.

  The archive contains the collections listed in @scheme[collections], which
  should be a list of collection paths; each collection path is, in
  turn, a list of relative-path strings.

  If the @scheme[#:replace?] argument is @scheme[#f], then attempting to
  unpack the archive will report an error when any of the collections exist
  already, otherwise unpacking the archive will overwrite an existing
  collection.

  If the @scheme[#:at-plt-home?] argument is @scheme[#t], then the archived
  collections will be installed into the PLT installation directory
  instead of the user's directory if the main @filepath{collects} directory
  is writable by the user. If the @scheme[#:test-plt-collects?] argument is
  @scheme[#f] (the default is @scheme[#t]) and the @scheme[#:at-plt-home?] argument
  is @scheme[#t], then installation fails if the main @filepath{collects}
  directory is not writable.

  The optional @scheme[#:extra-setup-collections] argument is a list of
  collection paths that are not included in the archive, but are
  set-up when the archive is unpacked.

  The optional @scheme[#:file-filter] argument is the same as for
  @scheme[pack-plt].}

@defproc[(pack-collections
          (dest path-string?)
          (name string?)
          (collections (listof (listof path-string?)))
          (replace? boolean?)
          (extra-setup-collections (listof path-string?))
          [filter (path-string? . -> . boolean?) std-filter]
          [at-plt-home? boolean? #f]) void?]{
  Old, keywordless variant of @scheme[pack-collections-plt] for backward compatibility.}

@defproc[(pack-plt
            (dest path-string?)
            (name string?)
            (paths (listof path-string?))
            [#:file-filter filter-proc
                           (path-string? . -> . boolean?) std-filter]
            [#:encode? encode? boolean? #t]
            [#:file-mode file-mode-sym symbol? 'file]
            [#:unpack-unit unit200-expr any/c #f]
            [#:collections collection-list (listof path-string?) null]
            [#:plt-relative? plt-relative? any/c #f]
            [#:at-plt-home? at-plt-home? any/c #f]
            [#:test-plt-dirs dirs (or/c (listof path-string?) false/c) #f]
            [#:requires mod-and-version-list
                        (listof (listof path-string?)
                                (listof exact-integer?))
                        null]
            [#:conflicts mod-list
                         (listof (listof path-string?)) null])
         void?]{

  Creates the @filepath{.plt} file specified by the pathname @scheme[dest],
  using the string @scheme[name] as the name reported to @|setup-plt| as
  the archive's description. The @scheme[paths] argument must be a list of
  relative paths for directories and files; the contents of these files and
  directories will be packed into the archive.

  The @scheme[#:file-filter] procedure is called with the relative path of each
  candidate for packing. If it returns @scheme[#f] for some path, then that
  file or directory is omitted from the archive. If it returns @scheme['file]
  or @scheme['file-replace] for a file, the file is packed with that mode,
  rather than the default mode. The default is @scheme[std-filter].
  
  If the @scheme[#:encode?] argument is @scheme[#f], then the output archive
  is in raw form, and still must be gzipped and mime-encoded (in that
  order). The default value is @scheme[#t].

  The @scheme[#:file-mode] argument must be @scheme['file] or
  @scheme['file-replace], indicating the default mode for a file in the
  archive. The default is @scheme['file].

  The @scheme[#:unpack-unit] argument is usually
  @scheme[#f]. Otherwise, it must be an S-expression for a
  @scheme[mzlib/unit200]-style unit that performs the work of
  unpacking; see @secref["format-of-.plt-archives"] more information
  about the unit. If the @scheme[#:unpack-unit] argument is
  @scheme[#f], an appropriate unpacking unit is generated.

  The @scheme[#:collections] argument is a list of collection paths to be
  compiled after the archive is unpacked. The default is the @scheme[null].

  If the @scheme[#:plt-relative?] argument is true (the default is
  @scheme[#f]), the archive's files and directories are to be unpacked
  relative to the user's add-ons directory or the PLT installation
  directories, depending on whether the @scheme[#:at-plt-home?]
  argument is true and whether directories specified by
  @scheme[#:test-plt-dirs] are writable by the user.

  If the @scheme[#:at-plt-home?] argument is true (the default is
  @scheme[#f]), then @scheme[#:plt-relative?] must be true, and the
  archive is unpacked relative to the PLT installation directory. In
  that case, a relative path that starts with @filepath{collects} is
  mapped to the installation's main @filepath{collects} directory, and
  so on, for the following the initial directory names:

  @itemize[
     @item{@filepath{collects}}
     @item{@filepath{doc}}
     @item{@filepath{lib}}
     @item{@filepath{include}}
   ]

  If @scheme[#:test-plt-dirs] is a @scheme[list], then
  @scheme[#:at-plt-home?] must be @scheme[#t]. In that case, when the archive
  is unpacked, if any of the relative directories in the
  @scheme[#:test-plt-dirs] list is unwritable by the current user, then the
  archive is unpacked in the user's add-ons directory after all.

  The @scheme[#:requires] argument should have the shape @scheme[(list
      (list _coll-path _version) _...)]  where each
      @scheme[_coll-path] is a non-empty list of relative-path
      strings, and each @@scheme[_version] is a (possibly empty) list
      of exact integers. The indicated collections must be installed
      at unpacking time, with version sequences that match as much of
      the version sequence specified in the corresponding
      @@scheme[_version]. A collection's version is indicated by the
      @schemeidfont{version} field of its @filepath{info.ss} file.

  The @scheme[#:conflicts] argument should have the shape
       @scheme[(list _coll-path _...)]  where each @scheme[_coll-path]
       is a non-empty list of relative-path strings. The indicated
       collections must @emph{not} be installed at unpacking time.}

@defproc[(pack
          (dest path-string?)
          (name string?)
          (paths (listof path-string?))
          (collections (listof path-string?))
          [filter (path-string? . -> . boolean?) std-filter]
          [encode? boolean? #t]
          [file-mode symbol? 'file]
          [unpack-unit boolean? #f]
          [plt-relative? boolean? #t]
          [requires (listof (listof path-string?)
                            (listof exact-integer?)) null]
          [conflicts (listof (listof path-string?)) null]
          [at-plt-home? boolean? #f]) void?]{
  Old, keywordless variant of @scheme[pack-plt] for backward compatibility.}

@defproc[(std-filter (p path-string?)) boolean?]{
  Returns @scheme[#t] unless @scheme[p], after stripping its
  directory path and converting to a byte string, matches one of the
  following regular expressions: @litchar{^CVS$}, @litchar{^[.]svn$},
  @litchar{^[.]cvsignore}, @litchar{^compiled$}, @litchar{^doc}, 
  @litchar{~$}, @litchar{^#.*#$}, @litchar{^[.]#}, or @litchar{[.]plt$}.}

@defproc[(mztar (path path-string?)
                (output output-port?)
                (filter (path-string? . -> . boolean?))
                (file-mode (symbols 'file 'file-replace))) void?]{
   Called by @scheme[pack] to write one directory/file @scheme[path] to the
   output port @scheme[output] using the filter procedure @scheme[filter]
   (see @scheme[pack] for a description of @scheme[filter]). The
   @scheme[file-mode] argument specifies the default mode for packing a file,
   either @scheme['file] or @scheme['file-replace].}

@; ----------------------------------------

@subsection{Installing a Single @filepath{.plt} File}

The @schememodname[setup/plt-single-installer] module provides a
function for installing a single @filepath{.plt} file, and
@schememodname[setup/plt-installer] wraps it with a GUI
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

   The @scheme[get-dir-proc] procedure is called if the installer needs a
   target directory for installation, and a @scheme[#f] result means that
   the user canceled the installation. Typically, @scheme[get-dir-proc] is
   @scheme[current-directory].}

@defproc[(install-planet-package [file path-string?] 
                                 [directory path-string?] 
                                 [spec (list/c string? string? 
                                               (listof string?) 
                                               exact-nonnegative-integer?
                                               exact-nonnegative-integer?)])
         void?]{

 Similar to @scheme[run-single-installer], but runs the setup process
 to install the archive @scheme[file] into @scheme[directory] as the
 @|PLaneT| package described by @scheme[spec]. The user-specific
 documentation index is not rebuilt, so @scheme[reindex-user-documentation]
 should be run after a set of @|PLaneT| packages are installed.}

@defproc[(reindex-user-documentation) void?]{
  Similar to @scheme[run-single-installer], but runs only the part of
  the setup process that rebuilds the user-specific documentation
  start page, search page, and master index.}

@defproc[(clean-planet-package [directory path-string?] 
                               [spec (list/c string? string? 
                                             (listof string?) 
                                             exact-nonnegative-integer?
                                             exact-nonnegative-integer?)])
         void?]{
  Undoes the work of @scheme[install-planet-package]. The user-specific
 documentation index is not rebuilt, so @scheme[reindex-user-documentation]
 should be run after a set of @|PLaneT| packages are removed.}}


@subsubsection[#:tag "gui-unpacking"]{GUI Installer}

@defmodule[setup/plt-installer]{ The
  @schememodname[setup/plt-installer] library in the setup collection
  defines procedures for installing a @filepath{.plt} archive with a
  GUI (using the facilities of @schememodname[scheme/gui/base]).}

@defproc[(run-installer (filename path-string?)) void?]{
  Run the installer on the @filepath{.plt} file
  in @scheme[filename] and show the output in a window. This is a
  composition of @scheme[with-installer-window] and
  @scheme[run-single-installer] with a @scheme[get-dir-proc] that prompts
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
  turns on the busy cursor before calling @scheme[do-install] in a separate
  thread. 

  Returns before the installation process is complete;
  @scheme[cleanup-thunk] is called on a queued callback to the
  eventspace active when @scheme[with-installer-window] is
  invoked.}

@defproc[(run-single-installer (file path-string?)
                               (get-dir-proc (-> (or/c path-string? false/c))))
         void?]{
  The same as the export from @schememodname[setup/plt-single-installer], 
  but with a GUI.}

@; ----------------------------------------

@subsubsection{GUI Unpacking Signature}
      
@defmodule[setup/plt-installer-sig]{
  @defsignature[setup:plt-installer^ ()]{
  Provides two names: @scheme[run-installer] and @scheme[on-installer-run].}
}

@; ----------------------------------------

@subsubsection{GUI Unpacking Unit}

@defmodule[setup/plt-installer-unit]{

Imports @scheme[mred^] and exports @scheme[setup:plt-installer^]. }

@; ------------------------------------------------------------------------

@subsection[#:tag "unpacking-.plt-archives"]{Unpacking @filepath{.plt} Archives}

@defmodule[setup/unpack]{The @schememodname[setup/unpack]
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

Unpacks @scheme[archive]. 

The @scheme[main-collects-parent-dir] argument is passed along to
@scheme[get-target-plt-directory].

The @scheme[print-status] argument is used to report unpacking
progress.

The @scheme[get-target-directory] argument is used to get the
destination directory for unpacking an archive whose content is
relative to an arbitrary directory.

If @scheme[force?] is true, then version and required-collection
mismatches (comparing information in the archive to the current
installation) are ignored.

The @scheme[get-target-plt-directory] function is called to select a
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

Traverses the content of @scheme[archive], which must be a
@filepath{.plt} archive that is created with the default unpacking
unit and configuration expression. The configuration expression is not
evaluated, the unpacking unit is not invoked, and not files are
unpacked to the filesystem. Instead, the information in the archive is
reported back through @scheme[on-config], @scheme[on-setup-unit],
@scheme[on-directory], and @scheme[on-file], each of which can build on
an accumulated value that starts with @scheme[initial-value] and whose
final value is returned.

The @scheme[on-config-fn] function is called once with an S-expression
that represents a function to implement configuration information.
The second argument to @scheme[on-config] is @scheme[initial-value],
and the function's result is passes on as the last argument to @scheme[on-setup-unit].

The @scheme[on-setup-unit] function is called with the S-expression
representation of the installation unit, an input port that points to
the rest of the file, and the accumulated value. This input port is
the same port that will be used in the rest of processing, so if
@scheme[on-setup-unit] consumes any data from the port, then that data
will not be consumed by the remaining functions. (This means that
on-setup-unit can leave processing in an inconsistent state, which is
not checked by anything, and therefore could cause an error.)
The result of @scheme[on-setup-unit] becomes the new accumulated value.

For each directory that would be created by the archive when unpacking
normally, @scheme[on-directory] is called with the directory path and the
accumulated value up to that point, and its result is the new
accumulated value.

For each file that would be created by the archive when unpacking
normally, @scheme[on-file] is called with the file path, an input port
containing the contents of the file, and the accumulated value up to
that point; its result is the new accumulated value. The input port
can be used or ignored, and parsing of the rest of the file continues
the same either way. After @scheme[on-file] returns control, however,
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
        @scheme['name] --- a human-readable string describing the archive's
        contents. This name is used only for printing messages to the
        user during unpacking.}

      @item{
        @scheme['unpacker] --- a symbol indicating the expected unpacking
        environment. Currently, the only allowed value is @scheme['mzscheme].}

      @item{
        @scheme['requires] --- collections required to be installed before
        unpacking the archive, which associated versions; see the
        documentation of @scheme[pack] for details.}

     @item{
        @scheme['conflicts] --- collections required @emph{not} to be installed
        before unpacking the archive.}

     @item{
        @scheme['plt-relative?] --- a boolean; if true, then the archive's
        content should be unpacked relative to the plt add-ons directory.}

     @item{
        @scheme['plt-home-relative?] --- a boolean; if true and if
        @scheme['plt-relative?] is true, then the archive's content should be
        unpacked relative to the PLT Scheme installation.}

     @item{
        @scheme['test-plt-dirs] --- @scheme[#f] or a list of path strings;
        in the latter case, a true value of @scheme['plt-home-relative?] is
        cancelled if any of the directories in the list (relative to the PLT
        Scheme installation) is unwritable by the user.}
   ]

   The procedure is extracted from the archive using the @scheme[read]
   and @scheme[eval] procedures in a fresh namespace.  }

 @item{
   An old-style, unsigned unit using @scheme[(lib mzlib/unit200)] that
   drives the unpacking process. The unit accepts two imports: a path
   string for the parent of the main @filepath{collects} directory and
   an @scheme[unmztar] procedure. The remainder of the unpacking
   process consists of invoking this unit. It is expected that the
   unit will call @scheme[unmztar] procedure to unpack directories and
   files that are defined in the input archive after this unit. The
   result of invoking the unit must be a list of collection paths
   (where each collection path is a list of strings); once the archive
   is unpacked, @|setup-plt| will compile and setup the specified
   collections.

   The @scheme[unmztar] procedure takes one argument: a filter
   procedure. The filter procedure is called for each directory and
   file to be unpacked. It is called with three arguments:

   @itemize[
      @item{
        @scheme['dir], @scheme['file], @scheme['file-replace] 
        --- indicates whether the item to be
        unpacked is a directory, a file, or a file to be replaced, }

      @item{
        a relative path string --- the pathname of the directory or file
        to be unpacked, relative to the unpack directory, and}

      @item{
        a path string for the unpack directory (which can vary for a
        PLT-relative install when elements of the archive start with
        @scheme["collects"], @scheme["lib"], etc.).}
   ]
   
   If the filter procedure returns @scheme[#f] for a directory or file, the
   directory or file is not unpacked. If the filter procedure returns
   @scheme[#t] and the directory or file for @scheme['dir] or @scheme['file]
   already exists, it is not created. (The file for @scheme[file-replace]
   need not exist already.)

   When a directory is unpacked, intermediate directories are created
   as necessary to create the specified directory. When a file is
   unpacked, the directory must already exist.

   The unit is extracted from the archive using @scheme[read] and
   @scheme[eval].}  ]

Assuming that the unpacking unit calls the @scheme[unmztar] procedure, the
archive should continue with @tech{unpackables}. @tech{Unpackables} are
extracted until the end-of-file is found (as indicated by an @litchar{=}
in the base64-encoded input archive).

An @deftech{unpackable} is one of the following:

@itemize[
   @item{
     The symbol @scheme['dir] followed by a list. The @scheme[build-path]
     procedure will be applied to the list to obtain a relative path for
     the directory (and the relative path is combined with the target
     directory path to get a complete path).

     The @scheme['dir] symbol and list are extracted from the archive
     using @scheme[read] (and the result is @emph{not}
     @scheme[eval]uated).}

   @item{
     The symbol @scheme['file], a list, a number, an asterisk, and the file
     data. The list specifies the file's relative path, just as for
     directories. The number indicates the size of the file to be
     unpacked in bytes. The asterisk indicates the start of the file
     data; the next n bytes are written to the file, where n is the
     specified size of the file.

     The symbol, list, and number are all extracted from the archive
     using @scheme[read] (and the result is @emph{not}
     @scheme[eval]uated). After the number is read, input characters
     are discarded until an asterisk is found. The file data must
     follow this asterisk immediately.}
   
   @item{
     The symbol @scheme['file-replace] is treated like @scheme['file], 
     but if the file exists on disk already, the file in the archive replaces
     the file on disk.}
]

@; ----------------------------------------------------------

@section[#:tag "dirs"]{Finding Installation Directories}

@defmodule[setup/dirs]{
  The @schememodname[setup/dirs] library provides several procedures for locating
  installation directories:}

@defproc[(find-collects-dir) (or/c path? false/c)]{
  Returns a path to the installation's main @filepath{collects} directory, or
  @scheme[#f] if none can be found. A @scheme[#f] result is likely only
  in a stand-alone executable that is distributed without libraries.}

@defproc[(find-user-collects-dir) path?]{
  Returns a path to the user-specific @filepath{collects} directory; the
  directory indicated by the returned path may or may not exist.}

@defproc[(get-collects-search-dirs) (listof path?)]{
  Returns the same result as @scheme[(current-library-collection-paths)],
  which means that this result is not sensitive to the value of the 
  @scheme[use-user-specific-search-paths] parameter.}

@defproc[(find-doc-dir) (or/c path? false/c)]{
  Returns a path to the installation's @filepath{doc} directory.
  The result is @scheme[#f] if no such directory is available.}

@defproc[(find-user-doc-dir) path?]{
  Returns a path to a user-specific @filepath{doc} directory. The directory
  indicated by the returned path may or may not exist.}

@defproc[(get-doc-search-dirs) (listof path?)]{
  Returns a list of paths to search for documentation, not including
  documentation stored in individual collections. Unless it is
  configured otherwise, the result includes any non-@scheme[#f] result of
  @scheme[(find-doc-dir)] and @scheme[(find-user-doc-dir)]---but the latter is
  included only if the value of the @scheme[use-user-specific-search-paths]
  parameter is @scheme[#t].}

@defproc[(find-lib-dir) (or/c path? false/c)]{
  Returns a path to the installation's @filepath{lib} directory, which contains
  libraries and other build information. The result is @scheme[#f] if no such
  directory is available.}

@defproc[(find-dll-dir) (or/c path? false/c)]{
  Returns a path to the directory that contains DLLs for use with the
  current executable (e.g., @filepath{libmzsch.dll} under Windows).
  The result is @scheme[#f] if no such directory is available, or if no
  specific directory is available (i.e., other than the platform's normal
  search path).}

@defproc[(find-user-lib-dir) path?]{
  Returns a path to a user-specific @filepath{lib} directory; the directory
  indicated by the returned path may or may not exist.}

@defproc[(get-lib-search-dirs) (listof path?)]{
  Returns a list of paths to search for libraries. Unless it is
  configured otherwise, the result includes any non-@scheme[#f] result of
  @scheme[(find-lib-dir)], @scheme[(find-dll-dir)],
  and @scheme[(find-user-lib-dir)]---but the last is included only if the
  value of the @scheme[use-user-specific-search-paths] parameter
  is @scheme[#t].}

@defproc[(find-include-dir) (or/c path? false/c)]{
  Returns a path to the installation's @filepath{include} directory, which
  contains @filepath{.h} files for building MzScheme extensions and embedding
  programs. The result is @scheme[#f] if no such directory is available.}

@defproc[(find-user-include-dir) path?]{
  Returns a path to a user-specific @filepath{include} directory; the
  directory indicated by the returned path may or may not exist.}

@defproc[(get-include-search-dirs) (listof path?)]{
  Returns a list of paths to search for @filepath{.h} files. Unless it is
  configured otherwise, the result includes any non-@scheme[#f] result of
  @scheme[(find-include-dir)] and @scheme[(find-user-include-dir)]---but the
  latter is included only if the value of the
  @scheme[use-user-specific-search-paths] parameter is @scheme[#t].}

@defproc[(find-console-bin-dir) (or/c path? false/c)]{
  Returns a path to the installation's executable directory, where the
  stand-alone MzScheme executable resides. The result is @scheme[#f] if no
  such directory is available.}

@defproc[(find-gui-bin-dir) (or/c path? false/c)]{
  Returns a path to the installation's executable directory, where the
  stand-alone MrEd executable resides. The result is @scheme[#f] if no such
  directory is available.}

@defthing[absolute-installation? boolean?]{
  A binary boolean flag that is true if this installation is using
  absolute path names.}

@; ------------------------------------------------------------------------

@include-section["info.scrbl"]

@; ------------------------------------------------------------------------

@section[#:tag "getinfo"]{Reading @filepath{info.ss} Files}

@defmodule[setup/getinfo]{ The @schememodname[setup/getinfo] library
   provides functions for accessing fields in @filepath{info.ss}
   files.}

@defproc[(get-info (collection-names (listof string?)))
         (or/c
          (symbol? [(-> any)] . -> . any)
          false/c)]{
   Accepts a list of strings naming a collection or sub-collection,
   and calls @scheme[get-info/full] with the full path corresponding to the
   named collection.}

@defproc[(get-info/full (path path?))
         (or/c
          (symbol? [(-> any)] . -> . any)
          false/c)]{

   Accepts a path to a directory. If it finds either a well-formed
   an @filepath{info.rkt} file or an @filepath{info.ss} file (with
   preference for the @filepath{info.rkt} file), 
   it returns an info procedure that accepts either one
   or two arguments. The first argument to the info procedure is
   always a symbolic name, and the result is the value of the name in
   the @filepath{info.ss} file, if the name is defined. The optional
   second argument, @scheme[_thunk], is a procedure that takes no
   arguments to be called when the name is not defined; the result of
   the info procedure is the result of the @scheme[_thunk] in that
   case. If the name is not defined and no @scheme[_thunk] is
   provided, then an exception is raised.
   
   @scheme[get-info/full] returns @scheme[#f] if there is
   no @filepath{info.rkt} or @filepath{info.ss} file in the directory. If there is a
   @filepath{info.rkt} file that has the wrong shape (i.e., not a module
   using @schememodname[setup/infotab] or @scheme[(lib "infotab.ss" "setup")]),
   or if the @filepath{info.rkt} file fails to load, then an exception
   is raised. If the @filepath{info.rkt} file loaded, @scheme[get-info/full]
   returns the @scheme[get-info] file. If the @filepath{info.rkt} file does not exist, 
   then @scheme[get-info/full] does
   the same checks for the @filepath{info.ss} file, either raising an exception
   or returning the @scheme[get-info] function from the @filepath{info.ss} file.}

@defproc[(find-relevant-directories
          (syms (listof symbol?))
          (mode (symbols 'preferred 'all-available) 'preferred)) (listof path?)]{

   Returns a list of paths identifying installed directories (i.e.,
   collections and installed @|PLaneT| packages) whose
   @filepath{info.ss} file defines one or more of the given
   symbols. The result is based on a cache that is computed by
   @|setup-plt| and stored in the @indexed-file{info-domain}
   sub-directory of each collection directory (as determined by the
   @envvar{PLT_COLLECTION_PATHS} environment variable, etc.) and the
   file @filepath{cache.ss} in the user add-on directory.

   The result is in a canonical order (sorted lexicographically by
   directory name), and the paths it returns are suitable for
   providing to @scheme[get-info/full].

   If @scheme[mode] is specified, it must be either
   @scheme['preferred] (the default) or @scheme['all-available]. If
   mode is @scheme['all-available], @scheme[find-relevant-collections]
   returns all installed directories whose info files contain the
   specified symbols---for instance, all installed PLaneT packages
   will be searched if @scheme['all-available] is specified. If mode
   is @scheme['preferred], then only a subset of ``preferred''
   packages will be searched, and in particular only the directory
   containing the most recent version of any PLaneT package will be
   returned.

   No matter what @scheme[mode] is specified, if more than one
   collection has the same name, @scheme[find-relevant-directories]
   will only search the one that occurs first in the
   @envvar{PLT_COLLECTION_PATHS} environment variable.}

@defproc[(reset-relevant-directories-state!) void?]{
   Resets the cache used by @scheme[find-relevant-directories].}

@; ------------------------------------------------------------------------

@section[#:tag "main-collects"]{Paths Relative to @filepath{collects}}

@defmodule[setup/main-collects]

@defproc[(path->main-collects-relative [path (or/c bytes? path-string?)])
         (or/c path? (cons/c 'collects (listof bytes?)))]{

Checks whether @scheme[path] has a prefix that matches the prefix to
the main @filepath{collects} directory as determined by
@scheme[(find-collects-dir)]. If so, the result is a list starting
with @scheme['collects] and containing the remaining path elements as
byte strings. If not, the path is returned as-is.

The @scheme[path] argument should be a complete path. Applying
@scheme[simplify-path] before @scheme[path->main-collects-relative] is
usually a good idea.

For historical reasons, @scheme[path] can be a byte string, which is
converted to a path using @scheme[bytes->path].}

@defproc[(main-collects-relative->path
          [rel (or/c bytes? path-string?
                     (cons/c 'collects
                             (or/c (listof bytes?) bytes?)))])
         path?]{


The inverse of @scheme[path->main-collects-relative]: if @scheme[rel]
is a pair that starts with @scheme['collects], then it is converted
back to a path relative to @scheme[(find-collects-dir)].

For historical reasons, a single byte string is allowed in place of a
list of byte strings after @scheme['collects], in which case it is
assumed to be a relative path after conversion with
@scheme[bytes->path].

Also for historical reasons, if @scheme[rel] is any kind of value other
than specified in the contract above, it is returned as-is.}

@; ------------------------------------------------------------------------

@section[#:tag "xref"]{Cross-References for Installed Manuals}

@defmodule[setup/xref]

@defproc[(load-collections-xref [on-load (-> any/c) (lambda () (void))])
         xref?]{

Like @scheme[load-xref], but automatically find all cross-reference files for
manuals that have been installed with @exec{setup-plt}.}

@; ----------------------------------------------------------------------

@index-section[]
