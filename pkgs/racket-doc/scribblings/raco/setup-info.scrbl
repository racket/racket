#lang scribble/doc

@(require scribble/manual
          scribble/bnf
          "common.rkt"
         (for-label racket
                    setup/dirs
                    setup/getinfo
                    setup/main-collects
                    setup/collection-name
                    setup/matching-platform
                    scribble/core
                    scribble/base
                    scribble/decode
                    (only-in scribble/html-properties
                             body-id
                             document-source)
                    ;; info -- no bindings from this are used
                    (only-in info)
                    (only-in ffi/unsafe ffi-lib)
                    racket/path
                    setup/collects
                    racket/runtime-path
                    pkg/path
                    scribblings/main/contents))

@title[#:tag "setup-info"]{Controlling @exec{raco setup} with @filepath{info.rkt} Files}

To compile a collection's files to bytecode, @exec{raco setup} uses the
@racket[compile-collection-zos] procedure. That procedure, in turn,
consults the collection's @filepath{info.rkt} file, if it exists, for
specific instructions on compiling the collection. See
@racket[compile-collection-zos] for more information on the fields of
@filepath{info.rkt} that it uses, and see @secref["info.rkt"] for
information on the format of an @filepath{info.rkt} file.

Additional fields are used by the
@seclink["top" #:doc '(lib "pkg/scribblings/pkg.scrbl") "Racket package manager"]
and are documented in @secref["metadata" #:doc '(lib "pkg/scribblings/pkg.scrbl")].
The @exec{raco test} command also recognizes additional fields, which are
documented in @secref["test-config-info" #:doc '(lib "scribblings/raco/raco.scrbl")].

Optional @filepath{info.rkt} fields trigger additional actions by
@exec{raco setup}:

@itemize[

 @item{@as-index{@racketidfont{scribblings}} : @racket[(listof (cons/c string? list?))] ---
   A list of documents to build. Each document in the list is itself
   represented as a list, where each document's list starts with a
   string that is a collection-relative path to the document's source
   file. A document name (which is derived from the source module's
   name by default) is intended to be globally unique in the same way
   as a package or module name. See @secref["doc-info"] for more
   information about a @racketidfont{scribblings} value.
   Before a document is rendered by @exec{raco setup}, the document's
   main @racket[part] is adjusted in several
   ways; see @secref["doc-adjust"].}

 @item{@as-index{@racketidfont{release-note-files}} : @racket[(listof (cons/c string? (cons/c string? list?)))] ---
   A list of release-notes text files to link from the main documentation pages.
   Each note is itself represented as a list, and the list can specify auxiliary
   notes that are grouped with the main note.

   A @racketidfont{release-note-files} entry must be a value
   that can be generated from an expression matching the following
   @racket[_entry] grammar:

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
   only if the current platform matches the definition.

   On Mac OS, when a Mach-O file is copied, if the copied file
   includes a library reference that starts @litchar{@"@"loader_path/},
   and if the referenced library exists in a different location among
   the paths listed by @racket[(get-lib-search-dirs)], then the
   library reference is updated to an absolute path.

   On Unix, when an ELF file is copied, if the copied file includes an
   RPATH setting of @litchar{$ORIGIN} and the file is being installed
   to a user-specific location, then the file's RPATH is adjusted to
   @litchar{$ORIGIN:} followed by the path to the main installation's
   library directory as reported by @racket[(find-lib-dir)].

   On Windows, deleting a previously installed foreign library may be
   complicated by a lock on the file, if it is in use. To compensate,
   @exec{raco setup} deletes a foreign-library file by first renaming
   the file to have the prefix @filepath{raco-setup-delete-}; it then
   attempts to delete the renamed file and merely issues a warning on
   a failure to delete the renamed file. Meanwhile, in modes where
   @exec{raco setup} removes uninstalled libraries, it attempts to
   delete any file in the foreign-library directory whose name starts
   with @filepath{raco-setup-delete-} (in an attempt to clean up after
   previous failures).}

 @item{@indexed-racket[move-foreign-libs] : @racket[(listof (and/c
   path-string? relative-path?))] --- Like @racket[copy-foreign-libs],
   but the original file is removed after it is copied (which makes sense
   for precompiled packages).}

 @item{@indexed-racket[copy-shared-files] : @racket[(listof (and/c
   path-string? relative-path?))] --- Files to copy into a
   directory where shared files are found.
   If @racket[install-platform] is defined, then the files are copied
   only if the current platform matches the definition.

   On Windows, uninstalled files are deleted in the same way as for
   @racket[copy-foreign-libs], and the name prefix
   @filepath{raco-setup-delete-} is similarly special.}

 @item{@indexed-racket[move-shared-files] : @racket[(listof (and/c
   path-string? relative-path?))] --- Like @racket[copy-shared-files],
   but the original file is removed after it is copied (which makes sense
   for precompiled packages).}

 @item{@indexed-racket[copy-man-pages] : @racket[(listof (and/c
   path-string? relative-path? filename-extension))] --- Files to copy
   into a @tt{man} directory. The file suffix determines its category;
   for example, @litchar{.1} should be used for a @tt{man} page
   describing an executable.

   On Windows, uninstalled files are deleted in the same way as for
   @racket[copy-foreign-libs], and the name prefix
   @filepath{raco-setup-delete-} is similarly special.}

 @item{@indexed-racket[move-man-pages] : @racket[(listof (and/c
   path-string? relative-path? filename-extension))] --- Like
   @racket[copy-man-pages], but the original file is removed after it
   is copied (which makes sense for precompiled packages).}

 @item{@indexed-racket[install-platform] : @racket[platform-spec?]
   If this specification matches the current platform, the foreign
   libraries associated with this package are copied or moved into
   useful locations. See @racket[copy-foreign-libs],
   @racket[move-foreign-libs], @racket[copy-shared-files], and
   @racket[move-shared-files]. Also see @racket[matching-platform?] for
   information on the way that the specification is compared to
   @racket[(system-type)] and @racket[(system-library-subpath #f)].}

 @item{@indexed-racket[install-collection] : @racket[path-string?] ---
   A library module relative to the collection that provides
   @racket[installer]. The @racket[installer] procedure must accept
   one, two, three, or four arguments:

   @itemlist[

   @item{The first argument is a directory path to the parent of the
   Racket installation's @filepath{collects} directory.}

   @item{The second argument, if accepted, is a path to the
   collection's own directory.}

   @item{The third argument, if accepted, is a boolean indicating
   whether the collection is installed as user-specific (@racket[#t])
   or installation-wide (@racket[#f]).}

   @item{The fourth argument, if accepted, is a boolean indicating
   whether the collection is installed as installation-wide and should
   nevertheless avoid modifying the installation; an
   @racket[installer] procedure that does not accept this argument is
   never called when the argument would be @racket[#t]. An installer
   that does accept this argument is called with @racket[#t] to that
   it can perform user-specific work, even though the collection is
   installed installation-wide.}

   ]}

 @item{@indexed-racket[pre-install-collection] : @racket[path-string?] ---
   Like @racket[install-collection], except that the corresponding
   installer procedures are called @emph{before} the normal @filepath{.zo} build,
   instead of after. The provided procedure is
   @racket[pre-installer], so it can be provided by the
   same file that provides an @racket[installer] procedure.}

 @item{@indexed-racket[post-install-collection] : @racket[path-string?]  ---
   Like @racket[install-collection] for a procedure that is called right after the
   @racket[install-collection] procedure is executed. The
   @DFlag{no-install} flag can be provided to @exec{raco setup}
   to disable @racket[install-collection] and @racket[pre-install-collection],
   but not @racket[post-install-collection].  The @racket[post-install-collection]
   function is therefore expected to perform operations that are always needed,
   even after an installation that contains pre-compiled files. The
   provided procedure is @racket[post-installer], so it
   can be provided by the same file that provides an
   @racket[installer] procedure.}

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
   by @exec{raco setup} or @exec{raco make} (see
   @secref["Dependency\x20Files"]). If the @filepath{.dep} file
   indicates another module, that module's @filepath{.zo} is deleted
   only if it also has an accompanying @filepath{.dep} file. In that
   case, the @filepath{.dep} file is deleted, and additional used
   modules are deleted based on the used module's @filepath{.dep}
   file, etc. Supplying a specific list of collections to @exec{raco
   setup} disables this dependency-based deletion of compiled files.}

 @item{@racket[compile-omit-paths], @racket[compile-omit-files], and
   @racket[compile-include-files] --- Used indirectly via
   @racket[compile-collection-zos].}

 @item{@racket[module-suffixes] and @racket[doc-module-suffixes] ---
   Used indirectly via @racket[get-module-suffixes].}

 @item{@indexed-racket[main-doc-index] --- A collection name (in the
   sense of @racket[collection-name?]) or a list of collection names to
   be added to a @exec{raco setup} request when @DFlag{doc-index}
   is specified without @DFlag{avoid-main}.

   @history[#:added "9.0.0.11"]}

 @item{@indexed-racket[user-doc-index] --- A collection name (in the
   sense of @racket[collection-name?]) or a list of collection names to
   be added to a @exec{raco setup} request when @DFlag{doc-index}
   is specified without @DFlag{no-user}.

   @history[#:added "9.0.0.11"]}

]

@; ----------------------------------------
@section[#:tag "doc-info"]{Document Descriptions in @filepath{info.rkt} Files}

A @racketidfont{scribblings} entry is introduced in @secref["setup-info"]
as having the shape @racket[(listof (cons/c string? list?))], but it
more precisely must be a value that can be generated from an expression
matching the following @racket[_entry] grammar:

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
  [category (list category-name)
            (list category-name sort-number)
            (list category-name sort-number lang-fam)]
  [category-name symbol
                 string
                 (box string)]
  [lang-fam (list string ...)]
  [name string
        #f]
]

A document entry @racket[_doc] must have at least a @racket[_src-string],
and it optionally continues with information on how to
build the document. If a document's list contains a second item,
@racket[_flags], it must be a list of mode symbols (described
below). If a document's list contains a third item,
@racket[_category], it must be a list that categorizes the document
(described further below). If a document's list contains a fourth
item, @racket[_name], it is a name to use for the generated
documentation, instead of defaulting to the source file's name
(sans extension), where @racket[#f] means to use the default; a
non-@racket[#f] value for @racket[_name] must fit the grammar
of a collection-name element as checked by
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
        be rebuilt if any other document is rebuilt---except for
        documents that have the @racket['no-depend-on] flag.}

  @item{@racket['depends-all-main] : Indicates that the document
        should be rebuilt if any other document is rebuilt that is
        installed into the main installation---except for documents
        that have the @racket['no-depend-on] flag.}

  @item{@racket['depends-all-user] : Indicates that the document
        should be rebuilt if any other document is rebuilt that is
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

  @item{@racket['every-main-layer] : With @racket['main-doc],
        indicates that the document should be rendered separately
        at every installation layer (see @secref["layered-install"]).}

]

The @racket[_category] list specifies how to show the document in
the root table of contents and, for the @racket[_lang-fam] part,
how to classify the documentation's content for searching. This
information can be extended or overridden through a
@racket['doc-properties] table within the @racket[tag-prefix] of
the document's main @racket[part], but we first consider
@racket[_category] on its own:

@itemlist[

   @item{A @racket[_category] list must start with a @racket[_category-name], which
   determines where the manual appears in a document listing such as
   the root documentation page. A category is a symbol, string, or a
   boxed string. If it is a string or a boxed string, then the string is the category label on
   the root page (when the document's language families include the
   language family used for the listing, which is @racket["Racket"]
   for the root documentation page). If it is a symbol, then it should
   be one of the following categories listed below:

   @itemize[

     @item{@racket['getting-started] : High-level, introductory
        documentation, typeset at the same level as other category
        titles.}

     @item{@racket['core] : A core reference or library for a language
        family as may be specified with a @racket[_lang-fam].}

     @item{@racket['racket-core] : A core reference or library for
        Racket. This category normally should be used only by specific
        packages in the main Racket distribution. When rendering a
        listing for a language family other than @racket["Racket"],
        these documents appear after @racket['library] instead of
        after @racket['core].}

     @item{@racket['teaching] : Documentation for a teaching language
        or library. Documents in this category appear after
        @racket['language] if @racket['racket-core] is moved to
        later.}

     @item{@racket['language] : Documentation for a prominent
        programming language. If @racket['racket-core] is moved to
        later, documents in this category appear immediately after
        @racket['racket-core] and before @racket['teaching].}

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

     @item{@racket['drracket-plugin] : Documentation for DrRacket
           Plugins.}

     @item{All string and boxed-string categories as ordered by
        @racket[string<=?] appear at this point relative to other
        categories.}

     @item{@racket['library] : Documentation for miscellaneous libraries.}

     @item{All documents whose language families do not include the
        current language family appear at this point, at least for
        most categories. Documents are ordered by @racket[string<=?]
        on the first family name; within a language family, they are
        ordered as in a documentation listing for that language
        family. A document whose category is @racket['language],
        @racket['teaching], @racket['experimental], @racket['legacy],
        or @racket['racket-core] is always listed independent of its
        language family, however.

        Unless a document's category is a boxed string, the label used
        for the category in this section is prefixed by the first
        family name in the document's families. A boxed string avoid
        this prefixing.}

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

   If the @racket[_category] list is not given, or if the category symbol is unrecognized,
   the documentation is added to the (@racket['library]) category.}

   @item{If the category list has a second element, @racket[_sort-number], it must be a real number
   that designates the manual's sorting position with the category;
   manuals with the same sorting position are ordered
   alphabetically. For a pair of manuals with sorting numbers
   @racket[_n] and @racket[_m], the groups for the manuals are
   separated by space if @racket[(truncate (/ _n 10))]and
   @racket[(truncate (/ _m 10))] are different.}

   @item{If the category list has a third element, @racket[_lang-fam], then
   it must be a list of strings, where each string names a language
   family. The default for @racket[_lang-fam] is @racket[(list "Racket")].
   This language family list is used to organize a listing of all documentation,
   and is also used for index entries that are extracted from the
   document and used for searching. For index entries, the
   document, a part within the document, or an individual index
   entry may specify its own language family, and @racket[_lang-fam]
   provides only a default for entries that do not otherwise specify a
   language family. See @secref["doc-adjust"] for more information.}

   @item{If a document's main @racket[part] has a @racket[tag-prefix]
   hash table that maps @racket['doc-properties] to another hash
   table, the inner hash table can override and generalize the
   @racket[_category] list:

   @itemlist[

    @item{If @racket['language-family] is mapped to a list of strings,
    it provides a replacement for @racket[_lang-fam].}

    @item{If @racket['category] is mapped to a hash table
    @racket[_cat-ht], it is used to get a @racket[_category]
    replacement specific to a language family. If @racket[_cat-ht]
    maps the listing's language family name to a list, that list is
    used of @racket[_category]. Otherwise, if @racket[_cat-ht] maps
    @racket['default] to a list, that list is used instead of
    @racket[_category]. In either case, the replacement list cannot
    contain a @racket[_lang-fam] component; a
    @racket['language-family] mapping (as described in the previous
    bullet) is the only way to replace a @racket[_lang-fam] component.}

   ]}

]

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
@filepath{synced.rktd} file to represent the installation.

The destination for a rendered document depends on whether the
enclosing collection is part of a Racket installation or installed as
a package in user @tech[#:doc '(lib
"pkg/scribblings/pkg.scrbl")]{package scope}. When the document is in
user scope, then it is rendered within the package in the same
location as for pre-rendered documentation. An exception is when the
documentation is declared in @racketidfont{scribblings} with
@racket['depends-all], @racket['depends-all-main], or
@racket['depends-all-user] and without @racket['every-main-layer]; in
that case, it is rendered in a more central location (and not included
in a pre-rendered form) as part of the strategy described in
@secref["doc-listing"].

If the a document's main @racket[part]'s has a @racket[tag-prefix] as
a hash table, if that hash table has @racket['doc-properties] mapped
to another hash table, and if the inner table maps @racket['supplant]
to a string, then @exec{raco setup} copies a rendered
@filepath{index.html} to a sibling directory name by the
@racket['supplant] string. This step, which is performed at the end of
the @exec{raco setup} document-rendering phase, is intended to
support document listing as described in @secref["doc-listing"].

@history[#:changed "6.4" @elem{Allow a category to be a string
                              instead of a symbol.}
         #:changed "8.9.0.6" @elem{Add the @racket['drracket-plugin]
                                   category symbol.}
         #:changed "8.14.0.5" @elem{Added optional @racket[_lang-fam]
                                    within @racket[_category].}
         #:changed "9.0.0.11" @elem{Added support for @racket['doc-properties]
                                    in a document's main @racket[part] and for
                                    boxed-string category names.}]

@section[#:tag "doc-adjust"]{Document Setup Adjustments}

Before a document is rendered by @exec{raco setup}, its main
@racket[part] is adjusted in several ways:

@itemlist[

  @item{The @racket[tag-prefix] field of the @racket[part] is
  adjusted to have the named document's module path as its
  @racket['tag-prefix], which means that other documents can refer
  to the rendered document via @racket[secref] or
  @racket[other-doc].}

  @item{A @racket['(part "top")] tag is added to the
  @racket[part]'s @racket[tag] field if it is not present already.}

  @item{A @racket[document-version] style property is added using
  @racket[(version)] if no @racket[document-version] property is
  present already.}

  @item{A @racket[body-id] style property is added with
  @racket["doc-racket-lang-org"] if no @racket[body-id] property
  is present already.}

  @item{A @racket[document-source] style property is added with
  the document's module path.}

  @item{A default language family is determined as
  @racket[_lang-fam] from @racket[_category] in a
  @racket[scribblings] entry or (if not present) the value of a
  @racket['default-language-family] key in the @racket[part]'s
  @racket[tag-prefix] as a hash table (perhaps originally supplied
  to @racket[title]). That list of strings, if either, is added as
  @racket['language-family] to a new table that is paired with
  @racket['index-extras] (if any) already in the table. That way,
  @racket[_category] or the main @racket[part] of a document can
  supply the default language family for all index entries
  generated from the document.}

  @item{When the @racket[part]'s @racket[tag-prefix] is a hash table with
  @racket['doc-properties] mapped to a hash table value, the value
  is recorded for cross references using the tag
  @racket[`(doc-properties "top")] combined with the document's
  module path. This addition allows a @racket['doc-properties]
  table to configure the document's listing in more general ways
  than a @racket[_category] specification within
  @racketidfont{scribblings} as described in @secref["doc-info"].}

]

The document's rendering may be further adjusted at the renderer level
(see @secref["renderer" #:doc '(lib
"scribblings/scribble/scribble.scrbl")]), including configuration at
the level of CSS or Latex.

@section[#:tag "doc-listing"]{Rendering Documentation Listings}

@defmodule[scribblings/main/contents]

@history[#:added "9.0.0.11"]

@defproc[(build-contents [#:user? user? any/c #f]
                         [#:supplant supplant (or/c #f string?) #f]
                         [#:style style (or/c style? #f string? symbol? (listof symbol?)) #f]
                         [#:main-language-family language-family string? (get-main-language-family)]
                         [#:title-content title-content content? (list language-family
                                                                       (element (style #f '(aux)) " Documentation"))]
                         [#:self-path self-path (or/c #f path-string?) #f]
                         [#:bug-url bug-url (or/c #f string?) #f]
                         [#:default-category default-category list? '(language)]
                         [#:doc-properties doc-properties hash? (make-default-doc-properties
                                                                 main-family
                                                                 default-category
                                                                 supplant)]
                         [#:default-language-family default-language-family (or/c #f (nonempty-listof string?)) #f]
                         [#:version doc-version (or/c #f string?) #f]
                         [#:date doc-date (or/c #f string?) #f])
           pre-part?]{

Creates the content of a document that lists all installed
documentation from the perspective of @racket[main-family].

The @racket[language-family] argument selects the language family used
to render the document (i.e., compared to language families of listed
documents), while the @racket[default-language-family] argument
specifies the language families that the document listing declares for
itself.

Use this function as follows:

@itemlist[

 @item{Create a subcollection (say,
  @filepath{my-language/scribblings/main}) with an @filepath{info.rkt}
  file and a sub-collectiopn of that one (say,
  @filepath{my-language/scribblings/main/user}) with its own
  @filepath{info.rkt} file. Each layer will have one document.}

 @item{In the outer subcollection, create a document (say,
   @filepath{my-language.scrbl}) that renders on the assumption that
   it's in installation scope (i.e., the @racket[user?] argument to
   @racket[build-contents] should be @racket[#f]). This is the
   document that @racket[secref] might reasonably refer to, in case
   that's useful. Make sure that the document is listed in
   @racketidfont{scribblings} for the subcollection's
   @filepath{info.rkt}, and include the @racket['depends-all] or
   @racket['depends-all-main] flag and the @racket['no-depend-on]
   flag.}

 @item{In the nested collection, create another document (say,
   @filepath{user-my-language.scrbl}) that also has the flags
   @racket['user-doc], @racket['depends-all-user], and
   @racket['no-depend-on] in the nested subcollection's
   @filepath{info.rkt} as @racketidfont{scribblings}. Use the category
   @racket['(omit)]. The document should render as in user scope
   (i.e., the @racket[user?] argument to @racket[build-contents]
   should be @racket[#t]). If the containing package is in
   installation scope, this document will be rendered only when there
   are some other user-scope documents installed. This documentation
   generally should @emph{not} be the target of a cross-reference,
   because it won't always get rendered.}

 @item{In the latter document (perhaps
   @filepath{user-my-language.scrbl}), include the @racket['supplant]
   key in a @racket['doc-properties] table in @racket[tag-prefix] for
   the document's main @racket[part]. The value of @racket['supplant]
   should be the name of the destination for the former document
   (perhaps @filepath{my-language}). A @exec{raco setup} that builds
   documentation will arrange for the @filepath{index.html} of the
   supplanting document to be added or to overrwite
   @filepath{index.html} for the supplanted document. That way, when
   user-scope documentation exists at all, there will be a user-scope
   document using the name of the main document, whether or not the
   main document would render into user scope. It's also important
   that a @racket['depends-all] flag puts a document in the user-scope
   root documentation directory, instead of keeping it in the
   installed package's directory.}

 @item{In the outer subcollection's @filepath{info.rkt}, include a
   @racketidfont{main-doc-index} entry with the subcollection's own
   name. That entry cases the document to be rerendered when packages
   are installed. Putting the document in a fresh subcollection
   minimize the work that this rebuild triggers.}

 @item{In the nested subcollection's @filepath{info.rkt}, include a
   @racketidfont{user-doc-index} entry with the subcollection's own
   name.}

]

}

@defproc[(make-default-doc-properties [#:language-family language-family string? (get-main-language-family)]
                                      [#:default-category default-category list? '(language)]
                                      [#:supplant supplant (or/c #f string?) #f])
         hash?]{

Constructs a hash table suitable for th2 @racket[#:doc-properties]
argument of @racket[build-contents].

}
