#lang scribble/doc
@(require scribble/manual
          "common.rkt"
          (for-label racket/base
                     racket/contract
                     setup/dirs))

@title[#:tag "config-file"]{Installation Configuration and Search Paths}

A configuration-directory path is built into the Racket executable as
selected at install time, or its location can be changed via the
@envvar{PLTCONFIGDIR} directory or @DFlag{config}/@Flag{G}
command-line flag. Use @racket[find-config-dir] to locate the
configuration directory.

Modify the @filepath{config.rktd} file as described below to configure
other directories, but use the @racketmodname[setup/dirs] library (which
combines information from the configuration files and other sources)
to locate configured directories.

A @as-index{@filepath{config.rktd}} file in the configuration directory should
contain a @racket[read]able hash table with any of the following
symbolic keys, where an relative path is relative to the main collection
directory:

@itemlist[

 @item{@indexed-racket['doc-dir] --- a path, string, or byte string for the
       main documentation directory. The value defaults to a
       @filepath{doc} sibling directory of the
       main collection directory.}

 @item{@indexed-racket['lib-dir] --- a path, string, or byte string for the
       main library directory; it defaults to a @filepath{lib}
       sibling directory of the main collection directory.}

 @item{@indexed-racket['lib-search-dirs] --- a list of paths, strings, byte
       strings, or @racket[#f] representing the search path for
       directories containing foreign libraries; each @racket[#f] in
       the list, if any, is replaced with the default search path,
       which is the user- and version-specific @filepath{lib}
       directory followed by the main library directory.}

 @item{@indexed-racket['dll-dir] --- a path, string, or byte string for a
       directory containing shared libraries for the main
       executable; it defaults to the main library directory.}

 @item{@indexed-racket['share-dir] --- a path, string, or byte string for the
       main shared-file directory, which normally includes installed packages;
       it defaults to a @filepath{share} sibling directory of the main
       collection directory.}

 @item{@indexed-racket['links-file] --- a path, string, or byte string for the
       @tech[#:doc reference-doc]{collection links file}; it defaults
       to a @filepath{links.rktd} file in the main shared-file directory.}

 @item{@indexed-racket['links-search-files] --- like @racket['lib-search-dirs],
       but for @tech[#:doc reference-doc]{collection links file}.}

 @item{@indexed-racket['pkgs-dir] --- a path, string, or byte string for
       packages that have installation scope; it defaults to
       @filepath{pkgs} in the main shared-file directory.}

 @item{@indexed-racket['pkgs-search-dirs] --- like @racket['lib-search-dirs],
       but for packages in installation scope.}

 @item{@indexed-racket['bin-dir] --- a path, string, or byte string for the
       installion's directory containing executables; it defaults to a
       @filepath{bin} sibling directory of the main collection
       directory.}

 @item{@indexed-racket['apps-dir] --- a path, string, or byte string
       for the installation's directory for @filepath{.desktop} files;
       it defaults to a @filepath{applications} subdirectory of the
       @racket['share-dir] directory.}

 @item{@indexed-racket['man-dir] --- a path, string, or byte string for the
       installation's man-page directory; it defaults to a @filepath{man}
       sibling directory of the main collection directory.}

 @item{@indexed-racket['man-dir] --- a path, string, or byte string for the
       main man-page directory; it defaults to a @filepath{man}
       sibling directory of the main collection directory.}

 @item{@indexed-racket['doc-search-dirs] --- like @racket['lib-search-dirs],
       but for directories containing documentation.}

 @item{@indexed-racket['include-dir] --- a path, string, or byte string for
       the main directory containing C header files; it defaults to an
       @filepath{include} sibling directory of the main collection
       directory.}

 @item{@indexed-racket['include-search-dirs] --- like
       @racket[doc-search-dirs], but for directories containing C
       header files.}

 @item{@indexed-racket['doc-search-url] --- a URL string that is augmented
       with version and search-tag queries to form a remote
       documentation reference.}

 @item{@indexed-racket['installation-name] --- a string for the installation
       name, which is used for packages in @exec{user} 
       @tech[#:doc '(lib "pkg/scribblings/pkg.scrbl")]{package
       scope}; the default is @racket[(version)].}

 @item{@indexed-racket['build-stamp] --- a string that identifies a build,
       which can be used to augment the Racket version number to more
       specifically identify the build. An empty string is normally
       appropriate for a release build.}

 @item{@indexed-racket['catalogs] --- a list of URL strings used as the search
       path for resolving package names; an @racket[#f] in the list
       is replaced with the default search path.}

 @item{@indexed-racket['default-scope] --- either @racket["user"] or
       @racket["installation"], determining the default @tech[#:doc
       '(lib "pkg/scribblings/pkg.scrbl")]{package scope} for
       package-management operations.}

 @item{@indexed-racket['absolute-installation?] --- a boolean that is
       @racket[#t] if the installation uses absolute path names,
       @racket[#f] otherwise.}

 @item{@indexed-racket['cgc-suffix] --- a string used as the suffix (before
       the actual suffix, such as @filepath{.exe}) for a
       @filepath{CGC} executable. Use Windows-style casing, and the
       string will be downcased as appropriate (e.g., for a Unix
       binary name). A @racket[#f] value means that if the
       @exec{racket} binary identifies itself as CGC, then the suffix
       is @racket[""], otherwise it is @racket["CGC"].}

 @item{@indexed-racket['3m-suffix] --- analogous to @racket['cgc-suffix], but
       for 3m. A @racket[#f] value means that if the @filepath{racket}
       binary identifies itself as CGC, then the suffix is
       @racket["3m"], otherwise it is @racket[""].}

]
