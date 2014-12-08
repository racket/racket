#lang scribble/manual
@(require scribble/bnf
          "common.rkt"
          (for-label syntax/modcollapse))

@title[#:tag "catalog-protocol"]{Package Catalog Protocol}

A @tech{package catalog} is specified by a URL in one of three
forms:

@itemlist[

 @item{@litchar{http://} or @litchar{https://} --- a remote URL}

 @item{@litchar{file://} ending with @litchar{.sqlite} --- a local
       SQLite database}

 @item{@litchar{file://} without @litchar{.sqlite} --- a local
 directory}

]

@section{Remote and Directory Catalogs}

In the case of a remote URL or a local directory naming a
@tech{package catalog}, the URL/path is extended as follows to obtain
information about packages:

@itemlist[

 @item{@litchar{pkg} and @nonterm{package} path elements, where
       @nonterm{package} is a @tech{package name}, plus a
       @exec{version=}@nonterm{version} query (where @nonterm{version}
       is a Racket version number) in the case of a remote URL.

       This URL/path form is use to obtain information about
       @nonterm{package}. An HTTP request for a remote URL should
       respond with a @racket[read]-able hash table, as described
       below. A path in a local directory formed by adding
       @filepath{pkg} and @nonterm{package} should refer to a file
       that similarly contains a @racket[read]-able hash table.

       The hash table should supply the following keys:

       @itemlist[

        @item{@racket['source] (required) --- a @tech{package source}
              string, typically a remote URL. If this source is a
              relative URL, then it is treated as relative to the
              catalog.

              @history[#:changed "6.0.1.7" @elem{Added relative-path support
                                                 to clients of a catalog server.}]}

        @item{@racket['checksum] (requires) --- a string for a
              @tech{checksum}.}

        @item{@racket['name] --- a string that is the same as
              @nonterm{package}.}

        @item{@racket['author] --- a string for the author of the
              package, normally an e-mail address.}

        @item{@racket['description] --- a string describing the
              package.}

        @item{@racket['tags] --- a list of strings that describe the
              package's categorization.}

        @item{@racket['dependencies] --- a list of dependencies for
              the package, in the same shape as a @racket[deps]
              @filepath{info.rkt} field as described in
              @secref["metadata"].}

        @item{@racket['modules] --- a list of module paths for modules
              that are provided by the package; each module path should
              be normalized in the sense of
              @racket[collapse-module-path].}

        @item{@racket['versions] (optional) --- a hash table mapping
              version strings and @racket['default] to hash tables,
              where each version-specific hash table provides mappings
              to override the ones in the main hash table, and
              @racket['default] applies to any version not otherwise
              mapped.

              Clients of a remote catalog may request information for
              a specific version, but they should also check for a
              @racket['versions] entry in a catalog response, in case
              a catalog with version-specific mappings is implemented
              as a directory or by a file-serving HTTP server. A
              @racket['default] mapping, meanwhile, allows the main
              hash table to provide information that is suitable for
              clients at version 5.3.6 and earlier (which do not check
              for @racket['versions]).}

       ]}
 

 @item{@litchar{pkgs} path element: Obtains a list of package names
       that are mapped by the @tech{package catalog}.  An HTTP request for a remote URL
       should respond with a @racket[read]-able list of strings. A
       path in a local directory formed by adding @filepath{pkgs}
       should refer to a file that similarly
       contains a @racket[read]-able list of strings.

       This URL/path form is used by @command-ref{catalog-copy} and
       tools that allow a user to browse an catalog.

       In the case of a local directory, if no @filepath{pkgs} file is
       available, a list is created by listing all files in the
       @filepath{pkg} directory.}

 @item{@litchar{pkgs-all} path element: Obtains a hash table mapping
       package names to package details. An HTTP request for a remote
       URL should respond with a @racket[read]-able hash table mapping
       strings to hash tables. A path in a local directory formed by
       adding @filepath{pkgs-all} should refer to a
       file that similarly contains a @racket[read]-able hash table.

       This URL/path form is a shortcut for a @litchar{pkgs} URL/path
       form combined with a @litchar{pkgs/}@nonterm{package} query for
       each package.

       In the case of a local directory, if no @filepath{pkgs-all}
       file is available, a list is created from files in the
       @filepath{pkg} directory.}

]

Note that a local directory served as files through an HTTP server
works as a remote URL, as long as the @filepath{pkgs} and
@filepath{pkgs-all} files are present.

The source for the PLT-hosted @tech{package catalog} is in the
@racket[(collection-file-path "pkg-catalog" "meta")]
directory of the full Racket distribution.

@; ----------------------------------------

@section{SQLite Catalogs}

A SQLite database @tech{package catalog} is meant to be constructed and queries
using the @racketmodname[pkg/db] library, but the database can be
constructed in any way as long as it contains the following tables:

@itemlist[

 @item{A @tt{catalog} table with the format

        @verbatim[#:indent 2]{(id SMALLINT, 
                               url TEXT,
                               pos SMALLINT)}

       Normally, the only row in this table is @tt{(0, "local", 0)},
       but a database that records the content of a set of other
       catalogs can also be used as an catalog, in which case each row
       represents an catalog; the @tt{id} field is a unique identifier
       for each catalog, the @tt{url} field is the catalog's URL, and the
       @tt{pos} column orders the catalog relative to others (where a
       lower @tt{pos} takes precedence).}

 @item{A @tt{pkg} table with the format

       @verbatim[#:indent 2]{(name TEXT,
                              catalog SMALLINT,
                              author TEXT,
                              source TEXT,
                              checksum TEXT,
                              desc TEXT)}

        The @tt{catalog} field is normally @tt{0}; in the case that the
        database reflects multiple other catalogs, the @tt{catalog} field
        indicates the package entry's source catalog.

        The @tt{pkg} and @tt{catalog} fields together determine a unique
        row in the table.}

 @item{A @tt{tags} table with the form

             @verbatim[#:indent 2]{(pkg TEXT,
                                    catalog SMALLINT,
                                    tag TEXT)}

       where the @tt{pkg} and @tt{catalog} combination identifies a unique
       row in @tt{pkg}.}

 @item{A @tt{modules} table with the form

            @verbatim[#:indent 2]{(name TEXT,
                                   pkg TEXT,
                                   catalog SMALLINT,
                                   checksum TEXT)}

       where the @tt{pkg} and @tt{catalog} combination identifies a unique
       row in @tt{pkg}, and @tt{name} is a printed module path.

       This table is not currently used by any @exec{raco pkg}
       command, but it can be used to suggest package installations to
       provide a particular library.}

 @item{A @tt{dependencies} table with the form

            @verbatim[#:indent 2]{(onpkg TEXT,
                                   onversion TEXT,
                                   onplatform TEXT,
                                   pkg TEXT,
                                   catalog SMALLINT,
                                   checksum TEXT)}

       where the @tt{pkg} and @tt{catalog} combination identifies a unique
       row in @tt{pkg}, and @tt{onpkg}, @tt{onversion}, and @tt{onplatform}
       represent the dependency; @tt{onversion} or @tt{onplatform} is an
       empty string if the dependency has no version or platform specification.

       This table is not currently used by any @exec{raco pkg}
       command.}

]
