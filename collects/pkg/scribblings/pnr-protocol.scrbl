#lang scribble/manual
@(require scribble/bnf
          "common.rkt")

@title[#:tag "pnr-protocol"]{Package Name Resolver Protocol}

A @tech{package name resolver} is specified by a URL in one of three
forms:

@itemlist[

 @item{@litchar{http://} or @litchar{https://} --- a remote URL}

 @item{@litchar{file://} ending with @litchar{.sqlite} --- a local
       SQLite database}

 @item{@litchar{file://} without @litchar{.sqlite} --- a local
 directory}

]

@section{Remote and Directory Indexes}

In the case of a remote URL or a local directory naming an
@tech{index}, the URL/path is extended as follows to obtain
information about packages:

@itemlist[

 @item{@litchar{pkg} and @nonterm{package} path elements, where
       @nonterm{package} is a @tech{package name}, plus a
       @exec{version=}@nonterm{version} query (where @nonterm{version}
       is the Racket version number) in the case of a remote URL:

       This URL/path form is use to obtain information about
       @nonterm{package}. An HTTP request for a remote URL should
       respond with a @racket[read]-able hash table, as described
       below. A path in a local directory formed by adding
       @filepath{pkg} and @nonterm{package} should refer to a file
       that similarly contains a @racket[read]-able hash table.

       The hash table should supply the following keys:

       @itemlist[

        @item{@racket['source] (required) --- a @tech{package source}
              string, typically a remote URL.}

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

       ]}

 @item{@litchar{pkgs} path element: Obtains a list of package names
       that are mapped by the @tech{index}.  An HTTP request for a remote URL
       should respond with a @racket[read]-able list of strings. A
       path in a local directory formed by adding @filepath{pkg} and
       @nonterm{package} should refer to a file that similarly
       contains a @racket[read]-able list of strings.

       This URL/path form is used by @command-ref{index-copy} and
       tools that allow a user to browse an index.

       In the case of a local directory, if no @filepath{pkgs} file is
       available, a list is created by listing all files in the
       @filepath{pkg} directory.}

 @item{@litchar{pkgs-all} path element: Obtains a hash table mapping
       package names to package details. An HTTP request for a remote
       URL should respond with a @racket[read]-able hash table mapping
       strings to hash tables. A path in a local directory formed by
       adding @filepath{pkg} and @nonterm{package} should refer to a
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

The source for the PLT-hosted @tech{package name resolvers} is in the
@racket[(collection-file-path "pkg-index" "meta")]
directory of the full Racket distribution.

@; ----------------------------------------

@section{SQLite Indexes}

A SQLite database @tech{index} is meant to be constructed and queries
using the @racketmodname[pkg/pnr-db] library, but the database can be
constructed in any way as long as it contains the following tables:

@itemlist[

 @item{A @tt{pnr} table with the format

        @verbatim[#:indent 2]{(id SMALLINT, 
                               url TEXT,
                               pos SMALLINT)}.

       Normally, the only row in this table is @tt{(0, "local", 0)},
       but a database that records the content of a set of other
       indexes can also be used as an index, in which case each row
       represents an index; the @tt{id} field is a unique identifier
       for each index, the @tt{url} field is the index's URL, and the
       @tt{pos} column orders the index relative to others (where a
       lower @tt{pos} takes precedence).}

 @item{A @tt{pkg} table with the format

       @verbatim[#:indent 2]{(name TEXT,
                              pnr SMALLINT,
                              author TEXT,
                              source TEXT,
                              checksum TEXT,
                              desc TEXT)}

        The @tt{pnr} field is normally @tt{0}; in the case that the
        database reflects multiple other indexes, the @tt{pnr} field
        indicates the package entry's source index.

        The @tt{pkg} and @tt{pnr} fields together determine a unique
        row in the table.}

 @item{A @tt{tags} table with the form

             @verbatim[#:indent 2]{(pkg TEXT,
                                    pnr TEXT,
                                    tag TEXT)}

       where the @tt{pkg} and @tt{pnr} combination identifies a unique
       row in @tt{pkg}.}

 @item{A @tt{modules} table with the form

            @verbatim[#:indent 2]{(name TEXT,
                                   pkg TEXT,
                                   pnr SMALLINT,
                                   checksum TEXT)}

       where the @tt{pkg} and @tt{pnr} combination identifies a unique
       row in @tt{pkg}, and @racket[name] is a printed module path.

       This table is not currently used by any @exec{raco pkg}
       command, but it can be used to suggest package installations to
       provide a particular library.}

]
