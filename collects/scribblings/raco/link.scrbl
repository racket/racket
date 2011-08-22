#lang scribble/doc
@(require scribble/manual
          scribble/bnf 
          "common.rkt"
          (for-label racket/base))

@title[#:tag "link"]{@exec{raco link}: Library Collection Links}

The @exec{raco link} command inspects and modifies a @tech[#:doc
reference-doc]{collection links file} to display, add, or remove
mappings from collection names to filesystem directories.

For example, the command

@commandline{raco link maze}

installs a user-specific link for the @racket["maze"] collection,
mapping it to the @filepath{maze} subdirectory of the current
directory. Supply multiple directory paths to create multiple links at
once, especially with a command-shell wildcard:

@commandline{raco link *}

By default, the linked collection name is the same as each directory's
name, but the collection name can be set separately for a single
directory with the @DFlag{name} flag.

To remove the link created by the first example above, use

@commandline{raco link --remove maze}

or 

@commandline{raco link -r maze}

Like link-adding mode, removing mode accepts multiple directory paths to
remove multiple links, and all links that match any directory are
removed.  If @DFlag{name} is used with @DFlag{remove}, then only
links matching both the collection name and directory are removed.

Full command-line options:

@itemlist[

 @item{@Flag{s} or @DFlag{show} --- Shows the current link table. If
       any other command-line arguments are provided that modify the
       link table, the table is shown after modifications. If no
       directory arguments are provided, and if none of @Flag{r},
       @DFlag{remove}, @Flag{i}, @DFlag{installation}, @Flag{f}, or
       @DFlag{file} are specified, then the link table is shown for
       both the user-specific and installation-wide @tech[#:doc
       reference-doc]{collection links files}.}

 @item{@Flag{n} @nonterm{name} or @DFlag{name} @nonterm{name} --- Sets
       the collection name for adding or removing a single link.  By
       default, the collection name for an added link is derived from
       the directory name. When the @Flag{r} or @DFlag{remove} flag is
       also used, only links with a collection name matching
       @nonterm{name} are removed.}

 @item{@Flag{x} @nonterm{regexp} or @DFlag{version-regexp}
       @nonterm{regexp} --- Sets a version regexp that limits the link
       to use only by Racket versions (as reported by
       @racket[version]) matching @nonterm{regexp}. When the @Flag{r}
       or @DFlag{remove} flag is also used, only links with a
       version regexp matching @nonterm{regexp} are removed.}

 @item{@Flag{i} or @DFlag{installation} --- Reads and writes links in
       installation-wide @tech[#:doc reference-doc]{collection links
       file} instead of the user-specific @tech[#:doc
       reference-doc]{collection links file}. This flag is mutally
       exclusive with @Flag{f} and @DFlag{file}.}

 @item{@Flag{f} @nonterm{file} or @DFlag{file} @nonterm{file} ---
       Reads and writes links in @nonterm{file} instead of the
       user-specific @tech[#:doc reference-doc]{collection links
       file}.  This flag is mutally exclusive with @Flag{i} and
       @DFlag{installation}.}

 @item{@DFlag{repair} --- Enables repairs to the existing file content
       when the content is erroneous. The file is repaired by deleting
       individual links when possible.}

]

@; ----------------------------------------

@section{API for Collection Links}

@defmodule[setup/link]

@defproc[(links [dirs (listof path?)]
                [#:file file path-string? (find-system-path 'links-file)]
                [#:name name (or/c string? #f) #f]
                [#:version-regexp version-regexp (or/c regexp? #f) #f]
                [#:error error-proc (symbol? string? any/c ... . -> . any) error]
                [#:remove? remove? any/c #f]
                [#:show? show? any/c #f]
                [#:repair? repair? any/c #f])
          (listof string?)]{

A function version of the @exec{raco link} command. The
@racket[error-proc] argument is called to raise exceptions that would
be fatal to the @exec{raco link} command.

The result is a list of top-level collections that are mapped by
@racket[file] and that apply to the running version of Racket.}
