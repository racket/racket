#lang scribble/doc
@(require scribble/manual
          scribble/bnf 
          "common.rkt"
          (for-label racket/base
                     racket/contract
                     setup/link))

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

 @item{@Flag{l} or @DFlag{list} --- Shows the current link table. If
       any other command-line arguments are provided that modify the
       link table, the table is shown after modifications. If no
       directory arguments are provided, and if none of @Flag{u},
       @DFlag{user}, @Flag{i}, @DFlag{installation}, @Flag{f}, or
       @DFlag{file} are specified, then the link table is shown for
       both the user-specific and installation-wide @tech[#:doc
       reference-doc]{collection links files}.}

 @item{@Flag{n} @nonterm{name} or @DFlag{name} @nonterm{name} --- Sets
       the collection name for adding a single link or removing
       matching links.  By default, the collection name for an added
       link is derived from the directory name. When the @Flag{r} or
       @DFlag{remove} flag is also used, only links with a collection
       name matching @nonterm{name} are removed, and if no directory
       arguments are provided, all links with a match to
       @nonterm{name} are removed. This flag is mutually exclusive with
       @Flag{d} and @DFlag{root}.}

 @item{@Flag{d} or @DFlag{root} --- Treats each directory as a
       collection root that contains collection directories, instead of
       a directory for a specific collection. When the @Flag{r} or
       @DFlag{remove} flag is also used, only collection-root links
       that match a directory are removed. This flag is mutually
       exclusive with @Flag{n} and @DFlag{name}.}

 @item{@Flag{x} @nonterm{regexp} or @DFlag{version-regexp}
       @nonterm{regexp} --- Sets a version regexp that limits the link
       to use only by Racket versions (as reported by
       @racket[version]) matching @nonterm{regexp}. When the @Flag{r}
       or @DFlag{remove} flag is also used, only links with a
       version regexp matching @nonterm{regexp} are removed.}

 @item{@Flag{r} or @DFlag{remove} --- Selects remove mode instead
       of add mode.}

 @item{@Flag{u} or @DFlag{user} --- Limits listing and removal
       of links to the user-specific @tech[#:doc
       reference-doc]{collection links file} and not the
       collection-wide @tech[#:doc reference-doc]{collection links
       file}. This flag is mutually exclusive with @Flag{i},
       @DFlag{installation}, @Flag{f}, and @DFlag{file}.}

 @item{@Flag{i} or @DFlag{installation} --- Reads and writes links in
       installation-wide @tech[#:doc reference-doc]{collection links
       file} and not the user-specific @tech[#:doc
       reference-doc]{collection links file}. This flag is mutually
       exclusive with @Flag{u}, @DFlag{user}, @Flag{f}, and
       @DFlag{file}.}

 @item{@Flag{f} @nonterm{file} or @DFlag{file} @nonterm{file} ---
       Reads and writes links in @nonterm{file} instead of the
       user-specific @tech[#:doc reference-doc]{collection links
       file}.  This flag is mutually exclusive with @Flag{u},
       @DFlag{user}, @Flag{i}, and @DFlag{installation}.}

 @item{@DFlag{repair} --- Enables repairs to the existing file content
       when the content is erroneous. The file is repaired by deleting
       individual links when possible.}

]

@; ----------------------------------------

@section{API for Collection Links}

@defmodule[setup/link]

@defproc[(links [dir path?] ...
                [#:user? user? any/c #t]
                [#:file file (or/c path-string? #f) #f]
                [#:name name (or/c string? #f) #f]
                [#:root? root? any/c #f]
                [#:version-regexp version-regexp (or/c regexp? #f) #f]
                [#:error error-proc (symbol? string? any/c ... . -> . any) error]
                [#:remove? remove? any/c #f]
                [#:show? show? any/c #f]
                [#:repair? repair? any/c #f]
                [#:with-path? with-path? any/c #f])
          list?]{

A function version of the @exec{raco link} command that always works
on a single file---either @racket[file] if it is a path string, the
user-specific @tech[#:doc reference-doc]{collection links file} if
@racket[user?] is true, of the installation-wide @tech[#:doc
reference-doc]{collection links file} if @racket[user?] is false.

The @racket[error-proc] argument is called to raise exceptions that
would be fatal to the @exec{raco link} command.

If @racket[remove?] is true, the result is a list of entries that were
removed from the file.  If @racket[remove?] is @racket[#f] but
@racket[root?] is true, the result is a list of paths for collection
roots. If @racket[remove?] and @racket[root?] are both @racket[#f],
the result is a list for top-level collections that are mapped by
@racket[file] and that apply to the running version of Racket; the
list is a list of strings for collection names if @racket[with-path?]
is @racket[#f], or it is a list of pairs of collection-name strings
and complete paths if @racket[with-path?] is true.}
