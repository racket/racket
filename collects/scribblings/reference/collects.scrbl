#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "collects"]{Libraries and Collections}

A @deftech{library} is @racket[module] declaration for use by multiple
programs. Racket further groups libraries into @deftech{collections}
that can be easily distributed and added to a local Racket
installation. 

Some libraries are distributed via @|PLaneT| packages. Such libraries are
referenced through a @racket[planet] module path (see
@racket[require]) and are downloaded by Racket on demand.

Other collections are distributed with Racket, in which case each
collection is a directory that is located in a @filepath{collects}
directory relative to the Racket executable. A collection can also be
installed in a user-specific directory.  More generally, the search
path for installed collections can be configured through the
@racket[current-library-collection-paths] parameter. Finally, the
location of collections can be specified through the @tech{collection
links files}; see @secref["links-file"] for more information.  In all
of these cases, the collections are referenced through @racket[lib]
paths (see @racket[require]) or symbolic shorthands.

For example, the following module uses the @filepath{getinfo.rkt}
library module from the @filepath{setup} collection, and the
@filepath{cards.rkt} library module from the @filepath{games}
collection's @filepath{cards} subcollection:

@racketmod[
racket
(require (lib "setup/getinfo.rkt")
         (lib "games/cards/cards.rkt"))
....
]

This example is more compactly and more commonly written using
symbolic shorthands:

@racketmod[
racket
(require setup/getinfo
         games/cards/cards)
....
]

When an identifier @racket[_id] is used in a @racket[require] form, it
is converted to @racket[(lib _rel-string)] where @racket[_rel-string]
is the string form of @racket[_id].

A @racket[_rel-string] in @racket[(lib _rel-string)] consists of one
or more path elements that name collections, and then a final path
element that names a library file; the path elements are separated by
@litchar{/}. If @racket[_rel-string] contains no @litchar{/}s, then
@litchar{/main.rkt} is implicitly appended to the path. If
@racket[_rel-string] contains @litchar{/} but does not end with a file
suffix, then @litchar{.rkt} is implicitly appended to the path.

The translation of a @racket[planet] or @racket[lib] path to a
@racket[module] declaration is determined by the @tech{module name
resolver}, as specified by the @racket[current-module-name-resolver]
parameter.

For the default @tech{module name resolver}, the search path for
collections is determined by the content of @racket[(find-system-path
'links-file)] (if it exists) and the
@racket[current-library-collection-paths] parameter. The collection
links and then list of paths in
@racket[current-library-collection-paths] is searched from first to
last to locate the first that contains @racket[_rel-string]. In other
words, the filesystem tree for each element in the link table and
search path is spliced together with the filesystem trees of other
path elements. Some Racket tools rely on unique resolution of module
path names, so an installation and
@racket[current-library-collection-paths] configuration should not
allow multiple files to match the same collection and file name.

The value of the @racket[current-library-collection-paths] parameter
is initialized in the Racket executable to the result of
@racket[(find-library-collection-paths)].


@defproc[(find-library-collection-paths [pre-extras (listof path-string?) null]
                                        [post-extras (listof path-string?) null]) 
         (listof path?)]{

Produces a list of paths as follows:

@itemize[

@item{The path produced by @racket[(build-path (find-system-path
    'addon-dir) (version) "collects")] is the first element of the
  default collection path list, unless the value of the
  @racket[use-user-specific-search-paths] parameter is @racket[#f].}

 @item{Extra directories provided in @racket[pre-extras] are included
  next to the default collection path list, converted to complete
  paths relative to the executable.}

 @item{If the directory specified by @racket[(find-system-path
    'collects-dir)] is absolute, or if it is relative (to the
  executable) and it exists, then it is added to the end of the
  default collection path list.}

 @item{Extra directories provided in @racket[post-extras] are included
  last in the default collection path list, converted to complete
  paths relative to the executable.}

 @item{If the @indexed-envvar{PLTCOLLECTS} environment variable is
  defined, it is combined with the default list using
  @racket[path-list-string->path-list]. If it is not defined, the
  default collection path list (as constructed by the first three
  bullets above) is used directly.

  Note that on @|AllUnix|, paths are separated by @litchar{:}, and
  on Windows by @litchar{;}.  Also,
  @racket[path-list-string->path-list] splices the default paths at an
  empty path, for example, with many Unix shells you can set
  @envvar{PLTCOLLECTS} to @tt{":`pwd`"}, @tt{"`pwd`:"}, or
  @tt{"`pwd`"} to specify search the current directory after, before,
  or instead of the default paths, respectively.}

]}


@defproc*[([(collection-file-path [file path-string?] [collection path-string?] ...+)
            path?]
           [(collection-file-path [file path-string?] [collection path-string?] ...+
                                  [#:fail fail-proc (string? . -> . any)])
            any])]{

Returns the path to the file indicated by @racket[file] in the
collection specified by the @racket[collection]s, where the second
@racket[collection] (if any) names a sub-collection, and so on.

If @racket[file] is not found, but @racket[file] ends in
@filepath{.rkt} and a file with the suffix @filepath{.ss} exists, then
the directory of the @filepath{.ss} file is used. If @racket[file] is
not found and the @filepath{.rkt}/@filepath{.ss} conversion does not
apply, but a directory corresponding to the @racket[collection]s is
found, then a path using the first such directory is
returned.

Finally, if the collection is not found, and if @racket[fail-proc] is
provided, then @racket[fail-proc] is applied to an error message (that
does not start @scheme["collection-file-path:"] or otherwise claim a
source), and its result is the result of
@racket[collection-file-path].  If @racket[fail-proc] is not provided
and the collection is not found, then the
@exnraise[exn:fail:filesystem].}


@defproc*[([(collection-path [collection path-string?] ...+)
            path?]
           [(collection-path [collection path-string?] ...+
                             [#:fail fail-proc (string? . -> . any)])
            any])]{

Like @racket[collection-file-path], but without a specified file name,
so that the first directory indicated by @racket[collection]s is
returned. The @racket[collection-file-path] function normally should
be used, instead, to support splicing of library-collection trees at
the file level.}


@defparam[current-library-collection-paths paths (listof (and/c path? complete-path?))]{

Parameter that determines a list of complete directory paths for
library collections used by @racket[require]. See
@secref["collects"] for more information.}


@defboolparam[use-user-specific-search-paths on?]{

Parameter that determines whether user-specific paths, which are in
the directory produced by @racket[(find-system-path 'addon-dir)], are
included in search paths for collections and other files. For example,
the initial value of @racket[find-library-collection-paths] omits the
user-specific collection directory when this parameter's value is
@racket[#f].}

@defboolparam[use-collection-link-paths on?]{

Parameter that determines whether @tech{collection links files} are
used to locate collections.}

@; ----------------------------------------------------------------------

@section[#:tag "links-file"]{Collection Links}

The @deftech{collection links files} are used by
@racket[collection-file-path], @racket[collection-path], and the
default @tech{module name resolver} to locate collections before
trying the @racket[(current-library-collection-paths)] search
path, but only if the @racket[use-collection-link-paths] parameter is set to
@racket[#t]. Furthermore, a user-specific @tech{collection links file} takes
precedence over an installation-wide @tech{collection links file}, but
the user-specific @tech{collection links file} is used only the
@racket[use-user-specific-search-paths] parameter is set to
@racket[#t].

The path of the user-specific @tech{collection links file} is by
@racket[(find-system-path 'links-file)], while an installation-wide
@tech{collection links file} is @filepath{links.rktd} in the
@filepath{config} collection within the installation's main collection
directory. Each @tech{collection links file} is cached by Racket, but
the file is re-read if its timestamp changes.

Each @tech{collection links file} is @racket[read] with default reader
parameter settings to obtain a list. Every element of the list must be
a link specification with one of the forms @racket[(list _string
_path)], @racket[(list _string _path _regexp)], @racket[(list 'root
_path)], or @racket[(list 'root _regexp)]. A @racket[_string] names a
top-level @tech{collection}, in which case @racket[_path] is a path
that can be used as the collection's path (directly, as opposed to a
subdirectory of @racket[_path] named by @racket[_string]). A
@racket['root] entry, in contrast, acts like an path in
@racket[(current-library-collection-paths)]. If @racket[_path] is a
relative path, it is relative to the directory containing the
@tech{collection links file}. If @racket[_regexp] is specified in a
link, then the link is used only if @racket[(regexp-match?  _regexp
(version))] produces a true result.

A single top-level collection can have multiple links in a
@tech{collection links file}, and any number of @racket['root] entries
can appear. The corresponding paths are effectively spliced together,
since the paths are tried in order to locate a file or sub-collection.

The @exec{raco link} command-link tool can display, install, and
remove links in the @tech{collection links file}. See @secref[#:doc
raco-doc "link"] in @other-manual[raco-doc] for more information.
