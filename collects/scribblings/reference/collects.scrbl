#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "mz:collects"]{Libraries and Collections}

A @deftech{library} is @scheme[module] declaration for use by multiple
programs. Scheme further groups libraries into @deftech{collections}
that can be easily distributed and easily added to a local MzScheme
installation. 

Some collections are distributed via @|PLaneT|. Such collections are
referenced through a @scheme[planet] module path (see
@scheme[require]) and are downloaded by Scheme on demand.

Other collections are distributed with PLT Scheme, in which case each
collection is a directory that is located in a @file{collects}
directory relative to the @exec{mzscheme}. A collection can also be
installed in a user-specific directory.  More generally, the search
path for installed collections can be configured through the
@scheme[current-library-collection-paths] parameter. In all of these
cases, the collections are referenced through @scheme[lib] paths (see
@scheme[require]).

For example, the following module uses the @file{match.ss} library module
from the default @file{mzlib} collection, the @file{getinfo.ss}
library module from the @file{setup} collection, and the
@file{cards.ss} library module from the @file{games} collection's
@file{cards} subcollection:

@schemeblock[
(module my-game mzscheme
  (require (lib "mzlib/match.ss")
           (lib "setup/getinfo.ss" "setup")
           (lib "games/cards/cards.ss"))
  ....)
]

In general, the @scheme[_rel-string] in @scheme[(lib _rel-string)]
consists of one or more path elements that name collections, and then
a final path element that names a library file; the path elements are
separated by @litchar{/}.

The translation of a @scheme{planet} or @scheme{lib} path to a
@scheme[module] declaration is determined by the @tech{module name
resolver}, as specified by the @scheme[current-module-name-resolver]
parameter.

For the default @tech{module name resolver}, The search path for
collections is determined by the
@scheme[current-library-collection-paths] parameter. The list of paths
in @scheme[current-library-collection-paths] is searched from first to
last to locate the first collection in a @scheme[_rel-string]. To find
a sub-collection, the enclosing collection is first found; if the
sub-collection is not present in the found enclosing collection, then
the search continues by looking for another instance of the enclosing
collection, and so on. In other words, the directory tree for each
element in the search path is spliced together with the directory
trees of other path elements. (The ``splicing'' of tress applies only
to directories; a file within a collection is found only within the
first instance of the collection.)

The value of the @scheme[current-library-collection-paths] parameter
is initialized in @exec{mzscheme} to the result of
@scheme[(find-library-collection-paths)].


@defproc[(find-library-collection-paths) (listof path?)]{

Produces a list of paths as follows:

@itemize{

@item{The path produced by @scheme[(build-path (find-system-path
    'addon-dir) (version) "collects")] is the first element of the
  default collection path list, unless the value of the
  @scheme[use-user-specific-search-paths] parameter is @scheme[#f].}

 @item{If the executable embeds a list of search paths, they are
  included (in order) after the first element in the default
  collection path list. Embedded relative paths are included only when
  the corresponding directory exists relative to the executable.}

 @item{If the directory specified by @scheme[(find-system-path
    'collects-dir)] is absolute, or if it is relative (to the
  executable) and it exists, then it is added to the end of the
  default collection path list.}

 @item{If the @indexed-envvar{PLTCOLLECTS} environment variable is
  defined, it is combined with the default list using
  @scheme[path-list-string->path-list]. If it is not defined, the
  default collection path list (as constructed by the first three
  bullets above) is used directly.}

}}


@defproc[(collection-path [collection string?] ...+) path?]{

Returns the path to a directory containing the libraries of the
collection indicated by @scheme[collection]s, where the second
@scheme[collection] (if any) names a sub-collection, and so on. If the
collection is not found, the @exnraise[exn:fail:filesystem].}


@defparam[current-library-collection-paths paths (listof (and/c path? complete-path?))]{

Parameter that determines a list of complete directory paths for
library collections used by @scheme[require]. See
@secref["mz:collects"] for more information.}


@defboolparam[use-user-specific-search-paths on?]{

Parameter that determines whether user-specific paths, which are in
the directory produced by @scheme[(find-system-path 'addon-dir)], are
included in search paths for collections and other files. For example,
@scheme[find-library-collection-paths] omits the user-specific
collection directory when this parameter's value is @scheme[#f].}
