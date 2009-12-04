#lang scribble/doc
@(require "mz.ss")

@title[#:tag "collects"]{Libraries and Collections}

A @deftech{library} is @scheme[module] declaration for use by multiple
programs. Scheme further groups libraries into @deftech{collections}
that can be easily distributed and easily added to a local MzScheme
installation. 

Some collections are distributed via @|PLaneT|. Such collections are
referenced through a @scheme[planet] module path (see
@scheme[require]) and are downloaded by Scheme on demand.

Other collections are distributed with PLT Scheme, in which case each
collection is a directory that is located in a @filepath{collects}
directory relative to the @exec{mzscheme}. A collection can also be
installed in a user-specific directory.  More generally, the search
path for installed collections can be configured through the
@scheme[current-library-collection-paths] parameter. In all of these
cases, the collections are referenced through @scheme[lib] paths (see
@scheme[require]).

For example, the following module uses the @filepath{getinfo.ss}
library module from the @filepath{setup} collection, and the
@filepath{cards.ss} library module from the @filepath{games}
collection's @filepath{cards} subcollection:

@schememod[
scheme
(require (lib "setup/getinfo.ss")
         (lib "games/cards/cards.ss"))
....
]

This example is more compactly and more commonly written as

@schememod[
scheme
(require setup/getinfo
         games/cards/cards)
....
]

When an identifier @scheme[_id] is used in a @scheme[require] form, it
is converted to @scheme[(lib _rel-string)] where @scheme[_rel-string]
is the string form of @scheme[_id].

A @scheme[_rel-string] in @scheme[(lib _rel-string)] consists of one
or more path elements that name collections, and then a final path
element that names a library file; the path elements are separated by
@litchar{/}. If @scheme[_rel-string] contains no @litchar{/}s, then
then @litchar{/main.ss} is implicitly appended to the path. If
@scheme[_rel-string] contains @litchar{/} but does not end with a file
suffix, then @litchar{.ss} is implicitly appended to the path.

The translation of a @scheme[planet] or @scheme[lib] path to a
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


@defproc[(find-library-collection-paths [pre-extras (listof path-string?) null]
                                        [post-extras (listof path-string?) null]) 
         (listof path?)]{

Produces a list of paths as follows:

@itemize[

@item{The path produced by @scheme[(build-path (find-system-path
    'addon-dir) (version) "collects")] is the first element of the
  default collection path list, unless the value of the
  @scheme[use-user-specific-search-paths] parameter is @scheme[#f].}

 @item{Extra directories provided in @scheme[pre-extras] are included
  next to the default collection path list, converted to complete
  paths relative to the executable.}

 @item{If the directory specified by @scheme[(find-system-path
    'collects-dir)] is absolute, or if it is relative (to the
  executable) and it exists, then it is added to the end of the
  default collection path list.}

 @item{Extra directories provided in @scheme[post-extras] are included
  last in the default collection path list, converted to complete
  paths relative to the executable.}

 @item{If the @indexed-envvar{PLTCOLLECTS} environment variable is
  defined, it is combined with the default list using
  @scheme[path-list-string->path-list]. If it is not defined, the
  default collection path list (as constructed by the first three
  bullets above) is used directly.

  Note that under @|AllUnix|, paths are separated by @litchar{:}, and
  under Windows by @litchar{;}.  Also,
  @scheme[path-list-string->path-list] splices the default paths at an
  empty path, for example, with many Unix shells you can set
  @envvar{PLTCOLLECTS} to @tt{":`pwd`"}, @tt{"`pwd`:"}, or
  @tt{"`pwd`"} to specify search the current directory after, before,
  or instead of the default paths respectively.}

]}


@defproc[(collection-path [collection string?] ...+) path?]{

Returns the path to a directory containing the libraries of the
collection indicated by @scheme[collection]s, where the second
@scheme[collection] (if any) names a sub-collection, and so on. If the
collection is not found, the @exnraise[exn:fail:filesystem].}


@defparam[current-library-collection-paths paths (listof (and/c path? complete-path?))]{

Parameter that determines a list of complete directory paths for
library collections used by @scheme[require]. See
@secref["collects"] for more information.}


@defboolparam[use-user-specific-search-paths on?]{

Parameter that determines whether user-specific paths, which are in
the directory produced by @scheme[(find-system-path 'addon-dir)], are
included in search paths for collections and other files. For example,
@scheme[find-library-collection-paths] omits the user-specific
collection directory when this parameter's value is @scheme[#f].}
