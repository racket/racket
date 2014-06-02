#lang scribble/doc
@(require "mz.rkt"
          (for-label setup/dirs))

@title[#:tag "collects"]{Libraries and Collections}

A @deftech{library} is @racket[module] declaration for use by multiple
programs. Racket further groups libraries into @deftech{collections}.
Typically, collections are added via @deftech{packages} (see
@other-doc['(lib "pkg/scribblings/pkg.scrbl")]); the package manager
works outside of the Racket core, but it configures the core run-time
system through @tech{collection links files}.

Libraries in collections are referenced through @racket[lib] paths
(see @racket[require]) or symbolic shorthands. For example, the
following module uses the @filepath{getinfo.rkt} library module from
the @filepath{setup} collection, and the @filepath{cards.rkt} library
module from the @filepath{games} collection's @filepath{cards}
subcollection:

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

Libraries also can be distributed via @|PLaneT| packages. Such
libraries are referenced through a @racket[planet] module path (see
@racket[require]) and are downloaded by Racket on demand, instead of
referenced through @tech{collections}.

The translation of a @racket[planet] or @racket[lib] path to a
@racket[module] declaration is determined by the @tech{module name
resolver}, as specified by the @racket[current-module-name-resolver]
parameter.

@; ----------------------------------------------------------------------

@section[#:tag "collects-search"]{Collection Search Configuration}

For the default @tech{module name resolver}, the search path for
collections is determined by the
@racket[current-library-collection-links] parameter and the
@racket[current-library-collection-paths] parameter:

@itemlist[

 @item{The most primitive @tech{collection}-based modules are located
       in @filepath{collects} directory relative to the Racket
       executable. Libraries for a collection are grouped within a
       directory whose name matches the collection name. The path to
       the @filepath{collects} directory is normally included in
       @racket[current-library-collection-paths].}

 @item{Collection-based libraries also can be installed other
       directories, perhaps user-specific, that are structured like
       the @filepath{collects} directory. Those additional directories
       can be included in the
       @racket[current-library-collection-paths] parameter either
       dynamically, through command-line arguments to @exec{racket},
       or by setting the @envvar{PLTCOLLECTS} environment variable;
       see @racket[find-library-collection-paths].}

 @item{@tech{Collection links files} provide a mapping from top-level
       collection names to directories, plus additional
       @filepath{collects}-like directories (that have subdirectories
       with names that match collection names). Each @tech{collection
       links file} to be searched is referenced by the
       @racket[current-library-collection-links] parameter; the parameter
       references the file, and not the file's content, so
       that changes to the file can be detected and affect later
       module resolution. See also
       @racket[find-library-collection-links].}

 @item{The @racket[current-library-collection-links] parameter's value
       can also include hash tables that provide the same content as
       @tech{collection links files}: a mapping from collection names
       in symbol form to a list of paths for the collection, or from
       @racket[#f] to a list of @filepath{collects}-like paths.}

 @item{Finally, the @racket[current-library-collection-links]
       parameter's value includes @racket[#f] to indicate the point in
       the search process at which the @tech{module-name resolver} should
       check @racket[current-library-collection-paths] relative to the
       files and hash tables in @racket[current-library-collection-links].}

]

To resolve a module reference @racket[_rel-string], the default
@tech{module name resolver} searches collection links in
@racket[current-library-collection-links] from first to last to locate
the first directory that contains @racket[_rel-string], splicing a
search through in @racket[current-library-collection-paths] where in
@racket[current-library-collection-links] contains @racket[#f].  The
filesystem tree for each element in the link table and search path is
effectively @deftech[#:key "collection splicing"]{spliced} together with the filesystem trees of other path
elements that correspond to the same collection. Some Racket tools
rely on unique resolution of module path names, so an installation and
configuration should not allow multiple files to match the same
collection and file combination.

The value of the @racket[current-library-collection-links] parameter
is initialized by the @exec{racket} executable to the result of
@racket[(find-library-collection-links)], and the value of the
@racket[current-library-collection-paths] parameter is initialized to
the result of @racket[(find-library-collection-paths)].

@; ----------------------------------------------------------------------

@section[#:tag "links-file"]{Collection Links}

@deftech{Collection links files} are used by
@racket[collection-file-path], @racket[collection-path], and the
default @tech{module name resolver} to locate collections before
trying the @racket[(current-library-collection-paths)] search
path. The @tech{collection links files} to use are determined by the
@racket[current-library-collection-links] parameter, which is
initialized to the result of @racket[find-library-collection-links].

A @tech{collection links file} is @racket[read] with default reader
parameter settings to obtain a list. Every element of the list must be
a link specification with one of the forms @racket[(list _string
_path)], @racket[(list _string _path _regexp)], @racket[(list 'root
_path)], @racket[(list 'root _path _regexp)], @racket[(list 'static-root
_path)], @racket[(list 'static-root _path _regexp)]. A @racket[_string] names a
top-level @tech{collection}, in which case @racket[_path] is a path
that can be used as the collection's path (directly, as opposed to a
subdirectory of @racket[_path] named by @racket[_string]). A
@racket['root] entry, in contrast, acts like an path in
@racket[(current-library-collection-paths)].  A
@racket['static-root] entry is like a @racket['root] entry, but
where the immediate content of the directory is assumed not to change unless the
@tech{collection links file} changes. If @racket[_path] is a
relative path, it is relative to the directory containing the
@tech{collection links file}. If @racket[_regexp] is specified in a
link, then the link is used only if @racket[(regexp-match?  _regexp
(version))] produces a true result.

A single top-level collection can have multiple links in a
@tech{collection links file}, and any number of @racket['root] entries
can appear. The corresponding paths are effectively spliced together,
since the paths are tried in order to locate a file or sub-collection.

The @exec{raco link} command-link tool can display, install, and
remove links in a @tech{collection links file}. See @secref[#:doc
raco-doc "link"] in @other-manual[raco-doc] for more information.

@; ----------------------------------------

@section[#:tag "collects-api"]{Collection Paths and Parameters}

@defproc[(find-library-collection-paths [pre-extras (listof path-string?) null]
                                        [post-extras (listof path-string?) null]) 
         (listof path?)]{

Produces a list of paths, which is normally used to initialize
@racket[current-library-collection-paths], as follows:

@itemize[

@item{The path produced by @racket[(build-path (find-system-path
    'addon-dir) (get-installation-name) "collects")] is the first element of the
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
  @racket[path-list-string->path-list], as long as the value of
  @racket[use-user-specific-search-paths] is true. If it is not
  defined or if the value @racket[use-user-specific-search-paths] is
  @racket[#f], the default collection path list (as constructed by the
  first three bullets above) is used directly.

  Note that on @|AllUnix|, paths are separated by @litchar{:}, and
  on Windows by @litchar{;}.  Also,
  @racket[path-list-string->path-list] splices the default paths at an
  empty path, for example, with many Unix shells you can set
  @envvar{PLTCOLLECTS} to @tt{":`pwd`"}, @tt{"`pwd`:"}, or
  @tt{"`pwd`"} to specify search the current directory after, before,
  or instead of the default paths, respectively.}

]}

@defproc[(find-library-collection-links) 
         (listof (or/c #f (and/c path? complete-path?)))]{

Produces a list of paths and @racket[#f], which is normally used to
initialized @racket[current-library-collection-links], as follows:

@itemlist[

 @item{The list starts with @racket[#f], which causes the default
       @tech{module name resolver}, @racket[collection-file-path],
       and @racket[collection-path] to try paths in
       @racket[current-library-collection-paths] before
       @tech{collection links files}.}

 @item{As long as the values of
       @racket[use-user-specific-search-paths] and
       @racket[use-collection-link-paths] are true, the second element
       in the result list is the path of the user--specific
       @tech{collection links file}, which is @racket[(build-path
       (find-system-path 'addon-dir) (get-installation-name)
       "links.rktd")].}

 @item{As long as the value of @racket[use-collection-link-paths] is
       true, the rest of the list contains the result of
       @racket[get-links-search-files]. Typically, that function
       produces a list with a single path, @racket[(build-path
       (find-config-dir) "links.rktd")].}

]}


@defproc*[([(collection-file-path [file path-string?] [collection path-string?] ...+
                                  [#:check-compiled? check-compiled? any/c
                                                     (regexp-match? #rx"[.]rkt$" file)])
            path?]
           [(collection-file-path [file path-string?] [collection path-string?] ...+
                                  [#:fail fail-proc (string? . -> . any)]
                                  [#:check-compiled? check-compiled? any/c
                                                     (regexp-match? #rx"[.]rkt$" file)])
            any])]{

Returns the path to the file indicated by @racket[file] in the
collection specified by the @racket[collection]s, where the second
@racket[collection] (if any) names a sub-collection, and so on.  The
search uses the values of @racket[current-library-collection-links]
and @racket[current-library-collection-paths].

If @racket[file] is not found, but @racket[file] ends in
@filepath{.rkt} and a file with the suffix @filepath{.ss} exists, then
the directory of the @filepath{.ss} file is used. If @racket[file] is
not found and the @filepath{.rkt}/@filepath{.ss} conversion does not
apply, but a directory corresponding to the @racket[collection]s is
found, then a path using the first such directory is
returned.

If @racket[check-compiled?] is true, then the search also depends on
@racket[use-compiled-file-paths] and
@racket[current-compiled-file-roots]; if @racket[file] is not found,
then a compiled form of @racket[file] with the suffix @filepath{.zo}
is checked in the same way as the default @tech{compiled-load
handler}.  If a compiled file is found, the result from
@racket[collection-file-path] reports the location that @racket[file]
itself would occupy (if it existed) for the found compiled file.

Finally, if the collection is not found, and if @racket[fail-proc] is
provided, then @racket[fail-proc] is applied to an error message (that
does not start @scheme["collection-file-path:"] or otherwise claim a
source), and its result is the result of
@racket[collection-file-path].  If @racket[fail-proc] is not provided
and the collection is not found, then the
@exnraise[exn:fail:filesystem].

@history[#:changed "6.0.1.12" @elem{Added the @racket[check-compiled?] argument.}]}


@defproc*[([(collection-path [collection path-string?] ...+)
            path?]
           [(collection-path [collection path-string?] ...+
                             [#:fail fail-proc (string? . -> . any)])
            any])]{

  @deprecated[#:what "function" @racket[collection-file-path]]{
  @tech{Collection splicing} implies that a given collection can have
  multiple paths, such as when multiple @tech[#:doc
  '(lib "scribblings/guide/guide.scrbl")]{packages} provide modules for a
  collection.}

Like @racket[collection-file-path], but without a specified file name,
so that a directory indicated by @racket[collection]s is returned.

When multiple directories correspond to the collection, the first one
found in the search sequence (see @secref["collects-search"]) is returned.}


@defparam*[current-library-collection-paths paths
                                            (listof (and/c path-string? complete-path?))
                                            (listof (and/c path? complete-path?))]{

Parameter that determines a list of complete directory paths for
finding libraries (as referenced in @racket[require], for example)
through the default @tech{module name resolver} and for finding paths
through @racket[collection-path] and
@racket[collection-file-path]. See @secref["collects-search"] for more
information.}


@defparam*[current-library-collection-links paths
                                            (listof (or/c #f
                                                          (and/c path-string? complete-path?)
                                                          (hash/c (or/c (and/c symbol? module-path?) #f)
                                                                  (listof (and/c path-string? complete-path?)))))
                                            (listof (or/c #f
                                                          (and/c path? complete-path?)
                                                          (hash/c (or/c (and/c symbol? module-path?) #f)
                                                                  (listof (and/c path? complete-path?)))))]{


Parameter that determines @tech{collection links files}, additional
paths, and the relative search order of
@racket[current-library-collection-paths] for finding libraries (as
referenced in @racket[require], for example) through the default
@tech{module name resolver} and for finding paths through
@racket[collection-path] and @racket[collection-file-path]. See
@secref["collects-search"] for more information.}


@defboolparam[use-user-specific-search-paths on?]{

Parameter that determines whether user-specific paths, which are in
the directory produced by @racket[(find-system-path 'addon-dir)], are
included in search paths for collections and other files. For example,
the initial value of @racket[find-library-collection-paths] omits the
user-specific collection directory when this parameter's value is
@racket[#f].

If @Flag{U} or @DFlag{no-user-path} argument to @exec{racket}, then
@racket[use-user-specific-search-paths] is initialized to
@racket[#f].}


@defboolparam[use-collection-link-paths on?]{

Parameter that determines whether @tech{collection links files} are
included in the result of @racket[find-library-collection-links].

If this parameter's value is @racket[#f] on start-up, then
@tech{collection links files} are effectively disabled permanently for
the Racket process. In particular, if an empty string is provided as
the @Flag{X} or @DFlag{collects} argument to @exec{racket}, then not
only is @racket[current-library-collection-paths] initialized to the
empty list, but @racket[use-collection-link-paths] is initialized to
@racket[#f].}

