#lang scribble/doc
@(require scribble/manual "utils.rkt"
          (for-label scribble/xref
                     scribble/base-render
                     scribble/html-render
                     setup/xref))

@title[#:tag "xref"]{Cross-Reference Utilities}

@defmodule[scribble/xref]{The @racketmodname[scribble/xref] library
provides utilities for querying cross-reference information that was
collected from a document build.}

@; ------------------------------------------------------------------------

@defproc[(xref? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a cross-reference record created
by @racket[load-xref], @racket[#f] otherwise.}


@defproc[(load-xref [sources (listof (-> any/c))]
                    [#:demand-source demand-source 
                                     (tag? -> (or/c (-> any/c) #f)) 
                                     (lambda (_tag) #f)]
                    [#:render% using-render% (implementation?/c render<%>)
                               (render-mixin render%)]
                    [#:root root-path (or/c path-string? false/c) #f])
         xref?]{

Creates a cross-reference record given a list of functions that each
produce a serialized information obtained from @xmethod[render<%>
serialize-info]. If a @racket[sources] element produces @racket[#f],
its result is ignored.

The @racket[demand-source] function can effectively add a new source
to @racket[sources] in response to a search for information on the
given tag. The @racket[demand-source] function returns @racket[#f]
to indicate that no new sources satisfy the given tag.

Since the format of serialized information is specific to a rendering
class, the optional @racket[using-render%] argument accepts the
relevant class. It defaults to HTML rendering, partly because
HTML-format information is usable by other formats (including
Latex/PDF and text).

If @racket[root-path] is not @racket[#f], then file paths that are
serialized as relative to an instantiation-supplied @racket[root-path]
are deserialized as relative instead to the given @racket[root-path].

Use @racket[load-collections-xref] from @racketmodname[setup/xref] to
get all cross-reference information for installed documentation.}


@defproc[(xref-binding->definition-tag [xref xref?]
                                       [binding (or/c identifier?
                                                      (list/c (or/c module-path?
                                                                    module-path-index?)
                                                              symbol?)
                                                      (listof module-path-index?
                                                              symbol?
                                                              module-path-index?
                                                              symbol?
                                                              (one-of/c 0 1)
                                                              (or/c exact-integer? false/c)
                                                              (or/c exact-integer? false/c)))]
                                       [mode (or/c exact-integer? false/c)])
         (or/c tag? false/c)]{

Locates a tag in @racket[xref] that documents a module export. The
binding is specified in one of several ways, as described below; all
possibilities encode an exporting module and a symbolic name. The name
must be exported from the specified module. Documentation is found
either for the specified module or, if the exported name is
re-exported from other other module, for the other module
(transitively).

The @racket[mode] argument specifies the relevant phase level for the
binding. The @racket[binding] is specified in one of four ways:

@itemize[

 @item{If @racket[binding] is an identifier, then
       @racket[identifier-binding] is used with @racket[mode] to
       determine the binding.}

 @item{If @racket[binding] is a two-element list, then the first
       element provides the exporting module and the second the
       exported name. The @racket[mode] argument is effectively
       ignored.}

 @item{If @racket[binding] is a seven-element list, then it corresponds
       to a result from @racket[identifier-binding] using
       @racket[mode].}

 @item{If @racket[binding] is a five-element list, then the first
       element is as for the two-element-list case, and the remain
       elements are as in the last four elements of the seven-element
       case.}

]

If a documentation point exists in @racket[xref], a tag is returned,
which might be used with @racket[xref-tag->path+anchor] or embedded in
a document rendered via @racket[xref-render]. If no definition point
is found in @racket[xref], the result is @racket[#f].}


@defproc[(xref-tag->path+anchor [xref xref?]
                                [tag tag?]
                                [#:external-root-url root-url (or/c string? #f) #f]
                                [#:render% using-render% (implementation?/c render<%>)
                                           (render-mixin render%)])
         (values (or/c false/c path?)
                 (or/c false/c string?))]{

Returns a path and anchor string designated by the key @racket[tag]
according the cross-reference @racket[xref]. The first result is
@racket[#f] if no mapping is found for the given tag. The second
result is @racket[#f] if the first result is @racket[#f], and it can
also be @racket[#f] if the tag refers to a page rather than a specific
point in a page.

If @racket[root-url] is provided, then references to documentation in
the main installation are redirected to the given URL.

The optional @racket[using-render%] argument is as for
@racket[load-xref].}


@defproc[(xref-tag->index-entry [xref xref?] [tag tag?])
         (or/c false/c entry?)]{

Extract an @racket[entry] structure that provides addition information
about the definition (of any) referenced by @racket[tag]. This
function can be composed with @racket[xref-binding->definition-tag] to
obtain information about a binding, such as the library that exports
the binding and its original name.}


@defproc[(xref-render [xref xref?]
                      [doc part?]
                      [dest (or/c path-string? false/c)]
                      [#:render% using-render% (implemenation?/c render<%>)
                                 (render-mixin render%)]
                      [#:refer-to-existing-files? use-existing? any/c (not dest)])
         (or/c void? any/c)]{

Renders @racket[doc] using the cross-reference info in @racket[xref]
to the destination @racket[dest]. For example, @racket[doc] might be a
generated document of search results using link tags described in
@racket[xref].

If @racket[dest] is @racket[#f], no file is written, and the result is
an X-expression for the rendered page. Otherwise, the file
@racket[dest] is written and the result is @|void-const|.

The optional @racket[using-render%] argument is as for
@racket[load-xref]. It determines the kind of output that is
generated.

If @racket[use-existing?] is true, then files referenced during
rendering (such as image files) are referenced from their existing
locations, instead of copying to the directory of @racket[dest].}


@defproc[(xref-transfer-info [renderer (is-a?/c render<%>)]
                             [ci collect-info?]
                             [xref xref?])
         void?]{

Transfers cross-reference information to @racket[ci], which is the
initially collected information from @racket[renderer].}


@defproc[(xref-index [xref xref?]) (listof entry?)]{

Converts indexing information @racket[xref] into a list of
@racket[entry] structures.}


@defstruct[entry ([words (and/c (listof string?) cons?)]
                  [content list?]
                  [tag tag?]
                  [desc any/c])]{

Represents a single entry in a Scribble document index.

The @racket[words] list corresponds to
@racket[index-element-plain-seq]. The @racket[content] list
corresponds to @racket[index-element-entry-seq]. The @racket[desc]
value corresponds to @racket[index-element-desc]. The @racket[tag] is
the destination for the index link into the main document.}
