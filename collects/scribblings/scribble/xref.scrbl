#lang scribble/doc
@(require scribble/manual
          "utils.ss"
          (for-label scribble/xref
                     scribble/base-render
                     scribble/html-render
                     setup/xref))

@title[#:tag "xref"]{Cross-Reference Utilities}

@defmodule[scribble/xref]{The @schememodname[scribble/xref] library
provides utilities for querying cross-reference information that was
collected from a document build.}

@; ------------------------------------------------------------------------

@defproc[(xref? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a cross-reference record created
by @scheme[load-xref], @scheme[#f] otherwise.}


@defproc[(load-xref [sources (listof (-> any/c))]
                    [#:render% using-render% (subclass?/c render%)
                               (render-mixin render%)]
                    [#:root root-path (or/c path-string? false/c) #f])
         xref?]{

Creates a cross-reference record given a list of functions that each
produce a serialized information obtained from @xmethod[render%
serialize-info]. If a @scheme[sources] element produces @scheme[#f],
its result is ignored.

Since the format of serialized information is specific to a rendering
class, the optional @scheme[using-render%] argument accepts the
relevant class. It default to HTML rendering.

If @scheme[root-path] is not @scheme[#f], then file paths that are
serialized as relative to an instantiation-supplied @scheme[root-path]
are deserialized as relative instead to the given @scheme[root-path].

Use @scheme[load-collections-xref] from @schememodname[setup/xref] to
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

Locates a tag in @scheme[xref] that documents a module export. The
binding is specified in one of several ways, as described below; all
possibilities encode an exporting module and a symbolic name. The name
must be exported from the specified module. Documentation is found
either for the specified module or, if the exported name is
re-exported from other other module, for the other module
(transitively).

The @scheme[mode] argument specifies the relevant phase level for the
binding. The @scheme[binding] is specified in one of four ways:

@itemize{

 @item{If @scheme[binding] is an identifier, then
       @scheme[identifier-binding] is used with @scheme[mode] to
       determine the binding.}

 @item{If @scheme[binding] is a two-element list, then the first
       element provides the exporting module and the second the
       exported name. The @scheme[mode] argument is effectively
       ignored.}

 @item{If @scheme[binding] is a seven-element list, then it corresponds
       to a result from @scheme[identifier-binding] using
       @scheme[mode].}

 @item{If @scheme[binding] is a five-element list, then the first
       element is as for the two-element-list case, and the remain
       elements are as in the last four elements of the seven-element
       case.}

}

If a documentation point exists in @scheme[xref], a tag is returned,
which might be used with @scheme[xref-tag->path+anchor] or embedded in
a document rendered via @scheme[xref-render]. If no definition point
is found in @scheme[xref], the result is @scheme[#f].}


@defproc[(xref-tag->path+anchor [xref xref?]
                                [tag tag?]
                                [#:render% using-render% (subclass?/c render%)
                                           (render-mixin render%)])
         (values (or/c false/c path?)
                 (or/c false/c string?))]{

Returns a path and anchor string designated by the key @scheme[tag]
according the cross-reference @scheme[xref]. The first result is
@scheme[#f] if no mapping is found for the given tag. The second
result is @scheme[#f] if the first result is @scheme[#f], and it can
also be @scheme[#f] if the tag refers to a page rather than a specific
point in a page.

The optional @scheme[using-render%] argument is as for
@scheme[load-xref].}


@defproc[(xref-tag->index-entry [xref xref?]
                                [tag tag?])
         (or/c false/c entry?)]{

Extract an @scheme[entry] structure that provides addition information
about the definition (of any) referenced by @scheme[tag]. This
function can be composed with @scheme[xref-binding->definition-tag] to
obtain information about a binding, such as the library that exports
the binding and its original name.}


@defproc[(xref-render [xref xref?]
                      [doc part?]
                      [dest (or/c path-string? false/c)]
                      [#:render% using-render% (subclass?/c render%)
                                 (render-mixin render%)]
                      [#:refer-to-existing-files? use-existing? any/c (not dest)])
         (or/c void? any/c)]{

Renders @scheme[doc] using the cross-reference info in @scheme[xref]
to the destination @scheme[dest]. For example, @scheme[doc] might be a
generated document of search results using link tags described in
@scheme[xref].

If @scheme[dest] is @scheme[#f], no file is written, and the result is
an X-expression for the rendered page. Otherwise, the file
@scheme[dest] is written and the result is @|void-const|.

The optional @scheme[using-render%] argument is as for
@scheme[load-xref]. It determines the kind of output that is
generated.

If @scheme[use-existing?] is true, then files referenced during
rendering (such as image files) are referenced from their existing
locations, instead of copying to the directory of @scheme[dest].}


@defproc[(xref-transfer-info [renderer (is-a?/c render%)]
                             [ci collect-info?]
                             [xref xref?])
         void?]{

Transfers cross-reference information to @scheme[ci], which is the
initially collected information from @scheme[renderer].}
                             


@defproc[(xref-index [xref xref?]) (listof entry?)]{

Converts indexing information @scheme[xref] into a list of
@scheme[entry] structures.}


@defstruct[entry ([words (and/c (listof string?) cons?)]
                  [content list?]
                  [tag tag?]
                  [desc any/c])]{

Represents a single entry in a Scribble document index.

The @scheme[words] list corresponds to
@scheme[index-element-plain-seq]. The @scheme[content] list
corresponds to @scheme[index-element-entry-seq]. The @scheme[desc]
value corresponds to @scheme[index-element-desc]. The @scheme[tag] is
the destination for the index link into the main document.}
