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
                               (render-mixin render%)])
         xref?]{

Creates a cross-reference record given a list of functions that each
produce a serialized information obtained from @xmethod[render%
serialize-info].

Since the format of serialized information is specific to a rendering
class, the optional @scheme[using-render%] argument accepts the
relevant class. It default to HTML rendering.

Use @scheme[load-collections-xref] from @schememodname[setup/xref] to
get all cross-reference information for installed documentation.}


@defproc[(xref-binding->definition-tag [xref xref?]
                                       [mod (or/c module-path?
                                                  module-path-index?
                                                  path?
                                                  resolved-module-path?)]
                                       [sym symbol?])
         (or/c tag? false/c)]{

Locates a tag in @scheme[xref] that documents @scheme[sym] as defined
by @scheme[mod]. The @scheme[sym] and @scheme[mod] combination
correspond to the first two elements of a @scheme[identifier-binding]
list result.

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
                      [dest path-string?]
                      [#:render% using-render% (subclass?/c render%)
                                 (render-mixin render%)])
         void?]{

Renders @scheme[doc] using the cross-reference info in @scheme[xref]
to the destination @scheme[dest]. For example, @scheme[doc] might be a
generated document of search results using link tags described in
@scheme[xref].

The optional @scheme[using-render%] argument is as for
@scheme[load-xref]. It determines the kind of output that is
generated.}


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
