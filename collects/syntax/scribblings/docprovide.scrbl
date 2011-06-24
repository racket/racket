#lang scribble/doc
@(require "common.rkt" (for-label syntax/docprovide))

@title[#:tag "docprovide"]{Attaching Documentation to Exports}

@defmodule[syntax/docprovide]

@defform/subs[#:literals (all-from all-from-except)
              (provide-and-document doc-label-id doc-row ...)
              ([doc-row 
                (section-string (name type-datum doc-string ...) ...)
                (all-from prefix-id module-path doc-label-id)
                (all-from-except prefix-id module-path doc-label-id id ...)]
               [name
                id
                (local-name-id external-name-id)])]{

A form that exports names and records documentation information.

The @racket[doc-label-id] identifier is used as a key for accessing
the documentation through @racket[lookup-documentation].  The actual
documentation is organized into ``rows'', each with a section title.

A @racket[row] has one of the following forms:

@itemize[
  @item{@racket[(section-string (name type-datum doc-string ...) ...)]

     Creates a documentation section whose title is @racket[section-string],
     and provides/documents each @racket[name]. The @racket[type-datum] is arbitrary,
     for use by clients that call @racket[lookup-documentation]. The
     @racket[doc-string]s are also arbitrary documentation information,
     usually concatenated by clients.

     A @racket[name] is either an identifier or a renaming sequence
     @racket[(local-name-id extenal-name-id)].

     Multiple @racket[row]s with the same section name will be merged in the
documentation output. The final order of sections matches the order of
the first mention of each section.}

  @item{@racket[(all-from prefix-id module-path doc-label-id)]}
  @item{@racket[(all-from-except prefix-id module-path doc-label-id id ...)]

     Merges documentation and provisions from the specified module
into the current one; the @racket[prefix-id] is used to prefix the imports
into the current module (so they can be re-exported). If @racket[id]s are
provided, the specified @racket[id]s are not re-exported and their
documentation is not merged.}]}

@defproc[(lookup-documentation [module-path-v module-path?] 
                               [label-sym symbol?]) 
         any]{

Returns documentation for the specified module and label. The
@racket[module-path-v] argument is a quoted module path, like the
argument to @racket[dynamic-require]. The
@racket[label-sym] identifies a set of documentation using the symbol
as a label identifier in @racket[provide-and-document].}

