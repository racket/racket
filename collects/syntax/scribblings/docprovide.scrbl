#lang scribble/doc
@(require "common.ss"
          (for-label syntax/docprovide))

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

The @scheme[doc-label-id] identifier is used as a key for accessing
the documentation through @scheme[lookup-documentation].  The actual
documentation is organized into ``rows'', each with a section title.

A @scheme[row] has one of the following forms:

@itemize[
  @item{@scheme[(section-string (name type-datum doc-string ...) ...)]

     Creates a documentation section whose title is @scheme[section-string],
     and provides/documents each @scheme[name]. The @scheme[type-datum] is arbitrary,
     for use by clients that call @scheme[lookup-documentation]. The
     @scheme[doc-string]s are also arbitrary documentation information,
     usually concatenated by clients.

     A @scheme[name] is either an identifier or a renaming sequence
     @scheme[(local-name-id extenal-name-id)].

     Multiple @scheme[row]s with the same section name will be merged in the
documentation output. The final order of sections matches the order of
the first mention of each section.}

  @item{@scheme[(all-from prefix-id module-path doc-label-id)]}
  @item{@scheme[(all-from-except prefix-id module-path doc-label-id id ...)]

     Merges documentation and provisions from the specified module
into the current one; the @scheme[prefix-id] is used to prefix the imports
into the current module (so they can be re-exported). If @scheme[id]s are
provided, the specified @scheme[id]s are not re-exported and their
documentation is not merged.}]}

@defproc[(lookup-documentation [module-path-v module-path?] 
                               [label-sym symbol?]) 
         any]{

Returns documentation for the specified module and label. The
@scheme[module-path-v] argument is a quoted module path, like the
argument to @scheme[dynamic-require]. The
@scheme[label-sym] identifies a set of documentation using the symbol
as a label identifier in @scheme[provide-and-document].}

