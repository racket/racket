#lang scribble/doc
@(require "mz.rkt"
          (for-label racket/fasl
                     racket/serialize))

@title{Serializing Syntax}

@defproc[(syntax-serialize [stx syntax?]
                           [#:preserve-property-keys preserve-property-keys (listof symbol?)]
                           [#:provides-namespace provides-namespace (or/c namespace? #f) (current-namespace)]
                           [#:base-module-path-index base-module-path-index (or/c module-path-index? #f) #f])
         any/c]{

Converts @racket[stx] to a serialized form that is suitable for use
with @racket[s-exp->fasl] or @racket[serialize]. Although @racket[stx]
could be serialized with @racket[(compile `(quote-syntax ,stx))] and
then writing the compiled form, @racket[syntax-serialize] provides
more control over serialization:

@itemlist[

 @item{The @racket[preserve-property-keys] lists syntax-property keys
       to whose values should be preserved in serialization, even if
       the property value was not added as preserved with
       @racket[syntax-property] (so it would be discarded in compiled
       form). The values associated with the properties to preserve
       must be serializable in the sense required by
       @racket[syntax-property] for a preserved property.}

 @item{The @racket[provides-namespace] argument constrains how much
       the serialized syntax object can rely on @deftech{bulk
       bindings}, which are shared binding tables provided by
       exporting modules. If @racket[provides-namespace] is
       @racket[#f], then complete binding information is recorded in
       the syntax object's serialized form, and no bulk bindings will
       be needed from the namespace at deserialization. Otherwise,
       bulk bindings will be used only for modules declared in
       @racket[provides-namespace] (i.e., the deserialize-time
       namespace will have the same module declarations as
       @racket[provides-namespace]); note that supplying a namespace
       with no module bindings is equivalent to supplying
       @racket[#f].}

 @item{The @racket[base-module-path-index] argument specifies a
       @tech{module path index} to which binding information in
       @racket[stx] is relative. For example, if a syntax object
       originates from @racket[quote-syntax] in the body of a module,
       then @racket[base-module-path-index] could usefully be the
       enclosing module's module path index as produced by
       @racket[(variable-reference->module-path-index
       (#%variable-reference))] within the module. On deserialization,
       a different module path index can be supplied to substitute in
       place of @racket[base-module-path-index], which shifts any
       binding that is relative to the serialize-time module's
       identity to be relative to the module identity supplied at
       deserialize time. If @racket[base-module-path-index] is
       @racket[#f], then no shifting is supported at deserialize time,
       and any @racket[base-module-path-index] supplied at that time
       is ignored.}

]

A serialized syntax object is otherwise similar to compiled code: it
is version-specific, and deserialization will require a sufficiently
powerful @tech{code inspector}.

@history[#:added "8.0.0.13"]}

@defproc[(syntax-deserialize [v any/c]
                             [#:base-module-path-index base-module-path-index (or/c module-path-index? #f) #f])
         syntax?]{

Converts the result of @racket[syntax-serialize] back to a syntax
object. See @racket[syntax-serialize] for more information.

@history[#:added "8.0.0.13"]}
