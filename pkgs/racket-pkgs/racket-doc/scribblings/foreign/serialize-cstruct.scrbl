#lang scribble/doc
@(require "utils.rkt" 
          (for-label racket/serialize
                     ffi/serialize-cstruct)
          scribble/racket)

@title[#:tag "serialize-struct"]{Serializable C Struct Types}

@defmodule[ffi/serialize-cstruct]

@defform/subs[(define-serializable-cstruct _id ([field-id type-expr] ...)
                                           property ...)
              [(property (code:line #:alignment alignment-expr)
                         (code:line #:malloc-mode malloc-mode-expr)
                         (code:line #:serialize-inplace)
                         (code:line #:deserialize-inplace)
                         (code:line #:property prop-expr val-expr))]]{

Like @racket[define-cstruct], but defines a serializable type.
In addition to the bindings created by @racket[define-cstruct],
@racketidfont{make-@racketvarfont{id}/mode} is bound to a function that behaves
like @racketidfont{make-@racketvarfont{id}} but uses the mode or allocator
specified via @racket[malloc-mode-expr].

Instances of the new type fulfill the @racket[serializable?] predicate and can
be used with @racket[serialize] and @racket[deserialize]. Serialization may
fail if one of the fields contains an arbitrary pointer, an embedded
 non-serializable C struct, or a pointer to a non-serializable C struct.
Array-types are supported as long as they don't contain one of these types.

The @racket[malloc-mode-expr] arguments control the memory allocation
for this type during deserialization and
@racketidfont{make-@racketvarfont{id}/mode}. It can be one of the mode
arguments to @racket[malloc], or a procedure
@;
@racketblock[(-> exact-positive-integer? cpointer?)]
@;
that allocates memory of the given size. The default is
@racket[malloc] with @racket['atomic].

When @racket[#:serialize-inplace] is specified, the serialized
representation shares memory with the C struct object. While being more
efficient, especially for large objects, changes to the object after
serialization may lead to changes in the serialized representation.

A @racket[#:deserialize-inplace] option reuses the memory of the serialized
representation, if possible. This option is more efficient for large objects,
but it may fall back to allocation via @racket[malloc-mode-expr] for cyclic
structures. As the allocation mode of the serialized representation
will be @racket['atomic] by default or may be arbitrary if
@racket[#:serialize-inplace] is specified, inplace deserialisation
should be used with caution whenever the object contains pointers.

When the C struct contains pointers, it is advisable to use a custom
allocator. It should be based on a non-moving-memory allocation like
@racket['raw], potentially with manual freeing to avoid memory leaks
after garbage collection.
}


