#lang scribble/manual
@(require "common.rkt"
          (for-label (only-in ffi/unsafe cpointer?)))

@title[#:tag "pointer"]{Foreign Pointers}

A @deftech{pointer} object encapsulates a memory address and a
potentially empty sequence of symbolic @tech{tags}. A pointer object
may refer to an address that under the control of Racket's memory
manager and garbage collection, or it may refer to an address is
managed externally.

The base type for pointers is @racket[ptr_t] (aliased as
@racket[void_t*]), and new pointer types can be created via
@racket[define-ffi2-type] with either an existing pointer type, a
@racket[struct] type, a @racket[union] type, or an a @racket[array]
type.

A pointer's @deftech{tags} enable sanity checking that the right kind
of pointer is provided to an operation. If an operation expects a
pointer with a certain sequence of tags, it accepts a pointer with
additional tags added to the end, so additional tags form a
@deftech{pointer subtype}. A @racket[ptr_t] representation has no tags,
which means that a C conversion to Racket via @racket[ptr_t] is not
accepted by any context that expects some tag, whereas a pointer with
any tags is accepted as a @racket[ptr_t] representation to translate
to C. Whether a pointer object represents an address managed by
Racket's garbage collector is independent of its tags.

@deftogether[(
@defproc[(ptr_t? [v any/c]) boolean?]
@defproc[(ptr_t/gcable? [v any/c]) boolean?]
@defproc[(void_t*? [v any/c]) boolean?]
@defproc[(void_t*/gcable? [v any/c]) boolean?]
)]{

The @racket[ptr_t?] or @racket[void_t*?] predicate recognizes any
@tech{pointer} object, while @racket[ptr_t/gcable?] or
@racket[void_t*/gcable?] recognizes a pointer object that represents a
reference to memory that is managed by Racket's garbage collector.

}

@defform[(ffi2-malloc maybe-mode
                      maybe-type
                      n-expr maybe-bytes
                      maybe-as-type)
         #:grammar ([maybe-mode #:gcable
                                #:gcable-traced
                                #:gcable-immobile
                                #:gcable-traced-immobile
                                #:manual
                                ϵ]
                    [maybe-type type
                                ϵ]
                    [maybe-bytes #:bytes
                                 ϵ]
                    [maybe-as-type (code:line #:as as-type)
                                   ϵ])]{

Allocates memory.

The amount of allocated memory depends on @racket[n-expr]. If no type
is provided as @racket[maybe-type], or if @racket[maybe-bytes] is
@racket[#:bytes], then the allocated memory spans as many bytes as the
value of @racket[n-expr]. Otherwise, the allocated memory's size is
the result of @racket[(* n-expr (ffi2-sizeof maybe-type))].

If @racket[maybe-as] is specified, then the result is cast to
@racket[as-type] in the sense of @racket[ffi2-cast]. Otherwise, the
result is a @tech{pointer} object encapsulating the address of
allocated memory. If @racket[maybe-type] is a @racket[struct],
@racket[union], or size @racket[array] type (and @racket[maybe-as] is
not specified), the result is a pointer object using the
representation of @racket[maybe-type]. Otherwise, the result is a
pointer object using the representation of @racket[void_t*].

By default, allocation uses @racket[#:gcable] mode, but a
@racket[maybe-mode] specification can pick any of the supported modes:

@itemlist[

 @item{@racket[#:gcable]: Allocates uninitialized memory in Racket's
       garbage-collected space, i.e., GCable memory. The allocated
       memory becomes eligible for garbage collection when it is not
       referenced by any reachable pointer object or @tech{traced}
       allocated memory. Even before collection, the memory manager
       may relocate the object, but garbage collection or relocation
       cannot happen with a foreign-procedure call is active.}

 @item{@racket[#:gcable-immobile]: Like @racket[#:gcable], but the
       allocated memory will not be relocated by a different address
       by the memory manager as long as it is not collected.}

 @item{@racket[#:gcable-traced]: Like @racket[#:gcable], but the
       allocated memory is @deftech{traced}, meaning that it can
       itself contain references to other memory that is allocated
       using @racket[#:gcable] variants. In particular, references
       from traced memory are updated by the memory manager if it
       moves the referenced objects. Traced memory can also contain
       references to non-GCable memory, but beware of the possibility
       that Racket's garbage collector takes over an an address range
       that was formerly under external control but deallocated.}

 @item{@racket[#:gcable-traced-immobile]: Like
       @racket[#:gcable-traced], but the allocated memory will not be
       relocated by a different address by the memory manager as long
       as it is not collected (but it can contain references to mobile
       objects).}

 @item{@racket[#:manual]: Allocates outside of Racket's
       garbage-collected space. The allocated memory is never
       relocated by the garbage collection, and it must be freed
       explicitly with @racket[ffi2-free].}

]

}

@defproc[(ffi2-free [ptr void_t*?]) void?]{

Deallocates memory that was allocated with @racket[ffi2-malloc] in
@racket[#:manual] mode.

}

@defform[(ffi2-ref ptr-expr type maybe-offset)
         #:grammar ([maybe-offset offset-expr
                                  (code:line offset-expr #:bytes)
                                  ϵ])]{

Extracts a value from the address referenced by @racket[ptr-expr] plus
(potentially) @racket[maybe-offset]. The extracted value in memory
uses the C representation of @racket[type], and the result is the
value converted to the Racket representation of @racket[type].
No @tech{tags} of the result of @racket[ptr-expr] are checked; it
assumed to be a valid pointer to a @racket[type] representation.

If @racket[offset-expr] is followed by @racket[#:bytes], it the result
of @racket[offset-expr] is used as an offset to add to the address
represented by the result of @racket[ptr-expr]. If @racket[#:bytes] is
not present, that offset is multiplied by @racket[(ffi2-sizeof type)].

}

@defform[(ffi2-set! ptr-expr type maybe-offset val-expr)
         #:grammar ([maybe-offset offset-expr
                                  (code:line offset-expr #:bytes)
                                  ϵ])]{

Install a value at the address referenced by @racket[ptr-expr] plus
(potentially) @racket[maybe-offset]. The installed value in memory
uses the C representation of @racket[type], and it is converted from
the result of @racket[val-expr] as the Racket representation for
@racket[type]. No @tech{tags} of the result of @racket[ptr-expr] are
checked; it assumed to be a valid destination for a @racket[type]
representation.

When @racket[maybe-offset] is not empty, it specifies an address
offset in the same way as for @racket[ffi2-ref].


}

@defform[(ffi2-cast expr option ...)
         #:grammar ([option (code:line #:from from-type)
                            (code:line #:to to-type)
                            (code:line #:offset maybe-type n-expr)]
                    [maybe-type type
                                ϵ])]{

Converts the Racket representation produced by @racket[expr] from one
foreign type's representation to another. If @racket[from-type] or
@racket[to-type] is not specified, each defaults to @racket[ptr_t].
Both @racket[from-type] and @racket[to-type] must be pointer types,
or they must both be @tech{scalar} types.

If the @racket[#:offset] option is provided, the resulting pointer is
shifted to represent an address that is @racket[_n] bytes later, where
@racket[_n] is the result of @racket[n-expr] multiplied by
@racket[(ffi2-sizeof maybe-type)] or by @racket[1] if
@racket[maybe-type] is empty. An @racket[#:offset] option cannot be
provided for casting between scalar types.

}

@defform*[[(ffi2-add ptr-expr n-expr)
           (ffi2-add ptr-expr type n-expr)]]{

A shorthand for @racket[(ffi2-cast ptr-expr #:offset n-expr)]
or @racket[(ffi2-cast ptr-expr #:offset type n-expr #:to (array type *))].

}

@deftogether[(
@defproc[(ffi2-memcpy [dest ptr_t?]
                      [src ptr_t?]
                      [len exact-nonnegative-integer?]
                      [#:dest-offset dest-offset exact-integer? 0]
                      [#:src-offset src-offset exact-integer? 0])
         void?]
@defproc[(ffi2-memmove [dest ptr_t?]
                       [src ptr_t?]
                       [len exact-nonnegative-integer?]
                       [#:dest-offset dest-offset exact-integer? 0]
                       [#:src-offset src-offset exact-integer? 0])
         void?]
@defproc[(ffi2-memset [dest ptr_t?]
                      [byte byte_t?]
                      [len exact-nonnegative-integer?]
                      [#:dest-offset dest-offset exact-integer? 0])
         void?]
)]{

The @racket[ffi2-memcpy] and @racket[ffi2-memmove] functions copy
@racket[len] bytes from the address represented by @racket[(ffi2-add
src src-offset)] to the address represented by @racket[(ffi2-add dest
dest-offset)]. In the case of @racket[ffi2-memcpy], the source and
destination regions must not overlap.

The @racket[ffi2-memcpy] function sets @racket[len] bytes at the
address represented by @racket[(ffi2-add dest dest-offset)] so that
each byte's value is @racket[byte].

}


@deftogether[(
@defproc[(ptr_t->uintptr [ptr ptr_t?]) exact-nonnegative-integer?]
@defproc[(uintptr->ptr_t [int exact-nonnegative-integer?]) ptr_t?]
)]{

Conversions between addresses represented as pointers and addresses as
represented as integers.

Beware that the integer form of an address managed by Racket's garbage
collector can become immediately invalid, unless an object at the
address was allocated as immobile.

}
