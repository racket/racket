#lang scribble/doc
@(require "utils.rkt" 
          (for-label racket/serialize
                     ffi/serialize-cstruct
                     (except-in ffi/unsafe ->))
          scribble/racket
          scribble/example)

@(define serialize-eval (make-base-eval))
@examples[#:eval serialize-eval #:hidden (require ffi/serialize-cstruct
                                                  ffi/unsafe
                                                  racket/serialize)]

@title[#:tag "serialize-struct"]{Serializable C Struct Types}

@defmodule[ffi/serialize-cstruct]

@defform/subs[(define-serializable-cstruct _id ([field-id type-expr] ...)
                                           property ...)
              [(property (code:line #:alignment alignment-expr)
                         (code:line #:malloc-mode malloc-mode-expr)
                         (code:line #:serialize-inplace)
                         (code:line #:deserialize-inplace)
                         (code:line #:version vers)
                         (code:line #:other-versions ([other-vers deserialize-chain-expr
                                                                  convert-proc-expr
                                                                  unconvert-proc-expr
                                                                  cycle-convert-proc-expr]
                                                       ...))
                         (code:line #:property prop-expr val-expr))]]{

Like @racket[define-cstruct], but defines a serializable type, with
several changed additional bindings:

@itemize[

 @item{@racketidfont{make-@racketvarfont{id}} --- always uses
       @racket['atomic] allocation, even if @racket[#:malloc-mode] is
       specified (for historical reasons).}

 @item{@racketidfont{make-@racketvarfont{id}/mode} --- like behaves
       like @racketidfont{make-@racketvarfont{id}} but uses the mode
       or allocator specified via @racket[malloc-mode-expr] (for
       historical reasons).}

 @item{@racketidfont{deserialize:cstruct:@racketvarfont{id}} (for a
       @racket[vers] of @racket[0]) or
       @racketidfont{deserialize:cstruct:@racketvarfont{id}-v@racketvarfont{vers}}
       (for a @racket[vers] of @racket[1] or more) --- deserialization information that is
       automatically exported from a @racket[deserialize-info] submodule.}

 @item{@racketidfont{deserialize-chain:cstruct:@racketvarfont{id}}
       (for a @racket[vers] of @racket[0]) or
       @racketidfont{deserialize-chain:cstruct:@racketvarfont{id}-v@racketvarfont{vers}}
       (for a @racket[vers] of @racket[1] or more) --- deserialization
       information for use via @racket[#:other-versions] in other
       @racket[define-serializable-cstruct] forms.}

 @item{@racketidfont{deserialize:cstruct:@racketvarfont{id}} (for an
       @racket[other-vers] of @racket[0]) or
       @racketidfont{deserialize:cstruct:@racketvarfont{id}-v@racketvarfont{other-vers}}
       (for an @racket[other-vers] of @racket[1] or more) ---
       deserialization information that is automatically exported from
       a @racket[deserialize-info] submodule.}

]

Instances of the new type fulfill the @racket[serializable?] predicate and can
be used with @racket[serialize] and @racket[deserialize]. Serialization may
fail if one of the fields contains an arbitrary pointer, an embedded
 non-serializable C struct, or a pointer to a non-serializable C struct.
Array-types are supported as long as they don't contain one of these types.

The default @racket[vers] is @racket[0], and @racket[vers] must be a
literal, exact, non-negative integer. An @racket[#:other-versions]
clause provides deserializers for previous versions of the structure
with the name @racketvarfont{id}, so that previously serialized data can be
deserialized after a change to the declaration of @racketvarfont{id}. For
each @racket[other-vers], @racket[deserialize-chain-expr] should be
the value of a
@racketidfont{deserialize:cstruct:@racketvarfont{other-id}} binding
for some other @racket{other-id} declared with
@racket[define-serializable-cstruct] that has the same shape that the
previous version of @racketvarfont{id}; the function produced by
@racket[convert-proc-expr] should convert an instance of
@racket[_other-id] to an instance of @racketvarfont{id}. The functions
produced by @racket[unconvert-proc-expr] and
@racket[cycle-convert-proc-expr] are used if a record is involved in a
cycle; the function from @racket[unconvert-proc-expr] takes an
@racketvarfont{id} instance produced by @racket[convert-proc-expr]'s function
back to a @racket[_other-id], while @racket[cycle-convert-proc-expr]
returns two values: a shell instance of @racketidfont{id} and function to
accept a filled @racket[_other-id] whose content should be moved to
the shell instance of @racketidfont{id}.

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

@history[#:changed "1.1" @elem{Added @racket[#:version] and @racket[#:other-versions].}]

@examples[
#:eval serialize-eval
(define-serializable-cstruct _fish ([color _int]))
(define f0/s (serialize (make-fish 1)))
(fish-color (deserialize f0/s))

(define-serializable-cstruct _aq ([a (_gcable _fish-pointer)]
                                  [d (_gcable _aq-pointer/null)])
  #:malloc-mode 'nonatomic)
(define aq1 (make-aq/mode (make-fish 6) #f))
(code:line (set-aq-d! aq1 aq1) (code:comment "create a cycle"))
(define aq0/s (serialize aq1))
(aq-a (aq-d (aq-d (deserialize aq0/s))))
(code:comment @#,elem{Same shape as original @racket[aq]:})
(define-serializable-cstruct _old-aq ([a (_gcable _fish-pointer)]
                                      [d (_gcable _pointer)])
        #:malloc-mode 'nonatomic)
(code:comment @#,elem{Replace the original @racket[aq]:})
(define-serializable-cstruct _aq ([a (_gcable _fish-pointer)]
                                  [b (_gcable _fish-pointer)]
                                  [d (_gcable _aq-pointer/null)])
  #:malloc-mode 'nonatomic
  #:version 1
  #:other-versions ([0 deserialize-chain:cstruct:old-aq
                       (lambda (oa)
                         (make-aq/mode (old-aq-a oa)
                                       (old-aq-a oa)
                                       (cast (old-aq-d oa) _pointer _aq-pointer)))
                       (lambda (a)
                         (make-old-aq/mode (aq-a a)
                                           (aq-d a)))
                       (lambda ()
                         (define tmp-fish (make-fish 0))
                         (define a (make-aq/mode tmp-fish tmp-fish #f))
                         (values a
                                 (lambda (oa)
                                   (set-aq-a! a (old-aq-a oa))
                                   (set-aq-b! a (old-aq-a oa))
                                   (set-aq-d! a (cast (old-aq-d oa) _pointer _aq-pointer)))))]))
(code:comment "Deserialize old instance to new cstruct:")
(fish-color (aq-a (aq-d (aq-d (deserialize aq0/s)))))

(define aq1/s (serialize (make-aq/mode (make-fish 1) (make-fish 2) #f)))
(code:comment @#,elem{New version of @racket[fish]:})
(define-serializable-cstruct _old-fish ([color _int]))
(define-serializable-cstruct _fish ([weight _float]
                                    [color _int])
  #:version 1
  #:other-versions ([0 deserialize-chain:cstruct:old-fish
                       (lambda (of)
                         (make-fish 10.0 (old-fish-color of)))
                       (lambda (a) (error "cycles not possible!"))
                       (lambda () (error "cycles not possible!"))]))
(code:comment @#,elem{Deserialized content upgraded to new @racket[fish]:})
(fish-color (aq-b (deserialize aq1/s)))
(fish-weight (aq-b (deserialize aq1/s)))
]}

@close-eval[serialize-eval]

