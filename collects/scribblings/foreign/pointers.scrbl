#lang scribble/doc
@(require "utils.ss")

@title[#:tag "foreign:pointer-funcs"]{Pointer Functions}

@defproc[(cpointer? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a C pointer or a value that can
be used as a pointer: @scheme[#f] (used as a @cpp{NULL} pointer), byte
strings (used as memory blocks), or some additional internal objects
(@scheme[ffi-obj]s and callbacks, see @secref["foreign:c-only"]).
Returns @scheme[#f] for other values.}

@defproc[(ptr-equal? [cptr1 cpointer?][cptr2 cpointer?]) boolean?]{

Compares the values of the two pointers. Two different Scheme
pointer objects can contain the same pointer.}


@defproc[(ptr-add [cptr cpointer?][offset exact-integer?][type ctype? _byte]) 
         cpointer?]{

Returns a cpointer that is like @scheme[cptr] offset by
@scheme[offset] instances of @scheme[ctype].

The resulting cpointer keeps the base pointer and offset separate. The
two pieces are combined at the last minute before any operation on the
pointer, such as supplying the pointer to a foreign function. In
particular, the pointer and offset are not combined until after all
allocation leading up to a foreign-function call; if the called
function does not itself call anything that can trigger a garbage
collection, it can safely use pointers that are offset into the middle
of a GCable object.}


@defproc[(offset-ptr? [cptr cpointer?]) boolean?]{

A predicate for cpointers that have an offset, such as pointers that
were created using @scheme[ptr-add].  Returns @scheme[#t] even if such
an offset happens to be 0.  Returns @scheme[#f] for other cpointers
and non-cpointers.}


@defproc[(ptr-offset [cptr cpointer?]) exact-integer?]{

Returns the offset of a pointer that has an offset. The resulting
offset is always in bytes.}

@; ----------------------------------------------------------------------

@section{Pointer Dereferencing}

@defproc[(set-ptr-offset! [cptr cpointer?][offset exact-integer?][ctype ctype? _byte]) 
         void?]{

Sets the offset component of an offset pointer.  The arguments are
used in the same way as @scheme[ptr-add].  If @scheme[cptr] has no
offset, the @scheme[exn:fail:contract] exception is raised.}


@defproc[(ptr-add! [cptr cpointer?][offset exact-integer?][ctype ctype? _byte]) 
         void?]{

Like @scheme[ptr-add], but destructively modifies the offset contained
in a pointer.  The same operation could be performed using
@scheme[ptr-offset] and @scheme[set-ptr-offset!].}


@defproc*[([(ptr-ref [cptr cpointer?]
                     [type ctype?]
                     [offset exact-nonnegative-integer? 0])
            any]
           [(ptr-ref [cptr cpointer?]
                     [type ctype?]
                     [abs-tag (one-of/c 'abs)]
                     [offset exact-nonnegative-integer?])
            any]
           [(ptr-set! [cptr cpointer?]
                      [type ctype?]
                      [val any/c])
            void?]
           [(ptr-set! [cptr cpointer?]
                      [type ctype?]
                      [offset exact-nonnegative-integer?]
                      [val any/c])
            void?]
           [(ptr-set! [cptr cpointer?]
                      [type ctype?]
                      [abs-tag (one-of/c 'abs)]
                      [offset exact-nonnegative-integer?]
                      [val any/c])
            void?])]{

The @scheme[ptr-ref] procedure returns the object referenced by
@scheme[cptr], using the given @scheme[type]. The @scheme[ptr-set!]
procedure stores the @scheme[val] in the memory @scheme[cptr] points
to, using the given @scheme[type] for the conversion.

In each case, @scheme[offset] defaults to @scheme[0] (which is the
only value that should be used with @scheme[ffi-obj] objects, see
@secref["foreign:c-only"]).  If an @scheme[offset] index is
non-@scheme[0], the value is read or stored at that location,
considering the pointer as a vector of @scheme[type]s --- so the
actual address is the pointer plus the size of @scheme[type]
multiplied by @scheme[offset].  In addition, a @scheme['abs] flag can
be used to use the @scheme[offset] as counting bytes rather then
increments of the specified @scheme[type].

Beware that the @scheme[ptr-ref] and @scheme[ptr-set!] procedure do
not keep any meta-information on how pointers are used.  It is the
programmer's responsibility to use this facility only when
appropriate.  For example, on a little-endian machine:

@schemeblock[
> (define block (malloc _int 5))
> (ptr-set! block _int 0 196353)
> (map (lambda (i) (ptr-ref block _byte i)) '(0 1 2 3))
@#,(schemeresultfont "(1 255 2 0)")
]

In addition, @scheme[ptr-ref] and @scheme[ptr-set!] cannot detect when
offsets are beyond an object's memory bounds; out-of-bounds access can
easily lead to a segmentation fault or memory corruption.}


@defproc*[([(memmove [cptr cpointer?]
                     [src-cptr cpointer?]
                     [count exact-nonnegative-integer?]
                     [type ctype? _byte])
            void?]
           [(memmove [cptr cpointer?]
                     [offset exact-integer?]
                     [src-cptr cpointer?]
                     [count exact-nonnegative-integer?]
                     [type ctype? _byte])
            void?]
           [(memmove [cptr cpointer?]
                     [offset exact-integer?]
                     [src-cptr cpointer?]
                     [src-offset exact-integer?]
                     [count exact-nonnegative-integer?]
                     [type ctype? _byte])
            void?])]{

Copies to @scheme[cptr] from @scheme[src-cptr]. The destination
pointer can be offset by an optional @scheme[offset], which is in
@scheme[type] instances.  The source pointer can be similarly offset
by @scheme[src-offset].  The number of bytes copied from source to
destination is determined by @scheme[count], which is in @scheme[type]
instances when supplied.}

@defproc*[([(memcpy [cptr cpointer?]
                    [src-cptr cpointer?]
                    [count exact-nonnegative-integer?]
                    [type ctype? _byte])
            void?]
           [(memcpy [cptr cpointer?]
                    [offset exact-integer?]
                    [src-cptr cpointer?]
                    [count exact-nonnegative-integer?]
                    [type ctype? _byte])
            void?]
           [(memcpy [cptr cpointer?]
                    [offset exact-integer?]
                    [src-cptr cpointer?]
                    [src-offset exact-integer?]
                    [count exact-nonnegative-integer?]
                    [type ctype? _byte])
            void?])]{

Like @scheme[memmove], but the result is undefined if the destination
and source overlap.}

@defproc*[([(memset [cptr cpointer?]
                    [byte byte?]
                    [count exact-nonnegative-integer?]
                    [type ctype? _byte])
            void?]
           [(memset [cptr cpointer?]
                    [offset exact-integer?]
                    [byte byte?]
                    [count exact-nonnegative-integer?]
                    [type ctype? _byte])
            void?])]{

Similar to @scheme[memmove], but the destination is uniformly filled
with @scheme[byte] (i.e., an exact integer between 0 and 255
inclusive).}

@defproc[(cpointer-tag [cptr cpointer?]) any]{

Returns the Scheme object that is the tag of the given @scheme[cptr]
pointer.}


@defproc[(set-cpointer-tag! [cptr cpointer?][tag any/c]) void?]{

Sets the tag of the given @scheme[cptr]. The @scheme[tag] argument can
be any arbitrary value; other pointer operations ignore it.  When a
cpointer value is printed, its tag is shown if it is a symbol, a byte
string, a string. In addition, if the tag is a pair holding one of
these in its @scheme[car], the @scheme[car] is shown (so that the tag
can contain other information).}


@; ------------------------------------------------------------

@section{Memory Management}

For general information on C-level memory management with PLT Scheme,
see @|InsideMzScheme|.

@defproc[(malloc [bytes-or-type (or/c exact-nonnegative-integer? ctype?)]
                 [type-or-bytes (or/c exact-nonnegative-integer? ctype?) @#,elem{absent}]
                 [cptr cpointer? @#,elem{absent}]
                 [mode (one-of/c 'nonatomic 'stubborn 'uncollectable
                                 'eternal 'interior 'atomic-interior
                                 'raw)
                       @#,elem{absent}]
                 [fail-mode (one-of/c 'failok) @#,elem{absent}])
         cpointer?]{

Allocates a memory block of a specified size using a specified
allocation. The result is a @scheme[cpointer] to the allocated
memory.  Although not reflected above, the four arguments can appear in
any order since they are all different types of Scheme objects; a size
specification is required at minimum:

@itemize[

 @item{If a C type @scheme[bytes-or-type] is given, its size is used
       to the block allocation size.}

 @item{If an integer @scheme[bytes-or-type] is given, it specifies the
       required size in bytes.}

 @item{If both @scheme[bytes-or-type] and @scheme[type-or-bytes] are given, then the
       allocated size is for a vector of values (the multiplication of the size of
      the C type and the integer).}

 @item{If a @scheme[cptr] pointer is given, its content is copied to
       the new block.}

  @item{A symbol @scheme[mode] argument can be given, which specifies
  what allocation function to use.  It should be one of
  @indexed-scheme['nonatomic] (uses @cpp{scheme_malloc} from PLT
  Scheme's C API), @indexed-scheme['atomic]
  (@cpp{scheme_malloc_atomic}), @indexed-scheme['stubborn]
  (@cpp{scheme_malloc_stubborn}), @indexed-scheme['uncollectable]
  (@cpp{scheme_malloc_uncollectable}), @indexed-scheme['eternal]
  (@cpp{scheme_malloc_eternal}), @indexed-scheme['interior]
  (@cpp{scheme_malloc_allow_interior}),
  @indexed-scheme['atomic-interior]
  (@cpp{scheme_malloc_atomic_allow_interior}), or
  @indexed-scheme['raw] (uses the operating system's @cpp{malloc},
  creating a GC-invisible block).}  @item{If an additional
  @indexed-scheme['failok] flag is given, then
  @cpp{scheme_malloc_fail_ok} is used to wrap the call.}

]

If no mode is specified, then @scheme['nonatomic] allocation is used
when the type is a @scheme[_gcpointer]- or @scheme[_scheme]-based
type, and @scheme['atomic] allocation is used otherwise.}


@defproc[(free [cptr cpointer?]) void]{

Uses the operating system's @cpp{free} function for
@scheme['raw]-allocated pointers, and for pointers that a foreign
library allocated and we should free.  Note that this is useful as
part of a finalizer (see below) procedure hook (e.g., on the Scheme
pointer object, freeing the memory when the pointer object is
collected, but beware of aliasing).}


@defproc[(end-stubborn-change [cptr cpointer?]) void?]{

Uses @cpp{scheme_end_stubborn_change} on the given stubborn-allocated
pointer.}


@defproc[(malloc-immobile-cell [v any/c]) cpointer?]{

Allocates memory large enough to hold one arbitrary (collectable)
Scheme value, but that is not itself collectable or moved by the
memory manager. The cell is initialized with @scheme[v]; use the type
@scheme[_scheme] with @scheme[ptr-ref] and @scheme[ptr-set!] to get
or set the cell's value. The cell must be explicitly freed with
@scheme[free-immobile-cell].}


@defproc[(free-immobile-cell [cptr cpointer?]) void?]{

Frees an immobile cell created by @scheme[malloc-immobile-cell].}


@defproc[(register-finalizer [obj any/c][finalizer (any/c . -> . any)]) void?]{

Registers a finalizer procedure @scheme[finalizer-proc] with the given
@scheme[obj], which can be any Scheme (GC-able) object.  The finalizer
is registered with a will executor; see
@scheme[make-will-executor]. The finalizer is invoked when
@scheme[obj] is about to be collected.  (This is done by a thread that
is in charge of triggering these will executors.)

Finalizers are mostly intended to be used with cpointer objects (for
freeing unused memory that is not under GC control), but it can be
used with any Scheme object---even ones that have nothing to do with
foreign code.  Note, however, that the finalizer is registered for the
@italic{Scheme} object. If you intend to free a pointer object, then
you must be careful to not register finalizers for two cpointers that
point to the same address.  Also, be careful to not make the finalizer
a closure that holds on to the object.

For example, suppose that you're dealing with a foreign function that returns a C
string that you should free.  Here is an attempt at creating a suitable type:

@schemeblock[
(define bytes/free
  (make-ctype _pointer
              #f (code:comment @#,t{a Scheme bytes can be used as a pointer})
              (lambda (x)
                (let ([b (make-byte-string x)])
                  (register-finalizer x free)
                  b))))
]

The above code is wrong: the finalizer is registered for @scheme[x],
which is no longer needed once the byte string is created.  Changing
this to register the finalizer for @scheme[b] correct this problem,
but then @scheme[free] will be invoked on it instead of on @scheme[x].
In an attempt to fix this, we will be careful and print out a message
for debugging:

@schemeblock[
(define bytes/free
  (make-ctype _pointer
              #f (code:comment @#,t{a Scheme bytes can be used as a pointer})
              (lambda (x)
                (let ([b (make-byte-string x)])
                  (register-finalizer b
                    (lambda (ignored)
                      (printf "Releasing ~s\n" b)
                      (free x)))
                  b))))
]

but we never see any printout. The problem is that the finalizer is a
closure that keeps a reference to @scheme[b].  To fix this, you should
use the input argument to the finalizer.  Simply changing
@scheme[ignored] to @scheme[b] will solve this problem.  (Removing the
debugging message also avoids the problem, since the finalization
procedure would then not close over @scheme[b].)}


@defproc[(make-sized-byte-string [cptr cpointer?][length exact-nonnegative-integer?]) 
         bytes?]{

Returns a byte string made of the given pointer and the given length.
No copying is done.  This can be used as an alternative to make
pointer values accessible in Scheme when the size is known.

If @scheme[cptr] is an offset pointer created by @scheme[ptr-add], the
offset is immediately added to the pointer. Thus, this function cannot
be used with @scheme[ptr-add] to create a substring of a Scheme byte
string, because the offset pointer would be to the middle of a
collectable object (which is not allowed).}
