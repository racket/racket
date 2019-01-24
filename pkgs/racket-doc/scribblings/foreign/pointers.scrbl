#lang scribble/doc
@(require "utils.rkt"
          (for-label ffi/unsafe/custodian))

@title[#:tag "foreign:pointer-funcs"]{Pointer Functions}

@defproc[(cpointer? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a C pointer or a value that can
be used as a pointer: @racket[#f] (used as a @cpp{NULL} pointer), byte
strings (used as memory blocks), or a structure instance with the
@racket[prop:cpointer] @tech[#:doc reference.scrbl]{structure type
property}.  Returns @racket[#f] for other values.}

@defproc[(ptr-equal? [cptr1 cpointer?] [cptr2 cpointer?]) boolean?]{

Compares the values of the two pointers. Two different Racket
pointer objects can contain the same pointer.

If the values are both pointers that are not represented by
@racket[#f], a byte string, a callback, a pointer based on
@racket[_fpointer], or a structure with the @racket[prop:cpointer]
property, then the @racket[ptr-equal?] comparison is the
same as using @racket[equal?].}


@defproc[(ptr-add [cptr cpointer?] [offset exact-integer?] [type ctype? _byte]) 
         cpointer?]{

Returns a cpointer that is like @racket[cptr] offset by
@racket[offset] instances of @racket[ctype].

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
were created using @racket[ptr-add].  Returns @racket[#t] even if such
an offset happens to be 0.  Returns @racket[#f] for other cpointers
and non-cpointers.}


@defproc[(ptr-offset [cptr cpointer?]) exact-integer?]{

Returns the offset of a pointer that has an offset. The resulting
offset is always in bytes.}


@defproc[(cpointer-gcable? [cptr cpointer?]) boolean?]{

Returns @racket[#t] if @racket[cptr] is treated as a reference to
memory that is managed by the garbage collector, @racket[#f]
otherwise.}

@; ----------------------------------------------------------------------

@section{Pointer Dereferencing}

@defproc[(set-ptr-offset! [cptr cpointer?] [offset exact-integer?] [ctype ctype? _byte]) 
         void?]{

Sets the offset component of an offset pointer.  The arguments are
used in the same way as @racket[ptr-add].  If @racket[cptr] has no
offset, the @racket[exn:fail:contract] exception is raised.}


@defproc[(ptr-add! [cptr cpointer?] [offset exact-integer?] [ctype ctype? _byte]) 
         void?]{

Like @racket[ptr-add], but destructively modifies the offset contained
in a pointer.  The same operation could be performed using
@racket[ptr-offset] and @racket[set-ptr-offset!].}


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

The @racket[ptr-ref] procedure returns the object referenced by
@racket[cptr], using the given @racket[type]. The @racket[ptr-set!]
procedure stores the @racket[val] in the memory @racket[cptr] points
to, using the given @racket[type] for the conversion.

In each case, @racket[offset] defaults to @racket[0] (which is the
only value that should be used with @racket[ffi-obj] objects, see
@secref["foreign:c-only"]).  If an @racket[offset] index is
non-@racket[0], the value is read or stored at that location,
considering the pointer as a vector of @racket[type]s --- so the
actual address is the pointer plus the size of @racket[type]
multiplied by @racket[offset].  In addition, a @racket['abs] flag can
be used to use the @racket[offset] as counting bytes rather then
increments of the specified @racket[type].

Beware that the @racket[ptr-ref] and @racket[ptr-set!] procedure do
not keep any meta-information on how pointers are used.  It is the
programmer's responsibility to use this facility only when
appropriate.  For example, on a little-endian machine:

@racketblock[
> (define block (malloc _int 5))
> (ptr-set! block _int 0 196353)
> (map (lambda (i) (ptr-ref block _byte i)) '(0 1 2 3))
@#,(racketresultfont "(1 255 2 0)")
]

In addition, @racket[ptr-ref] and @racket[ptr-set!] cannot detect when
offsets are beyond an object's memory bounds; out-of-bounds access can
easily lead to a segmentation fault or memory corruption.}


@defproc*[([(memmove [cptr cpointer?]
                     [offset exact-integer? 0]
                     [src-cptr cpointer?]
                     [src-offset exact-integer? 0]
                     [count exact-nonnegative-integer?]
                     [type ctype? _byte])
            void?])]{

Copies to @racket[cptr] from @racket[src-cptr]. The destination
pointer can be offset by an optional @racket[offset], which is in
@racket[type] instances.  The source pointer can be similarly offset
by @racket[src-offset].  The number of bytes copied from source to
destination is determined by @racket[count], which is in @racket[type]
instances when supplied.}

@defproc*[([(memcpy [cptr cpointer?]
                    [offset exact-integer? 0]
                    [src-cptr cpointer?]
                    [src-offset exact-integer? 0]
                    [count exact-nonnegative-integer?]
                    [type ctype? _byte])
            void?])]{

Like @racket[memmove], but the result is undefined if the destination
and source overlap.}

@defproc*[([(memset [cptr cpointer?]
                    [offset exact-integer? 0]
                    [byte byte?]
                    [count exact-nonnegative-integer?]
                    [type ctype? _byte])
            void?])]{

Similar to @racket[memmove], but the destination is uniformly filled
with @racket[byte] (i.e., an exact integer between 0 and 255
inclusive). When a @racket[type] argument is present, the result
is that of a call to memset with no @racket[type] argument and the
@racket[count] multiplied by the size associated with the
@racket[type].}

@defproc[(cpointer-tag [cptr cpointer?]) any]{

Returns the Racket object that is the tag of the given @racket[cptr]
pointer.}


@defproc[(set-cpointer-tag! [cptr cpointer?] [tag any/c]) void?]{

Sets the tag of the given @racket[cptr]. The @racket[tag] argument can
be any arbitrary value; other pointer operations ignore it.  When a
cpointer value is printed, its tag is shown if it is a symbol, a byte
string, a string. In addition, if the tag is a pair holding one of
these in its @racket[car], the @racket[car] is shown (so that the tag
can contain other information).}


@; ------------------------------------------------------------

@section{Memory Management}

For general information on C-level memory management with Racket,
see @|InsideRacket|.

@defproc[(malloc [bytes-or-type (or/c (and/c exact-nonnegative-integer? fixnum?) 
                                      ctype?)]
                 [type-or-bytes (or/c (and/c exact-nonnegative-integer? fixnum?) 
                                      ctype?) 
                                @#,elem{absent}]
                 [cptr cpointer? @#,elem{absent}]
                 [mode (one-of/c 'raw 'atomic 'nonatomic 'tagged
                                 'atomic-interior 'interior
                                 'stubborn 'uncollectable 'eternal)
                       @#,elem{absent}]
                 [fail-mode (one-of/c 'failok) @#,elem{absent}])
         cpointer?]{

Allocates a memory block of a specified size using a specified
allocation. The result is a C pointer to the allocated
memory, or @racket[#f] if the requested size is zero.  Although 
not reflected above, the four arguments can appear in
any order, since they are all different types of Racket objects; a size
specification is required at minimum:

@itemize[

 @item{If a C type @racket[bytes-or-type] is given, its size is used
       to determine the block allocation size.}

 @item{If an integer @racket[bytes-or-type] is given, it specifies the
       required size in bytes.}

 @item{If both @racket[bytes-or-type] and @racket[type-or-bytes] are given,
       then the allocated size is for a vector of values (the multiplication of
       the size of the C type and the integer).}

 @item{If a @racket[cptr] pointer is given, its content is copied to
       the new block.}

  @item{A symbol @racket[mode] argument can be given, which specifies
  what allocation function to use.  It should be one of the following:

   @itemlist[

     @item{@indexed-racket['raw] --- Allocates memory that is outside
       the garbage collector's space and is not traced by the garbage
       collector (i.e., is treated as holding no pointers to
       collectable memory). This memory must be freed with
       @racket[free].}

     @item{@indexed-racket['atomic] --- Allocates memory that can be
       reclaimed by the garbage collector, is not traced by the
       garbage collector, and is initially filled with zeros.

       For the @3m[] and @CGC[] Racket variants, this allocation mode corresponds
       to @cpp{scheme_malloc_atomic} in the C API.}

     @item{@indexed-racket['nonatomic] --- Allocates memory that can
       be reclaimed by the garbage collector, is treated by the
       garbage collector as holding only pointers, and is initially
       filled with zeros.

       For the @3m[] and @CGC[] Racket variants, this allocation mode corresponds
       to @cpp{scheme_malloc} in the C API.

       For the @CS[] Racket variant, this mode is of limited use,
       because a pointer allocated this way cannot be passed to
       foreign functions that expect a pointer to pointers. The result
       can only be used with functions like @racket[ptr-set!] and
       @racket[ptr-ref].}

     @item{@indexed-racket['atomic-interior] --- Like
       @racket['atomic], but the allocated object will not be moved by
       the garbage collector as long as the allocated object is
       sufficiently retained as described below.

       For the @3m[] and @CGC[] Racket variants, ``sufficiently retained''
       means that the garbage collector does not collect the allocated
       object because some pointer (that is visible to the collector)
       refers to the object. Furthermore, that reference can point to
       the interior of the object, insteda of its starting address.
       This allocation mode corresponds to
       @cpp{scheme_malloc_atomic_allow_interior} in the C API.

       For the @CS[] Racket variant, ``sufficiently retained'' means that the
       specific C pointer object returned by @racket[malloc] remains
       accessible. Note that casting the pointer via @racket[cast], for example,
       generates a new pointer object which would not by itself
       prevent the result of @racket[malloc] from moving, even though
       a reference to the same memory could prevent the object from
       being reclaimed.}

     @item{@indexed-racket['nonatomic-interior] --- Like
       @racket['nonatomic], but the allocated object will not be moved
       by the garbage collector as long as the allocated object is
       retained.

       This mode is supported only for the @3m[] and @CGC[] Racket variants, and
       it corresponds to @cpp{scheme_malloc_allow_interior} in the C
       API.}

     @item{@indexed-racket['tagged] --- Allocates memory that must
       start with a @tt{short} value that is registered as a tag with
       the garbage collector.

       This mode is supported only for the @3m[] and @CGC[] Racket variants, and
       it corresponds to @cpp{scheme_malloc_tagged} in the C API.}

     @item{@indexed-racket['stubborn] --- Like @racket['nonatomic],
       but supports a hint to the GC via @racket[end-stubborn-change]
       after all changes to the object have been made.

       This mode is supported only for the @3m[] and @CGC[] Racket variants, and
       it corresponds to @cpp{scheme_malloc_stubborn} in the C API.}

     @item{@indexed-racket['eternal] --- Like @racket['raw], except the
       allocated memory cannot be freed.

       This mode is supported only for the @CGC[] Racket variant, and
       it corresponds to @cpp{scheme_malloc_uncollectable} in the C API.}

     @item{@indexed-racket['uncollectable] --- Allocates memory that is
       never collected, cannot be freed, and potentially contains
       pointers to collectable memory.

       This mode is supported only for the @CGC[] Racket variant, and
       it corresponds to @cpp{scheme_malloc_uncollectable} in the C API.}

   ]}

  @item{If an additional @indexed-racket['failok] flag is given, then
        some effort may be made to detect an allocation failure and
        raise @racket[exn:fail:out-of-memory] instead of crashing.}

]

If no mode is specified, then @racket['nonatomic] allocation is used
when the type is a @racket[_gcpointer]- or @racket[_scheme]-based
type, and @racket['atomic] allocation is used otherwise.

@history[#:changed "6.4.0.10" @elem{Added the @racket['tagged] allocation mode.}]}


@defproc[(free [cptr cpointer?]) void]{

Uses the operating system's @cpp{free} function for
@racket['raw]-allocated pointers, and for pointers that a foreign
library allocated and we should free.  Note that this is useful as
part of a finalizer (see below) procedure hook (e.g., on the Racket
pointer object, freeing the memory when the pointer object is
collected, but beware of aliasing).

Memory allocated with @racket[malloc] and modes other than
@racket['raw] must not be @racket[free]d, since those modes allocate
memory that is managed by the garbage collector.}


@defproc[(end-stubborn-change [cptr cpointer?]) void?]{

Uses @cpp{scheme_end_stubborn_change} on the given stubborn-allocated
pointer.}


@defproc[(malloc-immobile-cell [v any/c]) cpointer?]{

Allocates memory large enough to hold one arbitrary (collectable)
Racket value, but that is not itself collectable or moved by the
memory manager. The cell is initialized with @racket[v]; use the type
@racket[_scheme] with @racket[ptr-ref] and @racket[ptr-set!] to get
or set the cell's value. The cell must be explicitly freed with
@racket[free-immobile-cell].}


@defproc[(free-immobile-cell [cptr cpointer?]) void?]{

Frees an immobile cell created by @racket[malloc-immobile-cell].}


@defproc[(register-finalizer [obj any/c] [finalizer (any/c . -> . any)]) void?]{

Registers a finalizer procedure @racket[finalizer-proc] with the given
@racket[obj], which can be any Racket (GC-able) object. The finalizer
is registered with a ``late'' @tech[#:doc reference.scrbl]{will
executor} that makes wills ready for a value only after all
@tech[#:doc reference.scrbl]{weak box}es referencing the value have
been cleared, which implies that the value is unreachable and no
normal @tech[#:doc reference.scrbl]{will executor} has a will ready
for the value. The finalizer is invoked when the will for @racket[obj]
becomes ready in the ``late'' will executor, which means that the
value is unreachable (even from wills) by safe code.

The finalizer is invoked in a thread that is in charge of triggering
will executors for @racket[register-finalizer]. The given
@racket[finalizer] procedure should generally not rely on the
environment of the triggering thread, and it must not use any
parameters or call any parameter functions, except that relying on a
default logger and/or calling @racket[current-logger] is allowed.

Finalizers are mostly intended to be used with cpointer objects (for
freeing unused memory that is not under GC control), but it can be
used with any Racket object---even ones that have nothing to do with
foreign code.  Note, however, that the finalizer is registered for the
@italic{Racket} object that represents the pointer. If you intend to
free a pointer object, then you must be careful to not register
finalizers for two cpointers that point to the same address.  Also, be
careful to not make the finalizer a closure that holds on to the
object. Finally, beware that the finalizer is not guaranteed to
be run when a place exits; see @racketmodname[ffi/unsafe/alloc]
and @racket[register-finalizer-and-custodian-shutdown] for more
complete solutions.

As an example for @racket[register-finalizer],
suppose that you're dealing with a foreign function that returns a
C string that you should free.  Here is an attempt at creating a suitable type:

@racketblock[
(define @#,racketidfont{_sixteen-bytes/free}
  (make-ctype _pointer
              #f (code:comment @#,t{a Racket bytes can be used as a pointer})
              (lambda (x)
                (let ([b (make-sized-byte-string x 16)])
                  (register-finalizer x free)
                  b))))
]

The above code is wrong: the finalizer is registered for @racket[x],
which is no longer needed after the byte string is created.  Changing
the example to register the finalizer for @racket[b] corrects the problem,
but then @racket[free] is invoked @racket[b] it instead of on @racket[x].
In the process of fixing this problem, we might be careful and log a message
for debugging:

@racketblock[
(define @#,racketidfont{_sixteen-bytes/free}
  (make-ctype _pointer
              #f (code:comment @#,t{a Racket bytes can be used as a pointer})
              (lambda (x)
                (let ([b (make-sized-byte-string x 16)])
                  (register-finalizer b
                    (lambda (ignored)
                      (log-debug (format "Releasing ~s\n" b))
                      (free x)))
                  b))))
]

Now, we never see any logged event. The problem is that the finalizer is a
closure that keeps a reference to @racket[b]. Instead of referencing the
value that is finalized, use the input argument to the finalizer; simply changing
@racket[ignored] to @racket[b] above solves the problem.  (Removing the
debugging message also avoids the problem, since the finalization
procedure would then not close over @racket[b].)}


@deftogether[(
@defproc[(make-late-weak-box [v any/c]) weak-box?]
@defproc[(make-late-weak-hasheq [v any/c]) (and/c hash? hash-eq? hash-weak?)]
)]{

Like @racket[make-weak-box] and @racket[make-weak-hasheq], but with
``late'' weak references that last longer than references in the
result of @racket[make-weak-box] or @racket[make-weak-hasheq].
Specifically, a ``late'' weak reference remains intact if a value is
unreachable but not yet processed by a finalizer installed with
@racket[register-finalizer]. ``Late'' weak references are intended for
use by such finalizers.}


@defproc[(make-sized-byte-string [cptr cpointer?] [length exact-nonnegative-integer?]) 
         bytes?]{

Returns a byte string made of the given pointer and the given length.
No copying is performed. Beware that future implementations of Racket
may not support this function (in case of a byte string representation
that combines a size and byte-string content without an indirection).

Beware also that the representation of a Racket byte string normally
requires a nul terminator at the end of the byte string (after
@racket[length] bytes), but some functions work with a byte-string
representation that has no such terminator---notably
@racket[bytes-copy].

If @racket[cptr] is an offset pointer created by @racket[ptr-add], the
offset is immediately added to the pointer. Thus, this function cannot
be used with @racket[ptr-add] to create a substring of a Racket byte
string, because the offset pointer would be to the middle of a
collectable object (which is not allowed).}


@defproc[(void/reference-sink [v any/c] ...) void?]{

Returns @|void-const|, but unlike calling the @racket[void] function
where the compiler may optimize away the call and replace it with a
@|void-const| result, calling @racket[void/reference-sink] ensures
that the arguments are considered reachable by the garbage collector
until the call returns.

@history[#:added "6.10.1.2"]}

@; ----------------------------------------------------------------------

@section{Pointer Structure Property}

@defthing[prop:cpointer struct-type-property?]{

A @tech[#:doc reference.scrbl]{structure type property} that causes
instances of a structure type to work as C pointer values. The
property value must be either an exact non-negative integer indicating
an immutable field in the structure (which must, in turn, be
initialized to a C pointer value), a procedure that takes the
structure instance and returns a C pointer value, or a C pointer
value.

The @racket[prop:cpointer] property allows a structure instance to be
used transparently as a C pointer value, or it allows a C pointer
value to be transparently wrapped by a structure that may have
additional values or properties.}
