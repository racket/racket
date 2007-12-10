#lang scribble/doc
@(require "utils.ss")

@title[#:tag "foreign:pointer-funcs"]{Pointer Functions}

@;{

@defproc[(cpointer? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a C pointer or a value that can
be used as a pointer: @scheme[#f] (used as a @cpp{NULL} pointer), byte
strings (used as memory blocks), some additional internal objects
(@scheme[ffi-obj]s and callbacks, see @secref["c-only"]).  Returns
@scheme[#f] for other values.}

\scmutilsectionO{ptr-ref}{cptr ctype}{\Optional{'abs} offset}{procedure}

\scmutilsection{ptr-set!}{cptr ctype \Optional{\Optional{'abs} offset} value}{procedure}

The @scheme[pre-ref] procedure return the object referenced by
@var{cptr}, using the given @var{ctype}. The @scheme[ptr-set!]\
procedure stores the @var{value} in the memory @var{cptr} points to, using
the given @var{ctype} for the conversion, and returns @|void-const|.

In each case, @var{offset} defaults to $0$ (which is the only value
that should be used with @scheme[ffi-obj] objects, see
section~\ref{foreign:c-only}).  If an @var{offset} index is given, the
value is stored at that location, considering the pointer as a vector
of @var{ctype}s --- so the actual address is the pointer plus the size
of @var{ctype} multiplied by @var{offset}.  In addition, a @var{'abs}
flag can be used to use the @var{offset} as counting bytes rather then
increments of the specified @var{ctype}.

Beware that the @scheme[ptr-ref] and @scheme[ptr-set!]\ procedure do
not keep any meta-information on how pointers are used.  It is the
programmer's responsibility to use this facility only when
appropriate.  For example, on a little-endian machine:
%
@schemeblock[
> (define block (malloc _int 5))
> (ptr-set! block _int 0 196353)
> (map (lambda (i) (ptr-ref block _byte i)) '(0 1 2 3))
(1 255 2 0)
]
%
In addition, @scheme[ptr-ref] and @scheme[ptr-set!]\ cannot detect
when offsets are beyond an object's memory bounds; out-of-bounds
access can easily lead to a segmentation fault or memory corruption.

@defproc[(ptr-equal? [cptr$_1$ any/c][cptr$_2$ any/c]) any]

Compares the values of the two pointers.  (Note that two different Scheme
pointer objects can contain the same pointer.)

@defproc[(ptr-add [cptr any/c][offset-k nonnegative-exact-integer?][ctype any/c #f]) any]

Returns a cpointer that is like @var{cptr} offset by @var{offset-k}
instances of @var{ctype}. If @var{ctype} is not provided, @var{cptr}
is offset by @var{offset-k} bytes.

The resulting cpointer keeps the base pointer and offset separate. The two
pieces are combined at the last minute before any operation on the pointer,
such as supplying the pointer to a foreign function. In particular, the pointer
and offset are not combined until after all allocation leading up to a
foreign-function call; if the called function does not itself call anything
that can trigger a garbage collection, it can safey use pointers that are
offset into the middle of a GCable object.

@defproc[(offset-ptr? [cptr any/c]) any]

A predicate for cpointers that have an offset, such as pointers that were
created using @scheme[ptr-add].  Returns @scheme[#t] even if such an offset
happens to be 0.  Returns @scheme[#f] for other cpointers and non-cpointers.

@defproc[(ptr-offset [cptr any/c]) any]

Returns the offset of a pointer that has an offset.  (The resulting offset is
always in bytes.)

@defproc[(set-ptr-offset! [cptr any/c][offset-k nonnegative-exact-integer?][ctype any/c #f]) any]

Sets the offset component of an offset pointer.  The arguments are used in the
same way as @scheme[ptr-add].  Raises an error if it is given a pointer that
has no offset.

@defproc[(ptr-add! [cptr any/c][offset-k nonnegative-exact-integer?][ctype any/c #f]) any]

Like @scheme[ptr-add], but destructively modifies the offset contained in a
pointer.  (This can also be done using @scheme[ptr-offset] and
@scheme[set-ptr-offset!].)

@defproc[(cpointer-tag [cptr any/c]) any]

Returns the Scheme object that is the tag of the given @var{cptr} pointer.

@defproc[(set-cpointer-tag! [cptr any/c][tag any/c]) any]

Sets the tag of the given @var{cptr}. The @var{tag} argument can be
any arbitrary value; other pointer operations ignore it.  When a
cpointer value is printed, its tag is shown if it is a symbol, a byte
string, a string. In addition, if the tag is a pair holding one of
these in its @scheme[car], the @scheme[car] is shown (so that the tag
can contain other information).

\scmutilsection{memmove}{cptr \Optional{offset-k}
                         src-cptr \Optional{src-offset-k}
                         count-k \Optional{ctype}}

Copies to @var{cptr} from @var{src-cptr}. The destination pointer can be
offset by an optional @var{offset-k}, which is in bytes if @var{ctype}
is not supplied, or in @var{ctype} instances when supplied.
The source pointer can be similarly offset by @var{src-offset-k}.
The number of bytes copied from source to destination is determined by @var{count-k},
which is also in bytes if @var{ctype} is not supplied, or in @var{ctype}
instances when supplied.

\scmutilsection{memcpy}{cptr \Optional{offset-k}
                        src-cptr \Optional{src-offset-k}
                        count-k \Optional{count-ctype}}

Like @scheme[memmove], but the result is
undefined if the destination and source overlap.

\scmutilsection{memset}{cptr \Optional{offset-k}
                        byte
                        count-k \Optional{count-ctype}}

Similar to @scheme[memmove], but the destination is uniformly filled with
@var{byte} (i.e., an exact integer between 0 and 255 includive).

%------------------------------------------------------------

@section{Memory Management}

For general information on C-level memory management with MzScheme, see
{\InsideMzSchemeManual}.

\scmutilsection{malloc}{bytes-or-type
                        \Optional{type-or-bytes}
                        \Optional{cptr}
                        \Optional{mode} \Optional{'fail-ok}}{procedure}

Allocates a memory block of a specified size using a specified allocation. The result is a
@scheme[cpointer] to the allocated memory. The four arguments can appear in
any order since they are all different types of Scheme objects; a
size specification is required at minimum:
@itemize{
@item{If a C type @var{bytes-or-type} is given, its size is used to the block
  allocation size.}
@item{If an integer @var{bytes-or-type} is given, it specifies the required
  size in bytes.}
@item{If both @var{bytes-or-type} and @var{type-or-bytes} are given, then the
  allocated size is for a vector of values (the multiplication of the size of
  the C type and the integer).}
@item{If a @var{cptr} pointer is given, its contents is copied to the new
  block, it is expected to be able to do so.}
@item{A symbol @var{mode} argument can be given, which specifies what
  allocation function to use.  It should be one of @indexed-scheme['nonatomic] (uses
  @cpp{scheme_malloc} from MzScheme's C API), @indexed-scheme['atomic] (@cpp{scheme_malloc_atomic}),
  @indexed-scheme['stubborn] (@cpp{scheme_malloc_stubborn}), @indexed-scheme['uncollectable]
  (@cpp{scheme_malloc_uncollectable}), @indexed-scheme['eternal] ({\tt
    scheme_malloc_eternal}), @indexed-scheme['interior]
 (@cpp{scheme_malloc_allow_interior}), @indexed-scheme['atomic-interior]
 (@cpp{scheme_malloc_atomic_allow_interior}), or @indexed-scheme['raw] (uses the
  operating system's @cpp{malloc}, creating a GC-invisible block).}
@item{If an additional @indexed-scheme['failok] flag is given, then {\tt
    scheme_malloc_fail_ok} is used to wrap the call.}
}

If no mode is specified, then @scheme['nonatomic] allocation is used
when the type is any pointer-based type, and @scheme['atomic]
allocation is used otherwise.

@defproc[(free [cpointer any/c]) any]

Uses the operating system's @cpp{free} function for
@scheme['raw]-allocated pointers, and for pointers that a foreign
library allocated and we should free.  Note that this is useful as
part of a finalizer (see below) procedure hook (e.g., on the Scheme
pointer object, freeing the memory when the pointer object is
collected, but beware of aliasing).

@defproc[(end-stubborn-change [cpointer any/c]) any]

Uses @cpp{scheme_end_stubborn_change} on the given stubborn-allocated
pointer (see {\InsideMzSchemeManual}).

@defproc[(malloc-immobile-cell [v any/c]) any]

Allocates memory large enough to hold one arbitrary (collectable)
Scheme value, but that is not itself collectable or moved by the
memory manager. The cell is initialized with @var{v}; use the type
@scheme[_scheme] with @scheme[ptr-ref] and @scheme[ptr-set!] to get
or set the cell's value. The cell must be explicitly freed with
@scheme[free-immobile-cell].

@defproc[(free-immobile-cell [cpointer any/c]) any]

Frees an immobile cell created by @scheme[malloc-immobile-cell].

%% *** Documentation for the disabled C code.  A Scheme impl. is used now.
%% @defproc[(register-finalizer [cptr any/c][finalizer any/c]['pointer any/c #f]) any]
%% 
%% Registers a finalizer procedure @var{finalizer-proc} with the given @var{cptr}
%% object.  The finalizer is called by the primitive GC finalizer mechanism, make
%% sure no references to the object are recreated.  Using @scheme[#f] for
%% @var{finalizer-proc} means erase the existing finalizer, if any.  The finalizer
%% is registered for the Scheme pointer object --- be careful with aliasing.
%% 
%% If an optional @var{'pointer} symbol argument is used, the finalizer is
%% registered with the actual pointer rather than the Scheme object.  The
%% procedure gets a new C pointer object that points to the collected pointer.
%% This should be used only with pointers that the GC can access.

@defproc[(register-finalizer [obj any/c][finalizer any/c]) any]

Registers a finalizer procedure @var{finalizer-proc} with the given @var{obj}
which can be any Scheme (GC-able) object.  The finalizer is registered with a
will executor (see \MzSecRef{willexecutor}); it is invoked when @var{obj} is
about to be collected.  (This is done by a thread that is in charge of
triggering these will executors.)

This is mostly intended to be used with cpointer objects (for freeing
unused memory that is not under GC control), but it can be used with
any Scheme object --- even ones that have nothing to do with foreign
code.  Note, however, that the finalizer is registered for the
@italic{Scheme} object. If you intend to free a pointer object, then you
must be careful to not register finalizers for two cpointers that
point to the same address.  Also, be careful to not make the finalizer
a closure that holds on to the object.

For example, suppose that you're dealing with a foreign function that returns a C
string that you should free.  Here is an attempt at creating a suitable type:
%
@schemeblock[
  (define _bytes/free
    (make-ctype _pointer
                #f ; a Scheme bytes can be used as a pointer
                (lambda (x)
                  (let ([b (make-byte-string x)])
                    (register-finalizer x free)
                    b))))
]
%
This is wrong: the finalizer is registered for @scheme[x], which is no longer
needed once the byte string is created.  Changing this to register the
finalizer for @scheme[b] correct this problem, but then @scheme[free] will be
invoked on it instead of on @scheme[x].  In an attempt to fix this, we will be
careful and print out a message for debugging:
%
@schemeblock[
  (define _bytes/free
    (make-ctype _pointer
                #f ; a Scheme bytes can be used as a pointer
                (lambda (x)
                  (let ([b (make-byte-string x)])
                    (register-finalizer b
                      (lambda (_)
                        (printf "Releasing ~s\n" b)
                        (free x)))
                    b))))
]
%
but we never see any printout --- the problem is that the finalizer is a
closure that keeps a reference to @scheme[b].  To fix this, you should use the
input argument to the finalizer.  Simply changing the @scheme[_] to @scheme[b]
will solve this problem.  (Removing the debugging message also avoids the problem, 
since the finalization procedure would then not close over @scheme[b].)

@defproc[(make-sized-byte-string [cptr any/c][length any/c]) any]

Returns a byte string made of the given pointer and the given
length.  No copying is done.  This can be used as an alternative to make
pointer values accessible in Scheme when the size is known.

If @var{cptr} is an offset pointer created by @scheme[ptr-add], the
offset is immediately added to the pointer. Thus, this function cannot
be used with @scheme[ptr-add] to create a substring of a Scheme byte
string, because the offset pointer would be to the middle of a
collectable object (which is not allowed).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

}
