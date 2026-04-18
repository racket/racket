#lang scribble/manual
@(require "common.rkt"
          (for-label ffi/unsafe/alloc))

@(define cpp tt)
@(define (tech-place)
   (tech "place" #:doc '(lib "scribblings/reference/reference.scrbl")))

@title[#:tag "overview"]{Overview}

Although using the FFI requires writing no new C code, it provides
relatively little insulation against the issues that C programmers
face related to safety and memory management. An FFI programmer must
be particularly aware of memory management issues for data that spans
the Racket--C divide. Although the library name @racketmodname[ffi2]
and its exported bindings do not include the word
``unsafe,'' importing the library should be considered as a
declaration that your code is itself unsafe, therefore can lead to
serious problems in case of bugs. It is the responsibility of each
user of @racketmodname[ffi2] to provide a safe interface to its
own clients.

@; --------------------------------------------------

@section{Libraries, C Types, and Objects}

To use the FFI, you must have in mind

@itemlist[

 @item{a particular library from which you want to access a function
       or value, }

 @item{a particular symbol exported by the file, and}

 @item{the C-level type (typically a function type) of the exported
       symbol.}

]

The library corresponds to a file with a suffix such as
@filepath{.dll}, @filepath{.so}, or @filepath{.dylib} (depending on
the platform), or it might be a library within a @filepath{.framework}
directory on Mac OS.

Knowing the library's name and/or path is often the trickiest part of
using the FFI.  Sometimes, when using a library name without a path
prefix or file suffix, the library file can be located automatically,
especially on Unix.

The @racket[ffi2-lib] function gets a handle to a library. To extract
exports of the library, it's simplest to use
@racket[define-ffi2-definer]:

@racketmod[
racket/base
(require ffi2)

(define-ffi2-definer define-curses
  #:lib (ffi2-lib "libcurses"))
]

This @racket[define-ffi2-definer] declaration introduces a
@racket[define-curses] form for binding a Racket name to a value
extracted from @filepath{libcurses}---which might be located at
@filepath{/usr/lib/libcurses.so}, depending on the platform.

To use @racket[define-curses], we need the names and C types of
functions from @filepath{libcurses}. We'll start by using the
following functions:

@verbatim[#:indent 2]{
  WINDOW* initscr(void);
  int waddstr(WINDOW *win, char *str);
  int wrefresh(WINDOW *win);
  int endwin(void);
}

We make these functions callable from Racket as follows:

@margin-note{By convention, a @racketidfont{_t} suffix indicates a
representation of a C type, even if the type does not use that suffix
in C headers. Also by convention, a @racketidfont{*} suffix indicates
a pointer type.}

@racketblock[
(define-ffi2-type WINDOW_t* ptr_t)

(define-curses initscr (-> WINDOW_t*))
(define-curses waddstr (WINDOW_t* string_t . -> . int_t))
(define-curses wrefresh (WINDOW_t* . -> . int_t))
(define-curses endwin (-> int_t))
]

The definition of @racket[WINDOW_t*] binds a Racket name that reflects
a C type based on @racket[ptr_t], which creates a type representation
for a pointer type---usually one that is opaque. When a Racket value
is created to represent a @racket[WINDOW_t*], it is tagged with the
name @racket[WINDOW_t*] to distinguish it from other kinds of
pointers.

Each @racket[define-curses] form uses the given identifier as both the
name of the library export and the Racket identifier to
bind.@margin-note*{An optional @racket[#:c-id] clause for
@racket[define-curses] can specify a name for the library export that
is different from the Racket identifier to bind.} The @racket[(->
....)] part of each definition describes the C type of the exported
function, since the library file does not encode that information for
its exports. The types listed before the last subform of @racket[->]
are the argument types, while the last subform of @racket[->] is the
result type. The pre-defined @racket[int_t] type corresponds to the
@tt{int} C type, while @racket[string_t] corresponds to the @tt{char*}
type when it is intended as a string that is passed to a foreign
function.

At this point, @racket[initscr], @racket[waddstr], @racket[wrefresh],
and @racket[endwin] are normal Racket bindings to Racket functions
(that happen to call C functions), and so they can be exported from
the defining module or called directly:

@racketblock[
(define win (initscr))
(void (waddstr win "Hello"))
(void (wrefresh win))
(sleep 1)
(void (endwin))
]

@; --------------------------------------------------

@section{Defining a Type Conversion}

Our initial use of functions like @racket[waddstr] is sloppy, because
we ignore return codes. C functions often return error
codes, and checking them is a pain. A better approach is to build the
check into the @racket[waddstr] binding and raise an exception when
the code is non-zero.

We can use @racket[define-ffi2-type] again to derive a C type from
@racket[int_t], where the new type's conversion from C to Racket
raises an exception if the value is non-zero. More precisely, we
define a type constructor that is parameterized over a name to use
when reporting an error.

@racketblock[
(define-ffi2-type (status_t who) int_t
  #:c->racket (lambda (v)
                (unless (zero? v)
                  (error who "failed: ~a" v))))

(define-curses initscr (-> WINDOW_t*))
(define-curses waddstr (WINDOW_t* string_t . -> . (status_t 'waddstr)))
(define-curses wrefresh (WINDOW_t* . -> . (status_t 'wrefresh)))
(define-curses endwin (-> (status_t 'endwin)))
]

Using @racket[(status_t 'waddstr)] as a result type is the same as
using @racket[int_t] in terms of its C representation, but the
function specified with @racket[#:c->racket] is applied to the Racket
representation of an @racket[int_t] result, and that function can inspect or
convert the value. In this case, @racket[(status_t 'waddstr)] returns
@racket[(void)], since the status result is not needed after checking,
so @racket[waddstr], @racket[wrefresh], and @racket[endwin] will all
either return @racket[(void)] or raise an exception when they are
called.

@; --------------------------------------------------

@section{References as Arguments}

To get mouse events from @filepath{libcurses}, we must explicitly
enable them through the @racket[mousemask] function:

@verbatim[#:indent 2]{
typedef unsigned long mmask_t;
#define BUTTON1_CLICKED 004L

mmask_t mousemask(mmask_t newmask, mmask_t *oldmask);
}

Setting @racket[BUTTON1_CLICKED] in the mask enables button-click
events.  At the same time, @racket[mousemask] returns the current mask
by installing it into the pointer provided as its second
argument.

These kinds of call-with-a-reference interfaces are common in C. On
the Racket side, a procedure that returns two values would be better.
We could create a binding to @racket[mousemask] and then write a
Racket function to wrap it with an improved interface. Since that
pattern is common, @racket[->] helps with locally named arguments,
automatic argument-value expression, and post-processing to provide a
more Racket-like interface.

@racketblock[
(define-ffi2-type mmask_t ulong_t)
(define-ffi2-type mmask_t* (array_t mmask_t *))

(define-curses mousemask
  (mmask_t [old : mmask_t* = (ffi2-malloc mmask_t)]
           . -> . [r : mmask_t]
           #:result (values (ffi2-ref old mmask_t) r)))

(define BUTTON1_CLICKED #o004)

(define-values (old supported) (mousemask BUTTON1_CLICKED))
]

In this definition of @racket[mousemask], the second argument is
allocated automatically with @racket[= (ffi2-malloc mmask_t)]. Also,
the argument is given a name with @racket[old :] so that it can be
referenced after the C procedure returns. The return value is also
given a name with @racket[[r : mmask_t]]. Finally, the
@racket[#:result] option provides an expression that produces the
result for a call to @racket[mousemask], in this case working with the
allocated @racket[old] pointer and the original result @racket[r].

@; --------------------------------------------------

@section{C Structs}

Assuming that mouse events are supported, the @filepath{libcurses}
library reports them via @racket[getmouse], which accepts a pointer to
a @cpp{MEVENT} struct to fill with mouse-event information:

@verbatim[#:indent 2]{
 typedef struct {
    short id;
    int x, y, z;
    mmask_t bstate;
 } MEVENT;

 int getmouse(MEVENT *event);
}

To work with @cpp{MEVENT} values, we use @racket[define-ffi2-type]
with a @racket[struct_t] type:

@racketblock[
(define-ffi2-type MEVENT_t (struct_t
                             [id short_t]
                             [x int_t]
                             [y int_t]
                             [z int_t]
                             [bstate mmask_t]))
]

This definition binds many names in the same way that
@racket[define-struct] binds many names: @racket[MEVENT_t] is both a C
type representing the struct type and a constructor to allocate an
instance of @racket[MEVENT_t], @racket[MEVENT_t*] is a C type for a
pointer to an @racket[MEVENT_t], @racket[MEVENT_t-x] extracts the
@racket[x] field from an @racket[MEVENT_t] instance, and so on.

With this C struct declaration, we can define the function type for
@racket[getmouse]. The simplest approach is to define
@racket[getmouse] to accept an @racket[MEVENT_t*] pointer, so a caller
must explicitly allocate @racket[MEVENT_t] instance before calling
@racket[getmouse]:

@racketblock[
(define-curses getmouse (MEVENT_t* . -> . int_t))

(define m (MEVENT_t 0 0 0 0 0))
(when (zero? (getmouse m))
  (code:comment @#,t{use @racket[m]...})
  ....)
]

For a more Racket-like function, define @racket[getmouse]
to allocate automatically:

@racketblock[
(define-curses getmouse
  ([m : MEVENT_t* = (MEVENT_t 0 0 0 0 0)]
   . -> . (r : int_t)
   #:result (and (zero? r) m)))

(waddstr win (format "click me fast..."))
(wrefresh win)
(sleep 1)

(define m (getmouse))
(when m
  (waddstr win (format "at ~a,~a"
                       (MEVENT_t-x m)
                       (MEVENT_t-y m)))
  (wrefresh win)
  (sleep 1))

(endwin)
]

The difference between @racket[MEVENT_t*] and @racket[MEVENT_t] is
crucial. If the declared argument type were @racket[MEVENT_t] instead
of @racket[MEVENT_t*], then calling @racket[getmouse] would pass a
structure to the C function instead of a pointer---likely triggering a
crash. A pointer to an @racket[MEVENT_t] with uninitialized content
could be created with @racket[(ffi2-malloc MEVENT_t)] instead of
@racket[(MEVENT_t 0 0 0 0 0)], and that would work fine in this case,
while @racket[(ffi2-malloc MEVENT_t*)] would only allocate enough
space to hold a pointer---likely triggering a crash only later,
unfortunately.

@; --------------------------------------------------

@section{Pointers and Manual Allocation}

To get text from the user instead of a mouse click, @filepath{libcurses}
provides @racket[wgetnstr]:

@verbatim[#:indent 2]{
int wgetnstr(WINDOW *win, char *str, int n);
}

While the @cpp{char*} argument to @racket[waddstr] is treated as a
nul-terminated string, the @cpp{char*} argument to @racket[wgetnstr]
is treated as a buffer whose size is indicated by the final @cpp{int}
argument. The type @racket[string_t] does not work for such buffers.

One way to approach this function from Racket is to describe the
arguments in their rawest form, using plain @racket[ptr_t] for the
second argument to @racket[wgetnstr]:

@racketblock[
(define-curses wgetnstr (WINDOW_t* ptr_t int_t
                                   . -> . int_t))
]

To call this raw version of @racket[wgetnstr], allocate memory, zero
it, and pass the size minus one (to leave room a nul
terminator) to @racket[wgetnstr]:

@racketblock[
(define SIZE 256)
(define buffer (ffi2-malloc #:manual SIZE))
(ffi2-memset buffer 0 SIZE)

(void (wgetnstr win buffer (sub1 SIZE)))
]

When @racket[wgetnstr] returns, it has written bytes to
@racket[buffer]. At that point, we can use @racket[cast] to convert the
value from a raw pointer to a string:

@racketblock[
(ffi2-cast buffer #:from ptr_t #:to string_t)
]

Conversion via the @racket[string_t] type causes the data referenced
by the original pointer to be copied (and UTF-8 decoded), so the
memory referenced by @racket[buffer] is no longer needed. Since the
buffer was allocated in @racket[#:manual] mode, use @racket[ffi2-free]
later to release the allocated memory:

@racketblock[
(ffi2-free buffer)
]

@; --------------------------------------------------

@section{Pointers and GC-Managed Allocation}

Instead of allocating @racket[buffer] with @racket[#:manual], we could
have allocated it with the default @racket[#:gcable] mode:

@racketblock[
(define buffer (ffi2-malloc SIZE)) (code:comment @#,t{or @racket[(ffi2-malloc #:gcable SIZE)]})
]

Memory allocated with @racket[#:gcable] is managed by the garbage
collector, so @racket[ffi2-free] is neither necessary nor allowed when
the memory referenced by @racket[buffer] is no longer needed. Instead,
when @racket[buffer] becomes inaccessible, the allocated memory will
be reclaimed automatically.

Allowing the garbage collector (GC) to manage memory is usually
preferable.  It's easy to forget to call @racket[ffi2-free], and exceptions
or thread termination can easily skip a @racket[ffi2-free].

At the same time, using GC-managed memory adds a different burden on
the programmer: data managed by the GC may be moved to a new address
as the GC compacts allocated objects to avoid fragmentation. C
functions, meanwhile, expect to receive pointers to objects that will
stay put.

Fortunately, unless a C function calls back into the Racket runtime
system (perhaps through a function that is provided as an argument),
no garbage collection will happen between the time that a C function
is called and the time that the function returns.

Let's look a few possibilities related to allocation and pointers:

@itemlist[

 @item{Ok:

  @racketblock[
    (define p (ffi2-malloc #:gcable SIZE))
    (wgetnstr win p (sub1 SIZE))
  ]

 Although the data allocated by @racket[ffi2-malloc] can move
 around, @racket[p] will always point to it, and no garbage collection
 will happen between the time that the address is extracted from
 @racket[p] to pass to @racket[wgetnstr] and the time that
 @racket[wgetnstr] returns.}

 @item{Bad:

  @racketblock[
   (define p (ffi2-malloc #:gcable SIZE))
   (define i (ptr_t->uintptr p))
   (wgetnstr win (uintptr->ptr_t i) (sub1 SIZE))
  ]

  The data referenced by @racket[p] can move after the
  address is converted to an integer, in which case @racket[i] cast
  back to a pointer will be the wrong address.

  Obviously, casting a pointer to an integer is generally a bad idea,
  but the cast simulates another possibility, which is passing the
  pointer to a C function that retains the pointer in its own private
  store for later use. Such private storage is invisible to the Racket
  GC, so it has the same effect as casting the pointer to an integer.}

 @item{Ok:

  @racketblock[
   (define p (ffi2-malloc SIZE))
   (define p2 (ffi2-add p 4))
   (wgetnstr win p2 (- SIZE 5))
  ]

 The pointer @racket[p2] retains the original reference and
 only adds the @racket[4] at the last minute before calling
 @racket[wgetnstr] (i.e., after the point that garbage collection is
 allowed).}

 @item{Ok:

  @racketblock[
   (define p (ffi2-malloc #:gcable-immobile SIZE))
   (define i (ptr_t->uintptr p))
   (wgetnstr win (uintptr->ptr_t i) (sub1 SIZE))
   (black-box p)
  ]

  This is ok because @racket[p] itself stays accessible, so that
  the data it references isn't reclaimed. Allocating with
  @racket[#:gcable-immobile] puts data at a particular address and
  keeps it there.  A garbage collection will not change the address in
  @racket[p], and so @racket[i] (cast back to a pointer) will always
  refer to the data.}

]

Keep in mind that C struct constructors like @racket[MEVENT_t] are
effectively the same as @racket[(ffi-malloc ...)]; by default, the
result values can move in memory during a garbage collection. The same
is true of byte strings allocated with @racket[make-bytes], which (as
a convenience) can be used directly as a pointer value via the
@racket[byte_ptr_t] type (unlike character strings, which are always
copied for UTF-8 encoding or decoding). Constructors like
@racket[MEVENT_t] accept an allocation mode in the same way as
@racket[ffi2-malloc].

@; --------------------------------------------------

@section{Reliable Release of Resources}

Using GC-managed memory saves you from manual @racket[ffi2-free]s for
plain memory blocks, but C libraries often allocate resources and
require a matching call to a function that releases the resources. For
example, @filepath{libcurses} supports windows on the screen that are
created with @racket[newwin] and released with @racket[delwin]:

@verbatim[#:indent 2]{
WINDOW *newwin(int lines, int ncols, int y, int x);
int delwin(WINDOW *win);
}

In a sufficiently complex program, ensuring that every @racket[newwin]
is paired with @racket[delwin] can be challenging, especially if the
functions are wrapped by otherwise safe functions that are provided
from a library. A library that is intended to be safe for use in a
sandbox, say, must protect against resource leaks within the Racket
process as a whole when a sandboxed program misbehaves or is
terminated.

The @racketmodname[ffi/unsafe/alloc] library provides functions to
connect resource-allocating functions and resource-releasing
functions. The library then arranges for finalization to release a resource if
it becomes inaccessible (according to the GC) before it is explicitly
released. At the same time, the library handles tricky atomicity
requirements to ensure that the finalization is properly registered
and never run multiple times.

Using @racketmodname[ffi/unsafe/alloc], the @racket[newwin] and
@racket[delwin] functions can be defined with @racket[allocator]
and @racket[deallocator] wrappers, respectively:

@racketblock[
(require ffi/unsafe/alloc)

(define-curses delwin (WINDOW_t* . -> . (status_t 'delwin))
  #:wrap (deallocator))

(define-curses newwin (int_t int_t int_t int_t -> WINDOW_t*)
  #:wrap (allocator delwin))
]

A @racket[deallocator] wrapper makes a function cancel any existing
finalizer for the function's argument.  An @racket[allocator] wrapper
refers to the deallocator, so that the deallocator can be run if
necessary by a finalizer.

If a resource is scarce or visible to end users, then @tech[#:doc
ref-doc]{custodian} management is more appropriate than mere
finalization as implemented by @racket[allocator]. See the
@racketmodname[ffi/unsafe/custodian] library.

@; --------------------------------------------------

@section{Threads and Places}

Although older versions of @filepath{libcurses} are not thread-safe,
Racket coroutine threads do not correspond to OS-level threads, so
using Racket coroutine threads to call @filepath{libcurses} functions
creates no particular problems.

Racket parallel threads and @tech-place[]s, however, correspond to
OS-level threads. Using a foreign library from multiple places works
when the library is thread-safe. Calling a non-thread-safe library
from multiple places requires more care.

The simplest way to use a non-thread-safe library from multiple places
is to specify the @racket[#:in-original] option of @racket[->], which
routes every call to the function through the original Racket OS
thread instead of the calling thread. Most of the functions that we
initially used from @filepath{libcurses} can be made thread-safe
simply:

@racketblock[
(define-curses initscr
  (-> WINDOW_t* #:in-original))
(define-curses wrefresh
  (WINDOW_t* . -> . (status_t 'wrefresh) #:in-original))
(define-curses endwin
  (-> (status_t 'endwin) #:in-original))
]

The @racket[waddstr] function is not quite as straightforward,
depending on how much concurrency is needed. The problem with

@racketblock[
(define-curses waddstr
  (WINDOW_t* string_t . -> . (status_t 'waddstr) #:in-original))
]

is that it will block all threads from garbage collection until
@racket[waddstr] returns. Adding @racket[#:collect-safe] addresses
that problem:

@racketblock[
(define-curses waddstr
  (WINDOW_t* string_t . -> . (status_t 'waddstr)
             #:in-original
             #:collect-safe))
]

Now, however, the string argument to @racket[waddstr] might move
before the @racket[waddstr] call completes. To safely call
@racket[waddstr] while allowing garbage collection, we can define a
@racket[string_t/immobile] type that allocates bytes for the string
argument with @racket[#:gcable-immobile]:

@racketblock[
(define-ffi2-type string_t/immobile ptr_t
  #:predicate string?
  #:racket->c (lambda (s)
                (define bstr (string->bytes/utf-8 s))
                (define len (bytes-length bstr))
                (define p (ffi2-malloc #:gcable-immobile len))
                (ffi2-memcpy p (ffi2-cast bstr #:from bytes_ptr_t) len)
                (ffi2-set! p byte_t len 0)
                p)
  #:c->racket (lambda (p)
                (ffi2-cast p #:to string_t)))

(define-curses waddstr
  (WINDOW_t* string_t/immobile . -> .  (status_t 'waddstr)
             #:in-original
             #:collect-safe))
]
