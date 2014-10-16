#lang scribble/doc
@(require "utils.rkt"
          scribble/racket
          (for-syntax racket/base)
          (for-label ffi/unsafe/define))

@(define-syntax _MEVENT (make-element-id-transformer
                         (lambda (stx) #'@schemeidfont{_MEVENT})))
@(define-syntax _MEVENT-pointer (make-element-id-transformer
                                 (lambda (stx) #'@schemeidfont{_MEVENT-pointer})))
@(define-syntax _WINDOW-pointer (make-element-id-transformer
                                 (lambda (stx) #'@schemeidfont{_WINDOW-pointer})))
@(define-syntax _mmask_t (make-element-id-transformer
                          (lambda (stx) #'@schemeidfont{_mmask_t})))
@(define-syntax _string/immobile (make-element-id-transformer
                                  (lambda (stx) #'@schemeidfont{_string/immobile})))
   

@title[#:tag "intro"]{Overview}

Although using the FFI requires writing no new C code, it provides
very little insulation against the issues that C programmers face
related to safety and memory management. An FFI programmer must be
particularly aware of memory management issues for data that spans the
Racket--C divide. Thus, this manual relies in many ways on the
information in @|InsideRacket|, which defines how Racket
interacts with C APIs in general.

Since using the FFI entails many safety concerns that Racket
programmers can normally ignore, the library name includes
@racketidfont{unsafe}. Importing the library macro should be
considered as a declaration that your code is itself unsafe, therefore
can lead to serious problems in case of bugs: it is your
responsibility to provide a safe interface. If your library provides
an unsafe interface, then it should have @racketidfont{unsafe} in its
name, too.

For more information on the motivation and design of the Racket FFI,
see @cite["Barzilay04"].

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
directory on Mac OS X.

Knowing the library's name and/or path is often the trickiest part of
using the FFI.  Sometimes, when using a library name without a path
prefix or file suffix, the library file can be located automatically,
especially on Unix. See @racket[ffi-lib] for advice.

The @racket[ffi-lib] function gets a handle to a library. To extract
exports of the library, it's simplest to use
@racket[define-ffi-definer] from the @racketmodname[ffi/unsafe/define]
library:

@racketmod[
racket/base
(require ffi/unsafe
         ffi/unsafe/define)

(define-ffi-definer define-curses (ffi-lib "libcurses"))
]

This @racket[define-ffi-definer] declaration introduces a
@racket[define-curses] form for binding a Racket name to a value
extracted from @filepath{libcurses}---which might be located
at @filepath{/usr/lib/libcurses.so}, depending on
the platform.

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

@margin-note{By convention, an underscore prefix
indicates a representation of a C type (such as @racket[_int]) or a
constructor of such representations (such as @racket[_cpointer]).} 

@racketblock[
(define _WINDOW-pointer (_cpointer 'WINDOW))

(define-curses initscr (_fun -> _WINDOW-pointer))
(define-curses waddstr (_fun _WINDOW-pointer _string -> _int))
(define-curses wrefresh (_fun _WINDOW-pointer -> _int))
(define-curses endwin (_fun -> _int))
]

The definition of @racket[_WINDOW-pointer] creates a Racket value that
reflects a C type via @racket[_cpointer], which creates a type
representation for a pointer type---usually one that is opaque. The
@racket['WINDOW] argument could have been any value, but by
convention, we use a symbol matching the C base type.

Each @racket[define-curses] form uses the given identifier as both the
name of the library export and the Racket identifier to
bind.@margin-note*{An optional @racket[#:c-id] clause for
@racket[define-curses] can specify a name for the library export that
is different from the Racket identifier to bind.} The @racket[(_fun
... -> ...)] part of each definition describes the C type of the
exported function, since the library file does not encode that
information for its exports. The types listed to the left of @racket[->] are the
argument types, while the type to the right of @racket[->] is the
result type. The pre-defined @racket[_int] type naturally corresponds
to the @tt{int} C type, while @racket[_string] corresponds to the
@tt{char*} type when it is intended as a string to read.

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

@section{Function-Type Bells and Whistles}

Our initial use of functions like @racket[waddstr] is sloppy, because
we ignore return codes. C functions often return error
codes, and checking them is a pain. A better approach is to build the
check into the @racket[waddstr] binding and raise an exception when
the code is non-zero.

The @racket[_fun] function-type constructor includes many options to
help convert C functions to nicer Racket functions. We can use some of
those features to convert return codes into either @|void-const| or an
exception:

@racketblock[
(define (check v who)
  (unless (zero? v)
    (error who "failed: ~a" v)))

(define-curses initscr (_fun -> _WINDOW-pointer))
(define-curses waddstr (_fun _WINDOW-pointer _string -> (r : _int)
                             -> (check r 'waddstr)))
(define-curses wrefresh (_fun _WINDOW-pointer -> (r : _int)
                              -> (check r 'wrefresh)))
(define-curses endwin (_fun -> (r : _int)
                            -> (check r 'endwin)))                            
]

Using @racket[(r : _int)] as a result type gives the local name
@racket[r] to the C function's result. This name is then used in the
result post-processing expression that is specified after a second
@racket[->] in the @racket[_fun] form.

@; --------------------------------------------------

@section{By-Reference Arguments}

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

Since these kinds of call-by-reference interfaces are common in C,
@racket[_fun] cooperates with a @racket[_ptr] form to automatically
allocate space for a by-reference argument and extract the value put
there by the C function. Give the extracted value name to use in the
post-processing expression. The post-processing expression can combine
the by-reference result with the function's direct result (which, in
this case, reports a subset of the given mask that is actually
supported).

@racketblock[
(define _mmask_t _ulong)
(define-curses mousemask (_fun _mmask_t (o : (_ptr o _mmask_t)) 
                               -> (r : _mmask_t)
                               -> (values o r)))
(define BUTTON1_CLICKED #o004)

(define-values (old supported) (mousemask BUTTON1_CLICKED))
]

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

To work with @cpp{MEVENT} values, we use @racket[define-cstruct]:

@racketblock[
(define-cstruct _MEVENT ([id _short]
                         [x _int]
                         [y _int]
                         [z _int]
                         [bstate _mmask_t]))
]

This definition binds many names in the same way that
@racket[define-struct] binds many names: @racket[_MEVENT] is a C type
representing the struct type, @racket[_MEVENT-pointer] is a C type
representing a pointer to a @racket[_MEVENT], @racket[make-MEVENT]
constructs a @racket[_MEVENT] value, @racket[MEVENT-x] extracts
the @racket[x] fields from an @racket[_MEVENT] value, and so on.

With this C struct declaration, we can define the function type for
@racket[getmouse]. The simplest approach is to define
@racket[getmouse] to accept an @racket[_MEVENT-pointer], and then explicitly
allocate the @racket[_MEVENT] value before calling @racket[getmouse]:

@racketblock[
(define-curses getmouse (_fun _MEVENT-pointer -> _int))

(define m (make-MEVENT 0 0 0 0 0))
(when (zero? (getmouse m))
  (code:comment @#,t{use @racket[m]...})
  ....)
]

For a more Racket-like function, use @racket[(_ptr o _MEVENT)] and a
post-processing expression:

@racketblock[
(define-curses getmouse (_fun (m : (_ptr o _MEVENT))
                              -> (r : _int)
                              -> (and (zero? r) m)))

(waddstr win (format "click me fast..."))
(wrefresh win)
(sleep 1)

(define m (getmouse))
(when m
  (waddstr win (format "at ~a,~a"
                       (MEVENT-x m)
                       (MEVENT-y m)))
  (wrefresh win)
  (sleep 1))

(endwin)               
]

The difference between @racket[_MEVENT-pointer] and @racket[_MEVENT]
is crucial. Using @racket[(_ptr o _MEVENT-pointer)] would allocate
only enough space for a pointer to an @cpp{MEVENT} struct, which is
not enough space for an @cpp{MEVENT} struct.

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
argument. The C type @racket[_string] does not work for such
buffers.

One way to approach this function from Racket is to describe the
arguments in their rawest form, using plain @racket[_pointer] for the
second argument to @racket[wgetnstr]:

@racket[
(define-curses wgetnstr (_fun _WINDOW-pointer _pointer _int
                              -> _int))
]

To call this raw version of @racket[wgetnstr], allocate memory, zero
it, and pass the size minus one (to leave room a nul
terminator) to @racket[wgetnstr]:

@racketblock[
(define SIZE 256)
(define buffer (malloc 'raw SIZE))
(memset buffer 0 SIZE)

(void (wgetnstr win buffer (sub1 SIZE)))
]

When @racket[wgetnstr] returns, it has written bytes to
@racket[buffer]. At that point, we can use @racket[cast] to convert the
value from a raw pointer to a string:

@racketblock[
(cast buffer _pointer _string)
]

Conversion via the @racket[_string] type causes the data referenced by
the original pointer to be copied (and UTF-8 decoded), so the memory
referenced by @racket[buffer] is no longer needed. Memory allocated
with @racket[(malloc 'raw ...)] must be released with @racket[free]:

@racketblock[
(free buffer)
]

@; --------------------------------------------------

@section{Pointers and GC-Managed Allocation}

Instead of allocating @racket[buffer] with @racket[(malloc 'raw ...)],
we could have allocated it with @racket[(malloc 'atomic ...)]:

@racketblock[
(define buffer (malloc 'atomic SIZE))
]

Memory allocated with @racket['atomic] is managed by the garbage
collector, so @racket[free] is neither necessary nor allowed when the
memory referenced by @racket[buffer] is no longer needed. Instead,
when @racket[buffer] becomes inaccessible, the allocated memory will
be reclaimed automatically.

Allowing the garbage collector (GC) to manage memory is usually
preferable.  It's easy to forget to call @racket[free], and exceptions
or thread termination can easily skip a @racket[free].

At the same time, using GC-managed memory adds a different burden on
the programmer: data managed by the GC may be moved to a new address
as the GC compacts allocated objects to avoid fragmentation. C
functions, meanwhile, expect to receive pointers to objects that will
stay put.

Fortunately, unless a C function calls back into the Racket run-time
system (perhaps through a function that is provided as an argument),
no garbage collection will happen between the time that a C function
is called and the time that the function returns.

Let's look a few possibilities related to allocation and pointers:

@itemlist[

 @item{Ok:

  @racketblock[
    (define p (malloc 'atomic SIZE))
    (wgetnstr win p (sub1 SIZE))
  ]

 Although the data allocated by @racket[malloc] can move
 around, @racket[p] will always point to it, and no garbage collection
 will happen between the time that the address is extracted form
 @racket[p] to pass to @racket[wgetnstr] and the time that
 @racket[wgetnstr] returns.}

 @item{Bad: 

  @racketblock[
   (define p (malloc 'atomic SIZE))
   (define i (cast p _pointer _intptr))
   (wgetnstr win (cast i _intptr _pointer) (sub1 SIZE))
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
   (define p (malloc 'atomic SIZE))
   (define p2 (ptr-add p 4))
   (wgetnstr win p2 (- SIZE 5))
  ]  

 The pointer @racket[p2] retains the original reference and
 only adds the @racket[4] at the last minute before calling
 @racket[wgetnstr] (i.e., after the point that garbage collection is
 allowed).}

 @item{Ok:

  @racketblock[
   (define p (malloc 'atomic-interior SIZE))
   (define i (cast p _pointer _intptr))
   (wgetnstr win (cast i _intptr _pointer) (sub1 SIZE))
  ]

  This is ok assuming that @racket[p] itself stays accessible, so that
  the data it references isn't reclaimed. Allocating with
  @racket['atomic-interior] puts data at a particular address and
  keeps it there.  A garbage collection will not change the address in
  @racket[p], and so @racket[i] (cast back to a pointer) will always
  refer to the data.}

]

Keep in mind that C struct constructors like @racket[make-MEVENT] are
effectively the same as @racket[(malloc 'atomic ...)]; the result values
can move in memory during a garbage collection.  The same is true of
byte strings allocated with @racket[make-bytes], which (as a
convenience) can be used directly as a pointer value (unlike character
strings, which are always copied for UTF-8 encoding or decoding).

For more information about memory management and garbage collection,
see @secref[#:doc InsideRacket-doc "im:memoryalloc"] in
@|InsideRacket|.

@; --------------------------------------------------

@section{Reliable Release of Resources}

Using GC-managed memory saves you from manual @racket[free]s for plain
memory blocks, but C libraries often allocate resources and require a
matching call to a function that releases the resources. For example,
@filepath{libcurses} supports windows on the screen that
are created with @racket[newwin] and released with @racket[delwin]:

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
@racket[delwin] functions can be imported with @racket[allocator]
and @racket[deallocator] wrappers, respectively:

@racketblock[
(require ffi/unsafe/alloc)

(define-curses delwin (_fun _WINDOW-pointer -> _int)
  #:wrap (deallocator))

(define-curses newwin (_fun _int _int _int _int 
                            -> _WINDOW-pointer)
  #:wrap (allocator delwin))
]

A @racket[deallocator] wrapper makes a function cancel any existing
finalizer for the function's argument.  An @racket[allocator] wrapper
refers to the deallocator, so that the deallocator can be run if
necessary by a finalizer.

If a resource is scarce or visible to end users, then @tech[#:doc
reference.scrbl]{custodian} management is more appropriate than
mere finalization as implemented by @racket[allocator]. See the
@racketmodname[ffi/unsafe/custodian] library.

@; --------------------------------------------------

@section{Threads and Places}

Although older versions of @filepath{libcurses} are not thread-safe,
Racket threads do not correspond to OS-level threads, so using Racket
threads to call @filepath{libcurses} functions creates no particular
problems.

Racket @tech-place[]s, however, correspond to OS-level threads. Using
a foreign library from multiple places works when the library is
thread-safe. Calling a non-thread-safe library from multiple places
requires more care.

The simplest way to use a non-thread-safe library from multiple places
is to specify the @racket[#:in-original-place? #t] option of
@racket[_fun], which routes every call to the function through the
original Racket place instead of the calling place. Most of the
functions that we initially used from @filepath{libcurses} can be made
thread-safe simply:

@racketblock[
(define-curses initscr
  (_fun #:in-original-place? #t -> _WINDOW-pointer))
(define-curses wrefresh
  (_fun #:in-original-place? #t _WINDOW-pointer -> _int))
(define-curses endwin
  (_fun #:in-original-place? #t -> _int))
]

The @racket[waddstr] function is not quite as straightforward. The
problem with

@racketblock[
(define-curses waddstr
  (_fun #:in-original-place? #t _WINDOW-pointer _string -> _int))
]

is that the string argument to @racket[waddstr] might move in the
calling place before the @racket[waddstr] call completes in the
original place. To safely call @racket[waddstr], we can use a
@racket[_string/immobile] type that allocates bytes for the string
argument with @racket['atomic-interior]:

@racketblock[
(define _string/immobile
  (make-ctype _pointer
              (lambda (s)
                (define bstr (cast s _string _bytes))
                (define len (bytes-length bstr))
                (define p (malloc 'atomic-interior len))
                (memcpy p bstr len)
                p)
              (lambda (p)
                (cast p _pointer _string))))
 
(define-curses waddstr
  (_fun #:in-original-place? #t _WINDOW-pointer _string/immobile -> _int))
]

Beware that passing memory allocated with @racket['interior] (as
opposed to @racket['atomic-interior]) is safe only for functions that
read the memory without writing. A foreign function must not write to
@racket['interior]-allocated memory from a place other than the one
where the memory is allocated, due a place-specific treatment of
writes to implement generational garbage collection.

@; ------------------------------------------------------------

@section{More Examples}

For more examples of common FFI patterns, see the defined interfaces
in the @filepath{ffi/examples} collection. See also @cite["Barzilay04"].


