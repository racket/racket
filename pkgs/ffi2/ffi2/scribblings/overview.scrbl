#lang scribble/manual
@(require scribble/example
          (except-in "common.rkt"
                     round)
          (for-label ffi/unsafe/alloc
                     racket/class))

@(define cpp tt)
@(define (tech-place)
   (tech "place" #:doc '(lib "scribblings/reference/reference.scrbl")))

@(define ffi2-eval (make-base-eval))
@examples[#:eval ffi2-eval #:hidden (require ffi2 racket/class)]

@(define-syntax-rule (ffi2-examples c ...)
   (examples #:eval ffi2-eval #:label #f c ...))

@(define samples-page
   @hyperlink["https://www.cairographics.org/samples/"]{samples page})

@title[#:tag "overview" #:style 'toc]{Overview}

The @racketmodname[ffi2] library supports directly calling C functions
from Racket. To set up those calls, the C function must be described on
the Racket side, including the types for it's arguments and results that
imply conversions between Racket and C values. In principle,
descriptions of C functions could be extracted from header files, but
most C bindings for Racket are written independent of header files. This
section steps you through the process with an extended example.

@margin-note*{The running tutorial in this section is based on the original
 @hyperlink["https://prl.khoury.northeastern.edu/blog/2016/06/27/tutorial-using-racket-s-ffi/"]{FFI
  tutorial series} written by Asumu Takikawa.}

@local-table-of-contents[]

@; --------------------------------------------------
@section[#:tag "unsafe"]{Unsafety of Foreign Functions}

Although using the FFI requires writing no new C code, it provides
relatively little insulation against the issues that C programmers
face related to safety and memory management. An FFI programmer must
be particularly aware of memory management issues for data that spans
the Racket--C divide. Although the library name @racketmodname[ffi2]
and its exported bindings do not include the word
``unsafe,'' importing the library should be considered as a
declaration that your code is itself unsafe, therefore can lead to
serious problems in case of bugs. It is the responsibility of each
library that is implemented with @racketmodname[ffi2] to provide a
safe interface to its clients.

The running example in this section is intended to work within DrRacket,
which can display image results. Beware, however, that DrRacket runs
programs in the same Racket process that runs DrRacket itself. A misuse
of unsafe functionality can therefore crash not only the buggy program,
but also DrRacket. Take appropriate care while editing, and consider
falling back to command-line Racket to view error messages that might
appear just before a crash.

@; --------------------------------------------------
@section[#:tag "tutorial1"]{Foreign-Function Basics}

To use the FFI, you must have in mind

@itemlist[

 @item{@italic{A particular shared library with functions you want to
   call}: A shared library corresponds to a file with a suffix such as
  @filepath{.dll}, @filepath{.so}, or @filepath{.dylib} (depending on the
  platform), or it might be a library within a @filepath{.framework}
  directory on Mac OS.}

 @item{@italic{A particular set of names exported by the library}:
  Shared libraries can export names for any kind of C value, but they
  mostly export functions. The exported names generally match the names as
  used from C.}

 @item{@italic{The C-level type of the exported name}: This is typically
  a function type that involves a sequence of argument types and a result
  type.}

]

As a running example, let's try callings functions exported by the Cairo
graphics library. Specifically, we'll start out by imitating the ``multi
segment caps'' C sample code on Cairo's @|samples-page|:

@verbatim[#:indent 2]{
cairo_move_to (cr, 50.0, 75.0);
cairo_line_to (cr, 200.0, 75.0);

cairo_move_to (cr, 50.0, 125.0);
cairo_line_to (cr, 200.0, 125.0);

cairo_move_to (cr, 50.0, 175.0);
cairo_line_to (cr, 200.0, 175.0);

cairo_set_line_width (cr, 30.0);
cairo_set_line_cap (cr, CAIRO_LINE_CAP_ROUND);
cairo_stroke (cr);
}

The @tt{cr} variable here is a Cairo drawing context, which renders
commands to a drawing surface. As it happens, Racket bitmaps are
implemented using Cairo, and we can get a drawing surface that targets a
bitmap using its @racket[get-handle] method. That method returns a
pointer representation compatible with @racketmodname[ffi/unsafe], so we
must convert for @racketmodname[ffi2] it using @racket[cpointer->ptr_t].

@ffi2-examples[
 #:no-prompt
 (require racket/draw)

 (define bt (make-bitmap 256 256))
 (define bt-surface (cpointer->ptr_t (send bt get-handle)))
]

@; --------------------------------------------------

@subsection{Loading C Libraries}

The @racket[ffi2-lib] function gets a handle to a library by searching
the filesystem. That search that is based on the library file's base
name (without a platform-specific suffix for shared libraries) and a
list of versions to try (where @racket[#f] tries omitting the version):

@ffi2-examples[
 #:no-prompt
 (require ffi2)

 (define cairo-lib (ffi2-lib "libcairo" '("2" #f)))
]

Knowing the library's name and/or path is often the trickiest part of
using the FFI. Sometimes, when using a library name without a path
prefix or file suffix, the library file can be located automatically,
especially on Unix.

In the case of Cairo, the problem is simplified by the fact that the
library is included with a Racket distribution for Windows or Mac OS.
Using the base file name @filepath{libcairo} with version @filepath{2}
is likely to find the library as bundled with Racket or as supplied by
the operating system---usually something like
@filepath{/usr/lib/libcairo.2.so} on a Unix installation.

@; --------------------------------------------------

@subsection{Finding C Functions}

Assuming that @racket[cairo-lib] is successful defined for the loaded
Cairo shared library, we can use it extract the addresses of functions
from the library. Let's start with @cpp{cairo_create}, which accepts a
surface (like the one we have from a Racket bitmap) to create a drawing
context.

@verbatim[#:indent 2]{
cairo_t * cairo_create (cairo_surface_t *target);
}

We can get a pointer to this function using @racket[ffi2-lib-ref]:

@ffi2-examples[
 (ffi2-lib-ref cairo-lib 'cairo_create)
]

To call this function, we will need to give that pointer a type, casting
via @racket[ffi2-cast] to convert it to a Racket function. We could give
the function the simplest possible type, which is a function type using
@racket[->] that expects a generic pointer argument and returns a
generic pointer argument:
@;
@margin-note*{The @racket[->] form is used here with Racket's ``infix
 dot'' notation. Surrounding the @racket[->] with space-separated dots
 causes the @racket[->] to be moved to the front of the parenthesized
 form.}

@ffi2-examples[
 #:no-prompt
 (define cairo_create
   (ffi2-cast (ffi2-lib-ref cairo-lib 'cairo_create)
              #:to (ptr_t . -> . ptr_t)))
]

At this point, calling @racket[cairo_create] on @racket[bt-surface]
would work:

@ffi2-examples[
 (cairo_create bt-surface)
]

@; --------------------------------------------------

@subsection{Increasing Safety with Tagged Pointers}

Using a generic pointer type is especially dangerous, because there will
be many different pointer types that are used with Cairo functions. It's
better to give each pointer type a specific name. Named pointer types
not only improve readability, they enable checks that help prevent using
the wrong kind of pointer as a function argument.

@ffi2-examples[
 #:no-prompt
 (define-ffi2-type cairo_t* ptr_t)
 (define-ffi2-type cairo_surface_t* ptr_t)
]

The type @racket[cairo_surface_t*] is defined here as an extension of
@racket[ptr_t], which means that the C representation is a pointer. It
also means that a Racket representation for @racket[cairo_surface_t*]
will be accepted as a Racket representation of @racket[ptr_t]---but not
vice versa. The @racket[define-ffi2-type] form for
@racket[cairo_surface_t*] defines both the type and a predicate
@racket[cairo_surface_t*?] to recognize Racket pointer representations
that are specifically tagged as @racket[cairo_surface_t*] pointers. A
function that expects a @racket[cairo_surface_t*] pointer uses that
predicate to check arguments.

@ffi2-examples[
 #:no-prompt
 (define cairo_create
   (ffi2-cast (ffi2-lib-ref cairo-lib 'cairo_create)
              #:to (cairo_surface_t* . -> . cairo_t*)))
]

@ffi2-examples[
 (eval:error
  (cairo_create bt-surface))
]

In this case, we are sure that @racket[bt-surface] really is a surface
pointer, so we can cast it using @racket[ffi2-cast]:

@ffi2-examples[
 (cairo_surface_t*? bt-surface)
 (ffi2-cast bt-surface #:to cairo_surface_t*)
 (cairo_surface_t*? (ffi2-cast bt-surface #:to cairo_surface_t*))
 (cairo_create (ffi2-cast bt-surface #:to cairo_surface_t*))
]

@; --------------------------------------------------

@subsection{Reducing Boilerplate for Function Definitions}

Instead of finding a function pointer and then separately casting it to
the right function type, combine those two steps using the
@racket[define-ffi2-procedure] form:

@racketblock[
(define-ffi2-procedure cairo_create (cairo_surface_t* . -> . cairo_t*)
  #:lib cairo-lib)
]

In this definition, @racket[cairo_create] is used both as the name to
define and the name to locate in the C library. As we will see in later
examples, a @racket[#:c-id] option can separately specify the name to
find in the C library, if needed.

Since we will define many functions from @racket[cairo-lib], it's even
better to use @racket[define-ffi2-definer] to define a new form that's
like @racket[define-ffi2-procedure], except that it that has the
@racket[#:lib] part built in:

@ffi2-examples[
 #:no-prompt
 (define-ffi2-definer define-cairo
   #:lib cairo-lib)
 (eval:alts #,(hspace 1) (void))
 (define-cairo cairo_create    (cairo_surface_t* . -> . cairo_t*))
 (define-cairo cairo_move_to   (cairo_t* double_t double_t . -> . void_t))
 (define-cairo cairo_line_to   (cairo_t* double_t double_t . -> . void_t))
 (define-cairo cairo_set_line_width (cairo_t* double_t . -> . void_t))
 (define-cairo cairo_stroke    (cairo_t* . -> . void_t))
]

You may notice that the C type names @cpp{double} and @cpp{void} are
represented here by names that end in @litchar{_t}: @racket[double_t]
and @racket[void_t]. The @racketmodname[ffi2] convention is to use a
@litchar{_t} suffix for all type and type-constructor names, even for
traditional C types like @cpp{int}, @cpp{double}, and @cpp{void}.

At this point, we have enough functions to draw a line on the bitmap.

@ffi2-examples[
 #:no-prompt
 (require pict)
 (define (show bt)
   (linewidth 2 (frame (bitmap bt))))
]

@ffi2-examples[
 (define cr (cairo_create (ffi2-cast bt-surface #:to cairo_surface_t*)))
 (cairo_move_to cr 50.0 50.0)
 (cairo_line_to cr 206.0 206.0)
 (cairo_set_line_width cr 5.0)
 (cairo_stroke cr)
 (show bt)
]

@; --------------------------------------------------

@subsection{Defining a Type Conversion}

To match the original Cairo example, we'll need to call
@cpp{cairo_set_line_cap}. The @cpp{cairo_set_line_cap} function takes a
@cpp{cairo_line_cap_t} argument, where @cpp{cairo_line_cap_t} is defined
in C using @cpp{enum}. The most natural translation to Racket is to use
a symbol for each variant, instead of integer constants. We can define a
new type @racket[cairo_line_cap_t] using @racket[define-ffi2-type], and
we can supply @racket[#:predicate], @racket[#:racket->c], and
@racket[#:c->racket] functions to translate between symbols and
integers:

@ffi2-examples[
 #:no-prompt
 (require racket/list)
 (eval:alts #,(hspace 1) (void))
 (define line-cap-symbols '(butt round square))
 (eval:alts #,(hspace 1) (void))
 (define-ffi2-type cairo_line_cap_t int_t
   #:predicate (lambda (v) (memq v line-cap-symbols))
   #:racket->c (lambda (sym) (index-of line-cap-symbols sym))
   #:c->racket (lambda (i) (list-ref line-cap-symbols i)))
 (eval:alts #,(hspace 1) (void))
 (define-cairo cairo_set_line_cap (cairo_t* cairo_line_cap_t . -> . void_t))
 (define-cairo cairo_get_line_cap (cairo_t* . -> . cairo_line_cap_t))
]

@ffi2-examples[
 (eval:error
  (cairo_set_line_cap cr 'buzz))
 (cairo_set_line_cap cr 'round)
 (cairo_get_line_cap cr)
]

the @racket[#:predicate] function checks whether a Racket value is
suitable as an argument to @racket[#:racket->c]. It needs to be
provided here to replace the check that would otherwise be inherited
from @racket[int_t]. Separating the predicate from the conversion
function (instead of building a check into the conversion function)
supports better error checking and enables fine-grained control over
whether checks are improved. Checks are omitted with a module that
declares @racket[(#%declare #:unsafe)], for example.

Mapping between symbols and integers is common enough that
@racketmodname[ffi2] provides @racket[define-ffi-enum], which achieves
the same result for defining @racket[cairo_line_cap_t]:

@ffi2-examples[
 #:no-prompt
 (define-ffi2-enum cairo_line_cap_t int_t
   butt
   round
   square)
]

@; --------------------------------------------------

@subsection{Putting it All Together}

We can now implement the original Cairo example. Here's the complete
code and the result that DrRacket shows:

@ffi2-examples[
 #:no-prompt
 (eval:alts #,(begin @elem{@tt{#lang }@racketmodname[racket]}) (void))
 (eval:alts #,(hspace 1) (void))
 (require ffi2
          racket/draw
          pict)
 (eval:alts #,(hspace 1) (void))
 (define cairo-lib (ffi2-lib "libcairo" '("2" #f)))
 (define-ffi2-definer define-cairo #:lib cairo-lib)
 (eval:alts #,(hspace 1) (void))
 (define-ffi2-type cairo_t* void_t*)
 (define-ffi2-type cairo_surface_t* void_t*)
 (define-ffi2-enum cairo_line_cap_t int_t
   butt round square)
 (eval:alts #,(hspace 1) (void))
 (define-cairo cairo_create    (cairo_surface_t* . -> . cairo_t*))
 (define-cairo cairo_move_to   (cairo_t* double_t double_t . -> . void_t))
 (define-cairo cairo_line_to   (cairo_t* double_t double_t . -> . void_t))
 (define-cairo cairo_set_line_width (cairo_t* double_t . -> . void_t))
 (define-cairo cairo_stroke    (cairo_t* . -> . void_t))
 (define-cairo cairo_set_line_cap   (cairo_t* cairo_line_cap_t . -> . void_t))
 (eval:alts #,(hspace 1) (void))
 (define (make)
   (define bt (make-bitmap 256 256))
   (define bt-surface (cpointer->ptr_t (send bt get-handle)))
   (values bt
           (cairo_create (ffi2-cast bt-surface #:to cairo_surface_t*))))
 (define (show bt)
   (linewidth 2 (frame (bitmap bt))))
 (eval:alts #,(hspace 1) (void))
 (define-values (bt cr) (make))
 (eval:alts #,(hspace 1) (void))
 (cairo_move_to cr 50.0 75.0)
 (cairo_line_to cr 200.0 75.0)
 (eval:alts #,(hspace 1) (void))
 (cairo_move_to cr 50.0 125.0)
 (cairo_line_to cr 200.0 125.0)
 (eval:alts #,(hspace 1) (void))
 (cairo_move_to cr 50.0 175.0)
 (cairo_line_to cr 200.0 175.0)
 (eval:alts #,(hspace 1) (void))
 (cairo_set_line_width cr 30.0)
 (cairo_set_line_cap cr 'round)
 (cairo_stroke cr)
 (eval:alts #,(hspace 1) (void))
 (eval:alts (show bt) (void))
]


@ffi2-examples[
 #:no-prompt
 #:result-only
 #:no-inset
 (show bt)
]

@; --------------------------------------------------
@section[#:tag "tutorial2"]{Composite Foreign Data}

Many C functions work with simple forms of data---such as integers,
floating-point numbers, and opaque pointers---but things get more
interesting with arrays, C structures, and callback functions.

@; --------------------------------------------------

@subsection{Array Arguments}

Let's look at the ``dash'' example from Cairo's @|samples-page|:

@verbatim[#:indent 2]{
double dashes[] = {50.0, 10.0, 10.0, 10.0};
int    ndash  = sizeof(dashes)/sizeof(dashes[0]);
double offset = -50.0;

cairo_set_dash (cr, dashes, ndash, offset);
cairo_set_line_width (cr, 10.0);

cairo_move_to (cr, 128.0, 25.6);
cairo_line_to (cr, 230.4, 230.4);
cairo_rel_line_to (cr, -102.4, 0.0);
cairo_curve_to (cr, 51.2, 230.4, 51.2, 128.0, 128.0, 128.0);

cairo_stroke (cr);
}

Two of the new functions are straightforward to use:

@ffi2-examples[
 #:no-prompt
 (define-cairo cairo_rel_line_to (cairo_t* double_t double_t . -> . void_t))
 (define-cairo cairo_curve_to
   (cairo_t* double_t double_t double_t double_t double_t double_t . -> . void_t))
]

The most interesting function is @cpp{cairo_set_dash}, which takes an
array argument. The C type signature for @cpp{cairo_set_dash} is

@verbatim[#:indent 2]{
void cairo_set_dash (cairo_t *cr,
                     const double *dashes,
                     int num_dashes,
                     double offset);
}

where the @cpp{num_dashes} argument tells @cpp{cairo_set_dash} the
length of the @cpp{dashes} array.

With @racketmodname[ffi2], we set up the array argument by allocating
memory using @racket[ffi2-malloc] and then filling the memory with a
sequence of @racket[ffi2-set!] assignments. The allocated memory is then
passed to @cpp{cairo_set_dash} as a pointer. The following approach
defines @racket[cairo_set_dash_raw] as a direct reference to the C
function that expects a pointer, and then it defines a Racket-friendly
wrapper that converts a list into an allocated array.

@ffi2-examples[
 #:no-prompt
 (code:line
  (code:comment "Low-level binding: takes a pointer and length")
  (define-cairo cairo_set_dash_raw
    (cairo_t* void_t* int_t double_t . -> . void_t)
    #:c-id cairo_set_dash))
 (eval:alts #,(hspace 1) (void))
 (code:line
  (code:comment "Racket-friendly wrapper that takes a list")
  (define (cairo-set-dash ctx dashes offset)
    (define n (length dashes))
    (define arr (ffi2-malloc double_t n))
    (for ([d (in-list dashes)]
          [i (in-naturals)])
      (ffi2-set! arr double_t i d))
    (cairo_set_dash_raw ctx arr n offset)))
]

@ffi2-examples[
 (cairo-set-dash cr (list 50.0 10.0 10.0 10.0) -50.0)
]

This wrapper approach is fine, but the conversion from a Racket list to
a C array is also something that can be handled by a new converting
type. In fact, the @racketmodname[ffi2] library provides a
@racket[list_t] type constructor to implement that conversion, where
@racket[list_t] takes the element type as an argument. At the same time,
we need to pass the length of the list/array to @cpp{cairo_set_dash},
and that's a separate argument. To deal with that kind of interaction
among arguments, when you define a function type with @racket[->], you
can give names to arguments and refer to them in later expressions that
supply automatic arguments.

@ffi2-examples[
 #:no-prompt
 (define-cairo cairo_set_dash
   (cairo_t* [lst : (list_t double_t)] [int_t = (length lst)] double_t
             . -> . void_t))
]

The @racket[lst :] part of this definition gives a name to the value
supplied as the second argument to @racket[cairo_set_dash]. The
@racket[(list_t double_t)] part insists that the argument is a list of
floating-point numbers, and it converts that list to an allocated array.
For the third argument, meanwhile, the @racket[= (length lst)] part
computes the argument automatically (i.e., it's not merely optional, but
never provided by a caller). The end result is that
@racket[cairo_set_dash] behaves the same as the wrapper
@racket[cairo-set-dash] function.

@ffi2-examples[
 #:no-prompt
 (define-values (bt cr) (make))
 (eval:alts #,(hspace 1) (void))
 (define dashes '(50.0 10.0 10.0 10.0))
 (define offset -50.0)
 (eval:alts #,(hspace 1) (void))
 (cairo-set-dash cr dashes offset)
 (cairo_set_line_width cr 10.0)
 (eval:alts #,(hspace 1) (void))
 (cairo_move_to cr 128.0 25.6)
 (cairo_line_to cr 230.4 230.4)
 (cairo_rel_line_to cr -102.4 0.0)
 (cairo_curve_to cr 51.2 230.4 51.2 128.0 128.0 128.0)
 (eval:alts #,(hspace 1) (void))
 (cairo_stroke cr)
]

@ffi2-examples[
 (show bt)
]

@; --------------------------------------------------

@subsection{C Structs}

For a more advanced example, let's measure text to scale it into our bitmap.
The relevant Cairo function is @cpp{cairo_text_extents}:

@verbatim[#:indent 2]{
void cairo_text_extents (cairo_t *cr,
                         const char *utf8,
                         cairo_text_extents_t *extents);
}

where @cpp{cairo_text_extents_t} is a @cpp{struct}:

@verbatim[#:indent 2]{
typedef struct {
    double x_bearing;
    double y_bearing;
    double width;
    double height;
    double x_advance;
    double y_advance;
} cairo_text_extents_t;
}

We can define a @racket[cairo_text_extents_t] type for Racket using
@racket[define-ffi2-type] with the @racket[struct_t] type constructor:

@ffi2-examples[
 #:no-prompt
 (define-ffi2-type cairo_text_extents_t
   (struct_t
    [x_bearing double_t]
    [y_bearing double_t]
    [width     double_t]
    [height    double_t]
    [x_advance double_t]
    [y_advance double_t]))
]

This single declaration automatically creates several bindings:

@itemlist[

 @item{@racket[cairo_text_extents_t]: works both as a type and as a
  constructor of a @racket[cairo_text_extents_t] instance.}

 @item{@racket[cairo_text_extents_t*]: a type for a pointer to a
  @racket[cairo_text_extents_t] instance.}

 @item{@racket[cairo_text_extents_t*?]: a predicate for pointers to
  @racket[cairo_text_extents_t] instances.}

 @item{@racket[cairo_text_extents_t-width],
  @racket[cairo_text_extents_t-x_bearing], etc.: field accessors, which
  take a pointer to a @racket[cairo_text_extents_t] instance and extract
  a corresponding field value, converting it from C to Racket as needed.}

 @item{@racket[set-cairo_text_extents_t-width!], etc.: field mutators.}

]

Using this definition, you can construct instances directly:

@ffi2-examples[
 #:no-prompt
 (define extents (cairo_text_extents_t 0.0 0.0 0.0 0.0 0.0 0.0))
]

Or allocate one with @racket[ffi2-malloc]:

@ffi2-examples[
 #:no-prompt
 (define extents (ffi2-malloc cairo_text_extents_t))
]

We can define a ``raw'' version of @racket[cairo_text_extents] that will
write into a struct pointer that the caller provides:

@ffi2-examples[
 #:no-prompt
 (define-cairo cairo_text_extents_raw
   (cairo_t* string_t cairo_text_extents_t* . -> . void_t)
   #:c-id cairo_text_extents)
]

A @racket[string_t] type for the second argument lets a caller provide a
Racket string that is converted into a UTF-8-encoded, null-terminated
representation of the string for the C side.

Note that the third argument to @racket[cairo_text_extents_raw] is
@racket[cairo_text_extents_t*] (ending with @litchar{*}), not
@racket[cairo_text_extents_t] (no @litchar{*}). The C function accepts a
pointer argument and not an immediate struct argument. On the Racket
size, @racket[cairo_text_extents_t*] and @racket[cairo_text_extents_t]
have the same representation, but the distinction is crucial on the C
side, because functions accept and return struct values differently than
pointers.

To use @racket[cairo_text_extents_raw], a caller must allocate their own
@racket[cairo_text_extents_t] instance:

@ffi2-examples[
 (define extents (ffi2-malloc cairo_text_extents_t))
 (cairo_text_extents_raw cr "hello world" extents)
 (cairo_text_extents_t-width extents)
]

Similar to the way we made @racket[cairo_set_dash] allocate an array, we
can improve @racket[cairo_text_extents] side by having it automatically
allocate a @racket[cairo_text_extents_t] instance. But in addition to
passing that allocated memory on to the C function, we need to return it
to a caller in Racket. The @racket[#:result] option for @racket[->]
provides a result expression to substitute for the C function's result.

@ffi2-examples[
 #:no-prompt
 (define-cairo cairo_text_extents
   (cairo_t* string_t [exts : cairo_text_extents_t*
                       = (ffi2-malloc cairo_text_extents_t)]
             . -> . void_t
             #:result exts))
 ]

@ffi2-examples[
 (cairo_text_extents_t-width (cairo_text_extents cr "hello world"))
]

To round out the example, let's implement a function that draws text
scaled to fit the bitmap width:

@ffi2-examples[
 #:no-prompt
 (define-cairo cairo_show_text (cairo_t* string_t . -> . void_t))
 (define-cairo cairo_scale     (cairo_t* double_t double_t . -> . void_t))
 (eval:alts #,(hspace 1) (void))
 (define (fit-text cr str)
   (define padding 20)
   (cairo_move_to cr (/ padding 2.0) 128.0)
   (define extents (cairo_text_extents cr str))
   (define x-bearing (cairo_text_extents_t-x_bearing extents))
   (define width     (cairo_text_extents_t-width extents))
   (define scale (/ (- 256.0 padding) (+ x-bearing width)))
   (cairo_scale cr scale scale)
   (cairo_show_text cr str))
]

@ffi2-examples[
 (define-values (txt-bt txt-cr) (make))
 (fit-text txt-cr "Saluton, Mondo / Hallo, mundo")
 (show txt-bt)
]

@; --------------------------------------------------

@subsection[#:tag "tutorial:callbacks"]{Callbacks from C to Racket}

To save a Cairo-based drawing to a PNG file, we could use the
@racket[save-file] method of the bitmap that we're writing into. Let's
instead directly use Cairo's @cpp{cairo_surface_write_to_png_stream} to
write bytes to a file. The C signature is

@verbatim[#:indent 2]{
cairo_status_t
cairo_surface_write_to_png_stream (cairo_surface_t *surface,
                                   cairo_write_func_t write_func,
                                   void *closure_data);
}

where @cpp{cairo_write_func_t} is a function type:

@verbatim[#:indent 2]{
cairo_status_t
(*cairo_write_func_t) (void *closure_data,
                       const unsigned char *data,
                       unsigned int length);
}

The @cpp{closure_data} pointer as the third argument to
@cpp{cairo_surface_write_to_png_stream} is passed along as the first
argument to @cpp{write_func}, which is C's manual way of forming
closures. We'll let Racket take care of closures automatically for us,
so we'll just use a @cpp{NULL} pointer as @cpp{closure_data}. The
interesting part is the function pointer passed as @cpp{write_func}.

For the type of the function argument to
@racket[cairo_surface_write_to_png_stream], we can nest a @racket[->]
type for the argument inside a @racket[->] type for the function:

@ffi2-examples[
 #:no-prompt
(define-cairo cairo_surface_write_to_png_stream
  (cairo_surface_t* (ptr_t ptr_t int_t . -> . int_t) ptr_t . -> . int_t))
]

Then, when we pass a Racket function as the second argument to
@racket[cairo_surface_write_to_png_stream], it will be called to
write out bytes of the PNG encoding:

@ffi2-examples[
 #:no-prompt
 (eval:alts
  (define png-out (open-output-file "/tmp/img.png" #:exists 'truncate))
  (define png-out (open-output-bytes)))
 (eval:alts #,(hspace 1) (void))
 (cairo_surface_write_to_png_stream
  (ffi2-cast bt-surface #:to cairo_surface_t*)
  (lambda (ignored data len)
    (define png-data (make-bytes len))
    (ffi2-memcpy (ffi2-cast png-data #:from bytes_ptr_t) data len)
    (write-bytes png-data png-out)
    0)
  (uintptr_t->ptr_t 0))
 (eval:alts #,(hspace 1) (void))
 (close-output-port png-out)
]

This example illustrates a technique for converting a pointer to bytes
into a byte string. The @racket[write-bytes] function wants a byte
string, not a C-level pointer to bytes, so we copy from the
@racket[data] pointer into a freshly allocated byte string. The copy
operation needs two pointers, so we cast a byte string to a pointer
using @racket[bytes_ptr_t]. Note that casting from @racket[bytes_t],
instead of @racket[bytes_ptr_t], would make a copy of the byte string,
which would not work.

One catch here is that a callback from a foreign function is always in
@tech{atomic mode}. In this case, the callback writes to a port
@racket[png-out] that is not used from any other thread, so atomic mode
causes no problems; writing will not get stuck trying to acquire a lock.

Another subtlety is that the callback must remain live as long as it
might be called. The Racket procedure supplied as a callback will not be
garbage collected while it is being called, of course, but the C
representation of the callback is a wrapper that refers to the Racket
procedure, not the other way around. So, the callback could potentially
trigger a garbage collector that reclaims the callback wrapper. In this
case, the callback is kept live by virtue of being an argument to the
foreign function, and the callback is not used after
@racket[cairo_surface_write_to_png_stream] returns. When a callback is
registered for future use, then more care must be taken to retain the
pointer representing a callback.

@; --------------------------------------------------
@section[#:tag "tutorial-memory"]{Memory Management}

A big difference between Racket and C is that memory allocation is
generally automatic in Racket and explicit in C. This difference creates
two challenges for a Racket binding to C functions:

@itemlist[

 @item{When a foreign function allocates an object, often the object needs to
  be freed by a matching call to another foreign function.}

 @item{When a foreign function is given Racket-allocated arguments, and
  when the Racket memory manager runs later, then Racket-allocated objects
  might get freed or moved in memory. In that case, references held by
  foreign code become invalid. This is a problem only when a foreign code
  retains a reference across calls or when it invokes a callback that
  returns to Racket.}

]

@; --------------------------------------------------

@subsection{Reliable Release of Resources}

We are being sloppy with our calls to @racket[cairo_create]. As the
function name suggests, @racket[cairo_create] allocates a new drawing
context, and we are never deallocating it. The Racket pointer object
that refers to a @racket[cairo_t] is reclaimed, but not the memory (and
other resources) of the @racket[cairo_t] itself.

A good first step is to define @racket[cairo_destory] and apply it to
any drawing context that we no longer need, but what if we forget, or
what if an error occurs before we can reach a @racket[cairo_destory]
call? The @racketmodname[ffi2] library supports @defterm{finalization}
on an object to associate a clean-up action with a Racket object when
the object would otherwise be garbage-collected. Explicit deallocation
is generally better that relying on finalization, but finalization can
be appropriate in some cases and a good back-up in many cases.

The @racketmodname[ffi/unsafe/alloc] library further wraps finalization
support to make it easy to pair an allocator with a deallocator. It
supplies @racket[allocator] and @racket[deallocator] constructors that
are meant to be used with @racket[#:wrap] in
@racket[define-ffi2-procedure] and similar forms.

@ffi2-examples[
 #:no-prompt
 (require ffi/unsafe/alloc)
 (eval:alts #,(hspace 1) (void))
 (define-cairo cairo_destroy (cairo_t* . -> . void_t)
   #:wrap (deallocator))
 (define-cairo cairo_create  (cairo_surface_t* . -> . cairo_t*)
  #:wrap (allocator cairo_destroy))
]

We define @racket[cairo_destroy] first so that it can be referenced by
the definition of @racket[cairo_create]. The
@racket[#:wrap (deallocator)] part of the definition of
@racket[cairo_destroy] identifies it as a deallocator, which will
unregister finalization (if any) for its argument. The
@racket[#:wrap (allocator cairo_destroy)] part of the definition of
@racket[cairo_create] identifies it as an allocator whose result can be
finalized by calling the given deallocator.

@; --------------------------------------------------

@subsection{Pointers and GC-Managed Allocation}

Going back to the @seclink["tutorial:callbacks"]{callback example},
suppose we decide to use the @cpp{closure_data} argument, after all, to
pass along a pointer to an integer that is updated on each callback
invocation:

@ffi2-examples[
 #:no-prompt
 (eval:alts
  (define the-counter (ffi2-malloc (ffi2-sizeof int_t)))
  (define the-counter (ffi2-malloc #:gcable-immobile (ffi2-sizeof int_t))))
 (ffi2-set! the-counter int_t 0)
 (eval:alts #,(hspace 1) (void))
 (eval:alts
  (define png-out (open-output-file "/tmp/img.png" #:exists 'truncate))
  (define png-out (open-output-bytes)))
 (eval:alts #,(hspace 1) (void))
 (code:comment "CAUTION: maybe do not run, because this may crash!")
 (cairo_surface_write_to_png_stream
  (ffi2-cast bt-surface #:to cairo_surface_t*)
  (lambda (counter data len)
    (define png-data (make-bytes len))
    (ffi2-memcpy (ffi2-cast png-data #:from bytes_ptr_t) data len)
    (write-bytes png-data png-out)
    (ffi2-set! counter int_t (add1 (ffi2-ref counter int_t)))
    0)
  the-counter)
 (eval:alts #,(hspace 1) (void))
 (close-output-port png-out)
]

@ffi2-examples[
 (ffi2-ref the-counter int_t)
]

This may crash, or the counter may not increment correctly. Adding a
@racket[(collect-garbage)] call just before or after
@racket[(write-bytes png-data png-out)] greatly increases the chance of
a failing counter increment.

The problem is that a garbage collection during the callback is likely
to move the object allocated by @racket[ffi2-malloc] for
@racket[the-counter], but @cpp{cairo_surface_write_to_png_stream} will
continue to provide the address that it was originally given as its
third argument.

One solution is to follow C allocation conventions and use manual memory
management for @racket[the-counter]. Use the @racket[#:manual]
allocation mode for @racket[ffi2-malloc], instead of the default
@racket[#:gcable] mode, and release the counter with @racket[ffi2-free]
when it is no longer needed.

@racketblock[
 (define the-counter (ffi2-malloc #:manual (ffi2-sizeof int_t)))
 ....
 (define end-count (ffi2-ref the-counter int_t))
 (ffi2-free the-counter)
]

To avoid the drawbacks of manual memory management, another strategy is
to use the @racket[#:gcable-immobile] allocation mode, which allocates
an object with automatic memory management, but the memory manager is
not allowed to relocate the object as long as it is live.

@racketblock[
 (define the-counter (ffi2-malloc #:gcable-immobile (ffi2-sizeof int_t)))
]

The constructor created by @racket[define-ffi2-type] with a
@racket[struct_t] type uses @racket[ffi2-malloc], and it similarly
accepts an allocation mode, as does the @racket[list_t] type
constructor. In all cases, the default is @racket[#:gcable] allocation,
because most foreign functions use their arguments and return without
calling back to Racket. Beware of the @racket[ffi2-malloc] used for
conversion by @racket[string_t] and @racket[bytes_t], which is always in
@racket[#:gcable] mode; those convenience types may not be suitable for
some foreign functions.

@; --------------------------------------------------
@section[#:tag "tutorial3"]{Foreign Unions and Pointer Arithmetic}

At the Racket level, when a representation involves a choice between
different shapes, then a predicate associated with each shape easily
distinguishes the cases. At the C level, different shapes can be merged
with a @cpp{union} type to superimpose representations while relying on
some explicit indicator of which choice applies at any given point.

This level of representation detail and punning is closely related to
pointer arithmetic, which also ends up being used in similar places. As
in C, the @racketmodname[ffi2] library can provides a convenient veneer for
certain forms of pointer arithmetic by viewing pointers as references to
arrays.

@; --------------------------------------------------

@subsection{Declaring Union Types}

Let's work with Cairo
@hyperlink["https://www.cairographics.org/manual/cairo-Paths.html"]{path}
objects, which encode vector-graphics drawing inputs. A path in Cairo is
defined as a @cpp{cairo_path_t}:

@verbatim[#:indent 2]{
typedef struct {
    cairo_status_t status;
    cairo_path_data_t *data;
    int num_data;
} cairo_path_t;
}

The @cpp{data} field is a pointer, but not to just one
@cpp{cairo_path_data_t}; it is a pointer to an array of
@cpp{cairo_path_data_t}s, where @cpp{num_data} indicates the length of
that array.

Individual elements of the array are defined by a @cpp{union}:

@verbatim[#:indent 2]{
union _cairo_path_data_t {
    struct {
        cairo_path_data_type_t type;
        int length;
    } header;
    struct {
        double x, y;
    } point;
};
}

That is, each element is ether a header or a point. A header is followed
by @cpp{length}-1 points. The @cpp{cairo_path_data_type_t} within a header
is an integer that indicates whether encodes a move-to, line-to, etc.,
operation.

For the Racket version of these types, it will be helpful to pull out
@racket[path_header_t] and @racket[path_point_t] into their own
definitions before combining them with @racket[union_t].

@ffi2-examples[
 #:no-prompt
 (define-ffi2-enum cairo_path_data_type_t int_t
   move-to
   line-to
   curve-to
   close-path)
 (eval:alts #,(hspace 1) (void))
 (define-ffi2-type path_header_t
   (struct_t
    [type   cairo_path_data_type_t]
    [length int_t]))
 (eval:alts #,(hspace 1) (void))
 (define-ffi2-type path_point_t
   (struct_t
    [x double_t]
    [y double_t]))
 (eval:alts #,(hspace 1) (void))
 (define-ffi2-type cairo_path_data_t
   (union_t
    [header path_header_t]
    [point  path_point_t]))
 (eval:alts #,(hspace 1) (void))
 (define-ffi2-type cairo_status_t int_t)
 (eval:alts #,(hspace 1) (void))
 (define-ffi2-type cairo_path_t
   (struct_t
    [status cairo_status_t]
    [data   cairo_path_data_t*]
    [num_data int_t]))
 (eval:alts #,(hspace 1) (void))
 (define-cairo cairo_path_destroy (cairo_path_t* . -> . void_t)
   #:wrap (deallocator))
 (define-cairo cairo_copy_path (cairo_t* . -> . cairo_path_t*)
   #:wrap (allocator cairo_path_destroy))
 ]

Let's create a fresh context, add path elements in it, and get the
accumulated path before it is used by @racket[cairo_stroke]:

@ffi2-examples[
 #:no-prompt
 (define-values (bt cr) (make))
 (cairo_move_to cr 50.0 50.0)
 (cairo_line_to cr 206.0 206.0)
 (cairo_move_to cr 50.0 206.0)
 (cairo_line_to cr 115.0 115.0)
 (define a-path (cairo_copy_path cr))
 (cairo_stroke cr)
 (cairo_destroy cr)
 ]

The path should be value with status @racket[0] (success), and it should
have 8 components:

@ffi2-examples[
 (cairo_path_t-status a-path)
 (cairo_path_t-num_data a-path)
 (define data (cairo_path_t-data a-path))
 data
]

To access an individual element of the @racket[data] array, the
definition of @racket[cairo_path_data_t] also defined
@racket[cairo_path_data_t*-ref], which takes a pointer to a
@racket[cairo_path_data_t] array and returns a particular element of the
array. Since @racket[cairo_path_data_t] values are represented on the
Racket side by a pointer, we get a @racket[cairo_path_data_t*] back---in
other words, a kind of pointer arithmetic on the array pointer.

@ffi2-examples[
(cairo_path_data_t*-ref data 0)
]

A @racket[cairo_path_data_t] can be either the @racket[header] case or
the @racket[point] case. Defining @racket[cairo_path_data_t] as a @racket[union_t]
type gave us @racket[cairo_path_data_t-header] and @racket[cairo_path_data_t-point]
to recast the Racket representation as either of those. We know that that first
element of @racket[data] must be the @racket[header] case.

@ffi2-examples[
(define head1 (cairo_path_data_t-header (cairo_path_data_t*-ref data 0)))
(path_header_t-type head1)
(path_header_t-length head1)
]

A @racket['move-to] command has a single point, so we know that the
second element of @racket[data] is the @racket[point] case.

@ffi2-examples[
(define pt1 (cairo_path_data_t-point (cairo_path_data_t*-ref data 1)))
(path_point_t-x pt1)
(path_point_t-y pt1)
]

Next is @racket['line-to], and so on:

@ffi2-examples[
 (define head2 (cairo_path_data_t-header (cairo_path_data_t*-ref data 2)))
 (path_header_t-type head2)
 (define pt2 (cairo_path_data_t-point (cairo_path_data_t*-ref data 3)))
 (path_point_t-x pt2)
 (path_point_t-y pt2)
]

Now that we're done with this experiment, let's be good citizens by
cleaning up, although finalization will clean up after us if it must.

@ffi2-examples[
(cairo_path_destroy a-path)
]

Operations like @racket[cairo_path_data_t*-ref] and field accessors like
@racket[cairo_path_data_t-header] (or, more generally, accessors that
access compound types within other compound types) ultimately perform a
kind of pointer arithmetic. In case you ever need to take control of
pointer arithmetic yourself, @racketmodname[ffi2] provides
@racket[ffi2-add].

@; --------------------------------------------------

@subsection{Hiding Pointers through Conversion}

The Cairo path example illustrates how to traverse a complex structure,
but users of a set of Cairo bindings likely will not want to deal with
all of that complexity. Let's define a new type that converts the C
representation on demand by wrapping it as a sequence that's compatible
with @racket[for].

To make a sequence, we need a new Racket structure type that implements
@racket[prop:sequence]. We'll define @racket[auto_cairo_path_t] as a type
that wraps a pointer as a @racket[cairo-path] instance.

@ffi2-examples[
 #:no-prompt
 #:hidden
 ;; top-level needs this first?
 (struct cairo-path (ptr)
   #:property prop:sequence
   (lambda (p) (in-cairo-path p)))
]

@ffi2-examples[
 #:no-prompt
 (define-ffi2-type auto_cairo_path_t cairo_path_t*
   #:predicate (lambda (v) (cairo-path? v))
   #:c->racket (lambda (p) (cairo-path p))
   #:racket->c (lambda (rkt) (cairo-path-ptr rkt)))
 (eval:alts #,(hspace 1) (void))
 (eval:alts
  (struct cairo-path (ptr)
    #:property prop:sequence
    (lambda (p) (in-cairo-path p)))
  (void))
]

The reference to @racket[in-cairo-path] is the doorway to the main
conversion, which iterates through Cairo path pointers:

@ffi2-examples[
 #:no-prompt
 (define (in-cairo-path path)
   (define pp (cairo-path-ptr path))
   (code:comment "Read the path struct fields")
   (define path-struct (cairo-path-ptr path))
   (define array-ptr   (cairo_path_t-data path-struct))
   (define len         (cairo_path_t-num_data path-struct))
   (code:comment "Generate a sequence")
   (make-do-sequence
    (lambda ()
      (values
       (code:comment "pos->element: extract one path command at a given position")
       (lambda (pos)
         (define header-elem (cairo_path_data_t*-ref array-ptr pos))
         (define header (cairo_path_data_t-header header-elem))
         (define type   (path_header_t-type header))
         (define count  (sub1 (path_header_t-length header)))
         (define points
           (for/list ([i (in-range count)])
             (define pt-elem (cairo_path_data_t*-ref array-ptr (+ pos 1 i)))
             (define pt (cairo_path_data_t-point pt-elem))
             (list (path_point_t-x pt)
                   (path_point_t-y pt))))
         (cons type points))
       (code:comment "next-pos: advance past this element's header + data")
       (lambda (pos)
         (define header-elem (cairo_path_data_t*-ref array-ptr pos))
         (define header (cairo_path_data_t-header header-elem))
         (+ pos (path_header_t-length header)))
       (code:comment "initial position")
       0
       (code:comment "continue?")
       (lambda (pos) (< pos len))
       (code:comment "no other guards needed")
       #f
       #f))))
]

Let's redefine @racket[cairo_copy_path] and @racket[cairo_path_destroy]
and try the earlier example again.

@ffi2-examples[
 #:no-prompt
 (define-cairo cairo_path_destroy (auto_cairo_path_t . -> . void_t)
   #:wrap (deallocator))
 (define-cairo cairo_copy_path (cairo_t* . -> . auto_cairo_path_t)
   #:wrap (allocator cairo_path_destroy))
 (eval:alts #,(hspace 1) (void))
 (define-values (bt cr) (make))
 (cairo_move_to cr 50.0 50.0)
 (cairo_line_to cr 206.0 206.0)
 (cairo_move_to cr 50.0 206.0)
 (cairo_line_to cr 115.0 115.0)
 (define auto-path (cairo_copy_path cr))
 (cairo_stroke cr)
 (cairo_destroy cr)
]

@ffi2-examples[
 (for ([elem auto-path])
   (writeln elem))
 (cairo_path_destroy auto-path)
]

@close-eval[ffi2-eval]
