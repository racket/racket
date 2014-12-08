#lang scribble/doc
@(require "utils.rkt")

@title{Procedures}

A @defterm{primitive procedure} is a Racket-callable procedure that is
implemented in C.  Primitive procedures are created in Racket with
the function @cppi{scheme_make_prim_w_arity}, which takes a C function
pointer, the name of the primitive, and information about the number
of Racket arguments that it takes; it returns a Racket procedure
value.

The C function implementing the procedure must take two arguments: an
integer that specifies the number of arguments passed to the
procedure, and an array of @cpp{Scheme_Object*} arguments. The number
of arguments passed to the function will be checked using the arity
information.  (The arity information provided to
@cpp{scheme_make_prim_w_arity} is also used for the Racket
@racket[arity] procedure.) The procedure implementation is not allowed
to mutate the input array of arguments; as an exception, the procedure
can mutate the array if it is the same as the result of
@cpp{scheme_current_argument_stack}. The procedure may mutate the
arguments themselves when appropriate (e.g., a fill in a vector
argument).

The function @cppi{scheme_make_prim_closure_w_arity} is similar to
@cpp{scheme_make_prim_w_arity}, but it takes an additional count and
@cpp{Scheme_Object*} array that is copied into the created procedure;
the procedure is passed back to the C function when the closure is
invoked. In this way, closure-like data from the C world can be
associated with the primitive procedure.

The function @cppi{scheme_make_closed_prim_w_arity} is similar to
@cpp{scheme_make_prim_closure_w_arity}, but it uses an older calling
convention for passing closure data.

To work well with Scheme threads, a C function that performs
substantial or unbounded work should occasionally call
@cpp{SCHEME_USE_FUEL}; see @secref["usefuel"] for details.

@; ----------------------------------------------------------------------

@function[(Scheme_Object* scheme_make_prim_w_arity
           [Scheme_Prim* prim]
           [char* name]
           [int mina]
           [int maxa])]{

Creates a primitive procedure value, given the C function pointer
@var{prim}.  The form of @var{prim} is defined by:

@verbatim[#:indent 2]{
   typedef Scheme_Object *(Scheme_Prim)(int argc, 
                                        Scheme_Object **argv);
}

The value @var{mina} should be the minimum number of arguments that
must be supplied to the procedure. The value @var{maxa} should be the
maximum number of arguments that can be supplied to the procedure, or
-1 if the procedure can take arbitrarily many arguments. The
@var{mina} and @var{maxa} values are used for automatically checking
the argument count before the primitive is invoked, and also for the
Racket @indexed-racket[arity] procedure. The @var{name} argument is
used to report application arity errors at run-time.}

@function[(Scheme_Object* scheme_make_folding_prim
           [Scheme_Prim* prim]
           [char* name]
           [int mina]
           [int maxa]
           [short folding])]{

Like @cpp{scheme_make_prim_w_arity}, but if @var{folding} is non-zero,
the compiler assumes that an application of the procedure to constant
values can be folded to a constant. For example, @racket[+],
@racket[zero?], and @racket[string-length] are folding primitives, but
@racket[display] and @racket[cons] are not.}

@function[(Scheme_Object* scheme_make_prim
           [Scheme_Prim* prim])]{

Same as @cppi{scheme_make_prim_w_arity}, but the arity is (0, -1) and the
name ``UNKNOWN'' is assumed. This function is provided for backward
compatibility only.}

@function[(Scheme_Object* scheme_make_prim_closure_w_arity
           [Scheme_Prim_Closure_Proc* prim]
           [int c]
           [Scheme_Object** vals]
           [char* name]
           [int mina]
           [int maxa])]{

Creates a primitive procedure value that includes the @var{c} values
in @var{vals}; when the C function @var{prim} is invoked, the
generated primitive is passed as the last parameter. The form of
@var{prim} is defined by:

@verbatim[#:indent 2]{
   typedef
   Scheme_Object *(Scheme_Prim_Closure_Proc)(int argc, 
                                             Scheme_Object **argv, 
                                             Scheme_Object *prim);
}

The macro @cppdef{SCHEME_PRIM_CLOSURE_ELS} takes a primitive-closure
object and returns an array with the same length and content as
@var{vals}. (3m: see @secref["im:3m"] for a caution about
@cppi{SCHEME_PRIM_CLOSURE_ELS}.)}

@function[(Scheme_Object* scheme_make_closed_prim_w_arity
           [Scheme_Closed_Prim* prim]
           [void* data]
           [char* name]
           [int mina]
           [int maxa])]{

Creates an old-style primitive procedure value; when the C function
@var{prim} is invoked, @var{data} is passed as the first parameter.
The form of @var{prim} is defined by:

@verbatim[#:indent 2]{
   typedef
   Scheme_Object *(Scheme_Closed_Prim)(void *data, int argc, 
                                       Scheme_Object **argv);
}}

@function[(Scheme_Object* scheme_make_closed_prim
           [Scheme_Closed_Prim* prim]
           [void* data])]{

Creates a closed primitive procedure value without arity information.
This function is provided for backward compatibility only.}

@function[(Scheme_Object** scheme_current_argument_stack)]{

Returns a pointer to an internal stack for argument passing. When the
argument array passed to a procedure corresponds to the current
argument stack address, the procedure is allowed to modify the
array. In particular, it might clear out pointers in the argument
array to allow the arguments to be reclaimed by the memory manager (if
they are not otherwise accessible).}
