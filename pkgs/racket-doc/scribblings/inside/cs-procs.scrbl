#lang scribble/doc
@(require "utils.rkt")

@title[#:tag "cs-procs"]{Calling Procedures}

As an entry point into Racket, C programs should normally call Racket
procedures by using @cppi{racket_apply}, which calls the procedure in
the initial Racket thread of the main Racket place. Chez Scheme entry
points such as @cppi{Scall0} and @cppi{Scall} directly call a
procedure outside of any Racket thread, which will not work correctly
with Racket facilities such as threads, parameters, continuations, or
continuation marks.

The functions in this section are meant to be used as an entry point
to Racket, but not as a @emph{re-entry} point. When Racket calls a C
function that in turn calls back into Racket, the best approach is to
use the FFI (see @other-doc['(lib
"scribblings/foreign/foreign.scrbl")]) so that the C call recieves a
Racket callback that is wrapped as a plain C callback. That way, the
FFI can handle the details of boundary crossings between Racket and C.

@; ----------------------------------------------------------------------

@function[(ptr racket_apply [ptr proc] [ptr arg_list])]{

Applies the Racket procedure @var{proc} to the list of arguments
@var{arg_list}. The procedure is called in the original Racket thread
of the main Racket place. Applying @var{proc} must not raise an
exception or otherwise escape from the call to @var{proc}.

The result is a list of result values, where a single result from
@var{proc} causes @cpp{racket_apply} to return a list of length one.

Other Racket threads can run during the call to @var{proc}. At the
point that @var{proc} results, all Racket thread scheduling in the
main Racket place is suspended. No garbage collections will occur, so
other Racket places can block waiting for garbage collection.}

@together[(
@function[(ptr Scall0 [ptr proc])]
@function[(ptr Scall1 [ptr proc] [ptr arg1])]
@function[(ptr Scall2 [ptr proc] [ptr arg1] [ptr arg2])]
@function[(ptr Scall3 [ptr proc] [ptr arg1] [ptr arg2] [ptr arg3])]
)]{

Applies the Chez Scheme procedure @var{proc} to zero, one, two, or
three arguments. Beware that not all Racket procedures are Chez Scheme
procedures. (For example, an instance of a structure type that has
@racket[prop:procedure] is not a Chez Scheme procedure.)

The procedure is called outside of any Racket thread, and other Racket
threads are not scheduled during the call to @var{proc}. A garbage
collection may occur.}

@together[(
@function[(void Sinitframe [iptr num_args])]
@function[(void Sput_arg [iptr i] [ptr arg])]
@function[(ptr Scall [ptr proc] [iptr num_args])]
)]{

Similar to @cppi{Scall0}, but these functions are used in sequence to
apply a Chez Scheme procedure to an arbitrary number of arguments.
First, @cppi{Sinitframe} is called with the number of arguments. Then,
each argument is installed with @cppi{Sput_arg}, where the @var{i}
argument indicates the argumenrt position and @var{arg} is the
argument value. Finally, @cppi{Scall} is called with the procedure and
the number of arguments (which must match the number provided to
@cppi{Sinitframe}).}
