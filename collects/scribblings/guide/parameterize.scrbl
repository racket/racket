#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss"
          (for-label (only-in mzscheme fluid-let)))

@title[#:tag "parameterize"]{Dynamic Binding: @scheme[parameterize]}

@scheme[parameterize] is used to have values that are "dynamically scoped".
You get a parameter with @scheme[make-parameter].  The parameter itself
behaves as a function: call it with no inputs and you get its value,
call it with one value and it will set the value.  The settings that are
adjusted by a @scheme[parameterize] form are called @deftech{parameters}.
For example:

@margin-note{The term ``parameter'' is sometimes used to refer to the
             arguments of a function, but ``parameter'' in PLT Scheme
             has the more specific meaning described here.}


@examples[
(define p (make-parameter "blah"))
(p)
(p "meh")
(p)]

Many functions (including many primitive ones) use parameters as a way
to customize their behavior.  For example @scheme[printf] will print stuff
using the port that is the value of the @scheme[current-output-port]
parameter.  Now, say that you have some function that prints
something:

@examples[
(define (foo x) (printf "the value of x is ~s\n"))
]

You usually call this function and see something printed on the screen
-- but in some cases you want to use it to print something to a file
or whatever.  You could do this:

@examples[
(define (bar)
 (let ([old-stdout (current-output-port)])
  (current-output-port my-own-port)
  (foo some-value)
  (current-output-port old-stdout)))
]

One problem with this is that it is tedious to do -- but that's easily
solved with a macro.  (In fact, PLT still has a construct that does
that in some languages: @scheme[fluid-let].)  But there are more problems
here: what happens if the call to @scheme[foo] results in a runtime error?
This might leave the system in a bad state, where all output goes to
your port (and you won't even see a problem, since it won't print
anything).  A solution for that (which @scheme[fluid-let] uses too) is to
protect the saving/restoring of the parameter with @scheme[dynamic-wind],
which makes sure that if there's an error (and more, if you know about
continuations) then the value is still restored.

So the question is what's the point of having parameters instead of
just using globals and @scheme[fluid-let]?  There are two more problems that
you cannot solve with just globals.  One is what happens when you have
multiple threads -- in this case, setting the value temporarily will
affect other threads, which may still want to print to the standard
output.  Parameters solve this by having a specific value per-thread.
What happens is that each thread "inherits" the value from the thread
that created it, and changes in one thread are visible only in that
thread.

The other problem is more subtle.  Say that you have a parameter with
a numeric value, and you want to do the following:

@examples[
(define (foo)
 (parameterize ([p 'any-expression-goes-here])
  (foo)))
]

In Scheme, "tail calls" are important -- they are the basic tool for
creating loops and much more.  @scheme[parameterize] does some magic that
allows it to change the parameter value temporarily but still preserve
these tail calls.  For example, in the above case, you *will* get an
infinite loop, rather than get a stack overflow error -- what happens
is that each of these @scheme[parameterize] expressions can somehow detect
when there's an earlier @scheme[parameterize] that no longer needs to do its
cleanup.

Finally, @scheme[parameterize] actually uses two important parts of PLT to do
its job: it uses thread cells to implement per-thread values, and it
uses continuation marks to be able to preserve tail-calls.  Each of
these features is useful in itself.

@specform[(parameterize ([parameter-expr value-expr] ...)
            body ...+)]

The result of a @scheme[parameterize] form is the result of the last
@scheme[_body] expression. While the @scheme[_body] expressions are
evaluated, the parameter produced by each @scheme[_parameter-expr] is
set to the result of the corresponding @scheme[_value-expr].

Many parameters are built in. For example, the
@scheme[error-print-width] parameter controls how many characters of a
value are printed in an error message (in case the printed form of the
value is very large):

@interaction[
(parameterize ([error-print-width 10]) 
  (car (expt 10 1024)))
(parameterize ([error-print-width 5])
  (car (expt 10 1024)))
]

The @scheme[error-print-width] parameter acts like a kind of default
argument to the function that formats error messages. This
parameter-based argument can be configured far from the actual call to
the error-formatting function, which in this case is called deep
within the implementation of @scheme[car].

The @scheme[parameterize] form adjusts the value of a parameter only
while evaluating its body expressions. After the body produces a
value, the parameter reverts to its previous value. If control escapes
from the body due to an exception, as in the above example, then the
parameter value is restored in that case, too. Finally, parameter
values are thread-specific, so that multiple threads do not interfere
with each others' settings.

Use @scheme[make-parameter] to create a new parameter that works with
@scheme[parameterize]. The argument to @scheme[make-parameter] is the
value of the parameter when it is not otherwise set by
@scheme[parameterize]. To access the current value of the parameter,
call it like a function.

@interaction[
(define favorite-flavor (make-parameter 'chocolate))
(favorite-flavor)
(define (scoop)
  `(scoop of ,(favorite-flavor)))
(define (ice-cream n)
  (list (scoop) (scoop) (scoop)))
(parameterize ([favorite-flavor 'strawberry])
  (ice-cream 3))
(ice-cream 3)
]
