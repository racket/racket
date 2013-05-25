#lang scribble/manual

@begin[(require (for-label (only-meta-in 0 typed/racket))
                scribble/eval racket/sandbox
		"../utils.rkt" (only-in "quick.scrbl" typed-mod))]

@title[#:tag "optimization"]{Optimization in Typed Racket}

Typed Racket provides a type-driven optimizer that rewrites well-typed
programs to potentially make them faster. It should in no way make
your programs slower or unsafe.

@margin-note{For general information on Racket performance and
benchmarking, see @secref[#:doc '(lib
"scribblings/guide/guide.scrbl")]{performance}.}


@section{Turning the optimizer off}

Typed Racket's optimizer is turned on by default. If you want to
deactivate it (for debugging, for instance), you must add the
@as-index{@racket[#:no-optimize]} keyword when specifying the language of your
program:

@racketmod[typed/racket #:no-optimize]

@section{Getting the most out of the optimizer}
Typed Racket's optimizer can improve the performance of various common
Racket idioms. However, it does a better job on some idioms than on
others. By writing your programs using the right idioms, you can help
the optimizer help you.

@subsection{Numeric types}
Being type-driven, the optimizer makes most of its decisions based on
the types you assigned to your data. As such, you can improve the
optimizer's usefulness by writing informative types.

For example, the following programs both typecheck:
@racketblock[(define: (f (x : Real))  : Real  (+ x 2.5))
             (f 3.5)]
@racketblock[(define: (f (x : Float)) : Float (+ x 2.5))
             (f 3.5)]

However, the second one uses more informative types: the
@racket[Float] type includes only 64-bit floating-point numbers
whereas the
@racket[Real] type includes both exact and
@tech[#:doc '(lib "scribblings/reference/reference.scrbl") #:key
"inexact numbers"]{inexact}
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{real numbers}
and the @racket[Inexact-Real] type includes both 32- and 64-bit
floating-point numbers.
Typed Racket's optimizer can optimize the latter program to use
@tech[#:doc '(lib "scribblings/reference/reference.scrbl") #:key
"inexact numbers"]{float}
-specific operations whereas it cannot do anything with the
former program.

Thus, to get the most of Typed Racket's optimizer, you should use the
@racket[Float] type when possible. For similar reasons, you should use
floating-point literals instead of exact literals when doing
floating-point computations.

When mixing floating-point numbers and exact reals in arithmetic
operations, the result is not necessarily a @racket[Float]. For
instance, the result of @racket[(* 2.0 0)] is @racket[0] which is not
a @racket[Float]. This can result in missed optimizations. To prevent
this, when mixing floating-point numbers and exact reals, coerce exact
reals to floating-point numbers using @racket[exact->inexact]. This is
not necessary when using @racket[+] or @racket[-]. When mixing
floating-point numbers of different precisions, results use the
highest precision possible.

On a similar note, the @racket[Float-Complex] type is preferable to
the @racket[Complex] type for the same reason. Typed Racket can keep
float
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{complex numbers}
unboxed; as such, programs using
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{complex numbers}
can have better performance than equivalent programs that
represent
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{complex numbers}
as two
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{real numbers}.
As with floating-point literals, float
@tech[#:doc '(lib "scribblings/reference/reference.scrbl") #:key
"complex numbers"]{complex}
literals (such as @racket[1.0+1.0i]) should be preferred over exact
@tech[#:doc '(lib "scribblings/reference/reference.scrbl") #:key
"complex numbers"]{complex}
literals (such as @racket[1+1i]). Note that both parts of a literal must be
present and
@tech[#:doc '(lib "scribblings/reference/reference.scrbl") #:key
"inexact numbers"]{inexact}
for the literal to be of type
@racket[Float-Complex]; @racket[0.0+1.0i] is of type
@racket[Float-Complex] but @racket[+1.0i] is not.
To get the most of
Typed Racket's optimizer, you should also favor rectangular
coordinates over polar coordinates.

@subsection{Lists}
Typed Racket handles potentially empty lists and lists that are known
to be non-empty differently: when taking the @racket[car] or the
@racket[cdr] of a list Typed Racket knows is non-empty, it can skip
the check for the empty list that is usually done when calling
@racket[car] and @racket[cdr].

@racketblock[
(define: (sum (l : (Listof Integer))) : Integer
  (if (null? l)
      0
      (+ (car l) (sum (cdr l)))))
]

In this example, Typed Racket knows that if we reach the else branch,
@racket[l] is not empty. The checks associated with @racket[car] and
@racket[cdr] would be redundant and are eliminated.

In addition to explicitly checking for the empty list using
@racket[null?], you can inform Typed Racket that a list is non-empty
by using the known-length list type constructor; if your data is
stored in lists of fixed length, you can use the @racket[List] type
constructors.

For instance, the type of a list of two @racket[Integer]s can be
written either as:
@racketblock[(define-type List-2-Ints (Listof Integer))]
or as the more precise:
@racketblock[(define-type List-2-Ints (List Integer Integer))]

Using the second definition, all @racket[car] and @racket[cdr]-related
checks can be eliminated in this function:
@racketblock[
(define: (sum2 (l : List-2-Ints) : Integer)
  (+ (car l) (car (cdr l))))
]

@subsection{Vectors}

In addition to known-length lists, Typed Racket supports known-length
vectors through the @racket[Vector] type constructor. Known-length
vector access using constant indices can be optimized in a similar
fashion as @racket[car] and @racket[cdr].

@#reader scribble/comment-reader (racketblock
;; #(name r g b)
(define-type Color (Vector String Integer Integer Integer))
(define: x : Color (vector "red" 255 0 0))
(vector-ref x 0) ; good
(define color-name 0)
(vector-ref x color-name) ; good
(vector-ref x (* 0 10)) ; bad
)

In many such cases, however, @seclink[#:doc '(lib
"scribblings/guide/guide.scrbl") "define-struct"]{structs} are
preferable to vectors. Typed Racket can optimize struct access in all
cases.


@subsection[#:tag "optimization-coach"]{Optimization Coaching}

The Optimization Coach package provides optimization coaching support to help
you get the most of the Typed Racket optimizer.

Similar information (albeit without in-depth explanations or advice) is
available from the command line. When compiling a Typed Racket program, setting
the racket @seclink[#:doc '(lib "scribblings/reference/reference.scrbl")
"logging"]{logging} facilities to the @racket['debug] level and listening to
the @racket['TR-optimizer] logger causes Typed Racket to display performance
debugging information. Setting the Racket logging level can be done on the
command line with the @racket[-W] flag:

@commandline["racket -W debug@TR-optimizer my-typed-program.rkt"]
