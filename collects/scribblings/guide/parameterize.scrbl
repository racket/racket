#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "parameterize"]{Dynamic Binding: @scheme[parameterize]}

The @scheme[parameterize] form supports a kind of dynamic binding that
is useful for adjusting defaults or passing extra arguments through
layers of function calls. The settings that are adjusted by a
@scheme[parameterize] form are called @deftech{parameters}.

@margin-note{The term ``parameter'' is sometimes used to refer to the
             arguments of a function, but ``parameter'' in PLT Scheme
             has the more specific meaning described here.}

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
