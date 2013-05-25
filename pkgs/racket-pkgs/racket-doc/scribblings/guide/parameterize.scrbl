#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@(define param-eval (make-base-eval))

@title[#:tag "parameterize"]{Dynamic Binding: @racket[parameterize]}

@refalso["parameters"]{@racket[parameterize]}

The @racket[parameterize] form associates a new value with a
@deftech{parameter} during the evaluation of @racket[_body]
expressions:

@specform[(parameterize ([parameter-expr value-expr] ...)
            body ...+)]

@margin-note{The term ``parameter'' is sometimes used to refer to the
             arguments of a function, but ``parameter'' in Racket
             has the more specific meaning described here.}

For example, the @racket[error-print-width] parameter controls how
many characters of a value are printed in an error message:

@interaction[
(parameterize ([error-print-width 5])
  (car (expt 10 1024)))
(parameterize ([error-print-width 10])
  (car (expt 10 1024)))
]

More generally, parameters implement a kind of dynamic binding. The
@racket[make-parameter] function takes any value and returns a new
parameter that is initialized to the given value. Applying the
parameter as a function returns its current value:

@interaction[
#:eval param-eval
(define location (make-parameter "here"))
(location)
]

In a @racket[parameterize] form, each @racket[_parameter-expr] must
produce a parameter. During the evaluation of the @racket[body]s, each
specified parameter is given the result of the corresponding
@racket[_value-expr]. When control leaves the @racket[parameterize]
form---either through a normal return, an exception, or some other
escape---the parameter reverts to its earlier value:

@interaction[
#:eval param-eval
(parameterize ([location "there"])
  (location))
(location)
(parameterize ([location "in a house"])
  (list (location)
        (parameterize ([location "with a mouse"])
          (location))
        (location)))
(parameterize ([location "in a box"])
  (car (location)))
(location)
]

The @racket[parameterize] form is not a binding form like
@racket[let]; each use of @racket[location] above refers directly to
the original definition. A @racket[parameterize] form adjusts the
value of a parameter during the whole time that the
@racket[parameterize] body is evaluated, even for uses of the
parameter that are textually outside of the @racket[parameterize]
body:

@interaction[
#:eval param-eval
(define (would-you-could-you?)
  (and (not (equal? (location) "here"))
       (not (equal? (location) "there"))))

(would-you-could-you?)
(parameterize ([location "on a bus"])
  (would-you-could-you?))
]

If a use of a parameter is textually inside the body of a
@racket[parameterize] but not evaluated before the
@racket[parameterize] form produces a value, then the use does not see
the value installed by the @racket[parameterize] form:

@interaction[
#:eval param-eval
(let ([get (parameterize ([location "with a fox"])
             (lambda () (location)))])
  (get))
]

The current binding of a parameter can be adjusted imperatively by
calling the parameter as a function with a value. If a
@racket[parameterize] has adjusted the value of the parameter, then
directly applying the parameter procedure affects only the value
associated with the active @racket[parameterize]:

@interaction[
#:eval param-eval
(define (try-again! where)
  (location where))

(location)
(parameterize ([location "on a train"])
  (list (location)
        (begin (try-again! "in a boat")
               (location))))
(location)
]

Using @racket[parameterize] is generally preferable to updating a
parameter value imperatively---for much the same reasons that binding
a fresh variable with @racket[let] is preferable to using
@racket[set!]  (see @secref["set!"]).

It may seem that variables and @racket[set!] can solve many of the
same problems that parameters solve. For example, @racket[lokation]
could be defined as a string, and @racket[set!] could be used
to adjust its value:

@interaction[
#:eval param-eval
(define lokation "here")

(define (would-ya-could-ya?)
  (and (not (equal? lokation "here"))
       (not (equal? lokation "there"))))

(set! lokation "on a bus")
(would-ya-could-ya?)
]

Parameters, however, offer several crucial advantages over
@racket[set!]:

@itemlist[

 @item{The @racket[parameterize] form helps automatically reset the
       value of a parameter when control escapes due to an exception.
       Adding exception handlers and other forms to rewind a
       @racket[set!] is relatively tedious.}

 @item{Parameters work nicely with tail calls (see
       @secref["tail-recursion"]). The last @racket[_body] in a
       @racket[parameterize] form is in @tech{tail position} with
       respect to the @racket[parameterize] form.}

 @item{Parameters work properly with threads (see
       @refsecref["threads"]). The @racket[parameterize] form adjusts
       the value of a parameter only for evaluation in the current
       thread, which avoids race conditions with other threads.}

]

@; ----------------------------------------

@close-eval[param-eval]
