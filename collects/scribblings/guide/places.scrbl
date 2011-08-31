#lang scribble/doc
@(require scribble/manual "guide-utils.rkt"
          (for-label racket/flonum racket/place))

@title[#:tag "effective-places"]{Parallelism with Places}

The @racketmodname[racket/place] library provides support for
performance improvement through parallelism with the @racket[place]
form.  Two places communicate using @racket[place-channel-put] and
@racket[place-channel-get] functions.  Places contains the full
expressive power of the Racket language.  However, the places design
restricts both the methods of inter-place communication and the type
of values permitted inside communication messages.

The @racket[place] form spawns a new pristine racket execution
context, which the OS can schedule on any available processor. As a
starting example, the racket program below uses a place to determine
whether any number in the list has a double that is also in the list:

@codeblock{
#lang racket

(provide main)

(define (any-double? l)
  (for/or ([i (in-list l)])
    (for/or ([i2 (in-list l)])
      (= i2 (* 2 i)))))

(define (main)
  (define p (place ch
    (define l (place-channel-get ch))
    (define l-double? (any-double? l))
    (place-channel-put ch l-double?)))

  (place-channel-put p (list 1 2 4 8))
  (printf "Has double? ~a\n" (place-channel-get p))
  (place-wait p))
}

The first argument to the place form is an identifier, which the
@racket[place] form binds to an initial place-channel. The remaining
argument expressions form the body of the @racket[place] form. The
body expressions use the initial place-channel to communicate with the
place which spawned the new place.

In the example above, the place form has a body of three expressions.
The first receives a list of numbers over the initial place-channel
(@racket[ch]) and binds the list to @racket[l].  The second body
expression calls any-double? on the list and binds the result to
@racket[l-double?]. The last body expression sends the
@racket[l-double?] result back to the invoking place over the
@racket[ch] place-channel.

The macro that implements the @racket[place] form performs two actions with
subtle consequences.  First, it lifts the place body to an anonymous
module-scope function.  This has the consequence that any function
referred to by the place body must be defined at module-scope. Second,
the place form expands into a @racket[dynamic-place] call, which
@racket[dynamic-require]s the current module in a newly created place.
@margin-note{When using places inside DrRacket, the module containg
place code must be saved to a file before it will execute.}
As part of the @racket[dynamic-require] the current module body is
evaluated in the new place.  The consequence of this second action is
that places forms must not be called at module-scope or indirectly by
functions which are invoked at module scope. Both of these errors are 
demonstrated in the code bellow. Failing to follow this precaution
will result in an infinite spawning of places as each spawned place
evaluates the module body and spawns an additional place.

@codeblock{
#lang racket

(provide main)

; do not do this
(define p (place ch
  (place-channel-get ch)))

(define (indirect-place-invocation)
  (define p2 (place ch
    (place-channel-get ch))))


; do not do this either
(indirect-place-invocation)
}

The example above is executed by running @exec{racket -tm double.rkt}
from the command line.  The @Flag{t} tells racket to load the
@tt{double.rkt} module. The @Flag{m} instructs racket to run the
@racket[main] module.

