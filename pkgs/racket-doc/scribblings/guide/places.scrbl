#lang scribble/doc
@(require scribble/manual "guide-utils.rkt"
          (for-label racket/flonum racket/place))

@title[#:tag "effective-places"]{Parallelism with Places}

The @racketmodname[racket/place] library provides support for
performance improvement through parallelism with the @racket[place]
form. The @racket[place] form creates a @deftech{place}, which is
effectively a new Racket instance that can run in parallel to other
places, including the initial place.  The full power of the Racket
language is available at each place, but places can communicate only
through message passing---using the @racket[place-channel-put] and
@racket[place-channel-get] functions on a limited set of
values---which helps ensure the safety and independence of parallel
computations.

As a starting example, the racket program below uses a @tech{place} to
determine whether any number in the list has a double that is also in
the list:

@codeblock{
#lang racket

(provide main)

(define (any-double? l)
  (for/or ([i (in-list l)])
    (for/or ([i2 (in-list l)])
      (= i2 (* 2 i)))))

(define (main)
  (define p 
    (place ch
      (define l (place-channel-get ch))
      (define l-double? (any-double? l))
      (place-channel-put ch l-double?)))

  (place-channel-put p (list 1 2 4 8))
  
  (place-channel-get p))
}

The identifier @racket[ch] after @racket[place] is bound to a @deftech{place
channel}. The remaining body expressions within the @racket[place] form
are evaluated in a new place, and the body expressions use @racket[ch]
to communicate with the place that spawned the new place.

In the body of the @racket[place] form above, the new place receives a
list of numbers over @racket[ch] and binds the list to @racket[l].  It
then calls @racket[any-double?] on the list and binds the result to
@racket[l-double?]. The final body expression sends the
@racket[l-double?] result back to the original place over @racket[ch].

In DrRacket, after saving and running the above program, evaluate
@racket[(main)] in the interactions window to create the new
place. @margin-note*{When using @tech{places} inside DrRacket, the
module containg place code must be saved to a file before it will
execute.}  Alternatively, save the program as @filepath{double.rkt}
and run from a command line with

@commandline{racket -tm double.rkt}

where the @Flag{t} flag tells @exec{racket} to load the
@tt{double.rkt} module, the @Flag{m} flag calls the exported
@racket[main] function, and @Flag{tm} combines the two flags.

The @racket[place] form has two subtle features. First, it lifts the
@racket[place] body to an anonymous, module-level function.  This
lifting means that any binding referenced by the @racket[place] body
must be available in the module's top level. Second, the
@racket[place] form @racket[dynamic-require]s the enclosing module in
a newly created place. As part of the @racket[dynamic-require], the
current module body is evaluated in the new place.  The consequence of
this second feature is that @racket[place] should not appear immediately
in a module or in a function that is called in a module's top level;
otherwise, invoking the module will invoke the same module in a new
place, and so on, triggering a cascade of place creations that will
soon exhaust memory.

@codeblock{
#lang racket

(provide main)

; Don't do this!
(define p (place ch (place-channel-get ch)))

(define (indirect-place-invocation)
  (define p2 (place ch (place-channel-get ch))))

; Don't do this, either!
(indirect-place-invocation)
}

