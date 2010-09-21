#lang scribble/doc
@(require "mz.ss"
          (for-label racket/future))

@(define future-eval (make-base-eval))
@(interaction-eval #:eval future-eval (require racket/future))

@title[#:tag "futures"]{Futures for Parallelism}

@guideintro["effective-futures"]{futures}

@note-lib[racket/future]

@margin-note{Currently, parallel support for @racket[future] is
enabled by default for Windows, Linux x86/x86_64, and Mac OS X
x86/x86_64. To enable support for other platforms, use
@DFlag{enable-futures} with @exec{configure} when building
Racket.}

The @racket[future] and @racket[touch] functions from
@racketmodname[racket/future] provide access to parallelism as
supported by the hardware and operation system.
In contrast to @racket[thread], which provides concurrency for
arbitrary computations without parallelism, @racket[future] provides
parallelism for limited computations. A future executes its work in
parallel (assuming that support for parallelism is available) until it
detects an attempt to perform an operation that is too complex for the
system to run safely in parallel. Similarly, work in a future is
suspended if it depends in some way on the current continuation, such
as raising an exception. A suspended computation for a future is
resumed when @racket[touch] is applied to the future.

``Safe'' parallel execution of a future means that all operations
provided by the system must be able to enforce contracts and produce
results as documented. ``Safe'' does not preclude concurrent access to
mutable data that is visible in the program.  For example, a
computation in a future might use @racket[set!] to modify a shared
variable, in which case concurrent assignment to the variable can be
visible in other futures and threads. Furthermore, guarantees about
the visibility of effects and ordering are determined by the operating
system and hardware---which rarely support, for example, the guarantee
of sequential consistency that is provided for @racket[thread]-based
concurrency. At the same time, operations that seem obviously safe may
have a complex enough implementation internally that they cannot run
in parallel. See also @guidesecref["effective-futures"].

@deftogether[(
@defproc[(future [thunk (-> any)]) future?]
@defproc[(touch [f future?]) any]
)]{

 The @racket[future] procedure returns a future value that
 encapsulates @racket[thunk]. The @racket[touch] function forces the
 evaluation of the @racket[thunk] inside the given future, returning
 the values produced by @racket[thunk]. After @racket[touch] forces
 the evaluation of a @racket[thunk], the resulting values are retained
 by the future in place of @racket[thunk], and additional
 @racket[touch]es of the future return those values.

 Between a call to @racket[future] and @racket[touch] for a given
 future, the given @racket[thunk] may run speculatively in parallel to
 other computations, as described above.

@interaction[
#:eval future-eval
(let ([f (future (lambda () (+ 1 2)))])
  (list (+ 3 4) (touch f)))
]}
  
  
@defproc[(current-future) (or/c #f future?)]{
  Returns the descriptor for the future that is evaluating the current thunk.
  If not currently running inside a future thunk (or 
  futures are disabled), returns @racket[#f].
}


@defproc[(future? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a future value,
  @racket[#f] otherwise.
}

@defproc[(processor-count) exact-positive-integer?]{
  Returns the number of parallel computations units (e.g., processors
  or cores) that are available on the current machine.
}


@; ----------------------------------------------------------------------

@close-eval[future-eval]
