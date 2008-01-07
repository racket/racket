#lang scribble/doc
@require[scribble/manual]
@require[scribble/eval]
@require["guide-utils.ss"]
@require["contracts-utils.ss"]
@(require (for-label scheme/contract))

@title{Gotchas}

@question{What about @scheme[set!] on variables provided via @scheme[provide/contract]?}

The contract library assumes that variables exported via
@scheme[provide/contract] are not assigned to, but does not enforce
it. Accordingly, if you try to @scheme[set!] those variables, you may
find unexpected behavior. Consider the following example:

@interaction[
(module server scheme
  (define (inc-x!) (set! x (+ x 1)))
  (define x 0)
  (provide/contract [inc-x! (-> void?)]
                    [x integer?]))

(module client scheme
  (require 'server)

  (define (print-latest) (printf "x is ~s\n" x))

  (print-latest)
  (inc-x!)
  (print-latest))

(require 'client)
]

Both calls to @scheme[print-latest] print @scheme[0], even though the
value of @scheme[x] has been incremented (and the change is visible
inside the module @scheme[x]).

To work around this, export accessor functions, rather than
exporting the function directly, like this:

@schememod[
scheme

(define (get-x) x)
(define (inc-x!) (set! x (+ x 1)))
(define x 0)
(provide/contract [inc-x! (-> void?)]
                  [get-x (-> integer?)])
]

This is a bug we hope to address in a future release.
