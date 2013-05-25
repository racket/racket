#lang scribble/manual

@begin[(require "../utils.rkt" scribble/eval racket/sandbox)
       (require (for-label (only-meta-in 0 [except-in typed/racket for])))]

@(define the-eval (make-base-eval))
@(the-eval '(require (except-in typed/racket #%top-interaction #%module-begin)))
@(define the-top-eval (make-base-eval))
@(the-top-eval '(require (except-in typed/racket #%module-begin)))

@title{Utilities}

Typed Racket provides some additional utility functions to facilitate typed programming.

@defproc*[
([(assert [v (U #f A)]) A]
 [(assert [v A] [p? (A -> Any : B)]) B])]{
Verifies that the argument satisfies the constraint.  If no predicate
is provided, simply checks that the value is not
@racket[#f].

See also the @racket[cast] form.
}

@examples[#:eval the-top-eval
(define: x : (U #f String) (number->string 7))
x
(assert x)
(define: y : (U String Symbol) "hello")
y
(assert y string?)
(assert y boolean?)]

@defform*/subs[[(with-asserts ([id maybe-pred] ...) body ...+)]
              ([maybe-pred code:blank
                           (code:line predicate)])]{
Guard the body with assertions. If any of the assertions fail, the
program errors. These assertions behave like @racket[assert].
}


@defproc[(defined? [v any/c]) boolean?]{A predicate for determining if
@racket[v] is @emph{not} @|undefined-const|.}

@defproc[(index? [v any/c]) boolean?]{A predicate for the @racket[Index]
type.}

@defform*/subs[[(typecheck-fail orig-stx maybe-msg maybe-id)]
               ([maybe-msg code:blank (code:line msg-string)]
	        [maybe-id code:blank (code:line #:covered-id id)])]{
Explicitly produce a type error, with the source location or
@racket[orig-stx].  If @racket[msg-string] is present, it must be a literal string, it is used as
the error message, otherwise the error message 
@racket["Incomplete case coverage"] is used.
If @racket[id] is present and has 
type @racket[T], then the message @racket["missing coverage of T"] is added to
the error message.   

@examples[#:eval the-top-eval #:escape UNSYNTAX
(define-syntax (cond* stx)
  (syntax-case stx ()
    [(_ x clause ...)
     #`(cond clause ... [else (typecheck-fail #,stx "incomplete coverage"
                                              #:covered-id x)])]))

(define: (f [x  : (U String Integer)]) : Boolean
  (cond* x
         [(string? x) #t]
         [(exact-nonnegative-integer? x) #f]))
]

}

@(close-eval the-eval)
@(close-eval the-top-eval)
