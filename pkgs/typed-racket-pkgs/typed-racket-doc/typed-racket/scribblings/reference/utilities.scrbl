#lang scribble/manual

@begin[(require "../utils.rkt" scribble/eval racket/sandbox)
       (require (for-label (only-meta-in 0 [except-in typed/racket for])
                           typed/untyped-utils))]

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

@section{Untyped Utilities}

@defmodule[typed/untyped-utils]

These utilities help interface typed with untyped code, particularly typed
libraries that use types that cannot be converted into contracts, or export
syntax transformers that must expand differently in typed and untyped contexts.

@defform*/subs[[(require/untyped-contract maybe-begin module [name subtype] ...)]
               ([maybe-begin code:blank (code:line (begin expr ...))])]{
Use this form to import typed identifiers whose types cannot be converted into
contracts, but have @emph{subtypes} that can be converted into contracts.

For example, suppose we define and provide the Typed Racket function
@racketblock[(: negate (case-> (-> Index Fixnum)
                               (-> Integer Integer)))
             (define (negate x) (- x))]
Trying to use @racket[negate] within an untyped module will raise an error
because the cases cannot be distinguished by arity alone.

If the defining module for @racket[negate] is @racket["my-numerics.rkt"],
it can be imported and used in untyped code this way:
@racketblock[(require/untyped-contract
              "my-numerics.rkt"
              [negate  (-> Integer Integer)])]
The type @racket[(-> Integer Integer)] is converted into the contract used
for @racket[negate].

The @racket[require/untyped-contract] form expands into a submodule
with language @racketmodname[typed/racket/base]. Identifiers used in
@racket[subtype] expressions must be either in Typed Racket's base type
environment (e.g. @racket[Integer] and @racket[Listof]) or defined by an
expression in the @racket[maybe-begin] form, which is spliced into the
submodule. For example, the @racketmodname[math/matrix #:indirect] module imports and
reexports @racket[matrix-expt], which has a @racket[case->] type,
for untyped use in this way:
@racketblock[(provide matrix-expt)
             
             (require/untyped-contract
              (begin (require "private/matrix/matrix-types.rkt"))
              "private/matrix/matrix-expt.rkt"
              [matrix-expt  ((Matrix Number) Integer -> (Matrix Number))])]
The @racket[(require "private/matrix/matrix-types.rkt")] expression imports the
@racket[Matrix] type.

If an identifier @racket[name] is imported using @racket[require/untyped-contract],
reexported, and imported into typed code, it has its original type, not
@racket[subtype]. In other words, @racket[subtype] is used only to generate
a contract for @racket[name], not to narrow its type.

Because of limitations in the macro expander, @racket[require/untyped-contract]
cannot currently be used in typed code.
}

@defform[(define-typed/untyped-identifier name typed-name untyped-name)]{
Defines an identifier @racket[name] that expands to @racket[typed-name] in typed
contexts and to @racket[untyped-name] in untyped contexts. Each subform must be
an identifier.

Suppose we define and provide a Typed Racket function with this type:
@racketblock[(: my-filter (All (a) (-> (-> Any Any : a) (Listof Any) (Listof a))))]
This type cannot be converted into a contract because it accepts a predicate.
Worse, @racket[require/untyped-contract] does not help because
@racket[(All (a) (-> (-> Any Any) (Listof Any) (Listof a)))] is not a subtype.

In this case, we might still provide @racket[my-filter] to untyped code using
@racketblock[(provide my-filter)
             
             (define-typed/untyped-identifier my-filter
               typed:my-filter
               untyped:my-filter)]
where @racket[typed:my-filter] is the original @racket[my-filter], but imported
using @racket[prefix-in], and @racket[untyped:my-filter] is either a Typed Racket
implementation of it with type @racket[(All (a) (-> (-> Any Any) (Listof Any) (Listof a)))]
or an untyped Racket implementation.

Avoid this if possible. Use only in cases where a type has no subtype that can
be converted to a contract; i.e. cases in which @racket[require/untyped-contract]
cannot be used.
}

@defproc[(syntax-local-typed-context?) boolean?]{
Returns @racket[#t] if called while expanding code in a typed context; otherwise
@racket[#f].

This is the nuclear option, provided because it is sometimes, but rarely, useful.
Avoid.
}

@(close-eval the-eval)
@(close-eval the-top-eval)
