#lang scribble/doc
@(require scribble/manual
          "utils.ss"
          (for-label racket/sandbox))

@title[#:tag "eval"]{Evaluation and Examples}

@defmodule[scribble/eval]{The @racket[scribble/eval] library provides
utilities for evaluating code at document-build time and incorporating
the results in the document, especially to show example uses of
defined procedures and syntax.}

@defform*[[(interaction datum ...)
           (interaction #:eval eval-expr datum ...)]]{

Like @racket[racketinput], except that the result for each input
@racket[datum] is shown on the next line. The result is determined by
evaluating the @racket[quote]d form of the @racket[datum] using the
evaluator produced by @racket[eval-expr], if provided.

The @racket[eval-expr] must produce a sandbox evaluator via
@racket[make-evaluator] or @racket[make-module-evaluator] with the
@racket[sandbox-output] and @racket[sandbox-error-output] parameters
set to @racket['string]. If @racket[eval] is not provided, an
evaluator is created using @racket[make-base-eval]. See also
@racket[make-eval-factory].

If the value of @racket[current-print] in the sandbox is changed from
its default value, or if @racket[print-as-expression] in the sandbox
is set to @racket[#f], then each evaluation result is formatted to a
string by applying @racket[(current-print)] to the value (with the
output port set to a string port). Otherwise, result values are
typeset using @racket[to-element/no-color].

Uses of @racket[code:comment] and @racketidfont{code:blank} are
stipped from each @racket[datum] before evaluation.

If a @racket[datum] has the form @racket[(@#,indexed-racket[eval:alts]
#,(svar show-datum) #,(svar eval-datum))], then @svar[show-datum] is
typeset, while @svar[eval-datum] is evaluated.

If a @racket[datum] has the form
@racket[(@#,indexed-racket[eval:check] #,(svar eval-datum) #,(svar
expect-datum))], then both @svar[eval-datum] and @svar[check-datum]
are evaluated, and an error is raised if they are not @racket[equal?].}


@defform*[[(interaction-eval datum)
           (interaction-eval #:eval eval-expr datum)]]{

Like @racket[interaction], evaluates the @racket[quote]d form of
@racket[datum], but returns the empty string.}


@defform*[[(interaction-eval-show datum)
           (interaction-eval-show #:eval eval-expr datum)]]{

Like @racket[interaction-eval], but produces an element representing
the printed form of the evaluation result.}


@defform*[[(racketblock+eval datum ...)
           (racketblock+eval #:eval eval-expr datum ...)]]{

Combines @racket[racketblock] and @racket[interaction-eval].}


@defform*[[(racketmod+eval name datum ...)
           (racketmod+eval #:eval eval-expr name datum ...)]]{

Combines @racket[racketmod] and @racket[interaction-eval].}


@defform*[[(def+int defn-datum expr-datum ...)
           (def+int #:eval eval-expr defn-datum expr-datum ...)]]{

Like @racket[interaction], except the @racket[defn-datum] is
typeset as for @racket[racketblock] (i.e., no prompt) and a line of
space is inserted before the @racket[expr-datum]s.}


@defform*[[(defs+int (defn-datum ...) expr-datum ...)
           (defs+int #:eval eval-expr (defn-datum ...) expr-datum ...)]]{

Like @racket[def+int], but for multiple leading definitions.}


@defform*[[(examples datum ...)
           (examples #:eval eval-expr datum ...)]]{

Like @racket[interaction], but with an ``Examples:'' label prefixed.}


@defform*[[(defexamples datum ...)
           (defexamples #:eval eval-expr datum ...)]]{

Like @racket[examples], but each definition using @racket[define] or
@racket[define-struct] among the @racket[datum]s is typeset without a
prompt, and with line of space after it.}


@defproc[(make-base-eval) (any/c . -> . any)]{

Creates an evaluator using @racket[(make-evaluator 'racket/base)],
setting sandbox parameters to disable limits, setting the outputs to
@racket['string], and not adding extra security guards.}


@defproc[(make-base-eval-factory [mod-paths (listof module-path?)]) (-> (any/c . -> . any))]{

Produces a function that is like @racket[make-base-eval], except that
each module in @racket[mod-paths] is attached to the evaluator's
namespace. The modules are loaded and instantiated once (when the
returned @racket[make-base-eval]-like function is called the first
time) and then attached to each evaluator that is created.}


@defproc[(make-eval-factory [mod-paths (listof module-path?)]) (-> (any/c . -> . any))]{

Like @racket[make-base-eval-factory], but each module in @racket[mod-paths] is
also required into the top-level environment for each generated evaluator.}


@defproc[(close-eval [eval (any/c . -> . any)]) (one-of/c "")]{

Shuts down an evaluator produced by @racket[make-base-eval]. Use
@racket[close-eval] when garbage collection cannot otherwise reclaim
an evaluator (e.g., because it is defined in a module body).}


@defparam[scribble-eval-handler handler 
          ((any/c . -> . any) any/c boolean? . -> . any)]{

A parameter that serves as a hook for evaluation. The evaluator to use
is supplied as the first argument to the parameter's value, and the
second argument is the form to evaluate. The last argument is
@racket[#t] if exceptions are being captured (to display exception
results), @racket[#f] otherwise.}
