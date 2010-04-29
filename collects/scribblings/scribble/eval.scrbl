#lang scribble/doc
@(require scribble/manual
          "utils.ss"
          (for-label scheme/sandbox))

@title[#:tag "eval"]{Evaluation and Examples}

@defmodule[scribble/eval]{The @scheme[scribble/eval] library provides
utilities for evaluating code at document-build time and incorporating
the results in the document, especially to show example uses of
defined procedures and syntax.}

@defform*[[(interaction datum ...)
           (interaction #:eval eval-expr datum ...)]]{

Like @scheme[schemeinput], except that the result for each input
@scheme[datum] is shown on the next line. The result is determined by
evaluating the @scheme[quote]d form of the @scheme[datum] using the
evaluator produced by @scheme[eval-expr], if provided.

The @scheme[eval-expr] must produce a sandbox evaluator via
@scheme[make-evaluator] or @scheme[make-module-evaluator] with the
@scheme[sandbox-output] and @scheme[sandbox-error-output] parameters
set to @scheme['string]. If @scheme[eval] is not provided, an
evaluator is created using @scheme[make-base-eval]. See also
@scheme[make-eval-factory].

Uses of @scheme[code:comment] and @schemeidfont{code:blank} are
stipped from each @scheme[datum] before evaluation.

If a @scheme[datum] has the form @scheme[(@#,indexed-scheme[eval:alts]
#,(svar show-datum) #,(svar eval-datum))], then @svar[show-datum] is
typeset, while @svar[eval-datum] is evaluated.}


@defform*[[(interaction-eval datum)
           (interaction-eval #:eval eval-expr datum)]]{

Like @scheme[interaction], evaluates the @scheme[quote]d form of
@scheme[datum], but returns the empty string.}


@defform*[[(interaction-eval-show datum)
           (interaction-eval-show #:eval eval-expr datum)]]{

Like @scheme[interaction-eval], but produces an element representing
the printed form of the evaluation result.}


@defform*[[(schemeblock+eval datum ...)
           (schemeblock+eval #:eval eval-expr datum ...)]]{

Combines @scheme[schemeblock] and @scheme[interaction-eval].}


@defform*[[(schememod+eval name datum ...)
           (schememod+eval #:eval eval-expr name datum ...)]]{

Combines @scheme[schememod] and @scheme[interaction-eval].}


@defform*[[(def+int defn-datum expr-datum ...)
           (def+int #:eval eval-expr defn-datum expr-datum ...)]]{

Like @scheme[interaction], except the @scheme[defn-datum] is
typeset as for @scheme[schemeblock] (i.e., no prompt) and a line of
space is inserted before the @scheme[expr-datum]s.}


@defform*[[(defs+int (defn-datum ...) expr-datum ...)
           (defs+int #:eval eval-expr (defn-datum ...) expr-datum ...)]]{

Like @scheme[def+int], but for multiple leading definitions.}


@defform*[[(examples datum ...)
           (examples #:eval eval-expr datum ...)]]{

Like @scheme[interaction], but with an ``Examples:'' label prefixed.}


@defform*[[(defexamples datum ...)
           (defexamples #:eval eval-expr datum ...)]]{

Like @scheme[examples], but each definition using @scheme[define] or
@scheme[define-struct] among the @scheme[datum]s is typeset without a
prompt, and with line of space after it.}


@defproc[(make-base-eval) (any/c . -> . any)]{

Creates an evaluator using @scheme[(make-evaluator 'scheme/base)],
setting sandbox parameters to disable limits, set the outputs to
@scheme['string], and not add extra security guards.}


@defproc[(make-base-eval-factory [mod-paths (listof module-path?)]) (-> (any/c . -> . any))]{

Produces a function that is like @scheme[make-base-eval], except that
each module in @scheme[mod-paths] is attached to the evaluator's
namespace. The modules are loaded and instantiated once (when the
returned @scheme[make-base-eval]-like function is called the first
time) and then attached to each evaluator that is created.}


@defproc[(make-eval-factory [mod-paths (listof module-path?)]) (-> (any/c . -> . any))]{

Like @scheme[make-base-eval-factor], but each module in @scheme[mod-paths] is
also required into the top-level environment for each generated evaluator.}


@defproc[(close-eval [eval (any/c . -> . any)]) (one-of/c "")]{

Shuts down an evaluator produced by @scheme[make-base-eval]. Use
@scheme[close-eval] when garbage collection cannot otherwise reclaim
an evaluator (e.g., because it is defined in a module body).}


@defparam[scribble-eval-handler handler 
          ((any/c . -> . any) any/c boolean? . -> . any)]{

A parameter that serves as a hook for evaluation. The evaluator to use
is supplied as the first argument to the parameter's value, and the
second argument is the form to evaluate. The last argument is
@scheme[#t] if exceptions are being captured (to display exception
results), @scheme[#f] otherwise.}
