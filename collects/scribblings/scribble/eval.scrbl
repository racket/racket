#lang scribble/doc
@(require scribble/manual "utils.rkt" (for-label racket/sandbox racket/pretty))

@title[#:tag "eval"]{Evaluation and Examples}

@defmodule[scribble/eval]{The @racket[scribble/eval] library provides
utilities for evaluating code at document-build time and incorporating
the results in the document, especially to show example uses of
defined procedures and syntax.}

@defform/subs[(interaction maybe-eval maybe-escape datum ...)
              ([maybe-eval code:blank
                            (code:line #:eval eval-expr)]
               [maybe-escape code:blank
                            (code:line #:escape escape-id)])]{

Like @racket[racketinput], except that the result for each input
@racket[datum] is shown on the next line. The result is determined by
evaluating the @racket[quote]d form of the @racket[datum] using the
evaluator produced by @racket[eval-expr], if provided.

The @racket[eval-expr] must produce a sandbox evaluator via
@racket[make-evaluator] or @racket[make-module-evaluator] with the
@racket[sandbox-output] and @racket[sandbox-error-output] parameters
set to @racket['string]. If @racket[eval-expr] is not provided, an
evaluator is created using @racket[make-base-eval]. See also
@racket[make-eval-factory].

If the value of @racket[current-print] in the sandbox is changed from
its default value, or if @racket[print-as-expression] in the sandbox
is set to @racket[#f], then each evaluation result is formatted to a
port by applying @racket[(current-print)] to the value; the output
port is set to a pipe that supports specials in the sense of
@racket[write-special], and non-character values written to the port
are used as @tech{content}. Otherwise, when the default
@racket[current-print] is in place, result values are typeset using
@racket[to-element/no-color].

Certain patterns in @racket[datum] are treated specially:

@itemlist[

 @item{A @racket[datum] of the form 
       @racket[(@#,indexed-racket[code:line] _code-datum (@#,racketidfont{code:comment} _comment-datum ...))]
       is treated as @racket[_code-datum] for evaluation.}

 @item{Other uses of @racketidfont{code:comment} and
       @racketidfont{code:blank} are stripped from each @racket[datum]
       before evaluation.}

 @item{A @racket[datum] of the form 
       @racket[(@#,indexed-racket[eval:alts] #,(svar show-datum) #,(svar eval-datum))]
       is treated as @svar[show-datum] for typesetting and @svar[eval-datum] for evaluation.}

 @item{A @racket[datum] of the form 
       @racket[(@#,indexed-racket[eval:check] #,(svar eval-datum) #,(svar expect-datum))]
       is treated like @racket[_eval-datum], but @svar[check-datum] is also
       evaluated, and an error is raised if they are not @racket[equal?].}

 @item{A @racket[datum] of the form 
       @racket[(@#,indexed-racket[eval:result] _content-expr _out-expr _err-expr)]
       involves no sandboxed evaluation; instead, the @tech{content} result of @racket[_content-expr] is used as the
       typeset form of the result, @racket[_out-expr] is treated as output printed
       by the expression, and @racket[_err-expr] is error output printed by the
       expression. The @racket[_out-expr] and/or @racket[_err-expr] can be omitted,
       in which case they default to empty strings.

       Normally, @racketidfont{eval:result}
       is used in the second part of an @racketidfont{eval:alts} combination.}

 @item{A @racket[datum] of the form 
       @racket[(@#,indexed-racket[eval:results] _content-list-expr _out-expr _err-expr)]
       is treated like an @racketidfont{eval:result} form, except that @racket[_content-list-expr]
       should produce a list of @tech{content} for multiple results of evaluation. As
       with @racketidfont{eval:result}, @racket[_out-expr] and @racket[_err-expr] are optional.}

]

As an example,

@codeblock|{
#lang scribble/manual
@(require racket/sandbox
          scribble/eval)
@(define my-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'typed/racket/base)))
@interaction[#:eval my-evaluator

             (: my-sqr (Real -> Real))
             (define (my-sqr x)
               (* x x))
             (my-sqr 42)]
}|

uses an evaluator whose language is @racketmodname[typed/racket/base].}

@defform[(interaction0 maybe-eval maybe-escape datum ...)]{
Like @racket[interaction], but without insetting the code via
@racket[nested].}

@defform[(interaction-eval maybe-eval maybe-escape datum)]{

Like @racket[interaction], evaluates the @racket[quote]d form of
@racket[datum], but returns the empty string.}


@defform[(interaction-eval-show maybe-eval maybe-escape datum)]{

Like @racket[interaction-eval], but produces an element representing
the printed form of the evaluation result.}


@defform[(racketblock+eval maybe-eval maybe-escape datum ...)]{

Combines @racket[racketblock] and @racket[interaction-eval].}


@defform[(racketblock0+eval maybe-eval maybe-escape datum ...)]{

Combines @racket[racketblock0] and @racket[interaction-eval].}


@defform[(racketmod+eval maybe-eval maybe-escape name datum ...)]{

Combines @racket[racketmod] and @racket[interaction-eval].}


@defform[(def+int maybe-eval maybe-escape defn-datum expr-datum ...)]{

Like @racket[interaction], except the @racket[defn-datum] is
typeset as for @racket[racketblock] (i.e., no prompt) and a line of
space is inserted before the @racket[expr-datum]s.}


@defform[(defs+int maybe-eval maybe-escape (defn-datum ...) expr-datum ...)]{

Like @racket[def+int], but for multiple leading definitions.}


@defform[(examples maybe-eval maybe-escape datum ...)]{

Like @racket[interaction], but with an ``Examples:'' label prefixed.}


@defform[(defexamples maybe-eval maybe-escape datum ...)]{

Like @racket[examples], but each definition using @racket[define] or
@racket[define-struct] among the @racket[datum]s is typeset without a
prompt, and with line of space after it.}


@defproc[(make-base-eval [#:pretty-print? pretty-print? any/c #t])
         (any/c . -> . any)]{

Creates an evaluator using @racket[(make-evaluator 'racket/base)],
setting sandbox parameters to disable limits, setting the outputs to
@racket['string], and not adding extra security guards.

If @racket[pretty-print?] is true, the sandbox's printer is set to
@racket[pretty-print-handler].}


@defproc[(make-base-eval-factory [mod-paths (listof module-path?)]
                                 [#:pretty-print? pretty-print? any/c #t])
         (-> (any/c . -> . any))]{

Produces a function that is like @racket[make-base-eval], except that
each module in @racket[mod-paths] is attached to the evaluator's
namespace. The modules are loaded and instantiated once (when the
returned @racket[make-base-eval]-like function is called the first
time) and then attached to each evaluator that is created.}


@defproc[(make-eval-factory [mod-paths (listof module-path?)]
                            [#:pretty-print? pretty-print? any/c #t])
         (-> (any/c . -> . any))]{

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
