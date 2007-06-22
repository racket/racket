#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require["utils.ss"]

@title[#:tag "eval"]{Evaluation and Examples}

The @file{eval.ss} library provides utilities for evaluating code at
document-build time and incorporating the results in the document,
especially to show example uses of defined procedures and syntax.

@defform[(interaction datum ...)]{Like @scheme[schemeinput], except
that the result for each input @scheme[datum] is shown on the next
line. The result is determined by evaluating the syntax-quoted form of
the @scheme[datum].

Uses of @scheme[code:comment] and @schemeidfont{code:blank} are
stipped from each @scheme[datum] before evaluation.

If a @scheme[datum] has the form @scheme[(#,(scheme code:line)
_code-datum (#,(scheme code:comment) ...))], then only
@scheme[_code-datum] is evaluated.

If a datum has the form @scheme[(eval:alts #,(svar show-datum) #,(svar
eval-datum))], then @svar[show-datum] is typeset, while
@svar[eval-datum] is evaluated.}

@defform[(interaction-eval datum)]{Evaluates the syntax-quoted form of
each @scheme[datum] via @scheme[do-eval] and returns the empty string.}

@defform[(interaction-eval-show datum)]{Evaluates the syntax-quoted form of
@scheme[datum] and produces an element represeting the printed form of
the result.}

@defform[(schemeblock+eval datum ...)]{Combines @scheme[schemeblock]
and @scheme[interaction-eval].}

@defform[(schememod+eval name datum ...)]{Combines @scheme[schememod]
and @scheme[interaction-eval].}

@defform[(def+int defn-datum expr-datum ...)]{Like
@scheme[interaction], except the the @scheme[defn-datum] is typeset as
for @scheme[schemeblock] (i.e., no prompt) with a line of space
between the definition and the interactions.}

@defform[(defs+int (defn-datum ...) expr-datum ...)]{Like
@scheme[def+int], but for multiple leading definitions.}

@defform[(examples datum ...)]{Like @scheme[interaction], but with an
``Examples:'' label prefixed.}

@defform[(defexamples datum ...)]{Like @scheme[examples], but each
definition using @scheme[define] among the @scheme[datum]s is typeset
without a prompt, and with space after it.}

@defthing[current-int-namespace parameter?]{A parameter to hold the
namespace used by @scheme[interaction], etc.}
