#lang scribble/doc
@(require "mz.rkt"
          (for-label racket/keyword))

@title[#:tag "keywords"]{Keywords}

@guideintro["keywords"]{keywords}

A @deftech{keyword} is like an @tech{interned} symbol, but its printed
form starts with @litchar{#:}, and a keyword cannot be used as an
identifier. Furthermore, a keyword by itself is not a valid
expression, though a keyword can be @racket[quote]d to form an
expression that produces the symbol.

Two keywords are @racket[eq?] if and only if they print the same
(i.e., keywords are always @tech{interned}).

Like symbols, keywords are only weakly held by the internal keyword
table; see @secref["symbols"] for more information.

@see-read-print["keyword"]{keywords}

@defproc[(keyword? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a keyword, @racket[#f] otherwise.

@mz-examples[(keyword? '#:apple)
             (keyword? 'define)
             (keyword? '#:define)]}


@defproc[(keyword->string [keyword keyword?]) string?]{

Returns a string for the @racket[display]ed form of @racket[keyword],
not including the leading @litchar{#:}.

See also @racket[keyword->immutable-string] from
@racketmodname[racket/keyword].

@mz-examples[(keyword->string '#:apple)]}


@defproc[(string->keyword [str string?]) keyword?]{

Returns a keyword whose @racket[display]ed form is the same as that of
@racket[str], but with a leading @litchar{#:}.

@mz-examples[(string->keyword "apple")]}


@defproc[(keyword<? [a-keyword keyword?] [b-keyword keyword?] ...) boolean?]{

Returns @racket[#t] if the arguments are sorted, where the comparison
for each pair of keywords is the same as using
@racket[keyword->string] with @racket[string->bytes/utf-8] and
@racket[bytes<?].

@mz-examples[(keyword<? '#:apple '#:banana)]

@history/arity[]}


@; ----------------------------------------
@section{Additional Keyword Functions}

@note-lib-only[racket/keyword]
@(define keyword-eval (make-base-eval))
@examples[#:hidden #:eval keyword-eval (require racket/keyword)]

@history[#:added "7.6"]

@defproc[(keyword->immutable-string [sym keyword?]) (and/c string? immutable?)]{

Like @racket[keyword->string], but the result is an immutable string,
not necessarily freshly allocated.

@examples[#:eval keyword-eval
          (keyword->immutable-string '#:apple)
          (immutable? (keyword->immutable-string '#:apple))]

@history[#:added "7.6"]}

@; ----------------------------------------
@close-eval[keyword-eval]
