#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../scribble.ss"
          "eval.ss")
@(require (for-label scheme unstable/cce/debug unstable/syntax))

@title[#:style 'quiet #:tag "cce-debug"]{Debugging}

@defmodule[unstable/cce/debug]

This module provides macros and functions for printing out debugging
information.

@defform[(debug expr)]{

Logs debugging information before and after the evaluation of expression
@scheme[expr].

}

@defform/subs[
(with-debugging options ... expr)
([options (code:line #:name name-expr)
          (code:line #:source srcloc-expr)])
]{

Logs debugging information like @scheme[debug], with the option of explicitly
overriding the name and source location information for the expression.

}

@defproc[(dprintf [fmt string?] [arg any/c] ...) void?]{

Constructs a message in the same manner as @scheme[format], and logs it at the
debugging priority (like @scheme[log-debug]).

}

@deftogether[(
@defform[(begin/debug expr ...)]
@defform*[[(define/debug id expr)
           (define/debug (head args) body ...+)]]
@defform*[[(define/private/debug id expr)
           (define/private/debug (head args) body ...+)]]
@defform*[[(define/public/debug id expr)
           (define/public/debug (head args) body ...+)]]
@defform*[[(define/override/debug id expr)
           (define/override/debug (head args) body ...+)]]
@defform*[[(define/augment/debug id expr)
           (define/augment/debug (head args) body ...+)]]
@defform*[[(let/debug ([lhs-id rhs-expr] ...) body ...+)
           (let/debug loop-id ([lhs-id rhs-expr] ...) body ...+)]]
@defform[(let*/debug ([lhs-id rhs-expr] ...) body ...+)]
@defform[(letrec/debug ([lhs-id rhs-expr] ...) body ...+)]
@defform[(let-values/debug ([(lhs-id ...) rhs-expr] ...) body ...+)]
@defform[(let*-values/debug ([(lhs-id ...) rhs-expr] ...) body ...+)]
@defform[(letrec-values/debug ([(lhs-id ...) rhs-expr] ...) body ...+)]
@defform[(with-syntax/debug ([pattern stx-expr] ...) body ...+)]
@defform[(with-syntax*/debug ([pattern stx-expr] ...) body ...+)]
@defform[(parameterize/debug ([param-expr value-expr] ...) body ...+)]
)]{

These macros add logging based on @scheme[with-debugging] to the evaluation of
expressions in @scheme[begin], @scheme[define], @scheme[define/private],
@scheme[define/public], @scheme[define/override], @scheme[define/augment],
@scheme[let], @scheme[let*], @scheme[letrec], @scheme[let-values],
@scheme[let*-values], @scheme[letrec-values], @scheme[with-syntax],
@scheme[with-syntax*], and @scheme[parameterize].

}
