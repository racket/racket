#lang scribble/manual
@(require scribble/eval "utils.rkt"
          (for-label racket unstable/debug unstable/syntax))

@title{Debugging}

@defmodule[unstable/debug]

@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

This module provides macros and functions for printing out debugging
information.

@defform/subs[
(debug options ... expr)
([options (code:line #:name name-expr)
          (code:line #:source srcloc-expr)])
]{

Writes debugging information about the evaluation of @scheme[expr] to the
current error port.  The name and source location of the expression may be
overridden by keyword options; their defaults are the syntactic form of the
expression and its syntactic source location, respectively.

@examples[#:eval (eval/require 'unstable/debug)
(debug 0)
(debug #:name "one, two, three" (values 1 2 3))
(debug #:source (make-srcloc 'here 1 2 3 4)
  (error 'function "something went wrong"))
]

}

@defproc[(dprintf [fmt string?] [arg any/c] ...) void?]{

Constructs a message in the same manner as @scheme[format] and writes it to
@scheme[(current-error-port)], with indentation reflecting the number of nested
@scheme[debug] forms.

@examples[#:eval (eval/require 'unstable/debug)
(dprintf "level: ~a" 0)
(debug (dprintf "level: ~a" 1))
(debug (debug (dprintf "level: ~a" 2)))
]

}

@defform/subs[
(debugf function-expr argument ...)
([argument argument-expr (code:line argument-keyword argument-expr)])
]{

Logs debugging information for @scheme[(#%app function-expr argument ...)],
including the evaluation and results of the function and each argument.

@examples[#:eval (eval/require 'unstable/debug)
(debugf + 1 2 3)
]

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

These macros add logging based on @scheme[debug] to the evaluation of
expressions in @scheme[begin], @scheme[define], @scheme[define/private],
@scheme[define/public], @scheme[define/override], @scheme[define/augment],
@scheme[let], @scheme[let*], @scheme[letrec], @scheme[let-values],
@scheme[let*-values], @scheme[letrec-values], @scheme[with-syntax],
@scheme[with-syntax*], and @scheme[parameterize].

}
